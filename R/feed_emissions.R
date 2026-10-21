#' Calculate Feed Environmental Footprint (GHG Emissions and Water Use)
#'
#' Computes greenhouse gas emissions (total, fossil, and land-use change / deforestation)
#' and water deprivation footprint (m3) resulting from animal feed consumption
#' based on the INRAE/ADEME ECOALIM V9 Life Cycle Assessment (EF 3.1) database.
#'
#' @param automatic_cycle Logical. If TRUE, uses the built-in model for automatic farm cycle calculation. Default is FALSE.
#' @param saveoutput Logical. If TRUE (default), results are saved to output folder.
#' @param data_dir Path to the directory containing input CSV/data files. Defaults to `"user_data"`.
#' @param DMI_df Optional precalculated DMI dataframe.
#' @param population_df Optional precalculated population dataframe.
#' @return Tibble containing feed environmental footprints by animal category,
#'   including total GHG (kg CO2e), fossil GHG, LUC GHG, and water consumption (m3).
#' @export
calculate_feed_emissions <- function(automatic_cycle = FALSE,
                                     saveoutput = TRUE,
                                     data_dir = "user_data",
                                     DMI_df = NULL,
                                     population_df = NULL) {

  message("\U0001f33e Calculating feed environmental footprint (GHG & water)...")

  # --- 1. Load Reference Datasets ---
  ecoalim_path <- herdr_get_ecoalim_path(data_dir)
  if (is.null(ecoalim_path) || !file.exists(ecoalim_path)) {
    stop("Could not locate ecoalim.parquet in '", data_dir, "' or package extdata.")
  }
  ecoalim <- arrow::read_parquet(ecoalim_path)

  mapping_path <- file.path(data_dir, "mapping.csv")
  if (!file.exists(mapping_path)) {
    stop("Could not locate mapping.csv in '", data_dir, "'.")
  }
  name_mapping <- readr::read_csv(mapping_path, show_col_types = FALSE)

  feed_chars_path <- file.path(data_dir, "feed_characteristics.csv")
  if (!file.exists(feed_chars_path)) {
    stop("Could not locate feed_characteristics.csv in '", data_dir, "'.")
  }
  feed_chars_raw <- readr::read_csv(feed_chars_path, show_col_types = FALSE)

  # --- 2. Clean Reference Tables ---
  feed_chars <- feed_chars_raw %>%
    dplyr::filter(!is.na(ingredient)) %>%
    dplyr::mutate(DM_pct = suppressWarnings(as.numeric(DM_pct))) %>%
    dplyr::select(ingredient, DM_pct) %>%
    dplyr::distinct(ingredient, .keep_all = TRUE)

  mapping_clean <- name_mapping %>%
    dplyr::select(ingredient, agribalyse_name) %>%
    dplyr::distinct(ingredient, .keep_all = TRUE)

  ecoalim_clean <- ecoalim %>%
    dplyr::select(
      agribalyse_name = `Simapro LCI name`,
      ecoalim_feedstuff = Feedstuff,
      ecoalim_category = `Types of feedstuffs for livestock`,
      ef_climate_total = `Climate change - EF 3.1 (kg CO2 eq)`,
      ef_climate_fossil = `Climate change - Fossil (kg CO2 eq/kg)`,
      ef_climate_luc = `Climate change - Land use and LU change (kg CO2 eq/kg)`,
      ef_water = `Water use (m3 depriv.)`
    )

  # --- 3. Load Upstream Operational Data ---
  if (is.null(DMI_df)) {
    DMI_df <- suppressMessages(calculate_DMI(saveoutput = FALSE, data_dir = data_dir))
  }
  if (nrow(DMI_df) == 0) {
    message("\u26a0 No DMI data found. Returning empty structure.")
    return(dplyr::tibble())
  }
  DMI_df <- DMI_df %>%
    dplyr::distinct(region, diet_tag, subregion, animal_tag, class_flex, .keep_all = TRUE)

  if (is.null(population_df)) {
    population_df <- suppressMessages(calculate_population(automatic_cycle = automatic_cycle, saveoutput = FALSE, data_dir = data_dir))
  }

  diet_profiles <- readr::read_csv(file.path(data_dir, "diet_profiles.csv"), show_col_types = FALSE)
  diet_ingredients_raw <- readr::read_csv(file.path(data_dir, "diet_ingredients.csv"), show_col_types = FALSE)

  legacy_diet_mode <- any(c("region", "subregion", "class_flex") %in% names(diet_profiles)) &&
                      any(duplicated(diet_profiles$diet_tag))

  if (!legacy_diet_mode) {
    diet_profiles <- diet_profiles %>%
      dplyr::select(-dplyr::any_of(c("region", "subregion", "class_flex"))) %>%
      dplyr::distinct(diet_tag, .keep_all = TRUE)
    diet_ingredients_raw <- diet_ingredients_raw %>%
      dplyr::select(-dplyr::any_of(c("region", "subregion", "class_flex")))
    diet_join_keys <- "diet_tag"
  } else {
    diet_profiles <- diet_profiles %>%
      dplyr::distinct(diet_tag, region, subregion, class_flex, .keep_all = TRUE)
    diet_join_keys <- intersect(c("region", "subregion", "class_flex", "diet_tag"), names(diet_profiles))
  }

  # --- 4. Merge Diets, Characteristics and ECOALIM Factors ---
  joined <- DMI_df %>%
    dplyr::inner_join(diet_profiles, by = diet_join_keys) %>%
    dplyr::inner_join(diet_ingredients_raw, by = diet_join_keys, relationship = "many-to-many") %>%
    dplyr::left_join(feed_chars, by = "ingredient") %>%
    dplyr::left_join(mapping_clean, by = "ingredient") %>%
    dplyr::left_join(ecoalim_clean, by = "agribalyse_name")

  # Detect consumed ingredients without matching emission factor
  missing_factors <- joined %>%
    dplyr::filter(as.numeric(ingredient_share) > 0, is.na(ef_climate_total)) %>%
    dplyr::select(ingredient) %>%
    dplyr::distinct()

  if (nrow(missing_factors) > 0) {
    warning(
      "\u26A0 [herdr] Feed emissions disclaimer: No ECOALIM emission factor found for: ",
      paste(missing_factors$ingredient, collapse = ", "),
      ". Set their footprints to 0. Please map them in 'mapping.csv' if available.",
      call. = FALSE
    )
  }

  # --- 5. Calculate Feed Consumption and Footprints ---
  calc <- joined %>%
    dplyr::mutate(
      DM_pct = dplyr::case_when(
        is.na(DM_pct) | DM_pct <= 0 ~ 100,
        TRUE ~ DM_pct
      ),
      share_factor = dplyr::case_when(
        ingredient_type == "forage"        ~ forage_share / 100,
        ingredient_type == "concentrate"   ~ concentrate_share / 100,
        ingredient_type == "milk"          ~ milk_share / 100,
        ingredient_type == "milk_replacer" ~ milk_replacer_share / 100,
        TRUE ~ 0
      ),
      annual_dmi_kg_head = (DMI_kgday * 365) * share_factor * (as.numeric(ingredient_share) / 100),
      annual_fresh_kg_head = annual_dmi_kg_head / (DM_pct / 100)
    )

  # Join population safely
  pop_join_keys <- intersect(names(DMI_df), names(population_df))
  pop_join_keys <- pop_join_keys[!pop_join_keys %in% c("population", "n_heads", "n_standing_places")]

  pop_num_col <- if ("population" %in% names(population_df)) {
    "population"
  } else if ("n_heads" %in% names(population_df)) {
    "n_heads"
  } else {
    names(population_df)[which(sapply(population_df, is.numeric))[1]]
  }

  calc <- calc %>%
    dplyr::left_join(
      population_df %>% dplyr::select(dplyr::all_of(pop_join_keys), population = dplyr::all_of(pop_num_col)),
      by = pop_join_keys
    ) %>%
    dplyr::mutate(
      annual_dmi_kg_herd = annual_dmi_kg_head * population,
      annual_fresh_kg_herd = annual_fresh_kg_head * population,
      feed_CO2e_head_kg = annual_fresh_kg_head * dplyr::coalesce(ef_climate_total, 0),
      feed_CO2e_herd_kg = annual_fresh_kg_herd * dplyr::coalesce(ef_climate_total, 0),
      feed_CO2e_fossil_herd_kg = annual_fresh_kg_herd * dplyr::coalesce(ef_climate_fossil, 0),
      feed_CO2e_luc_herd_kg = annual_fresh_kg_herd * dplyr::coalesce(ef_climate_luc, 0),
      feed_water_herd_m3 = annual_fresh_kg_herd * dplyr::coalesce(ef_water, 0)
    )

  # --- 6. Aggregate Summary by Animal Tag ---
  summary_by_animal <- calc %>%
    dplyr::group_by(region, subregion, animal_tag, class_flex, animal_type, animal_subtype, population) %>%
    dplyr::summarise(
      feed_dmi_kg_year = sum(annual_dmi_kg_herd, na.rm = TRUE),
      feed_fresh_kg_year = sum(annual_fresh_kg_herd, na.rm = TRUE),
      feed_CO2e_total_kg_year = sum(feed_CO2e_herd_kg, na.rm = TRUE),
      feed_CO2e_fossil_kg_year = sum(feed_CO2e_fossil_herd_kg, na.rm = TRUE),
      feed_CO2e_luc_kg_year = sum(feed_CO2e_luc_herd_kg, na.rm = TRUE),
      feed_water_m3_year = sum(feed_water_herd_m3, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      feed_CO2e_per_head_year = dplyr::if_else(population > 0, feed_CO2e_total_kg_year / population, 0),
      feed_water_per_head_year = dplyr::if_else(population > 0, feed_water_m3_year / population, 0)
    )

  # --- 7. Save Output Reports if Requested ---
  if (saveoutput) {
    if (!dir.exists("output")) dir.create("output", recursive = TRUE)

    # Detailed ingredient breakdown
    ingredient_report <- calc %>%
      dplyr::select(
        region, subregion, animal_tag, class_flex, animal_type, animal_subtype, population,
        ingredient_type, ingredient, agribalyse_name, ecoalim_feedstuff,
        ingredient_share_pct = ingredient_share, DM_pct,
        annual_dmi_kg_head, annual_fresh_kg_head,
        annual_fresh_kg_herd,
        ef_climate_total_kgCO2e_kg = ef_climate_total,
        ef_climate_fossil_kgCO2e_kg = ef_climate_fossil,
        ef_climate_luc_kgCO2e_kg = ef_climate_luc,
        ef_water_m3_kg = ef_water,
        feed_CO2e_herd_kg, feed_CO2e_fossil_herd_kg, feed_CO2e_luc_herd_kg, feed_water_herd_m3
      )

    readr::write_csv(ingredient_report, "output/feed_emissions_by_ingredient.csv")
    readr::write_csv(summary_by_animal, "output/feed_emissions_summary.csv")
    message("\U0001f4be Feed emission reports saved to 'output/feed_emissions_summary.csv' and 'output/feed_emissions_by_ingredient.csv'")
  }

  summary_by_animal
}
