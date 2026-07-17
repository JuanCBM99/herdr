#' Calculate land use
#'
#' Computes total land use (m2) per animal based on validated DMI and specific origin countries.
#' @param automatic_cycle Logical. If TRUE, uses the built-in model for automatic farm cycle calculation. Default is FALSE.
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @param farm_country Character. The country of the farm/study (e.g., "Spain"). Default is "Spain".
#' @param year Numeric. The reference year for FAO trade data calculation if origins are missing. Default is 2022.
#' @export
calculate_land_use <- function(automatic_cycle = FALSE,
                               saveoutput = TRUE,
                               farm_country = "Spain",
                               year = 2022) {

  message("\U0001f7e2 Calculating land use...")

  # --- 1. Load reference data (Your local paths) ---
  fao_raw      <- readr::read_csv("user_data/fao_crop_yields.csv", show_col_types = FALSE)
  forage_raw   <- readr::read_csv("user_data/forage_yields.csv", show_col_types = FALSE)
  name_mapping <- readr::read_csv("user_data/mapping.csv", show_col_types = FALSE)

  # --- 2. Process yields by country ---
  yields_combined <- dplyr::bind_rows(
    fao_raw %>% dplyr::select(Area, Item, Year, Value),
    forage_raw %>% dplyr::select(Area, Item = ingredient, Year, Value)
  )

  fao_yields <- name_mapping %>%
    dplyr::left_join(yields_combined, by = c("yield_name" = "Item")) %>%
    tidyr::complete(tidyr::nesting(ingredient, yield_name, agribalyse_name, allocation), Area, Year) %>%
    dplyr::filter(!is.na(ingredient) | !is.na(Value)) %>%
    dplyr::group_by(Area, Year, yield_name) %>%
    dplyr::mutate(
      avg = mean(Value, na.rm = TRUE),
      dm_yield = dplyr::coalesce(Value, ifelse(is.nan(avg), NA, avg))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(Area), !is.na(dm_yield)) %>%
    dplyr::group_by(ingredient, Area) %>%
    dplyr::filter(Year == max(Year, na.rm = TRUE)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      ingredient,
      country_of_origin = Area,
      dm_yield,
      ha_per_kg = dplyr::if_else(dm_yield > 0, 1 / dm_yield, 0),
      economic_allocation = as.numeric(allocation)
    )

  # --- 3. Load operational data & Handle Hybrid Country Origins ---
  DMI_df <- suppressMessages(calculate_DMI(saveoutput = FALSE)) %>%
    dplyr::distinct(region, diet_tag, subregion, animal_tag, class_flex, .keep_all = TRUE)

  diet_profiles <- readr::read_csv("user_data/diet_profiles.csv", show_col_types = FALSE) %>%
    dplyr::distinct(diet_tag, region, subregion, class_flex, .keep_all = TRUE)

  # Read your main local diet file
  diet_ingredients_raw <- readr::read_csv("user_data/diet_ingredients.csv", show_col_types = FALSE) %>%
    dplyr::distinct(diet_tag, region, subregion, class_flex, ingredient, country_of_origin, .keep_all = TRUE)

  # If there are any NA values in origin, we trigger our FAO engine
  if (any(is.na(diet_ingredients_raw$country_of_origin))) {

    # SHIELD: If the Parquet file is missing, auto-download it from GitHub Releases
    path_parquet <- "user_data/fao_trade_matrix.parquet"
    if (!file.exists(path_parquet)) {
      message("\u23f3 FAO trade matrix not found locally.")
      message("Downloading background database (187 MB)... This will only happen once.")

      # URL pointing to the latest release assets
      url_release <- "https://github.com/JuanCBM99/herdr/releases/latest/download/fao_trade_matrix.parquet"

      tryCatch({
        download.file(url_release, destfile = path_parquet, mode = "wb")
        message("\u2705 Download completed successfully.")
      }, error = function(e) {
        stop("Error downloading the trade matrix. Please check your internet connection: ", e$message)
      })
    }

    message(paste0("\u23f3 Missing countries of origin found. Computing dynamic FAO background data for ", farm_country, " (", year, ")..."))

    year_col  <- paste0("Y", year)
    fao_items <- unique(stats::na.omit(name_mapping$yield_name))

    # 3A. Local production capacity
    df_prod <- readr::read_csv("user_data/produccion.csv", show_col_types = FALSE)
    prod_country_col <- if("Reporter Countries" %in% names(df_prod)) "Reporter Countries" else "Area"

    clean_prod <- df_prod %>%
      dplyr::filter(.data[[prod_country_col]] == farm_country, Year == year, Item %in% fao_items) %>%
      dplyr::group_by(Item) %>%
      dplyr::summarise(Production = sum(Value, na.rm = TRUE), .groups = "drop")

    # 3B. Extract trade records from Parquet (Only the target year is read)
    df_trade <- arrow::open_dataset(path_parquet) %>%
      dplyr::select(`Reporter Countries`, `Partner Countries`, Item, Element, dplyr::all_of(year_col)) %>%
      dplyr::filter(`Reporter Countries` == farm_country, Item %in% fao_items) %>%
      dplyr::collect() %>%
      dplyr::rename(Value = dplyr::all_of(year_col)) %>%
      dplyr::filter(!is.na(Value))

    clean_exp <- df_trade %>%
      dplyr::filter(Element == "Export quantity") %>%
      dplyr::group_by(Item) %>%
      dplyr::summarise(Total_Export = sum(Value, na.rm = TRUE), .groups = "drop")

    clean_imp <- df_trade %>%
      dplyr::filter(Element == "Import quantity") %>%
      dplyr::group_by(Item) %>%
      dplyr::mutate(Total_Import = sum(Value, na.rm = TRUE)) %>%
      dplyr::arrange(dplyr::desc(Value)) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::select(Item, Top_Partner = `Partner Countries`, Total_Import)

    # 3C. Calculate Self-Sufficiency Ratio (70% Rule)
    fao_dictionary <- clean_prod %>%
      dplyr::full_join(clean_imp, by = "Item") %>%
      dplyr::full_join(clean_exp, by = "Item") %>%
      dplyr::mutate(dplyr::across(c(Production, Total_Import, Total_Export), ~ tidyr::replace_na(., 0))) %>%
      dplyr::mutate(
        Apparent_Consumption = Production + Total_Import - Total_Export,
        Apparent_Consumption = ifelse(Apparent_Consumption <= 0, 1, Apparent_Consumption),
        Self_Sufficiency_Ratio = Production / Apparent_Consumption,
        Calculated_Origin = ifelse(Self_Sufficiency_Ratio >= 0.70, farm_country, Top_Partner)
      ) %>%
      dplyr::select(Item, Calculated_Origin)

    # 3D. Map back to herdr ingredients
    final_dictionary <- name_mapping %>%
      dplyr::filter(!is.na(yield_name)) %>%
      dplyr::left_join(fao_dictionary, by = c("yield_name" = "Item")) %>%
      dplyr::mutate(Calculated_Origin = tidyr::replace_na(Calculated_Origin, farm_country)) %>%
      dplyr::select(ingredient, Calculated_Origin)

    # 3E. Overwrite NAs with calculated data using coalesce
    diet_ingredients <- diet_ingredients_raw %>%
      dplyr::left_join(final_dictionary, by = "ingredient") %>%
      dplyr::mutate(
        country_of_origin = dplyr::coalesce(country_of_origin, Calculated_Origin)
      ) %>%
      dplyr::select(-Calculated_Origin)
  } else {
    diet_ingredients <- diet_ingredients_raw
  }

  population_df <- suppressMessages(calculate_population(automatic_cycle = automatic_cycle, saveoutput = FALSE)) %>%
    dplyr::distinct(animal_tag, region, subregion, class_flex, population)

  # --- 4. Validate ingredient shares ---
  invalid_share <- diet_ingredients %>%
    dplyr::group_by(diet_tag, region, subregion, class_flex, ingredient_type) %>%
    dplyr::summarise(total = sum(as.numeric(ingredient_share), na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(abs(total - 100) > 0.1)

  if (nrow(invalid_share) > 0) {
    warning("\u26A0 Ingredient shares do not sum to 100% in: ", paste(unique(invalid_share$diet_tag), collapse = ", "))
  }

  # --- 5. Merge data and calculate impact ---
  results <- DMI_df %>%
    dplyr::inner_join(diet_profiles, by = c("region", "subregion", "class_flex", "diet_tag")) %>%
    dplyr::inner_join(diet_ingredients, by = c("diet_tag", "region", "subregion", "class_flex")) %>%
    dplyr::left_join(fao_yields, by = c("ingredient", "country_of_origin"))

  # Check for missing yields
  missing_yields <- results %>% dplyr::filter(is.na(dm_yield)) %>% dplyr::select(ingredient, country_of_origin) %>% dplyr::distinct()
  if (nrow(missing_yields) > 0) {
    warning("\u26A0 Missing yield for: ", paste0(missing_yields$ingredient, "(", missing_yields$country_of_origin, ")", collapse = ", "))
  }

  results <- results %>%
    dplyr::mutate(
      ha_kg_allocated = dplyr::coalesce(ha_per_kg, 0) * dplyr::coalesce(economic_allocation, 1),
      share_factor = dplyr::case_when(
        ingredient_type == "forage"        ~ forage_share / 100,
        ingredient_type == "concentrate"   ~ concentrate_share / 100,
        ingredient_type == "milk"          ~ milk_share / 100,
        ingredient_type == "milk_replacer" ~ milk_replacer_share / 100,
        TRUE ~ 0
      ),
      annual_cons_kg = (DMI_kgday * 365) * share_factor * (as.numeric(ingredient_share) / 100),
      land_use_m2 = (ha_kg_allocated * annual_cons_kg) * 10000
    ) %>%
    dplyr::left_join(population_df, by = c("region", "subregion", "animal_tag", "class_flex")) %>%
    tidyr::drop_na(animal_tag) %>%
    dplyr::group_by(region, subregion, animal_tag, class_flex, ingredient, country_of_origin, animal_type, animal_subtype) %>%
    dplyr::summarise(
      population = dplyr::first(population),
      dm_yield = dplyr::first(dm_yield),
      land_use_per_animal_m2 = sum(land_use_m2, na.rm = TRUE),
      total_land_use_m2 = sum(land_use_m2 * population, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3)))

  # --- 6. Save output ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/land_use.csv")
    message("\U0001f4be Land use report saved to output/land_use.csv")
  }

  return(results)
}
