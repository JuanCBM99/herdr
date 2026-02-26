#' Calculate land use
#'
#' Computes total land use (m2) per animal based on derived DMI (ge / eb).
#' This version automatically cleans duplicates in CSVs and normalizes dietary shares.
#' @param automatic_cycle Logical. If TRUE, uses the built-in model for automatic farm cycle calculation. Default is FALSE.
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @export
calculate_land_use <- function(automatic_cycle = FALSE, saveoutput = TRUE) {

  message("\U0001f4be Calculating DMI and land use...")

  # --- 1. Data Loading (Initial Phase) ---
  # Loading all CSVs from user_data and applying distinct() to prevent duplication
  diet_csv        <- readr::read_csv("user_data/diet_profiles.csv", show_col_types = FALSE) %>%
    dplyr::distinct(diet_tag, region, subregion, class_flex, diet_tag, .keep_all = TRUE)

  ingredients_csv <- readr::read_csv("user_data/diet_ingredients.csv", show_col_types = FALSE) %>%
    dplyr::distinct(diet_tag, region, subregion, class_flex, ingredient, .keep_all = TRUE)

  crops_csv       <- readr::read_csv("user_data/crop_yields.csv", show_col_types = FALSE) %>%
    dplyr::select(ingredient, dry_matter_yield) %>%
    dplyr::distinct(ingredient, .keep_all = TRUE)

  # Internal data modules
  ge_req_df   <- calculate_ge(saveoutput = FALSE) %>%
    dplyr::distinct(animal_tag, region, subregion, class_flex, .keep_all = TRUE)

  char_df     <- calculate_weighted_variable(saveoutput = FALSE) %>%
    dplyr::distinct(animal_tag, region, subregion, class_flex, .keep_all = TRUE)

  pop_df      <- calculate_population(automatic_cycle = automatic_cycle, saveoutput = FALSE) %>%
    dplyr::distinct(animal_tag, region, subregion, class_flex, .keep_all = TRUE)

  # Identity keys for consistent joining
  join_keys <- c("region", "subregion", "animal_tag", "class_flex", "animal_type", "animal_subtype")

  # --- 2. Processing and Calculation Pipeline ---

  # STEP A: Join energy requirements with nutritional characterization (EB)
  animal_physics <- ge_req_df %>%
    dplyr::select(dplyr::all_of(join_keys), ge) %>%
    dplyr::left_join(
      char_df %>% dplyr::select(region, subregion, animal_tag, class_flex, eb, diet_tag),
      by = c("region", "subregion", "animal_tag", "class_flex")
    )

  # STEP B: Supply chain breakdown and land use calculation
  results <- animal_physics %>%
    # Join with diet to get shares (forage_share, concentrate_share, etc.)
    dplyr::left_join(
      diet_csv,
      by = c("diet_tag", "region", "subregion", "class_flex")
    ) %>%
    # Join with ingredients
    dplyr::inner_join(
      ingredients_csv,
      by = c("diet_tag", "region", "subregion", "class_flex")
    ) %>%
    # Join with crop yields
    dplyr::left_join(
      crops_csv,
      by = "ingredient"
    ) %>%

    dplyr::mutate(
      # Numerical safety
      yield_num = as.numeric(dry_matter_yield),
      ing_share_num = as.numeric(ingredient_share),

      # DMI Calculation: GE (MJ/day) / EB (MJ/kg DM)
      dm_ingested_derived = dplyr::if_else(eb > 0, ge / eb, 0),

      # Weighting factor based on ingredient type
      share_weight_factor = dplyr::case_when(
        ingredient_type == "forage"        ~ forage_share / 100,
        ingredient_type == "concentrate"          ~ concentrate_share / 100,
        ingredient_type == "milk"          ~ milk_share / 100,
        ingredient_type == "milk_replacer" ~ milk_replacer_share / 100,
        TRUE                               ~ 0
      ),

      # Annual Consumption (kg DM / animal / year)
      consumption_kg = (dm_ingested_derived * 365) * share_weight_factor * (ing_share_num / 100),

      # Land Use (m2): (Consumption / Yield) * 10,000 m2/ha
      land_use_m2_per_unit = dplyr::if_else(!is.na(yield_num) & yield_num > 0,
                                            (consumption_kg / yield_num) * 10000, 0)
    ) %>%

    # Join with population for total calculations
    dplyr::left_join(
      pop_df %>% dplyr::select(region, subregion, animal_tag, class_flex, population),
      by = c("region", "subregion", "animal_tag", "class_flex")
    ) %>%

    tidyr::drop_na(animal_tag) %>%

    # STEP C: Aggregation by animal identity
    dplyr::group_by(region, subregion, animal_tag, class_flex, animal_type, animal_subtype) %>%
    dplyr::summarise(
      population            = dplyr::first(population),
      DMI_day_kg            = dplyr::first(dm_ingested_derived),
      Total_Consumption_kg  = sum(consumption_kg, na.rm = TRUE),
      Land_use_per_animal   = sum(land_use_m2_per_unit, na.rm = TRUE),
      Land_use_Total_m2     = sum(land_use_m2_per_unit * population, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))

  # --- 3. Save Output ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/Land_use.csv")
    message("\U0001f4be Saved clean output to output/Land_use.csv")
  }

  return(results)
}
