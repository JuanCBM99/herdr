#' Calculate land use
#'
#' Computes total land use (m2) per animal based on validated DMI.
#' This version integrates calculate_dmi() to ensure physiological consistency.
#' @param automatic_cycle Logical. If TRUE, uses the built-in model for automatic farm cycle calculation. Default is FALSE.
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @export
calculate_land_use <- function(automatic_cycle = FALSE, saveoutput = TRUE) {

  message("\U0001f4be Calculating land use based on validated DMI...")

  # --- 1. Data Loading ---

  # Fetch validated DMI (Step 5) - This already includes GE/ed and Normality Filters
  dmi_df <- calculate_dmi(saveoutput = FALSE) %>%
    dplyr::distinct(region, subregion, animal_tag, class_flex, .keep_all = TRUE)

  # Loading dietary structure and yields
  diet_csv <- readr::read_csv("user_data/diet_profiles.csv", show_col_types = FALSE) %>%
    dplyr::distinct(diet_tag, region, subregion, class_flex, .keep_all = TRUE)

  ingredients_csv <- readr::read_csv("user_data/diet_ingredients.csv", show_col_types = FALSE) %>%
    dplyr::distinct(diet_tag, region, subregion, class_flex, ingredient, .keep_all = TRUE)

  crops_csv <- readr::read_csv("user_data/crop_yields.csv", show_col_types = FALSE) %>%
    dplyr::select(ingredient, dry_matter_yield) %>%
    dplyr::distinct(ingredient, .keep_all = TRUE)

  pop_df <- calculate_population(automatic_cycle = automatic_cycle, saveoutput = FALSE) %>%
    dplyr::distinct(animal_tag, region, subregion, class_flex, .keep_all = TRUE)

  # Identity keys
  join_keys <- c("region", "subregion", "animal_tag", "class_flex", "animal_type", "animal_subtype")

  # --- 2. Processing and Calculation Pipeline ---

  # STEP A: Join validated DMI with dietary shares
  results <- dmi_df %>%
    dplyr::left_join(
      diet_csv,
      by = c("region", "subregion", "class_flex")
    ) %>%
    # Join with ingredients breakdown
    dplyr::inner_join(
      ingredients_csv,
      by = c("diet_tag", "region", "subregion", "class_flex")
    ) %>%
    # Join with yields
    dplyr::left_join(
      crops_csv,
      by = "ingredient"
    ) %>%
    dplyr::mutate(
      yield_num = as.numeric(dry_matter_yield),
      ing_share_num = as.numeric(ingredient_share),

      # Weighting factor based on ingredient type
      share_weight_factor = dplyr::case_when(
        ingredient_type == "forage"        ~ forage_share / 100,
        ingredient_type == "concentrate"   ~ concentrate_share / 100,
        ingredient_type == "milk"          ~ milk_share / 100,
        ingredient_type == "milk_replacer" ~ milk_replacer_share / 100,
        TRUE                               ~ 0
      ),

      # Annual Consumption per ingredient (kg DM / animal / year)
      # Using dmi_day_kg directly from the calculate_dmi() output
      consumption_kg = (dmi_day_kg * 365) * share_weight_factor * (ing_share_num / 100),

      # Land Use (m2): (Consumption / Yield) * 10,000 m2/ha
      land_use_m2_per_unit = dplyr::if_else(!is.na(yield_num) & yield_num > 0,
                                            (consumption_kg / yield_num) * 10000, 0)
    ) %>%
    # Join with population
    dplyr::left_join(
      pop_df %>% dplyr::select(region, subregion, animal_tag, class_flex, population),
      by = c("region", "subregion", "animal_tag", "class_flex")
    ) %>%
    tidyr::drop_na(animal_tag) %>%

    # STEP B: Final Aggregation
    dplyr::group_by(region, subregion, animal_tag, class_flex, animal_type, animal_subtype) %>%
    dplyr::summarise(
      population            = dplyr::first(population),
      DMI_validated_kg      = dplyr::first(dmi_day_kg),
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
    message("\U0001f4be Saved optimized land use report to output/Land_use.csv")
  }

  return(results)
}
