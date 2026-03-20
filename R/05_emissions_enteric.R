#' Calculate methane emissions from enteric fermentation
#'
#' Computes enteric methane emissions based on Gross Energy (GE),
#' Digestible Energy (DE), NDF, and Ym factor using IPCC Tier 2 logic.
#'
#' @param automatic_cycle Logical. If TRUE, uses the built-in model for automatic farm cycle calculation. Default is FALSE.
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @export
calculate_emissions_enteric <- function(automatic_cycle = FALSE, saveoutput = TRUE) {

  message("\U0001f7e2 Calculating enteric fermentation emissions...")

  # --- 1. Data Loading and Propagation ---
  # We pass 'automatic_cycle' to ensure population and energy match the user choice
  diet_vars <- calculate_weighted_variable(saveoutput = FALSE)
  ge_df     <- calculate_ge(saveoutput = FALSE)
  pop_df    <- calculate_population(automatic_cycle = automatic_cycle, saveoutput = FALSE)

  if (nrow(diet_vars) == 0) {
    message("\u26a0 No diet data found. Returning empty structure.")
    return(dplyr::tibble())
  }

  # Mandatory identity keys for merging all modules
  join_keys <- c("region", "subregion", "animal_tag", "class_flex", "animal_type", "animal_subtype")

  # --- 2. Processing Pipeline: Merge Energy and Population ---
  results <- diet_vars %>%

    # Join Gross Energy (GE)
    dplyr::left_join(
      ge_df %>%
        dplyr::select(dplyr::all_of(join_keys), ge_MJ_day)) %>%

    # Join Population (The parameter automatic_cycle was already applied here)
    dplyr::left_join(
      pop_df %>%
        dplyr::select(dplyr::all_of(join_keys), population)) %>%

    # --- 3. Ym and Emission Factor Calculations (IPCC Tier 2) ---
    dplyr::mutate(
      # Numerical safety: replace NAs with 0
      across(c(de, ndf, ge_MJ_day, population), ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)),

      # Calculate Methane Conversion Factor (Ym) based on diet quality
      ym = dplyr::case_when(
        animal_type == "sheep" ~ 6.7,
        animal_type == "goat"  ~ 5.5,

        # Mature Dairy Cattle: Ym depends on DE and NDF thresholds
        animal_type == "cattle" & animal_tag == "mature_dairy_cattle" ~ dplyr::case_when(
          de >= 70 & ndf <= 35 ~ 5.7,
          de >= 70 & ndf > 35  ~ 6.0,
          de >= 63 & de < 70 & ndf > 37 ~ 6.3,
          de <= 62 & ndf > 38  ~ 6.5,
          TRUE ~ 6.5
        ),

        # Other Cattle Categories
        animal_type == "cattle" & animal_tag != "mature_dairy_cattle" ~ dplyr::case_when(
          de >= 75 ~ 3.0,
          de >= 72 ~ 4.0,
          de >= 62 & de <= 71 ~ 6.3,
          de < 62  ~ 7.0,
          TRUE ~ 6.3
        ),
        TRUE ~ NA_real_
      ),

      # Emission Factor (EF) (kg CH4/animal/year)
      # Formula: (GE * (Ym/100) * 365) / 55.65 (where 55.65 is methane energy density)
      ef_kg_animal_year = (ge_MJ_day * (ym / 100) * 365) / 55.65,

      # Total Enteric Emissions (Gg CH4/year)
      emissions_total = ef_kg_animal_year * (population / 1e6)
    ) %>%

    # --- 4. Final Cleanup ---
    dplyr::select(
      dplyr::all_of(join_keys),
      de, ndf, ge_MJ_day, ym, ef_kg_animal_year, population, emissions_total
    ) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))

  # --- 5. Save Results ---
  if (isTRUE(saveoutput) && nrow(results) > 0) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/enteric_emissions.csv")
    message("\U0001f4be Saved output to output/enteric_emissions.csv")
  }

  return(results)
}
