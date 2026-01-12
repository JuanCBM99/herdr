#' Calculate direct N₂O emissions from manure (Refactored)
#'
#' Computes direct N₂O emissions based on nitrogen excretion logic,
#' emission factors, management system, and climate (IPCC Eq 10.25).
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @export
calculate_N2O_direct_manure <- function(saveoutput = TRUE) {

  message("\U0001f4be Calculating direct N2O emissions from manure...")

  # --- 1. Master Dataset Construction (Data Merging) ---

  # Start with GE (Gross Energy) as the base dataframe
  results <- calculate_ge(saveoutput = FALSE) %>%
    dplyr::select(group, zone, identification, animal_type, animal_subtype, ge) %>%

    # 1.1 Join Group/Zone Specific Data (CP, Population)
    dplyr::left_join(
      calculate_weighted_variable(saveoutput = FALSE) %>%
        dplyr::select(group, zone, identification, animal_type, animal_subtype, cp),
      by = c("group", "zone", "identification", "animal_type", "animal_subtype")
    ) %>%
    dplyr::left_join(
      calculate_population(saveoutput = FALSE) %>%
        dplyr::select(group, zone, identification, animal_type, animal_subtype, population),
      by = c("group", "zone", "identification", "animal_type", "animal_subtype")
    ) %>%

    # 1.2 Join Universal Animal Data (Milk Yield, Fat, Weight Gain, NEg)
    dplyr::left_join(
      load_dataset("categories") %>% dplyr::select(identification, animal_type, animal_subtype, milk_yield, fat_content),
      by = c("identification", "animal_type", "animal_subtype")
    ) %>%
    dplyr::left_join(
      load_dataset("weights") %>% dplyr::select(identification, animal_type, animal_subtype, weight_gain),
      by = c("identification", "animal_type", "animal_subtype")
    ) %>%
    dplyr::left_join(
      calculate_NEg(saveoutput = FALSE) %>% dplyr::select(identification, animal_type, animal_subtype, NEg),
      by = c("identification", "animal_type", "animal_subtype")
    ) %>%

    # 1.3 Join Management Configuration (System, Climate, Duration)
    dplyr::left_join(
      load_dataset("n2o_direct"),
      by = c("identification", "animal_type", "animal_subtype")
    ) %>%

    # 1.4 Join Emission Factors (EF3) based on Management System and Climate
    dplyr::left_join(
      load_dataset("emission_factors_direct") %>% dplyr::select(management_system, climate, ef_value = value),
      by = c("management_system", "climate")
    ) %>%

    # --- 2. Calculations (Nitrogen Balance and Emissions) ---
    dplyr::mutate(
      # Type Cleaning (Numeric Safety)
      across(
        c(ge, cp, population, milk_yield, fat_content, weight_gain, NEg, management_duration, ef_value),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),

      # Intermediate Variables (Milk Protein and AWMS - Annualized Duration)
      milk_protein = 1.9 + 0.4 * fat_content,
      awms         = management_duration / 12,

      # Calculate N Retention (N_ret)
      N_retention = dplyr::case_when(
        animal_type %in% c("sheep", "goat") ~ 0.1,
        weight_gain > 0 & NEg > 0           ~ ((milk_yield * milk_protein) / 6.38) +
          ((weight_gain * (268 - (7.03 * NEg / weight_gain)) / 1000) / 6.25),
        TRUE                                ~ 0
      ),

      # Calculate N Intake (Eq 10.32) and N Excreted (Eq 10.31/10.31a)
      N_intake   = (ge / 18.45) * (cp / 100 / 6.25),

      # Final N Excreted (Conditional Logic by Animal Type)
      N_excreted = dplyr::if_else(
        animal_type %in% c("sheep", "goat"),
        (N_intake * (1 - N_retention)) * 365,
        (N_intake - N_retention) * 365
      ),

      # Final Direct N2O Emissions (IPCC Eq 10.25)
      N2O_emissions = population * N_excreted * awms * ef_value * (44 / 28)
    ) %>%

    # --- 3. Final Output and Cleanup ---
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))

  # --- 4. Save Output ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/N2O_direct_manure.csv")
    message("\U0001f4be Saved output to output/N2O_direct_manure.csv")
  }

  return(results)
}
