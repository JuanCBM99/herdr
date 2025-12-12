#' Calculate indirect N₂O emissions from leaching (Refactored)
#'
#' Computes indirect N₂O emissions derived from nitrogen leaching (IPCC Eq 10.27 and 10.29).
#' @export
calculate_N2O_indirect_leaching <- function(saveoutput = TRUE) {

  message("🟢 Calculating indirect N₂O emissions (leaching)...")

  # --- 1. Load and Configure Factors ---

  # Load and safely extract EF5 value
  ef_factors <- load_dataset("emission_factors_indirect")
  ef5_value  <- ef_factors$value[ef_factors$description == "EF5"][1]

  if (is.na(ef5_value) || length(ef5_value) == 0) {
    stop("Error: 'EF5' factor not found in 'emission_factors_indirect'.")
  }

  # --- 2. Processing and Calculation Pipeline ---

  # Base Data: Fetch N Excreted (N_excreted) from the direct N2O calculation
  results <- calculate_N2O_direct_manure(saveoutput = FALSE) %>%
    dplyr::select(group, zone, identification, animal_type, animal_subtype, N_excreted) %>%
    dplyr::distinct() %>%

    # Join Population Data
    dplyr::left_join(
      calculate_population(saveoutput = FALSE) %>%
        dplyr::select(group, zone, identification, animal_type, animal_subtype, population),
      by = c("group", "zone", "identification", "animal_type", "animal_subtype")
    ) %>%

    # Join System Parameters (n2o_indirect)
    dplyr::left_join(
      load_dataset("n2o_indirect"),
      by = c("identification", "animal_type", "animal_subtype")
    ) %>%

    # Join Leaching Fractions (frac_leach_ms)
    dplyr::left_join(
      load_dataset("fractions") %>% dplyr::select(management_system, frac_leach_ms),
      by = "management_system"
    ) %>%

    # Calculations
    dplyr::mutate(
      # Numeric Safety: NA replacement
      across(
        c(population, N_excreted, duration, frac_leach_ms),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),

      # Calculate AWMS (Annualized Duration)
      awms = duration / 12,

      # Calculate N Loss due to Leaching (N_leaching-MMS, Eq 10.27)
      N_leaching = population * N_excreted * awms * frac_leach_ms,

      # Define EF5 value (used in the final N2O conversion)
      EF5 = as.numeric(ef5_value),

      # Calculate Indirect N2O Emissions (N2O_L,mm, Eq 10.29)
      # N2O_L = EF5 * N_leaching * (44/28)
      N2O_L = EF5 * N_leaching * (44 / 28)
    ) %>%

    # Final Rounding
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))

  # --- 3. Save Output ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/N2O_indirect_leaching.csv")
    message("💾 Saved output to output/N2O_indirect_leaching.csv")
  }

  return(results)
}
