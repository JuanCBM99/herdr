#' Calculate indirect N₂O emissions from volatilization (Refactored)
#'
#' Computes indirect N₂O emissions derived from volatilization of excreted nitrogen (IPCC Eq 10.26 and 10.28).
#' @export
calculate_N2O_indirect_volatilization <- function(saveoutput = TRUE) {

  message("🟢 Calculating indirect N₂O emissions (volatilization)...")

  # --- 1. Data Processing and Joins ---

  # Base Data: Fetch N Excreted (N_excreted) from the direct N2O calculation
  results <- calculate_N2O_direct_manure(saveoutput = FALSE) %>%
    dplyr::select(group, zone, identification, animal_type, animal_subtype, N_excreted) %>%
    dplyr::distinct() # Ensure uniqueness

  # 1.1 Join Population Data (Specific to each Group/Zone)
  results <- results %>%
    dplyr::left_join(
      calculate_population(saveoutput = FALSE) %>%
        dplyr::select(group, zone, identification, animal_type, animal_subtype, population),
      by = c("group", "zone", "identification", "animal_type", "animal_subtype")
    ) %>%

    # 1.2 Join Management Parameters (System, Climate, Duration)
    dplyr::left_join(
      load_dataset("n2o_indirect"),
      by = c("identification", "animal_type", "animal_subtype")
    ) %>%

    # 1.3 Join Gas Fraction (Frac_Gas)
    dplyr::left_join(
      load_dataset("fractions") %>% dplyr::select(management_system, frac_gas_ms),
      by = "management_system"
    ) %>%

    # 1.4 Join Emission Factor (EF4)
    dplyr::left_join(
      load_dataset("emission_factors_indirect") %>% dplyr::select(climate, ef4 = value),
      by = "climate"
    ) %>%

    # --- 2. Calculations (N Loss and N2O Conversion) ---
    dplyr::mutate(
      # Numeric Safety: NA replacement
      across(
        c(population, N_excreted, duration, frac_gas_ms, ef4),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),

      # Calculate AWMS (Annualized Duration)
      awms = duration / 12,

      # Calculate N Loss due to Volatilization (N_volatilization-MMS, Eq 10.26)
      n_volatilization = population * N_excreted * awms * frac_gas_ms,

      # Calculate Indirect N2O Emissions (N2O_G,mm, Eq 10.28)
      # N2O_G = EF4 * N_volatilization * (44/28)
      n2o_g = ef4 * n_volatilization * (44 / 28)
    ) %>%

    # --- 3. Final Output and Cleanup ---
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))

  # --- 4. Save Output ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/N2O_indirect_volatilization.csv")
    message("💾 Saved output to output/N2O_indirect_volatilization.csv")
  }

  return(results)
}
