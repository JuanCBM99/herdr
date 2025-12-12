#' Calculate CH₄ Emissions from Manure Management (Refactored)
#'
#' Computes CH₄ emissions from manure based on Volatile Solids (VS),
#' population, and management factors (B0, MCF, AWMS). (IPCC Eq 10.23).
#' @export
calculate_CH4_manure <- function(saveoutput = TRUE) {

  message("🟢 Calculating CH₄ emissions from manure management...")

  # --- 1. Data Processing and Joins ---

  # Base Data: Fetch Volatile Solids (VS) by Group/Zone
  results <- calculate_vs(saveoutput = FALSE) %>%
    dplyr::select(group, zone, identification, animal_type, animal_subtype, vs) %>%

    # 1.1 Join Population Data (Specific to each ID/Zone)
    dplyr::left_join(
      calculate_population(saveoutput = FALSE) %>%
        dplyr::select(group, zone, identification, animal_type, animal_subtype, population),
      by = c("group", "zone", "identification", "animal_type", "animal_subtype")
    ) %>%

    # 1.2 Join Management Configuration (System, Climate, and Duration)
    dplyr::left_join(
      load_dataset("ch4_mm") %>%
        dplyr::select(identification, animal_category, animal_type, animal_subtype,
                      management_system, system_climate, management_duration),
      by = c("identification", "animal_type", "animal_subtype")
    ) %>%

    # 1.3 Join B0 Coefficient (Maximum Methane Potential)
    dplyr::left_join(
      load_dataset("coefficients") %>%
        dplyr::filter(coefficient == "b_0") %>%
        dplyr::select(animal_category = description, B0 = value),
      by = "animal_category"
    ) %>%

    # 1.4 Join MCF Factors (Methane Conversion Factors)
    dplyr::left_join(
      load_dataset("mcf") %>% dplyr::select(management_system, system_climate, mcf),
      by = c("management_system", "system_climate")
    ) %>%

    # --- 2. Calculations (IPCC Eq 10.23) ---
    dplyr::mutate(
      # Numeric Safety: Replace NAs with 0
      across(
        c(vs, population, management_duration, B0, mcf),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),

      # Calculate AWMS (Fraction of manure handled in this system/climate)
      awms = management_duration / 12,

      # Emission Factor (EF_CH4: kg CH4 / animal / year)
      # EF = (VS * 365) * (B0 * 0.67 * MCF * AWMS)
      EF_CH4_kg_year = (vs * 365) * (B0 * 0.67 * mcf * awms),

      # Total Emissions (Mg CH4 / year)
      Emissions_CH4_Mg_year = EF_CH4_kg_year * population / 1e6
    ) %>%

    # --- 3. Selection and Rounding ---
    dplyr::select(
      group, zone, identification, animal_type, animal_subtype,
      management_system, vs, B0, mcf, awms,
      EF_CH4_kg_year, population, Emissions_CH4_Mg_year
    ) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))

  # --- 4. Save Output ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/CH4_manure.csv")
    message("💾 Saved output to output/CH4_manure.csv")
  }

  return(results)
}
