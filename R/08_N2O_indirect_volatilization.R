#' Calculate indirect N₂O emissions from volatilization (Refactored)
#'
#' Computes indirect N₂O emissions derived from volatilization of excreted nitrogen.
#' @export
calculate_N2O_indirect_volatilization <- function(saveoutput = TRUE) {

  message("🟢 Calculating indirect N₂O emissions (volatilization)...")

  # --- 1. Procesamiento y Cálculo ---
  # Iniciamos con el cálculo base de N excretado
  results <- calculate_N2O_direct_manure(saveoutput = FALSE) %>%
    dplyr::select(group, zone, identification, animal_type, animal_subtype, N_excreted) %>%
    dplyr::distinct() %>% # Aseguramos unicidad

    # 1.1 Unir Población (Nivel específico: group/zone)
    dplyr::left_join(
      calculate_population(saveoutput = FALSE) %>%
        dplyr::select(group, zone, identification, animal_type, animal_subtype, population),
      by = c("group", "zone", "identification", "animal_type", "animal_subtype")
    ) %>%

    # 1.2 Unir Parámetros de Manejo (Nivel universal: tipo animal)
    dplyr::left_join(
      load_dataset("n2o_indirect"),
      by = c("identification", "animal_type", "animal_subtype")
    ) %>%

    # 1.3 Unir Fracción de Gas (Frac_Gas)
    dplyr::left_join(
      load_dataset("fractions") %>% dplyr::select(management_system, frac_gas_ms),
      by = "management_system"
    ) %>%

    # 1.4 Unir Factor de Emisión (EF4)
    dplyr::left_join(
      load_dataset("emission_factors_indirect") %>% dplyr::select(climate, ef4 = value),
      by = "climate"
    ) %>%

    # 2. Cálculos y Limpieza
    dplyr::mutate(
      # Limpieza de tipos (importante para evitar errores de texto vs número)
      across(
        c(population, N_excreted, duration, frac_gas_ms, ef4),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),

      # Fórmulas
      awms = duration / 12,
      n_volatilization = population * N_excreted * awms * frac_gas_ms,
      n2o_g = ef4 * n_volatilization * (44 / 28)
    ) %>%

    # 3. Redondeo final
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))

  # --- 4. Guardado ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/N2O_indirect_volatilization.csv")
    message("💾 Saved output to output/N2O_indirect_volatilization.csv")
  }

  return(results)
}
