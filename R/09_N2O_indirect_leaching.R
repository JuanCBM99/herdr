#' Calculate indirect N₂O emissions from leaching (Refactored)
#'
#' Computes indirect N₂O emissions from nitrogen leaching.
#' @export
calculate_N2O_indirect_leaching <- function(saveoutput = TRUE) {

  message("🟢 Calculating indirect N₂O emissions (leaching)...")

  # --- 1. Carga y Configuración de Factores ---
  # Cargamos y extraemos EF5 de una sola vez de forma segura
  ef_factors <- load_dataset("emission_factors_indirect")
  ef5_value  <- ef_factors$value[ef_factors$description == "EF5"][1]

  if (is.na(ef5_value) || length(ef5_value) == 0) {
    stop("Error: No se encontró el factor 'EF5' en 'emission_factors_indirect'.")
  }

  # --- 2. Procesamiento y Cálculo ---
  # Base: N excretado (Direct Manure)
  results <- calculate_N2O_direct_manure(saveoutput = FALSE) %>%
    dplyr::select(group, zone, identification, animal_type, animal_subtype, N_excreted) %>%
    dplyr::distinct() %>%

    # Unir Población
    dplyr::left_join(
      calculate_population(saveoutput = FALSE) %>%
        dplyr::select(group, zone, identification, animal_type, animal_subtype, population),
      by = c("group", "zone", "identification", "animal_type", "animal_subtype")
    ) %>%

    # Unir Parámetros de Sistema (n2o_indirect)
    dplyr::left_join(
      load_dataset("n2o_indirect"),
      by = c("identification", "animal_type", "animal_subtype")
    ) %>%

    # Unir Fracciones de Lixiviación (fractions)
    dplyr::left_join(
      load_dataset("fractions") %>% dplyr::select(management_system, frac_leach_ms),
      by = "management_system"
    ) %>%

    # Cálculos
    dplyr::mutate(
      # Limpieza de tipos (numéricos y NA -> 0)
      across(
        c(population, N_excreted, duration, frac_leach_ms),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),

      # Lógica matemática
      awms = duration / 12,
      N_leaching = population * N_excreted * awms * frac_leach_ms,

      # Usamos el valor EF5 extraído al inicio
      EF5 = as.numeric(ef5_value),
      N2O_L = EF5 * N_leaching * (44 / 28)
    ) %>%

    # Redondeo final
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))

  # --- 3. Guardado ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/N2O_indirect_leaching.csv")
    message("💾 Saved output to output/N2O_indirect_leaching.csv")
  }

  return(results)
}
