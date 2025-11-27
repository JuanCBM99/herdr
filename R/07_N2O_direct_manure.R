#' Calculate direct N₂O emissions from manure (Refactored)
#'
#' Computes direct N₂O emissions based on nitrogen excretion logic,
#' emission factors, management system, and climate.
#' @export
calculate_N2O_direct_manure <- function(saveoutput = TRUE) {

  message("🟢 Calculating direct N₂O emissions from manure...")

  # --- 1. Construcción del Dataset Maestro ---
  # Iniciamos con GE (Energía Bruta) como base
  results <- calculate_ge(saveoutput = FALSE) %>%
    dplyr::select(group, zone, identification, animal_type, animal_subtype, ge) %>%

    # 1.1 Unir datos Específicos (Nivel: Group/Zone)
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

    # 1.2 Unir datos Universales (Nivel: Animal Type)
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

    # 1.3 Unir Configuración de Manejo (n2o_direct)
    dplyr::left_join(
      load_dataset("n2o_direct"),
      by = c("identification", "animal_type", "animal_subtype")
    ) %>%

    # 1.4 Unir Factores de Emisión (depende de lo cargado en 1.3)
    dplyr::left_join(
      load_dataset("emission_factors_direct") %>% dplyr::select(management_system, climate, ef_value = value),
      by = c("management_system", "climate")
    ) %>%

    # --- 2. Cálculos y Limpieza ---
    dplyr::mutate(
      # Limpieza de tipos (Seguridad contra texto/NA)
      across(
        c(ge, cp, population, milk_yield, fat_content, weight_gain, NEg, management_duration, ef_value),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),

      # Constantes y Variables Intermedias
      milk_protein = 1.9 + 0.4 * fat_content,
      awms         = management_duration / 12,

      # Cálculo de N Retention (N_ret)
      # Nota: Se protege la división por cero si weight_gain es 0
      N_retention = dplyr::case_when(
        animal_type %in% c("sheep", "goat") ~ 0.1,
        weight_gain > 0 & NEg > 0           ~ ((milk_yield * milk_protein) / 6.38) +
          ((weight_gain * (268 - (7.03 * NEg / weight_gain)) / 1000) / 6.25),
        TRUE                                ~ 0
      ),

      # Cálculo de N Intake y N Excreted
      N_intake   = (ge / 18.45) * (cp / 100 / 6.25),

      N_excreted = dplyr::if_else(
        animal_type %in% c("sheep", "goat"),
        (N_intake * (1 - N_retention)) * 365, # Lógica ovinos/caprinos
        (N_intake - N_retention) * 365        # Lógica general (vacunos, etc.)
      ),

      # Emisiones finales
      Emisiones_N2O = population * N_excreted * awms * ef_value * (44 / 28)
    ) %>%

    # --- 3. Redondeo Final ---
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))

  # --- 4. Guardado ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/N2O_direct_manure.csv")
    message("💾 Saved output to output/N2O_direct_manure.csv")
  }

  return(results)
}
