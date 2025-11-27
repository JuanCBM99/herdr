#' Calculate CH₄ Emissions from Manure Management (Refactored)
#'
#' Computes CH₄ emissions from manure based on Volatile Solids (VS),
#' population, and management factors.
#' @export
calculate_CH4_manure <- function(saveoutput = TRUE) {

  message("🟢 Calculating CH₄ emissions from manure management...")

  # --- 1. Procesamiento y Cálculo ---
  # Base: Sólidos Volátiles (VS) calculados por Grupo/Zona
  results <- calculate_vs(saveoutput = FALSE) %>%
    dplyr::select(group, zone, identification, animal_type, animal_subtype, vs) %>%

    # 1.1 Unir Población (Dato específico)
    dplyr::left_join(
      calculate_population(saveoutput = FALSE) %>%
        dplyr::select(group, zone, identification, animal_type, animal_subtype, population),
      by = c("group", "zone", "identification", "animal_type", "animal_subtype")
    ) %>%

    # 1.2 Unir Configuración de Manejo (Dato universal)
    dplyr::left_join(
      load_dataset("ch4_mm") %>%
        dplyr::select(identification, animal_category, animal_type, animal_subtype,
                      management_system, system_climate, management_duration),
      by = c("identification", "animal_type", "animal_subtype")
    ) %>%

    # 1.3 Unir Coeficiente B0
    dplyr::left_join(
      load_dataset("coefficients") %>%
        dplyr::filter(coefficient == "b_0") %>%
        dplyr::select(animal_category = description, B0 = value),
      by = "animal_category"
    ) %>%

    # 1.4 Unir Factores MCF
    dplyr::left_join(
      load_dataset("mcf") %>% dplyr::select(management_system, system_climate, mcf),
      by = c("management_system", "system_climate")
    ) %>%

    # 2. Cálculos y Limpieza
    dplyr::mutate(
      # Limpieza de tipos (Seguridad numérica)
      across(
        c(vs, population, management_duration, B0, mcf),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),

      # Lógica Matemática
      awms = management_duration / 12,

      # Factor de Emisión (kg CH4 / animal / año)
      EF_CH4_kg_year = (vs * 365) * (B0 * 0.67 * mcf * awms),

      # Emisiones Totales (Mg CH4 / año) -> dividimos por 1.000.000 para pasar de kg a Mg (toneladas)
      Emissions_CH4_Mg_year = EF_CH4_kg_year * population / 1e6
    ) %>%

    # 3. Selección y Redondeo
    dplyr::select(
      group, zone, identification, animal_type, animal_subtype,
      management_system, vs, B0, mcf, awms,
      EF_CH4_kg_year, population, Emissions_CH4_Mg_year
    ) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))

  # --- 4. Guardado ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/CH4_manure.csv")
    message("💾 Saved output to output/CH4_manure.csv")
  }

  return(results)
}
