#' Calculate CH₄ Emissions from Manure Management
#'
#' Computes CH₄ emissions from manure for all animal categories, groups,
#' and zones.
#'
#' @param saveoutput Logical (optional). If TRUE, saves the result as CSV. Default TRUE.
#' @return Tibble with CH₄ emissions per animal category.
#' @export
calculate_CH4_manure <- function(saveoutput = TRUE) {

  message("🟢 Calculating CH₄ emissions from manure management...")

  # --- 1️⃣ Calcular Volatile Solids (versión sin filtros) ---
  # 'vs_df' tiene group, zone, identification, animal_type, animal_subtype, vs
  vs_df <- calculate_vs(saveoutput = FALSE) %>%
    dplyr::select(group, zone, identification, animal_type, animal_subtype, vs)

  # --- 2️⃣ Cargar datasets de configuración ---
  # 'ch4_data' NO tiene group ni zone
  ch4_data <- load_dataset("ch4_mm") %>%
    dplyr::select(identification, animal_category, animal_type, animal_subtype,
                  management_system, system_climate, management_duration)

  coefficients_df <- load_dataset("coefficients") %>%
    dplyr::filter(coefficient == "b_0") %>%
    dplyr::select(animal_category = description, B0 = value)

  MCF_data <- load_dataset("mcf") %>%
    dplyr::select(management_system, system_climate, mcf)

  # --- 3️⃣ Cargar Población ---
  message("🟢 Calculating population data...")
  message("  -> Cargando 'census.csv'...")
  pop_data_to_use <- calculate_population(saveoutput = FALSE)

  pop_df <- pop_data_to_use %>%
    dplyr::select(group, zone, identification, animal_type, animal_subtype, population)

  # --- 4️⃣ Unir datasets y calcular EF ---

  # Claves para unir VS (específico) con ch4_data (general)
  join_keys_universal <- c("identification", "animal_type", "animal_subtype")

  # ¡MODIFICADO! Empezamos con 'vs_df' (que tiene group/zone)
  joined <- vs_df %>%
    # Unimos los datos de manejo (ch4_data) usando solo las claves universales
    dplyr::left_join(ch4_data, by = join_keys_universal) %>%
    dplyr::left_join(coefficients_df, by = "animal_category") %>%
    dplyr::left_join(MCF_data, by = c("management_system", "system_climate")) %>%
    dplyr::mutate(
      awms = management_duration / 12,
      EF_CH4_kg_year = (vs * 365) * (B0 * 0.67 * mcf * awms)
    )

  # --- 5️⃣ Unir población y calcular emisiones totales ---
  # Claves para unir con la población (estas SÍ tienen group/zone)
  pop_join_keys <- c("group", "zone", "identification", "animal_type", "animal_subtype")

  final <- joined %>%
    # Usamos na_matches = "na" para k1 y k2
    dplyr::left_join(pop_df, by = pop_join_keys, na_matches = "na") %>%
    dplyr::mutate(
      population = tidyr::replace_na(population, 0), # Si no hay población, es 0
      Emissions_CH4_Mg_year = EF_CH4_kg_year * population / 1e6
    ) %>%
    dplyr::select(group, zone, identification, animal_type, animal_subtype,
                  management_system, vs, B0, mcf, awms,
                  EF_CH4_kg_year, population, Emissions_CH4_Mg_year)

  # --- 6️⃣ Guardar CSV ---
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    readr::write_csv(final, "output/CH4_manure.csv")
    message("💾 Saved output to output/CH4_manure.csv")
  }

  return(final)
}
