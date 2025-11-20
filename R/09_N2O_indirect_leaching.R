#' Calculate indirect N₂O emissions from leaching
#'
#' Computes indirect N₂O emissions from nitrogen leaching for all animal
#' categories, groups, and zones.
#'
#' @param saveoutput Logical. If TRUE, saves output to "output/N2O_indirect_leaching.csv". Default TRUE.
#' @return Tibble with N₂O emissions per category from leaching
#' @export
calculate_N2O_indirect_leaching <- function(saveoutput = TRUE) {

  message("🟢 Calculating indirect N₂O emissions (leaching)...")

  # --- 1️⃣ Cargar datasets de configuración ---
  n2o_indirect_df <- load_dataset("n2o_indirect")

  fractions_df <- load_dataset("fractions") %>%
    dplyr::select(management_system, frac_leach_ms)

  # --- ¡MODIFICADO! Cargamos el nuevo archivo de factores ---
  ef_factors <- load_dataset("emission_factors_indirect")

  # Extraer el valor EF5
  ef5_value <- ef_factors %>%
    dplyr::filter(description == "EF5") %>%
    dplyr::pull(value)

  # Validar que lo encontramos
  if (length(ef5_value) == 0) {
    stop("Error: No se encontró 'EF5' en 'emission_factors_indirect.csv/rda'.")
  }
  ef5_value <- ef5_value[1] # Asegurarse de que es un solo número

  # --- 2️⃣ Cargar Población ---
  message("  -> Calculando población...")
  pop_df <- calculate_population(saveoutput = FALSE) %>%
    dplyr::select(group, zone, identification, animal_type, animal_subtype, population)

  # --- 3️⃣ Calcular N_excreted ---
  message("  -> Calculando N-excreted...")
  df_n <- calculate_N2O_direct_manure(saveoutput = FALSE) %>%
    dplyr::select(group, zone, identification, animal_type, animal_subtype, N_excreted) %>%
    dplyr::distinct()

  # --- 4️⃣ Calcular emisiones indirectas ---

  # Claves de unión
  join_keys_universal <- c("identification", "animal_type", "animal_subtype")
  join_keys_specific <- c("group", "zone", "identification", "animal_type", "animal_subtype")

  df <- df_n %>%
    dplyr::left_join(pop_df, by = join_keys_specific, na_matches = "na") %>%
    dplyr::left_join(n2o_indirect_df, by = join_keys_universal) %>%
    dplyr::left_join(fractions_df, by = "management_system") %>%
    dplyr::mutate(
      awms = duration / 12,
      population = tidyr::replace_na(population, 0),
      N_leaching = population * N_excreted * awms * frac_leach_ms,

      # --- ¡MODIFICADO! ---
      EF5 = ef5_value, # <-- Usando el valor cargado

      N2O_L = EF5 * N_leaching * (44 / 28)
    )

  # --- 5️⃣ Guardar CSV ---
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    readr::write_csv(df, "output/N2O_indirect_leaching.csv")
    message("💾 Saved output to output/N2O_indirect_leaching.csv")
  }

  return(df)
}
