#' Calculate indirect N₂O emissions from volatilization
#'
#' Computes indirect N₂O emissions derived from volatilization of excreted nitrogen
#' for all animal categories, groups, and zones.
#'
#' @param saveoutput Logical. If TRUE, saves output to "output/N2O_indirect_volatilization.csv". Default TRUE.
#' @return Tibble with indirect N₂O emissions per category
#' @export
calculate_N2O_indirect_volatilization <- function(saveoutput = TRUE) {

  message("🟢 Calculating indirect N₂O emissions (volatilization)...")

  # --- 1️⃣ Cargar datasets de configuración ---
  # 'n2o_indirect_df' no tiene group/zone
  n2o_indirect_df <- load_dataset("n2o_indirect")

  fractions_df <- load_dataset("fractions") %>%
    dplyr::select(management_system, frac_gas_ms)

  ef4_df <- load_dataset("emission_factors_indirect") %>%
    dplyr::select(climate, ef4 = value)

  # --- 2️⃣ Cargar Población ---
  message("  -> Calculando población...")
  pop_df <- calculate_population(saveoutput = FALSE) %>%
    dplyr::select(group, zone, identification, animal_type, animal_subtype, population)

  # --- 3️⃣ Calcular N_excreted ---
  # (Esta función ya es robusta y devuelve N_excreted por group/zone)
  message("  -> Calculando N-excreted...")
  df_n <- calculate_N2O_direct_manure(saveoutput = FALSE) %>%
    dplyr::select(group, zone, identification, animal_type, animal_subtype, N_excreted) %>%
    dplyr::distinct() # Nos aseguramos de tener una sola fila por animal/zona

  # --- 4️⃣ Calcular emisiones indirectas ---

  # Claves para unir los datos de manejo (universales)
  join_keys_universal <- c("identification", "animal_type", "animal_subtype")
  # Claves para unir los datos de N y Población (específicos)
  join_keys_specific <- c("group", "zone", "identification", "animal_type", "animal_subtype")

  # Empezamos con 'df_n' (que tiene N_excreted por group/zone)
  df <- df_n %>%
    # Unimos la población (específica)
    dplyr::left_join(pop_df, by = join_keys_specific, na_matches = "na") %>%
    # Unimos los sistemas de manejo (universales)
    dplyr::left_join(n2o_indirect_df, by = join_keys_universal) %>%
    # Unimos las fracciones y factores de emisión
    dplyr::left_join(fractions_df, by = "management_system") %>%
    dplyr::left_join(ef4_df, by = "climate") %>%
    dplyr::mutate(
      awms = duration / 12,
      population = tidyr::replace_na(population, 0),
      n_volatilization = population * N_excreted * awms * frac_gas_ms,
      n2o_g = ef4 * n_volatilization * (44 / 28)
    )

  # --- 5️⃣ Guardar CSV ---
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    readr::write_csv(df, "output/N2O_indirect_volatilization.csv")
    message("💾 Saved output to output/N2O_indirect_volatilization.csv")
  }

  return(df)
}
