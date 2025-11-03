#' Calculate indirect N₂O emissions from leaching
#'
#' Computes indirect N₂O emissions from nitrogen leaching.
#'
#' @param animal Character string (optional). Animal type ("Cattle", "Sheep", "Goat"). Default NULL.
#' @param type Character string (optional). Subtype only for Cattle. Default NULL.
#' @param zone Character vector (optional). Filter by zone. Default NULL.
#' @param saveoutput Logical. If TRUE, saves output to "output/N2O_indirect_leaching.csv". Default TRUE.
#' @param population_df (Opcional) Un dataframe de población ya calculado.
#'   Si es NULL (por defecto), la función llamará a `calculate_population()`
#'   interactivamente.
#' @return Tibble with N₂O emissions per category from leaching
#' @export
calculate_N2O_indirect_leaching <- function(animal = NULL, type = NULL, zone = NULL, saveoutput = TRUE,
                                            population_df = NULL) { # <-- CAMBIO 1

  message("🟢 Calculating indirect N₂O emissions (leaching)...")

  # --- 1️⃣ Cargar datasets base ---
  n2o_indirect_df <- load_dataset("n2o_indirect")
  fractions_df <- load_dataset("fractions") %>% dplyr::select(management_system, frac_leach_ms)

  # --- CAMBIO 3: Lógica de población modificada ---
  message("🟢 Calculating population data...")

  # Variable para guardar la población que usaremos
  pop_data_to_use <- NULL

  if (!is.null(population_df)) {
    # Opción 1: Usamos la población que nos han "pasado"
    message("♻️ Usando datos de población proporcionados.")
    pop_data_to_use <- population_df

  } else {
    # Opción 2: Nadie nos pasó los datos. Llamamos a la función interactiva
    message("-> No se proporcionaron datos de población. Calculando...")
    pop_data_to_use <- calculate_population(saveoutput = FALSE)
  }

  # 'pop_df' se crea a partir de los datos que acabamos de obtener
  pop_df <- pop_data_to_use %>%
    dplyr::select(code, animal_type, animal_subtype, population)
  # --- Fin del Cambio 3 ---


  # --- 2️⃣ Calcular N_excreted usando la función directa ---

  # --- ¡ESTE ES EL CAMBIO CLAVE! ---
  # Ahora le pasamos la variable 'pop_data_to_use' que rellenamos arriba
  df_n <- calculate_N2O_direct_manure(
    animal = animal, type = type, zone = zone, saveoutput = FALSE,
    population_df = pop_data_to_use # <-- ¡CORREGIDO!
  ) %>%
    dplyr::select(code, animal_type, animal_subtype, zone, N_excreted) %>%
    dplyr::distinct()
  # --- Fin del Cambio Clave ---


  # --- 3️⃣ Filtrar zonas si se especifica ---
  if (!is.null(zone)) n2o_indirect_df <- n2o_indirect_df %>% dplyr::filter(zone %in% zone)

  # --- 4️⃣ Calcular emisiones ---
  df <- n2o_indirect_df %>%
    dplyr::mutate(awms = duration / 12) %>%
    dplyr::left_join(df_n, by = c("code","animal_type","animal_subtype")) %>%

    # Unimos con 'pop_df' (que ahora viene de nuestra lógica if/else)
    dplyr::left_join(pop_df, by = c("code","animal_type","animal_subtype")) %>%

    dplyr::left_join(fractions_df, by = "management_system") %>%
    dplyr::mutate(
      N_leaching = population * N_excreted * awms * frac_leach_ms,
      EF5 = 0.011,
      N2O_L = EF5 * N_leaching * (44 / 28)
    )

  # --- 5️⃣ Filtrado opcional ---
  if (!is.null(animal)) df <- df %>% dplyr::filter(animal_type %in% animal)
  if (!is.null(type)) df <- df %>% dplyr::filter(animal_subtype %in% type)
  if (!is.null(zone)) df <- df %>% dplyr::filter(zone %in% zone)

  # --- 6️⃣ Guardar CSV ---
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    readr::write_csv(df, "output/N2O_indirect_leaching.csv")
    message("💾 Saved output to output/N2O_indirect_leaching.csv")
  }

  df
}
