#' Calculate total animal population (Refactored)
#'
#' Orchestrates the population calculation by loading census data and dispatching
#' sub-calculations for each animal type (cattle, sheep, goat).
#' @export
calculate_population <- function(saveoutput = TRUE) {

  message("🟢 Calculating Total Population...")

  # --- 1. Carga de Datos ---
  census_base     <- load_dataset("census")
  rate_parameters <- load_dataset("rate_parameters")
  categories      <- load_dataset("categories")

  # Validación mínima para evitar errores en cascada
  stopifnot("animal_type" %in% names(census_base))

  # --- 2. Dispatcher (Gestor de Tráfico) ---
  # Creamos una lista para almacenar los resultados parciales
  results_list <- list()

  # A) VACUNO (Cattle)
  # Requiere lógica especial (pasa 'categories' para gestionar zonas vacías/NA)
  df_cattle <- census_base %>% dplyr::filter(tolower(animal_type) == "cattle")
  if (nrow(df_cattle) > 0) {
    message("  -> Processing Cattle...")
    results_list$cattle <- calculate_population_cattle(
      census_cattle = df_cattle,
      rate_parameters = rate_parameters,
      categories = categories
    )
  }

  # B) OVEJAS (Sheep)
  df_sheep <- census_base %>% dplyr::filter(tolower(animal_type) == "sheep")
  if (nrow(df_sheep) > 0) {
    message("  -> Processing Sheep...")
    results_list$sheep <- calculate_population_sheep(
      census_sheep = df_sheep,
      rate_parameters = rate_parameters
    )
  }

  # C) CABRAS (Goat)
  df_goat <- census_base %>% dplyr::filter(tolower(animal_type) == "goat")
  if (nrow(df_goat) > 0) {
    message("  -> Processing Goats...")
    results_list$goat <- calculate_population_goat(
      census_goat = df_goat,
      rate_parameters = rate_parameters
    )
  }

  # --- 3. Unificación y Metadatos ---
  message("  -> Combining and attaching metadata...")

  if (length(results_list) == 0) {
    warning("⚠️ No data found for any known animal type.")
    return(tibble::tibble())
  }

  final_result <- dplyr::bind_rows(results_list) %>%
    # Unimos con categories para recuperar info descriptiva (subtype, etc.)
    # Usamos la columna 'identification' como clave única
    dplyr::left_join(
      categories %>% dplyr::select(identification, animal_type, animal_subtype),
      by = "identification"
    ) %>%
    # Orden y selección final de columnas
    dplyr::select(
      group, zone, identification, animal_type, animal_subtype, population
    ) %>%
    # Limpieza final: aseguramos que población sea numérico y ordenamos
    dplyr::mutate(population = as.numeric(population)) %>%
    dplyr::arrange(group, zone, identification)

  # --- 4. Guardado ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(final_result, "output/population_result.csv")
    message("💾 Saved output to output/population_result.csv")
  }

  return(final_result)
}
