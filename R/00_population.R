#' Calculate total animal population (non-interactive version)
#'
#' Loads base population data (census.csv) and calculates the full
#' population structure for all animal types (cattle, sheep, etc.).
#'
#' @param saveoutput Logical (optional). If TRUE, saves the result as CSV. Default TRUE.
#'
#' @return A tibble with the full population, including columns 'group' and 'zone'.
#' @export
#'
calculate_population <- function(saveoutput = TRUE) {

  message("🟢 Loading census, rates, and categories...")

  # --- 1. Load datasets safely ---
  tryCatch({
    census_base     <- load_dataset("census")
    rate_parameters <- load_dataset("rate_parameters")
    categories      <- load_dataset("categories")
  }, error = function(e) {
    stop("Error loading datasets with load_dataset(). Original error: ", e$message)
  })

  # --- 2. The Dispatcher ---
  message("Splitting census by animal_type...")

  # Dividir el censo por animal_type
  census_split <- split(census_base, census_base$animal_type)

  results_list <- list() # Un lugar para guardar los resultados

  # --- 3. Calcular Vacuno (Cattle) ---
  if ("cattle" %in% names(census_split)) {
    results_list$cattle <- calculate_population_cattle(
      census_cattle = census_split$cattle,
      rate_parameters = rate_parameters,
      categories = categories # (Solo 'cattle' usa 'categories' para 'zone=NA')
    )
  }

  # --- 4. Calcular Ovejas (Sheep) ---
  if ("sheep" %in% names(census_split)) {
    results_list$sheep <- calculate_population_sheep(
      census_sheep = census_split$sheep,
      rate_parameters = rate_parameters
    )
  }

  # --- 5. Calcular Cabras (Goat) ---
  if ("goat" %in% names(census_split)) {
    results_list$goat <- calculate_population_goat(
      census_goat = census_split$goat,
      rate_parameters = rate_parameters
    )
  }

  # --- 6. Unir todos los resultados ---
  message("Combining all results...")
  final_result <- dplyr::bind_rows(results_list)

  # --- 7. Join with categories (Paso final) ---
  message("Attaching metadata (animal_type, subtype)...")
  result_with_metadata <- final_result %>%
    dplyr::left_join(
      # Seleccionamos las claves de 'categories'
      dplyr::select(categories, identification, animal_type, animal_subtype),
      by = "identification",
      na_matches = "na"
    ) %>%
    # Asegurarnos de que las columnas están en el orden correcto
    dplyr::select(
      group, zone, identification, animal_type, animal_subtype, population
    )

  # --- 8. Save output ---
  if (isTRUE(saveoutput)) {
    dir.create("output", showWarnings = FALSE)
    readr::write_csv(result_with_metadata, "output/population_result.csv")
    message("💾 Saved to output/population_result.csv")
  }

  return(result_with_metadata)
}
