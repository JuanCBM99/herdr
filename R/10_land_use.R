#' Calculate land use
#'
#' Computes total land use (m²) based on ingredient consumption, crop yields,
#' and animal population.
#'
#' @param animal Character string/vector (optional). Animal type (column `animal_type`). Default NULL.
#' @param type Character string/vector (optional). Subtype of animal (column `animal_subtype`). Default NULL.
#' @param zone Character vector (optional). Filter by zone (column `zone`). Default NULL.
#' @param saveoutput Logical. If TRUE, saves the result to "output/Land_use.csv". Default TRUE.
#' @param population_df (Opcional) Un dataframe de población ya calculado.
#'   Si es NULL (por defecto), la función llamará a `calculate_population()` interactivamente.
#' @return Tibble with total land use per ingredient.
#' @export
calculate_land_use <- function(animal = NULL, type = NULL, zone = NULL, saveoutput = TRUE,
                               population_df = NULL) {

  message("🟢 Calculating land use...")

  # --- 1) Cargar datasets base ---
  diet_df <- load_dataset("diet")
  ingredients_df <- load_dataset("ingredients")

  categories_df <- load_dataset("categories") %>%
    dplyr::select(.data$code, .data$animal_type, .data$animal_subtype, .data$dm_ingested_total)

  crops_df <- load_dataset("crops") %>%
    dplyr::select(.data$ingredient, .data$animal_type, .data$dry_matter_yield)

  # --- 2) Lógica de población ---
  message("🟢 Calculating population data...")

  pop_data_to_use <- NULL # Inicializar

  if (!is.null(population_df)) {
    message("♻️ Usando datos de población proporcionados.")
    pop_data_to_use <- population_df
  } else {
    message("-> No se proporcionaron datos de población. Calculando...")
    pop_data_to_use <- calculate_population(saveoutput = FALSE)
  }

  pop_df <- pop_data_to_use %>%
    dplyr::select(.data$code, .data$animal_type, .data$animal_subtype, .data$population)

  # --- 3) Calcular ingesta total por categoría y zona ---
  zones_df <- diet_df %>%
    dplyr::left_join(categories_df, by = c("code", "animal_type", "animal_subtype")) %>%
    dplyr::mutate(
      forage_kg        = .data$forage_share / 100 * .data$dm_ingested_total,
      concentrate_kg   = .data$feed_share / 100 * .data$dm_ingested_total,
      milk_kg          = .data$milk_share / 100 * .data$dm_ingested_total,
      milk_replacer_kg = .data$milk_replacer_share / 100 * .data$dm_ingested_total
    )

  # --- 4) Calcular consumo por ingrediente y Land Use ---
  result <- ingredients_df %>%
    dplyr::left_join(
      zones_df %>% dplyr::select(
        .data$code, .data$zone, .data$animal_type, .data$animal_subtype,
        .data$forage_kg, .data$concentrate_kg, .data$milk_kg, .data$milk_replacer_kg
      ),
      by = c("code", "zone", "animal_type", "animal_subtype")
    ) %>%
    dplyr::left_join(crops_df, by = c("ingredient", "animal_type")) %>%
    dplyr::left_join(pop_df, by = c("code", "animal_type", "animal_subtype")) %>%
    dplyr::mutate(
      Consumption_kg_per_animal = dplyr::case_when(
        .data$ingredient_type == "forage"        ~ .data$ingredient_share / 100 * .data$forage_kg,
        .data$ingredient_type == "milk_replacer" ~ .data$ingredient_share / 100 * .data$milk_replacer_kg,
        .data$ingredient_type == "milk"          ~ .data$ingredient_share / 100 * .data$milk_kg,
        TRUE                                     ~ .data$ingredient_share / 100 * .data$concentrate_kg
      ),
      Land_use_per_animal = dplyr::if_else(
        !is.na(.data$dry_matter_yield) & .data$dry_matter_yield > 0,
        .data$Consumption_kg_per_animal / .data$dry_matter_yield * 10000, # m²
        0
      ),
      Land_use = .data$Land_use_per_animal * .data$population
    )

  # --- 5) Filtrado opcional (usar .env para los argumentos) ---
  if (!is.null(animal)) {
    result <- result %>% dplyr::filter(.data$animal_type %in% .env$animal)
  }
  if (!is.null(type)) {
    result <- result %>% dplyr::filter(.data$animal_subtype %in% .env$type)
  }
  if (!is.null(zone)) {
    result <- result %>% dplyr::filter(.data$zone %in% .env$zone)
  }

  # --- 6) Guardar CSV ---
  if (isTRUE(saveoutput)) {
    dir.create("output", showWarnings = FALSE)
    readr::write_csv(result, "output/Land_use.csv")
    message("💾 Saved output to output/Land_use.csv")
  }

  return(result)
}
