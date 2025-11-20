#' Calculate land use
#'
#' Computes total land use (m²) for all animal categories, groups, and zones
#' based on the 'diet_tag' architecture.
#'
#' @param saveoutput Logical. If TRUE, saves the result to "output/Land_use.csv". Default TRUE.
#' @return Tibble with **total land use per animal**.
#' @export
calculate_land_use <- function(saveoutput = TRUE) {

  message("🟢 Calculating land use...")

  # --- 1) Cargar datasets base ---
  message("  -> Loading data (categories, diet, ingredients, crops, population)...")
  categories <- load_dataset("categories")
  diet <- load_dataset("diet")
  ingredients <- load_dataset("ingredients")
  crops <- load_dataset("crops")
  pop_df <- calculate_population(saveoutput = FALSE)

  # --- 2) Validaciones (Sin cambios) ---
  if (!"dm_ingested_total" %in% names(categories)) {
    stop("Error: 'categories.csv' debe contener la columna 'dm_ingested_total'.")
  }
  if (!"dry_matter_yield" %in% names(crops)) {
    stop("Error: 'crops.csv' debe contener la columna 'dry_matter_yield'.")
  }

  # --- 3) Preparar la Dieta Completa (Ingesta por Ingrediente) ---

  join_keys_diet <- c("group", "zone", "diet_tag")

  diet_full <- diet %>%
    dplyr::left_join(ingredients, by = join_keys_diet, na_matches = "na")

  translator <- categories %>%
    dplyr::select(identification, diet_tag, animal_type, animal_subtype, dm_ingested_total)

  joined <- translator %>%
    dplyr::left_join(diet_full, by = "diet_tag", na_matches = "na")

  # --- 4) Calcular Consumo por Ingrediente (Sin cambios) ---

  consumption_data <- joined %>%
    dplyr::mutate(weight = dplyr::case_when(
      ingredient_type == "feed"        ~ feed_share,
      ingredient_type == "forage"      ~ forage_share,
      ingredient_type == "milk"        ~ milk_share,
      ingredient_type == "milk_replacer" ~ milk_replacer_share,
      TRUE ~ 0
    )) %>%
    dplyr::mutate(
      dplyr::across(
        c("dm_ingested_total", "ingredient_share", "weight"),
        ~ tidyr::replace_na(., 0)
      )
    ) %>%
    dplyr::mutate(
      Consumption_kg_per_animal = dm_ingested_total * ingredient_share * weight / 10000
    ) %>%
    dplyr::select(
      group, zone, identification, animal_type, animal_subtype,
      ingredient, ingredient_type, Consumption_kg_per_animal
    )

  # --- 5) Calcular Land Use (por ingrediente) ---

  join_keys_pop <- c("group", "zone", "identification", "animal_type", "animal_subtype")
  join_keys_crops <- c("ingredient", "animal_type")

  result_detailed <- consumption_data %>%
    dplyr::left_join(pop_df, by = join_keys_pop, na_matches = "na") %>%
    dplyr::left_join(crops, by = join_keys_crops, na_matches = "na") %>%
    dplyr::mutate(
      population = tidyr::replace_na(population, 0),
      Land_use_per_animal = dplyr::if_else(
        !is.na(dry_matter_yield) & dry_matter_yield > 0,
        Consumption_kg_per_animal / dry_matter_yield * 10000, # m²
        0
      ),
      Land_use = Land_use_per_animal * population
    )

  # --- ¡NUEVO! Paso 5b: Agrupar a un nivel relevante ---
  # (Sumar el uso de tierra de todos los ingredientes para cada animal)
  result_aggregated <- result_detailed %>%
    dplyr::group_by(
      group, zone, identification, animal_type, animal_subtype, population
    ) %>%
    dplyr::summarise(
      Total_Consumption_kg_per_animal = sum(Consumption_kg_per_animal, na.rm = TRUE),
      Land_use_per_animal = sum(Land_use_per_animal, na.rm = TRUE),
      Land_use_Total_m2 = sum(Land_use, na.rm = TRUE), # Esta es la columna clave
      .groups = "drop"
    )

  # --- 6) Guardar CSV ---
  if (isTRUE(saveoutput)) {
    dir.create("output", showWarnings = FALSE)
    # Guardamos el resultado agregado, no el detallado
    readr::write_csv(result_aggregated, "output/Land_use.csv")
    message("💾 Saved output to output/Land_use.csv")
  }

  return(result_aggregated) # Devolvemos el agregado
}
