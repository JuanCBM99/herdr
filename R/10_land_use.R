#' Calculate land use (Refactored & Fixed)
#'
#' Computes total land use (m²) per animal based on diet and yields.
#' @export
calculate_land_use <- function(saveoutput = TRUE) {

  message("🟢 Calculating land use...")

  # --- 1. Carga de Datos ---
  categories  <- load_dataset("categories")
  diet        <- load_dataset("diet")
  ingredients <- load_dataset("ingredients")
  crops       <- load_dataset("crops")
  pop_df      <- calculate_population(saveoutput = FALSE)

  stopifnot("dm_ingested_total" %in% names(categories), "dry_matter_yield" %in% names(crops))

  # --- 2. Procesamiento y Cálculo ---
  results <- diet %>%
    dplyr::left_join(ingredients, by = c("group", "zone", "diet_tag")) %>%
    dplyr::left_join(
      dplyr::select(categories, identification, diet_tag, animal_type, animal_subtype, dm_ingested_total),
      by = "diet_tag"
    ) %>%
    dplyr::left_join(
      dplyr::select(crops, ingredient, dry_matter_yield),
      by = "ingredient"
    ) %>%

    dplyr::mutate(
      share_weight = dplyr::case_when(
        ingredient_type == "feed"          ~ feed_share,
        ingredient_type == "forage"        ~ forage_share,
        ingredient_type == "milk"          ~ milk_share,
        ingredient_type == "milk_replacer" ~ milk_replacer_share,
        TRUE                               ~ 0
      ),

      across(
        c(dm_ingested_total, ingredient_share, share_weight, dry_matter_yield),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),

      consumption_kg = dm_ingested_total * ingredient_share * share_weight / 10000,

      land_use_m2_per_unit = dplyr::if_else(dry_matter_yield > 0, (consumption_kg / dry_matter_yield) * 10000, 0)
    ) %>%

    dplyr::left_join(pop_df, by = c("group", "zone", "identification", "animal_type", "animal_subtype")) %>%
    dplyr::mutate(
      population = tidyr::replace_na(population, 0),
      land_use_total = land_use_m2_per_unit * population
    ) %>%
    # >>> ADICIÓN MÍNIMA AQUÍ <<<
    tidyr::drop_na(identification) %>%
    # >>> FIN DE LA ADICIÓN <<<
    dplyr::group_by(group, zone, identification, animal_type, animal_subtype, population) %>%
    dplyr::summarise(
      Total_Consumption_kg = sum(consumption_kg, na.rm = TRUE),
      Land_use_per_animal  = sum(land_use_m2_per_unit, na.rm = TRUE),
      Land_use_Total_m2    = sum(land_use_total, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))

  # --- 3. Guardado ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/Land_use.csv")
    message("💾 Saved output to output/Land_use.csv")
  }

  return(results)
}
