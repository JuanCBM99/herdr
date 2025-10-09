#' Calculate land use
#'
#' Computes land use (m²) based on ingredient consumption and crop yields.
#'
#' @param animal Character string (optional). Animal type (column `animal_type`). Default NULL.
#' @param type Character string (optional). Subtype of animal (column `animal_subtype`). Default NULL.
#' @param zone Character vector (optional). Filter by zone (column `zone`). Default NULL.
#' @param saveoutput Logical. If TRUE, saves the result to "output/Land_use.csv". Default TRUE.
#' @return Tibble with land use per ingredient.
#' @export
calculate_land_use <- function(animal = NULL, type = NULL, zone = NULL, saveoutput = TRUE) {

  message("🟢 Calculating land use...")

  # --- 1️⃣ Cargar datasets base ---
  diet_df <- load_dataset("diet")
  ingredients_df <- load_dataset("ingredients")
  categories_df <- load_dataset("categories") %>% dplyr::select(code, animal_type, animal_subtype, dm_ingested_total)
  crops_df <- load_dataset("crops") %>% dplyr::select(ingredient, animal_type, dry_matter_yield)

  # --- 2️⃣ Calcular ingesta total por categoría y zona ---
  zones_df <- diet_df %>%
    dplyr::left_join(categories_df, by = c("code","animal_type","animal_subtype")) %>%
    dplyr::mutate(
      forage_kg = forage_share / 100 * dm_ingested_total,
      concentrate_kg = feed_share / 100 * dm_ingested_total,
      milk_kg = milk_share / 100 * dm_ingested_total,
      milk_replacer_kg = milk_replacer_share / 100 * dm_ingested_total
    )

  # --- 3️⃣ Calcular consumo por ingrediente ---
  diet_consumption <- ingredients_df %>%
    dplyr::left_join(zones_df %>%
                       dplyr::select(code, zone, animal_type, animal_subtype,
                                     forage_kg, concentrate_kg, milk_kg, milk_replacer_kg),
                     by = c("code", "zone", "animal_type", "animal_subtype")) %>%
    dplyr::mutate(
      Consumption_kg = dplyr::case_when(
        ingredient_type == "Forage" ~ ingredient_share / 100 * forage_kg,
        ingredient_type == "Milk_replacer" ~ ingredient_share / 100 * milk_replacer_kg,
        ingredient_type == "Milk" ~ ingredient_share / 100 * milk_kg,
        TRUE ~ ingredient_share / 100 * concentrate_kg
      )
    ) %>%
    dplyr::left_join(crops_df, by = c("ingredient","animal_type")) %>%
    dplyr::mutate(
      Land_use = ifelse(dry_matter_yield > 0,
                        Consumption_kg / dry_matter_yield * 10000,  # m²
                        0)
    )

  result <- diet_consumption %>%
    dplyr::select(code, zone, animal_type, animal_subtype,
                  ingredient, ingredient_type,
                  ingredient_share, forage_kg, concentrate_kg, milk_kg, milk_replacer_kg,
                  Consumption_kg, dry_matter_yield, Land_use)

  # --- 4️⃣ Filtrado opcional ---
  if (!is.null(animal)) result <- result %>% dplyr::filter(.data$animal_type %in% animal)
  if (!is.null(type)) result <- result %>% dplyr::filter(.data$animal_subtype %in% type)
  if (!is.null(zone)) result <- result %>% dplyr::filter(.data$zone %in% zone)

  # --- 5️⃣ Guardar CSV ---
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    readr::write_csv(result, "output/Land_use.csv")
    message("💾 Saved output to output/Land_use.csv")
  }

  result
}
