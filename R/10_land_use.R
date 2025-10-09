#' Calcular uso de tierra
#'
#' Calcula el uso de tierra (m²) basado en consumo de ingredientes y rendimiento de cultivos.
#'
#' @param animal Character string (opcional). Tipo de ganado (columna `animal_type`). Default NULL.
#' @param type Character string (opcional). Subtipo de ganado (columna `animal_subtype`). Default NULL.
#' @param zone Character vector (opcional). Filtrar por zona (columna `Zone`). Default NULL.
#' @param saveoutput Logical. Si TRUE, guarda el resultado en "output/Land_use.csv". Default FALSE.
#'
#' @return Tibble con uso de tierra por ingrediente.
#' @export
#' @examples
#' \donttest{
#' calculate_land_use(animal = "Cattle", type = "Dairy", zone = c("A"), saveoutput = TRUE)
#' }
calculate_land_use <- function(animal = NULL, type = NULL, zone = NULL, saveoutput = TRUE) {

  # Añadir ingesta total a zones
  zones <- diet %>%
    dplyr::left_join(
      categories %>%
        dplyr::select(code, animal_type, animal_subtype, dm_ingested_total),
      by = c("code","animal_type","animal_subtype")
    ) %>%
    dplyr::mutate(
      forage_kg = forage_share/100 * dm_ingested_total,
      concentrate_kg = feed_share/100 * dm_ingested_total,
      milk_kg = milk_share/100 * dm_ingested_total,
      milk_replacer_kg = milk_replacer_share/100 * dm_ingested_total
    )

  # Unir diet con datos de ingesta y calcular consumo
  diet <- ingredients %>%
    dplyr::left_join(
      zones %>%
        dplyr::select(code, zone, animal_type, animal_subtype,
                      forage_kg, concentrate_kg, milk_kg, milk_replacer_kg),
      by = c("code", "zone", "animal_type", "animal_subtype")
    ) %>%
    dplyr::mutate(
      ingredient_share,
      Consumption_kg = dplyr::case_when(
        ingredient_type == "Forage" ~ ingredient_share/100 * forage_kg,
        ingredient_type == "Milk_replacer" ~ ingredient_share/100 * milk_replacer_kg,
        ingredient_type == "Milk" ~ ingredient_share/100 * milk_kg,
        TRUE ~ ingredient_share/100 * concentrate_kg
      )
    ) %>%
    dplyr::left_join(
      crops %>% dplyr::select(ingredient, animal_type, dry_matter_yield),
      by = c("ingredient","animal_type")
    ) %>%
    dplyr::mutate(
      Land_use = ifelse(
        dry_matter_yield > 0,
        Consumption_kg / dry_matter_yield * 10000,  # m²
        0
      )
    )

  result <- diet %>%
    dplyr::select(code, zone, animal_type, animal_subtype,
                  ingredient, ingredient_type,
                  ingredient_share, forage_kg, concentrate_kg, milk_kg, milk_replacer_kg,
                  Consumption_kg, dry_matter_yield, Land_use)

  # --- FILTRO OPCIONAL (ya con captura del argumento) ---
  if (!is.null(animal)) result <- result %>% dplyr::filter(.data$animal_type %in% animal)
  if (!is.null(type)) result <- result %>% dplyr::filter(.data$animal_subtype %in% type)
  if (!is.null(zone)) {
    z <- zone
    result <- result %>% dplyr::filter(.data$zone %in% z)
  }


  if (saveoutput) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(result, "output/Land_use.csv")
  }

  return(result)
}




