#' Calculate Volatile Solids (VS) for Animals
#'
#' Computes volatile solids (VS) for animal categories based on gross energy (GE),
#' digestible energy (DE), ash content, and urinary energy fraction.
#' Uses outputs from `calculate_GE()` and `calculate_weighted_variable()`.
#'
#' @param animal Character string (optional). Filter by animal type ("Cattle", "Sheep", "Goat"). Default NULL.
#' @param type Character string (optional). Only applies if `animal = "Cattle"` (e.g., "Dairy", "Beef"). Default NULL.
#' @param zone Character vector (optional). Filter by zone. Default NULL.
#' @param urinary_energy Numeric. Fraction of energy lost in urine. Default 0.04.
#' @param saveoutput Logical (optional). If TRUE, saves the result as CSV. Default FALSE.
#'
#' @examples
#' \dontrun{
#' # All Cattle
#' calculate_VS(animal = "Cattle")
#'
#' # Dairy Cattle in specific zones
#' calculate_VS(animal = "Cattle", type = "Dairy", zone = c("North", "South"))
#' }
#'
#' @export
calculate_vs <- function(animal = NULL, type = NULL, zone = NULL, urinary_energy = 0.04, saveoutput = TRUE) {

  # --- Obtener GE ---
  GE_df <- calculate_ge(animal = animal, type = type, zone = zone) %>%
    select(code, animal_type, animal_subtype, zone, ge, de)

  # --- Obtener variables ponderadas ---
  weighted_vars <- calculate_weighted_variable(animal = animal, type = type, zones = zone) %>%
    select(code, animal_type, animal_subtype, zone, ash)

  # --- Unir y calcular VS ---
  result <- inner_join(GE_df, weighted_vars,
                       by = c("code", "animal_type", "animal_subtype", "zone")) %>%
    mutate(
      urinary_energy = urinary_energy,
      vs = ((ge * (1 - de/100)) + (urinary_energy * ge)) * ((1 - ash/100) / 18.45)
    ) %>%
    select(code, animal_type, animal_subtype, zone, ge, de, ash, urinary_energy, vs)

  # --- Guardar CSV si saveoutput = TRUE ---
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    write.csv(result, "output/VS.csv", row.names = FALSE)
  }

  result
}




