#' Calculate Volatile Solids (VS) for Animals
#'
#' Computes volatile solids (VS) based on GE, DE, ash, and urinary energy fraction.
#'
#' @param animal Character string (optional). Filter by animal type. Default NULL.
#' @param type Character string (optional). Only applies if `animal = "Cattle"`. Default NULL.
#' @param zone Character vector (optional). Filter by zone. Default NULL.
#' @param urinary_energy Numeric. Fraction of energy lost in urine. Default 0.04.
#' @param saveoutput Logical. If TRUE, saves the result as CSV. Default TRUE.
#' @return Tibble with code, animal_type, animal_subtype, zone, ge, de, ash, urinary_energy, vs.
#' @export
calculate_vs <- function(animal = NULL, type = NULL, zone = NULL, urinary_energy = 0.04, saveoutput = TRUE) {

  message("🟢 Calculating Volatile Solids (VS)...")

  # --- GE ---
  GE_df <- calculate_ge(animal = animal, type = type, zone = zone, saveoutput = FALSE) %>%
    dplyr::select(code, animal_type, animal_subtype, zone, ge, de)

  # --- Weighted nutritional variables (ash) ---
  weighted_vars <- calculate_weighted_variable(animal = animal, type = type, zones = zone, saveoutput = FALSE) %>%
    dplyr::select(code, animal_type, animal_subtype, zone, ash)

  # --- Unir datasets ---
  result <- dplyr::inner_join(GE_df, weighted_vars,
                              by = c("code", "animal_type", "animal_subtype", "zone"))

  # --- Calcular VS ---
  result <- result %>%
    dplyr::mutate(
      urinary_energy = urinary_energy,
      vs = ((ge * (1 - de/100)) + (urinary_energy * ge)) * ((1 - ash/100) / 18.45)
    ) %>%
    dplyr::select(code, animal_type, animal_subtype, zone, ge, de, ash, urinary_energy, vs)

  # --- Guardar CSV ---
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    write.csv(result, "output/VS.csv", row.names = FALSE)
    message("💾 Saved output to output/VS.csv")
  }

  result
}

