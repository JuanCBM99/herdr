#' Calculate Volatile Solids (VS) for Animals
#'
#' Computes volatile solids (VS) for all animals based on GE, DE, ash,
#' and urinary energy fraction.
#'
#' @param urinary_energy Numeric. Fraction of energy lost in urine. Default 0.04.
#' @param saveoutput Logical. If TRUE, saves the result as CSV. Default TRUE.
#' @return Tibble with VS for all animal categories, groups, and zones.
#' @export
calculate_vs <- function(urinary_energy = 0.04, saveoutput = TRUE) { # <-- Filtros eliminados

  message("🟢 Calculating Volatile Solids (VS)...")

  # --- 1. Calcular GE (versión sin filtros) ---
  # ge_df contendrá 'group', 'zone', 'identification', 'ge', 'de'
  GE_df <- calculate_ge(saveoutput = FALSE) %>%
    dplyr::select(group, zone, identification, animal_type, animal_subtype, ge, de)

  # --- 2. Calcular variables de dieta (ash) (versión sin filtros) ---
  # weighted_vars contendrá 'group', 'zone', 'identification', 'ash'
  weighted_vars <- calculate_weighted_variable(saveoutput = FALSE) %>%
    dplyr::select(group, zone, identification, animal_type, animal_subtype, ash)

  # --- 3. Unir datasets ---
  # ¡MODIFICADO! El join ahora incluye 'group' y 'zone'
  join_keys <- c("group", "zone", "identification", "animal_type", "animal_subtype")

  # ¡MODIFICADO! Añadimos na_matches = "na" para k1 y k2
  result <- dplyr::inner_join(GE_df, weighted_vars,
                              by = join_keys,
                              na_matches = "na")

  # --- 4. Calcular VS ---
  result <- result %>%
    dplyr::mutate(
      urinary_energy = urinary_energy,
      vs = ((ge * (1 - de/100)) + (urinary_energy * ge)) * ((1 - ash/100) / 18.45)
    ) %>%
    dplyr::select(
      group, zone, identification, animal_type, animal_subtype,
      ge, de, ash, urinary_energy, vs
    )

  # --- 5. Guardar CSV ---
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    readr::write_csv(result, "output/VS.csv") # Usamos write_csv
    message("💾 Saved output to output/VS.csv")
  }

  result
}
