#' Calculate Volatile Solids (VS) for Animals (Refactored)
#'
#' Computes volatile solids (VS) based on Gross Energy (GE), Digestible Energy (DE),
#' and Ash content.
#'
#' @param urinary_energy Numeric. Fraction of energy lost in urine. Default 0.04.
#' @param saveoutput Logical. If TRUE, saves the result as CSV. Default TRUE.
#' @return Tibble with VS for all animal categories.
#' @export
calculate_vs <- function(urinary_energy = 0.04, saveoutput = TRUE) {

  message("🟢 Calculating Volatile Solids (VS)...")

  # Claves de unión completas
  join_keys <- c("group", "zone", "identification", "animal_type", "animal_subtype")

  # --- 1. Procesamiento y Cálculo ---
  results <- calculate_ge(saveoutput = FALSE) %>%
    dplyr::select(all_of(join_keys), ge, de) %>%

    # Unir datos de dieta (Ash)
    dplyr::left_join(
      calculate_weighted_variable(saveoutput = FALSE) %>%
        dplyr::select(all_of(join_keys), ash),
      by = join_keys
    ) %>%

    dplyr::mutate(
      # Limpieza de tipos (Seguridad numérica)
      across(
        c(ge, de, ash),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),

      # Añadimos el parámetro como columna por transparencia
      ue_factor = as.numeric(urinary_energy),

      # Cálculo de VS
      # Fórmula: [ Energía ingerida y no digerida + Energía urinaria ] / Contenido energético de materia orgánica
      vs = ((ge * (1 - de/100)) + (ue_factor * ge)) * ((1 - ash/100) / 18.45)
    ) %>%

    # Selección y redondeo
    dplyr::select(
      all_of(join_keys),
      ge, de, ash, urinary_energy = ue_factor, vs
    ) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))

  # --- 2. Guardado ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/VS.csv")
    message("💾 Saved output to output/VS.csv")
  }

  return(results)
}
