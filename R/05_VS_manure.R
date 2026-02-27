#' Calculate Volatile Solids (VS) for Animals
#'
#' Computes volatile solids (VS) based on Gross Energy (GE), Digestible Energy (DE),
#' and Ash content using IPCC Tier 2 methodology.
#'
#' @param urinary_energy Numeric. Fraction of energy lost in urine. Default 0.04.
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @return Tibble with VS for all animal categories.
#' @export
calculate_vs <- function(urinary_energy = 0.04, saveoutput = TRUE) {

  message("\U0001f7e2 Calculating Volatile Solids (VS)...")

  join_keys <- c("region", "subregion", "animal_tag", "class_flex", "animal_type", "animal_subtype")

  results <- calculate_ge(saveoutput = FALSE) %>%
    dplyr::select(dplyr::all_of(join_keys), ge, de) %>%

    dplyr::left_join(
      calculate_weighted_variable(saveoutput = FALSE) %>%
        dplyr::select(dplyr::all_of(join_keys), ash),
      by = join_keys
    ) %>%

    dplyr::mutate(
      across(
        c(ge, de, ash),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),
      ue_factor = as.numeric(urinary_energy),
      vs = ((ge * (1 - de/100)) + (ue_factor * ge)) * ((1 - ash/100) / 18.45)
    ) %>%

    dplyr::select(
      dplyr::all_of(join_keys),
      ge, de, ash, urinary_energy = ue_factor, vs
    ) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))

  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/VS.csv")
    message("\U0001f4be Saved output to output/VS.csv")
  }

  return(results)
}
