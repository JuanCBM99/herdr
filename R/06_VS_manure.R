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

  # --- 1. Data Merging ---
  results <- calculate_ge(saveoutput = FALSE) %>%
    dplyr::select(dplyr::all_of(join_keys), GE_MJday, DE_pct) %>%
    dplyr::left_join(
      calculate_weighted_variable(saveoutput = FALSE) %>%
        dplyr::select(dplyr::all_of(join_keys), ASH_pct),
      by = join_keys
    ) %>%

    # --- 2. VS Calculation (IPCC Eq 10.24) ---
    dplyr::mutate(
      across(
        c(GE_MJday, DE_pct, ASH_pct),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),

      VS_kgday = ((GE_MJday * (1 - DE_pct/100)) + (urinary_energy * GE_MJday)) * ((1 - ASH_pct/100) / 18.45)
    ) %>%

    # --- 3. Final Output and Cleanup ---
    dplyr::select(
      dplyr::all_of(join_keys),
      GE_MJday, DE_pct, ASH_pct, VS_kgday
    ) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))

  # --- 4. Save Output ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/VS.csv")
    message("\U0001f4be Saved output to output/VS.csv")
  }

  return(results)
}
