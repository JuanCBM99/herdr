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

  # Mandatory 4-key identity structure for cross-module consistency
  join_keys <- c("region", "subregion", "animal_tag", "class_flex", "animal_type", "animal_subtype")

  # --- 1. Data Merging ---

  # Fetch GE and DE (from Energy Module) - GE results already use the 4-key structure
  results <- calculate_ge(saveoutput = FALSE) %>%
    dplyr::select(dplyr::all_of(join_keys), ge_MJ_day, de) %>%

    # Join Ash content (from Diet Characteristics) - Essential for VS calculation
    dplyr::left_join(
      calculate_weighted_variable(saveoutput = FALSE) %>%
        dplyr::select(dplyr::all_of(join_keys), ash),
      by = join_keys
    ) %>%

    # --- 2. VS Calculation (IPCC Eq 10.24) ---
    dplyr::mutate(
      # Numerical safety: replace NAs with 0
      across(
        c(ge_MJ_day, de, ash),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),

      # Define Urinary Energy Factor
      ue_factor = as.numeric(urinary_energy),

      # Final VS calculation (kg dm/animal/day)
      # Formula: VS = [GE * (1 - DE/100) + (UE * GE)] * [(1 - ASH/100) / 18.45]
      vs = ((ge_MJ_day * (1 - de/100)) + (ue_factor * ge_MJ_day)) * ((1 - ash/100) / 18.45)
    ) %>%

    # --- 3. Final Output and Cleanup ---
    dplyr::select(
      dplyr::all_of(join_keys),
      ge_MJ_day, de, ash, urinary_energy = ue_factor, vs
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
