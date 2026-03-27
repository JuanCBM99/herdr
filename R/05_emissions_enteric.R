#' Calculate methane emissions from enteric fermentation
#'
#' Computes enteric methane emissions based on Gross Energy (GE),
#' Digestible Energy (DE), NDF, and Ym factor using IPCC Tier 2 logic.
#'
#' @param automatic_cycle Logical. If TRUE, uses the built-in model for automatic farm cycle calculation. Default is FALSE.
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @export
calculate_emissions_enteric <- function(automatic_cycle = FALSE, saveoutput = TRUE) {

  message("\U0001f7e2 Calculating enteric fermentation emissions...")

  # --- 1. Data Loading and Propagation ---
  diet_vars <- calculate_weighted_variable(saveoutput = FALSE)
  ge_df     <- calculate_ge(saveoutput = FALSE)
  pop_df    <- calculate_population(automatic_cycle = automatic_cycle, saveoutput = FALSE)

  if (nrow(diet_vars) == 0) {
    message("\u26a0 No diet data found. Returning empty structure.")
    return(dplyr::tibble())
  }

  join_keys <- c("animal_tag", "region", "subregion", "class_flex", "animal_type", "animal_subtype")

  # --- 2. Processing Pipeline: Merge Energy and Population ---
  results <- diet_vars %>%

    dplyr::left_join(
      ge_df %>%
        dplyr::select(dplyr::all_of(join_keys), GE_MJday)) %>%

    dplyr::left_join(
      pop_df %>%
        dplyr::select(dplyr::all_of(join_keys), population)) %>%

    # --- 3. Ym and Emission Factor Calculations (IPCC Tier 2) ---
    dplyr::mutate(
      across(c(DE_pct, NDF_pct, GE_MJday, population), ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)),
      Ym_pct = dplyr::case_when(
        animal_type == "sheep" ~ 6.7,
        animal_type == "goat"  ~ 5.5,
        animal_type == "cattle" & animal_tag == "mature_dairy_cattle" ~ dplyr::case_when(
          DE_pct >= 70 & NDF_pct <= 35 ~ 5.7,
          DE_pct >= 70 & NDF_pct > 35  ~ 6.0,
          DE_pct >= 63 & DE_pct < 70 & NDF_pct > 37 ~ 6.3,
          DE_pct <= 62 & NDF_pct > 38  ~ 6.5,
          TRUE ~ 6.5
        ),
        animal_type == "cattle" & animal_tag != "mature_dairy_cattle" ~ dplyr::case_when(
          DE_pct >= 75 ~ 3.0,
          DE_pct >= 72 ~ 4.0,
          DE_pct >= 62 & DE_pct <= 71 ~ 6.3,
          DE_pct < 62  ~ 7.0,
          TRUE ~ 6.3
        ),
        TRUE ~ NA_real_
      ),

      EF_kgheadyear = (GE_MJday * (Ym_pct / 100) * 365) / 55.65,

      total_CH4_enteric_Ggyear = EF_kgheadyear * (population / 1e6)
    ) %>%

    # --- 4. Final Cleanup ---
    dplyr::select(
      dplyr::all_of(join_keys),
      DE_pct, NDF_pct, GE_MJday, Ym_pct, EF_kgheadyear, population, total_CH4_enteric_Ggyear
    ) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))

  # --- 5. Save Results ---
  if (isTRUE(saveoutput) && nrow(results) > 0) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/enteric_emissions.csv")
    message("\U0001f4be Saved output to output/enteric_emissions.csv")
  }

  return(results)
}
