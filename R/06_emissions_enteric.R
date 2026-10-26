#' Calculate methane emissions from enteric fermentation
#'
#' Computes enteric methane emissions based on Gross Energy (GE),
#' Digestible Energy (DE), NDF, and Ym factor using IPCC Tier 2 logic for ruminants,
#' and the IPCC Tier 1 fixed emission factor for swine (1.5 kg CH4/head/year, high
#' productivity systems; IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.10 and Eq. 10.19).
#'
#' @param automatic_cycle Logical. If TRUE, uses the built-in model for automatic farm cycle calculation. Default is FALSE.
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @param data_dir Path to the directory containing input CSV/data files. Defaults to "user_data".
#' @param diet_vars Optional precalculated weighted nutritional variables dataframe.
#' @param ge_df Optional precalculated Gross Energy dataframe.
#' @param pop_df Optional precalculated population dataframe.
#' @export
calculate_emissions_enteric <- function(automatic_cycle = FALSE, saveoutput = TRUE, data_dir = "user_data",
                                        diet_vars = NULL, ge_df = NULL, pop_df = NULL) {
  message("Calculating enteric fermentation emissions...")

  # --- 1. Data Loading and Propagation ---
  if (is.null(diet_vars)) diet_vars <- suppressMessages(calculate_weighted_variable(saveoutput = FALSE, data_dir = data_dir))
  if (is.null(ge_df)) ge_df <- suppressMessages(calculate_ge(saveoutput = FALSE, data_dir = data_dir, de_df = diet_vars))
  if (is.null(pop_df)) pop_df <- suppressMessages(calculate_population(automatic_cycle = automatic_cycle, saveoutput = FALSE, data_dir = data_dir))
  ruminant_file <- file.path(data_dir, "ruminant_definitions.csv")
  livestock_definitions <- if (file.exists(ruminant_file)) {
    suppressMessages(readr::read_csv(ruminant_file, show_col_types = FALSE))
  } else {
    NULL
  }

  if (nrow(diet_vars) == 0) {
    message("[herdr] No diet data found. Returning empty structure.")
    return(dplyr::tibble())
  }

  join_keys <- c("animal_tag", "region", "subregion", "class_flex", "animal_type", "animal_subtype")

  # --- 2. Processing Pipeline: Merge Energy and Population ---
  results <- diet_vars %>%
    dplyr::left_join(
      ge_df %>% dplyr::select(dplyr::all_of(join_keys), GE_MJday),
      by = join_keys
    ) %>%
    dplyr::left_join(
      pop_df %>% dplyr::select(dplyr::all_of(join_keys), population),
      by = join_keys
    )

  if (!is.null(livestock_definitions) && "milk_yield_kg_year" %in% names(livestock_definitions)) {
    def_keys <- intersect(names(livestock_definitions), join_keys)
    results <- results %>%
      dplyr::left_join(
        livestock_definitions %>% dplyr::select(dplyr::all_of(c(def_keys, "milk_yield_kg_year"))),
        by = def_keys
      )
  }

  results <- results %>%
    # --- 3. Ym and Emission Factor Calculations (IPCC Tier 2 for ruminants, Tier 1 for swine) ---
    dplyr::mutate(
      dplyr::across(c(DE_pct, NDF_pct, GE_MJday, population), ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)),
      milk_yield_kg_year = if ("milk_yield_kg_year" %in% names(.)) tidyr::replace_na(suppressWarnings(as.numeric(milk_yield_kg_year)), 0) else 0,
      Ym_pct = dplyr::case_when(
        animal_type == "sheep" ~ 6.7,
        animal_type == "goat" ~ 5.5,
        animal_type == "cattle" & animal_subtype == "dairy" & milk_yield_kg_year > 0 ~ dplyr::case_when(
          DE_pct >= 70 & NDF_pct < 35 ~ 5.7,
          DE_pct >= 70 & NDF_pct >= 35 ~ 6.0,
          DE_pct >= 63 & DE_pct < 70 & NDF_pct > 37 ~ 6.3,
          DE_pct <= 62 & NDF_pct > 38 ~ 6.5,
          TRUE ~ 6.5
        ),
        animal_type == "cattle" ~ dplyr::case_when(
          DE_pct >= 75 ~ 3.0,
          DE_pct >= 72 ~ 4.0,
          DE_pct > 62 & DE_pct < 72 ~ 6.3,
          DE_pct <= 62 ~ 7.0,
          TRUE ~ 6.3
        ),
        TRUE ~ 0
      ),

      # EF in kg CH4 / head / year
      EF_kgheadyear = dplyr::case_when(
        # [IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.10 + Eq. 10.19] Swine Tier 1,
        # high productivity systems (kg CH4/head/year)
        animal_type == "swine" ~ 1.5,
        # Ruminants Tier 2 energy balance
        animal_type %in% c("cattle", "sheep", "goat") ~ (GE_MJday * (Ym_pct / 100) * 365) / 55.65,
        # Poultry and non-producing categories: negligible enteric emissions
        TRUE ~ 0
      ),
      total_CH4_enteric_Ggyear = EF_kgheadyear * (population / 1e6)
    ) %>%
    # --- 4. Final Cleanup ---
    dplyr::select(
      dplyr::all_of(join_keys),
      DE_pct, NDF_pct, GE_MJday, Ym_pct, EF_kgheadyear, population, total_CH4_enteric_Ggyear
    )

  # --- 5. Save Results ---
  if (isTRUE(saveoutput) && nrow(results) > 0) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/enteric_emissions.csv")
    message("[herdr] Saved output to output/enteric_emissions.csv")
  }

  return(results)
}
