#' Calculate methane emissions from enteric fermentation
#'
#' Computes enteric methane emissions based on Gross Energy (GE),
#' Digestible Energy (DE), NDF, and Ym factor using IPCC Tier 2 logic for ruminants,
#' and IPCC Tier 1 with metabolic weight scaling for swine (Table 10.10 & Section 10.2.4).
#'
#' @param automatic_cycle Logical. If TRUE, uses the built-in model for automatic farm cycle calculation. Default is FALSE.
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @param data_dir Character. Path to the folder containing input CSV files. Default is \code{"user_data"}.
#' @export
calculate_emissions_enteric <- function(automatic_cycle = FALSE, saveoutput = TRUE, data_dir = "user_data") {

  message("\U0001f7e2 Calculating enteric fermentation emissions...")

  # --- 1. Data Loading and Propagation ---
  diet_vars <- suppressMessages(calculate_weighted_variable(saveoutput = FALSE, data_dir = data_dir))
  ge_df     <- suppressMessages(calculate_ge(saveoutput = FALSE, data_dir = data_dir))
  pop_df    <- suppressMessages(calculate_population(automatic_cycle = automatic_cycle, saveoutput = FALSE, data_dir = data_dir))
  ruminant_file <- file.path(data_dir, "ruminant_definitions.csv")
  livestock_definitions <- if (file.exists(ruminant_file)) {
    suppressMessages(readr::read_csv(ruminant_file, show_col_types = FALSE))
  } else {
    NULL
  }
  weights_file <- file.path(data_dir, "livestock_weights.csv")
  weights_csv <- if (file.exists(weights_file)) {
    suppressMessages(readr::read_csv(weights_file, show_col_types = FALSE))
  } else {
    NULL
  }

  if (nrow(diet_vars) == 0) {
    message("\u26a0 No diet data found. Returning empty structure.")
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

  if (!is.null(weights_csv)) {
    w_keys <- intersect(names(weights_csv), join_keys)
    w_cols <- intersect(names(weights_csv), c(w_keys, "initial_weight_kg", "final_weight_kg", "adult_weight_kg"))
    results <- results %>%
      dplyr::left_join(
        weights_csv %>% dplyr::select(dplyr::all_of(w_cols)),
        by = w_keys
      )
  }

  results <- results %>%
    # --- 3. Ym and Emission Factor Calculations (IPCC Tier 2 for ruminants, Tier 1 for swine) ---
    dplyr::mutate(
      dplyr::across(c(DE_pct, NDF_pct, GE_MJday, population), ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)),
      milk_yield_kg_year = if ("milk_yield_kg_year" %in% names(.)) tidyr::replace_na(suppressWarnings(as.numeric(milk_yield_kg_year)), 0) else 0,
      initial_weight_kg  = if ("initial_weight_kg" %in% names(.)) tidyr::replace_na(suppressWarnings(as.numeric(initial_weight_kg)), 0) else 0,
      final_weight_kg    = if ("final_weight_kg" %in% names(.)) tidyr::replace_na(suppressWarnings(as.numeric(final_weight_kg)), 0) else 0,
      adult_weight_kg    = if ("adult_weight_kg" %in% names(.)) tidyr::replace_na(suppressWarnings(as.numeric(adult_weight_kg)), 0) else 0,

      live_weight = dplyr::case_when(
        initial_weight_kg > 0 & final_weight_kg > 0 ~ (initial_weight_kg + final_weight_kg) / 2,
        final_weight_kg > 0 ~ final_weight_kg,
        adult_weight_kg > 0 ~ adult_weight_kg,
        initial_weight_kg > 0 ~ initial_weight_kg,
        TRUE ~ 72
      ),

      Ym_pct = dplyr::case_when(
        animal_type == "sheep" ~ 6.7,
        animal_type == "goat"  ~ 5.5,
        animal_type == "cattle" & animal_subtype == "dairy" & milk_yield_kg_year > 0  ~ dplyr::case_when(
          DE_pct >= 70 & NDF_pct < 35 ~ 5.7,
          DE_pct >= 70 & NDF_pct >= 35  ~ 6.0,
          DE_pct >= 63 & DE_pct < 70 & NDF_pct > 37 ~ 6.3,
          DE_pct <= 62 & NDF_pct > 38  ~ 6.5,
          TRUE ~ 6.5
        ),
        animal_type == "cattle" ~ dplyr::case_when(
          DE_pct >= 75 ~ 3.0,
          DE_pct >= 72 ~ 4.0,
          DE_pct > 62 & DE_pct < 72 ~ 6.3,
          DE_pct <= 62  ~ 7.0,
          TRUE ~ 6.3
        ),
        TRUE ~ 0
      ),

      # EF in kg CH4 / head / year
      EF_kgheadyear = dplyr::case_when(
        # [IPCC 2019 Refinement Table 10.10 & Section 10.2.4] Swine Tier 1 with metabolic liveweight scaling
        animal_type == "swine" ~ dplyr::if_else(
          live_weight > 0,
          1.5 * (live_weight / 72)^0.75,
          1.5
        ),
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
    ) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3)))

  # --- 5. Save Results ---
  if (isTRUE(saveoutput) && nrow(results) > 0) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/enteric_emissions.csv")
    message("\U0001f4be Saved output to output/enteric_emissions.csv")
  }

  return(results)
}
