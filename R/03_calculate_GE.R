#' Calculate Gross Energy (GE)
#'
#' Computes gross energy requirements by aggregating Net Energy (NE) components.
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @param data_dir Character. Path to the folder containing input CSV files. Default is \code{"user_data"}.
#' @param de_df Optional precalculated weighted variable dataframe (with DE_pct).
#' @param NEm_df Optional precalculated NEm dataframe.
#' @param NEa_df Optional precalculated NEa dataframe.
#' @param NEg_df Optional precalculated NEg dataframe.
#' @param NEl_df Optional precalculated NEl dataframe.
#' @param NE_work_df Optional precalculated NE_work dataframe.
#' @param NE_pregnancy_df Optional precalculated NE_pregnancy dataframe.
#' @param NE_wool_df Optional precalculated NE_wool dataframe.
#' @export
calculate_ge <- function(saveoutput = TRUE, data_dir = "user_data",
                         de_df = NULL,
                         NEm_df = NULL, NEa_df = NULL, NEg_df = NULL,
                         NEl_df = NULL, NE_work_df = NULL,
                         NE_pregnancy_df = NULL, NE_wool_df = NULL) {
  message("Calculating Gross Energy (GE)...")

  join_keys_univ <- c("region", "subregion", "animal_tag", "class_flex", "animal_type", "animal_subtype")

  # --- 1. Database Preparation ---
  if (is.null(de_df)) {
    de_df <- calculate_weighted_variable(saveoutput = FALSE, data_dir = data_dir)
  }
  de_df <- de_df %>%
    dplyr::select(dplyr::all_of(join_keys_univ), DE_pct)

  # --- 2. NE Components Collection ---
  message("  -> Fetching NE components...")

  get_ne <- function(df, func, col_name, ...) {
    if (!is.null(df)) {
      dplyr::select(df, dplyr::all_of(join_keys_univ), dplyr::all_of(col_name))
    } else {
      suppressMessages(func(saveoutput = FALSE, data_dir = data_dir, ...)) %>%
        dplyr::select(dplyr::all_of(join_keys_univ), dplyr::all_of(col_name))
    }
  }

  NEm_df <- get_ne(NEm_df, calculate_NEm, "NEm_MJday")
  NEa_df <- get_ne(NEa_df, calculate_NEa, "NEa_MJday", nem_df = NEm_df)
  NEg_df <- get_ne(NEg_df, calculate_NEg, "NEg_MJday")
  NEl_df <- get_ne(NEl_df, calculate_NEl, "NEl_MJday")
  NE_work_df <- get_ne(NE_work_df, calculate_NE_work, "NEwork_MJday", nem_df = NEm_df)
  NE_pregnancy_df <- get_ne(NE_pregnancy_df, calculate_NE_pregnancy, "NEpregnancy_MJday", nem_df = NEm_df)
  NE_wool_df <- get_ne(NE_wool_df, calculate_NE_wool, "NEwool_MJday")

  # --- 3. Aggregation and GE Calculation (IPCC) ---
  results <- de_df %>%
    dplyr::left_join(NEm_df, by = join_keys_univ) %>%
    dplyr::left_join(NEa_df, by = join_keys_univ) %>%
    dplyr::left_join(NEg_df, by = join_keys_univ) %>%
    dplyr::left_join(NEl_df, by = join_keys_univ) %>%
    dplyr::left_join(NE_work_df, by = join_keys_univ) %>%
    dplyr::left_join(NE_pregnancy_df, by = join_keys_univ) %>%
    dplyr::left_join(NE_wool_df, by = join_keys_univ) %>%
    # 4. Calculations and Cleanup
    dplyr::mutate(
      across(
        c(DE_pct, NEm_MJday, NEa_MJday, NEg_MJday, NEl_MJday, NEwork_MJday, NEpregnancy_MJday, NEwool_MJday),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),
      de_safe = dplyr::if_else(DE_pct == 0, 60, DE_pct),
      de_percent = de_safe / 100,
      REM = 1.123 - (4.092e-3 * de_safe) + (1.126e-5 * de_safe^2) - (25.4 / de_safe),
      REG = 1.164 - (5.16e-3 * de_safe) + (1.308e-5 * de_safe^2) - (37.4 / de_safe),
      REM = dplyr::if_else(REM <= 0, 1, REM),
      REG = dplyr::if_else(REG <= 0, 1, REG),
      GE_MJday = ((NEm_MJday + NEa_MJday + NEl_MJday + NEwork_MJday + NEpregnancy_MJday) / REM + (NEg_MJday + NEwool_MJday) / REG) / de_percent
    ) %>%
    # 5. Final Selection
    dplyr::select(
      region, subregion, animal_tag, class_flex, animal_type, animal_subtype,
      NEm_MJday, NEa_MJday, NEg_MJday, NEl_MJday, NEwork_MJday, NEpregnancy_MJday, NEwool_MJday, GE_MJday, DE_pct, REM, REG
    )
  # --- 6. Save Output ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/ge_result.csv")
    message("[herdr] Saved output to output/ge_result.csv")
  }

  return(results)
}
