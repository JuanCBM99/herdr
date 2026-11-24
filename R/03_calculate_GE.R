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
#' @return A tibble with daily Net Energy components, REM, REG, DE_pct, and Gross Energy
#'   requirements (\code{GE_MJday}, MJ/day) by animal cohort.
#' @examples
#' \dontrun{
#' calculate_ge(saveoutput = FALSE, data_dir = "user_data")
#' }
#' @export
calculate_ge <- function(saveoutput = TRUE, data_dir = "user_data",
                         de_df = NULL,
                         NEm_df = NULL, NEa_df = NULL, NEg_df = NULL,
                         NEl_df = NULL, NE_work_df = NULL,
                         NE_pregnancy_df = NULL, NE_wool_df = NULL) {
  message("Calculating Gross Energy (GE)...")

  join_keys_univ <- c("farm_id", "region", "subregion", "animal_tag", "animal_type", "animal_subtype")

  # --- 1. Database Preparation ---
  if (is.null(de_df)) {
    de_df <- calculate_weighted_variable(saveoutput = FALSE, data_dir = data_dir)
  }
  de_df <- de_df %>%
    dplyr::select(dplyr::all_of(join_keys_univ), DE_pct)

  # --- 2. NE Components Collection (Load shared tables & validate ONCE) ---
  message("  -> Fetching NE components...")

  need_ne <- any(vapply(
    list(NEm_df, NEa_df, NEg_df, NEl_df, NE_work_df, NE_pregnancy_df, NE_wool_df),
    is.null,
    logical(1)
  ))

  if (need_ne) {
    weights_shared <- herdr_read_input_csv(
      file.path(data_dir, "livestock_weights.csv"),
      optional = TRUE
    )
    categories_shared <- herdr_read_input_csv(
      file.path(data_dir, "ruminant_definitions.csv"),
      extra_char_cols = c("c", "a", "b"),
      optional = TRUE
    )
    coeff_path <- file.path(data_dir, "ipcc_coefficients.csv")
    coefficients_shared <- if (file.exists(coeff_path)) {
      readr::read_csv(
        coeff_path,
        col_types = readr::cols(description = "c"),
        show_col_types = FALSE
      )
    } else {
      tibble::tibble(description = character(), value = numeric())
    }
    if (nrow(categories_shared) > 0) {
      herdr_validate_ruminant_coefficients(categories_shared, coefficients_shared)
    }
  }

  get_ne <- function(df, func, col_name, ...) {
    if (!is.null(df)) {
      dplyr::select(df, dplyr::all_of(join_keys_univ), dplyr::all_of(col_name))
    } else {
      suppressMessages(func(saveoutput = FALSE, data_dir = data_dir, ...)) %>%
        dplyr::select(dplyr::all_of(join_keys_univ), dplyr::all_of(col_name))
    }
  }

  NEm_df <- get_ne(
    NEm_df, calculate_NEm, "NEm_MJday",
    weights = weights_shared, categories = categories_shared, coefficients = coefficients_shared, validate = FALSE
  )
  NEa_df <- get_ne(
    NEa_df, calculate_NEa, "NEa_MJday",
    nem_df = NEm_df, weights = weights_shared, categories = categories_shared, coefficients = coefficients_shared
  )
  NEg_df <- get_ne(
    NEg_df, calculate_NEg, "NEg_MJday",
    weights = weights_shared, categories = categories_shared, coefficients = coefficients_shared, validate = FALSE
  )
  NEl_df <- get_ne(
    NEl_df, calculate_NEl, "NEl_MJday",
    categories = categories_shared, weights = weights_shared
  )
  NE_work_df <- get_ne(
    NE_work_df, calculate_NE_work, "NEwork_MJday",
    nem_df = NEm_df, categories = categories_shared
  )
  NE_pregnancy_df <- get_ne(
    NE_pregnancy_df, calculate_NE_pregnancy, "NEpregnancy_MJday",
    nem_df = NEm_df, categories = categories_shared, coefficients = coefficients_shared, validate = FALSE
  )
  NE_wool_df <- get_ne(
    NE_wool_df, calculate_NE_wool, "NEwool_MJday",
    categories = categories_shared, weights = weights_shared
  )

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
      dplyr::across(
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
      farm_id, region, subregion, animal_tag, animal_type, animal_subtype,
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
