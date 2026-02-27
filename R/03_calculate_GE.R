#' Calculate Gross Energy (GE)
#'
#' Computes gross energy requirements by aggregating Net Energy (NE) components.
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @export
calculate_ge <- function(saveoutput = TRUE) {

  message("\U0001f7e2 Calculating Gross Energy (GE)...")

  # --- 1. Database Preparation ---
  de_df <- calculate_weighted_variable(saveoutput = FALSE) %>%
    dplyr::select(region, subregion, animal_tag, class_flex, animal_type, animal_subtype, de)

  join_keys_univ <- c("region", "subregion", "animal_tag", "class_flex", "animal_type", "animal_subtype")

  # --- 2. NE Components Collection ---
  message("  -> Fetching NE components...")

  get_ne <- function(func, col_name) {
    suppressMessages(func(saveoutput = FALSE)) %>%
      dplyr::select(dplyr::all_of(join_keys_univ), dplyr::all_of(col_name))
  }

  NEm_df          <- get_ne(calculate_NEm, "NEm")
  NEa_df          <- get_ne(calculate_NEa, "NEa")
  NEg_df          <- get_ne(calculate_NEg, "NEg")
  NEl_df          <- get_ne(calculate_NEl, "NEl")
  NE_work_df      <- get_ne(calculate_NE_work, "NE_work")
  NE_pregnancy_df <- get_ne(calculate_NE_pregnancy, "NE_pregnancy")
  NE_wool_df      <- get_ne(calculate_NE_wool, "NE_wool")

  # --- 3. Aggregation and GE Calculation (IPCC) ---
  results <- de_df %>%
    dplyr::left_join(NEm_df,          by = join_keys_univ) %>%
    dplyr::left_join(NEa_df,          by = join_keys_univ) %>%
    dplyr::left_join(NEg_df,          by = join_keys_univ) %>%
    dplyr::left_join(NEl_df,          by = join_keys_univ) %>%
    dplyr::left_join(NE_work_df,      by = join_keys_univ) %>%
    dplyr::left_join(NE_pregnancy_df, by = join_keys_univ) %>%
    dplyr::left_join(NE_wool_df,      by = join_keys_univ) %>%

    dplyr::mutate(
      across(
        c(de, NEm, NEa, NEg, NEl, NE_work, NE_pregnancy, NE_wool),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),
      de_safe = dplyr::if_else(de == 0, 60, de),
      de_percent = de_safe / 100,
      rem = 1.123 - (4.092e-3 * de_safe) + (1.126e-5 * de_safe^2) - (25.4 / de_safe),
      reg = 1.164 - (5.16e-3 * de_safe) + (1.308e-5 * de_safe^2) - (37.4 / de_safe),
      rem = dplyr::if_else(rem <= 0, 1, rem),
      reg = dplyr::if_else(reg <= 0, 1, reg),
      ge = ((NEm + NEa + NEl + NE_work + NE_pregnancy) / rem + (NEg + NE_wool) / reg) / de_percent
    ) %>%

    dplyr::select(
      region, subregion, animal_tag, class_flex, animal_type, animal_subtype,
      NEm, NEa, NEg, NEl, NE_pregnancy, ge, de, rem, reg
    ) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))

  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/ge_result.csv")
    message("\U0001f4be Saved output to output/ge_result.csv")
  }

  return(results)
}
