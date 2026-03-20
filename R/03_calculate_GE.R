#' Calculate Gross Energy (GE)
#'
#' Computes gross energy requirements by aggregating Net Energy (NE) components.
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @export
calculate_ge <- function(saveoutput = TRUE) {

  message("\U0001f7e2 Calculating Gross Energy (GE)...")

  # --- 1. Database Preparation ---
  # Base data from weighted variables using the 4-key identity structure
  de_df <- calculate_weighted_variable(saveoutput = FALSE) %>%
    dplyr::select(region, subregion, animal_tag, class_flex, animal_type, animal_subtype, de)

  # Universal join keys including class_flex for grazing/stall differentiation
  join_keys_univ <- c("region", "subregion", "animal_tag", "class_flex", "animal_type", "animal_subtype")

  # --- 2. NE Components Collection ---
  message("  -> Fetching NE components...")

  # Helper function to call NE functions and select necessary columns
  get_ne <- function(func, col_name) {
    suppressMessages(func(saveoutput = FALSE)) %>%
      dplyr::select(dplyr::all_of(join_keys_univ), dplyr::all_of(col_name))
  }

  # Fetching all energy puzzle pieces
  NEm_df          <- get_ne(calculate_NEm, "NEm_MJ_day")
  NEa_df          <- get_ne(calculate_NEa, "NEa_MJ_day")
  NEg_df          <- get_ne(calculate_NEg, "NEg_MJ_day")
  NEl_df          <- get_ne(calculate_NEl, "NEl_MJ_day")
  NE_work_df      <- get_ne(calculate_NE_work, "NE_work_MJ_day")
  NE_pregnancy_df <- get_ne(calculate_NE_pregnancy, "NE_pregnancy_MJ_day")
  NE_wool_df      <- get_ne(calculate_NE_wool, "NE_wool_MJ_day")

  # --- 3. Aggregation and GE Calculation (IPCC) ---
  results <- de_df %>%
    dplyr::left_join(NEm_df,          by = join_keys_univ) %>%
    dplyr::left_join(NEa_df,          by = join_keys_univ) %>%
    dplyr::left_join(NEg_df,          by = join_keys_univ) %>%
    dplyr::left_join(NEl_df,          by = join_keys_univ) %>%
    dplyr::left_join(NE_work_df,      by = join_keys_univ) %>%
    dplyr::left_join(NE_pregnancy_df, by = join_keys_univ) %>%
    dplyr::left_join(NE_wool_df,      by = join_keys_univ) %>%

    # 4. Calculations and Cleanup
    dplyr::mutate(
      # Numerical safety: replace NAs with 0
      across(
        c(de, NEm_MJ_day, NEa_MJ_day, NEg_MJ_day, NEl_MJ_day, NE_work_MJ_day, NE_pregnancy_MJ_day, NE_wool_MJ_day),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),

      # Digestibility (DE) handling
      de_safe = dplyr::if_else(de == 0, 60, de),
      de_percent = de_safe / 100,

      # REM (Maintenance) and REG (Growth) ratios per IPCC
      rem = 1.123 - (4.092e-3 * de_safe) + (1.126e-5 * de_safe^2) - (25.4 / de_safe),
      reg = 1.164 - (5.16e-3 * de_safe) + (1.308e-5 * de_safe^2) - (37.4 / de_safe),

      rem = dplyr::if_else(rem <= 0, 1, rem),
      reg = dplyr::if_else(reg <= 0, 1, reg),

      # FINAL IPCC GE FORMULA:
      # GE = [ (NE_Maint + NE_Activity + NE_Lact + NE_Work + NE_Preg) / REM + (NE_Growth + NE_Wool) / REG ] / DE%
      ge_MJ_day = ((NEm_MJ_day + NEa_MJ_day + NEl_MJ_day + NE_work_MJ_day + NE_pregnancy_MJ_day) / rem + (NEg_MJ_day + NE_wool_MJ_day) / reg) / de_percent
    ) %>%

    # 5. Final Selection
    dplyr::select(
      region, subregion, animal_tag, class_flex, animal_type, animal_subtype,
      NEm_MJ_day, NEa_MJ_day, NEg_MJ_day, NEl_MJ_day, NE_pregnancy_MJ_day, ge_MJ_day, de, rem, reg
    ) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))

  # --- 4. Save Output ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/ge_result.csv")
    message("\U0001f4be Saved output to output/ge_result.csv")
  }

  return(results)
}
