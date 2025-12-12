#' Calculate Gross Energy (GE) (Fixed & Robust)
#'
#' Computes gross energy requirements by aggregating all necessary Net Energy (NE) components (NEm, NEa, NEg, NEl, NEwork, NEp, NEwool)
#' and adjusting for feed digestibility (DE) and conversion ratios (REM, REG).
#' @export
calculate_ge <- function(saveoutput = TRUE) {

  message("🟢 Calculating Gross Energy (GE)...")

  # --- 1. Data Preparation and Setup ---

  # 1. Base Data: Fetch DE (Digestibility of Energy) by Group/Zone/ID
  # This result is the baseline for all subsequent joins.
  de_df <- calculate_weighted_variable(saveoutput = FALSE) %>%
    dplyr::select(group, zone, identification, animal_type, animal_subtype, de)

  # Universal join keys for all NE components
  join_keys_univ <- c("identification", "animal_type", "animal_subtype")

  # 2. Fetch and Clean NE Components
  message("  -> Fetching NE components...")

  # Helper function to call auxiliary NE functions and select only the NE column needed
  get_ne <- function(func, col_name) {
    suppressMessages(func(saveoutput = FALSE)) %>%
      dplyr::select(all_of(join_keys_univ), all_of(col_name))
  }

  # Call and clean dataframes for each NE component
  NEm_df          <- get_ne(calculate_NEm, "NEm")
  NEa_df          <- get_ne(calculate_NEa, "NEa")
  NEg_df          <- get_ne(calculate_NEg, "NEg")
  NEl_df          <- get_ne(calculate_NEl, "NEl")
  NE_work_df      <- get_ne(calculate_NE_work, "NE_work")
  NE_pregnancy_df <- get_ne(calculate_NE_pregnancy, "NE_pregnancy")
  NE_wool_df      <- get_ne(calculate_NE_wool, "NE_wool")

  # --- 3. Data Aggregation and Calculation Pipeline ---

  # Chained Left Join: Merge all NE components onto the DE base data
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
      # Safety Check: Replace NAs with 0 for all energy inputs
      across(
        c(de, NEm, NEa, NEg, NEl, NE_work, NE_pregnancy, NE_wool),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),

      # Prepare DE for calculation (convert to fraction and handle zero division)
      de_safe = dplyr::if_else(de == 0, 60, de), # Arbitrary safe value if DE is zero
      de_percent = de_safe / 100,

      # Calculate REM (Net Energy for Maintenance/Digestible Energy Ratio)
      rem = 1.123 - (4.092e-3 * de_safe) + (1.126e-5 * de_safe^2) - (25.4 / de_safe),
      # Calculate REG (Net Energy for Growth/Digestible Energy Ratio)
      reg = 1.164 - (5.16e-3 * de_safe) + (1.308e-5 * de_safe^2) - (37.4 / de_safe),

      # Safety Check: Ensure REM/REG are not zero (default to 1)
      rem = dplyr::if_else(rem == 0, 1, rem),
      reg = dplyr::if_else(reg == 0, 1, reg),

      # Final Gross Energy (GE) Calculation (IPCC Eq 10.16)
      # GE = [ (NE_Maint/REM) + (NE_Prod/REG) ] / DE%
      ge = ((NEm + NEa + NEl + NE_work + NE_pregnancy) / rem + (NEg + NE_wool) / reg) / de_percent
    ) %>%

    # 5. Final Select and Rounding
    dplyr::select(
      group, zone, identification, animal_type, animal_subtype,
      NEm, NEa, NEg, NE_work, NE_pregnancy, NEl, NE_wool,
      de, rem, reg, ge
    ) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))

  # --- 4. Save Output ---

  # Save Output to CSV
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/ge_result.csv")
    message("💾 Saved output to output/ge_result.csv")
  }

  return(results)
}
