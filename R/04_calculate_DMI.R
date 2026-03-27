#' Calculate Dry Matter Intake (DMI)
#'
#' Computes daily Dry Matter Intake (kg DM/day) based on metabolic demand (GE/ED).
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @export
calculate_DMI <- function(saveoutput = TRUE) {

  message("\U0001f37d Calculating Dry Matter Intake (DMI)...")

  # --- 1. Load Data ---
  ge_req    <- calculate_ge(saveoutput = FALSE)
  diet_char <- calculate_weighted_variable(saveoutput = FALSE)
  weights   <- readr::read_csv("user_data/livestock_weights.csv", show_col_types = FALSE)

  # --- 2. Calculation ---
  results <- ge_req %>%
    dplyr::select(region, subregion, animal_tag, class_flex, animal_type, animal_subtype, GE_MJday) %>%
    dplyr::left_join(
      diet_char %>% dplyr::select(region, subregion, animal_tag, class_flex, ED_MJkg),
      by = c("region", "subregion", "animal_tag", "class_flex")
    ) %>%
    dplyr::left_join(
      weights %>% dplyr::select(region, subregion, animal_tag, class_flex, initial_weight_kg, final_weight_kg),
      by = c("region", "subregion", "animal_tag", "class_flex")
    ) %>%
    dplyr::mutate(
      DMI_kgday    = dplyr::if_else(ED_MJkg > 0, GE_MJday / ED_MJkg, 0),
      DMI_bw_pct = dplyr::if_else(((initial_weight_kg + final_weight_kg)/2) > 0, (DMI_kgday / ((initial_weight_kg + final_weight_kg)/2)) * 100, 0)
    )

  # --- 3. Physiological Normality Filters ---

  # A. High Intake Warning (Based on physiological maximums)
  # Cattle > 4.4% (NRC 1996) | Sheep > 5.5% (DSN 2004) | Goat > 6.7% (DGFN 2008)
  warn_high <- results %>%
    dplyr::filter(
      (animal_type == "Cattle" & DMI_bw_pct > 4.4) |
        (animal_type == "Sheep"  & DMI_bw_pct > 5.5) |
        (animal_type == "Goat"   & DMI_bw_pct > 6.7)
    )

  if (nrow(warn_high) > 0) {
    warning(paste0(
      "\u26A0 Intake Warning: DMI (% BW) exceeds maximum physiological limits for: ",
      paste(unique(warn_high$animal_tag), collapse = ", "),
      ". This is physically unlikely. Check GE requirements or diet ed (Ref: NRC 1996 / DSN 2004)."
    ))
  }

  # B. Low Intake Warning (< 0.8% BW)
  # Minimum survival threshold recorded in global databases.
  warn_low <- results %>% dplyr::filter(DMI_bw_pct < 0.8 & DMI_bw_pct > 0)

  if (nrow(warn_low) > 0) {
    warning(paste0(
      "\u26A0 Intake Warning: DMI is extremely low (< 0.8% BW) for: ",
      paste(unique(warn_low$animal_tag), collapse = ", "),
      ". Animal is below survival maintenance (Ref: NRC 1996)."
    ))
  }

  # --- 4. Final Selection & Rounding ---
  final_DMI <- results %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3))) %>%
    dplyr::select(
      region, subregion, animal_tag, class_flex, animal_type, animal_subtype,
      GE_MJday,
      ED_MJkg,
      DMI_kgday,
      DMI_bw_pct
    )

  # --- 5. Save Output ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(final_DMI, "output/DMI_report.csv")
    message("\U0001f4be Saved DMI report to output/DMI_report.csv")
  }

  return(final_DMI)
}
