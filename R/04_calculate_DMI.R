#' Calculate Dry Matter Intake (DMI)
#'
#' Computes daily Dry Matter Intake (kg DM/day) based on metabolic demand (GE/ed).
#' Includes physiological normality filters differentiated by species.
#'
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @export
calculate_dmi <- function(saveoutput = TRUE) {

  message("\U0001f37d Calculating Dry Matter Intake (DMI)...")

  # --- 1. Load Data ---
  ge_req    <- calculate_ge(saveoutput = FALSE)
  diet_char <- calculate_weighted_variable(saveoutput = FALSE)
  weights   <- readr::read_csv("user_data/livestock_weights.csv", show_col_types = FALSE)

  # --- 2. Calculation ---
  results <- ge_req %>%
    dplyr::select(region, subregion, animal_tag, class_flex, animal_type, animal_subtype, ge) %>%
    dplyr::left_join(
      diet_char %>% dplyr::select(region, subregion, animal_tag, class_flex, ed),
      by = c("region", "subregion", "animal_tag", "class_flex")
    ) %>%
    dplyr::left_join(
      weights %>% dplyr::select(region, subregion, animal_tag, class_flex, average_weight),
      by = c("region", "subregion", "animal_tag", "class_flex")
    ) %>%
    dplyr::mutate(
      dmi_day    = dplyr::if_else(ed > 0, ge / ed, 0),
      dmi_bw_pct = dplyr::if_else(average_weight > 0, (dmi_day / average_weight) * 100, 0)
    )

  # --- 3. Physiological Normality Filters ---

  # A. High Intake Warning (Based on physiological maximums)
  # Cattle > 4.4% (NRC 1996) | Sheep > 5.5% (DSN 2004) | Goat > 6.7% (DGFN 2008)
  warn_high <- results %>%
    dplyr::filter(
      (animal_type == "Cattle" & dmi_bw_pct > 4.4) |
        (animal_type == "Sheep"  & dmi_bw_pct > 5.5) |
        (animal_type == "Goat"   & dmi_bw_pct > 6.7)
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
  warn_low <- results %>% dplyr::filter(dmi_bw_pct < 0.8 & dmi_bw_pct > 0)

  if (nrow(warn_low) > 0) {
    warning(paste0(
      "\u26A0 Intake Warning: DMI is extremely low (< 0.8% BW) for: ",
      paste(unique(warn_low$animal_tag), collapse = ", "),
      ". Animal is below survival maintenance (Ref: NRC 1996)."
    ))
  }

  # --- 4. Final Selection & Rounding ---
  final_dmi <- results %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3))) %>%
    dplyr::select(
      region, subregion, animal_tag, class_flex, animal_type, animal_subtype,
      ge_mj_day = ge,
      eb_mj_kg = ed,
      dmi_day_kg = dmi_day,
      dmi_bw_pct
    )

  # --- 5. Save Output ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(final_dmi, "output/DMI_report.csv")
    message("\U0001f4be Saved DMI report to output/DMI_report.csv")
  }

  return(final_dmi)
}
