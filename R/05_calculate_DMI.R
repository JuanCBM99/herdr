#' Calculate Dry Matter Intake (DMI)
#'
#' Computes daily Dry Matter Intake (kg DM/day) based on metabolic demand (GE/ED)
#' for ruminants and metabolizable energy parameters (FEDNA) for poultry.
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @export
calculate_DMI <- function(saveoutput = TRUE) {

  message("\U0001f37d Calculating Dry Matter Intake (DMI)...")

  # --- 1. Load Dependencies and Inputs ---
  ge_req    <- calculate_ge(saveoutput = FALSE)
  diet_char <- calculate_weighted_variable(saveoutput = FALSE)
  weights   <- readr::read_csv("user_data/livestock_weights.csv", show_col_types = FALSE)
  poultry_eng  <- calculate_monogastric_energy(saveoutput = FALSE)

  join_keys <- c("region", "subregion", "animal_tag", "class_flex")

  # --- 2. Merge Data Assets and Harmonize Missing Values ---
  results <- ge_req %>%
    dplyr::select(dplyr::all_of(join_keys), animal_type, animal_subtype, GE_MJday) %>%
    dplyr::left_join(
      diet_char %>% dplyr::select(diet_tag, dplyr::all_of(join_keys), ED_kcalkg, poultry_ME_kcal_kg, swine_ME_kcal_kg),
      by = join_keys
    ) %>%
    dplyr::left_join(
      weights %>% dplyr::select(dplyr::all_of(join_keys), initial_weight_kg, final_weight_kg),
      by = join_keys
    ) %>%
    dplyr::left_join(
      poultry_eng %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(join_keys), ~ as.character(.))) %>%
        dplyr::select(dplyr::all_of(join_keys), ME_total_kcal_day),
      by = join_keys
    ) %>%
    dplyr::mutate(
      dplyr::across(
        c(GE_MJday, ED_kcalkg, poultry_ME_kcal_kg, initial_weight_kg, final_weight_kg, ME_total_kcal_day),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),

      # --- 3. Compute DMI by Species Type ---
      DMI_kgday = dplyr::case_when(
        animal_type == "poultry" & poultry_ME_kcal_kg > 0 ~ (ME_total_kcal_day) / poultry_ME_kcal_kg,
        animal_type == "swine" & swine_ME_kcal_kg > 0 ~ (ME_total_kcal_day) / swine_ME_kcal_kg,
        animal_type == "poultry" & poultry_ME_kcal_kg <= 0 ~ 0,
        # For ruminants: GE (MJ) to kcal conversion factor is 239.005 (1 MJ = 239.005 kcal)
        ED_kcalkg > 0 ~ GE_MJday / ED_kcalkg * 239.005,
        TRUE ~ 0
      ),

      # Calculate intake relative to Body Weight (% BW)
      W_mean = (initial_weight_kg + final_weight_kg) / 2,
      DMI_bw_pct = dplyr::if_else(W_mean > 0, (DMI_kgday / W_mean) * 100, 0)
    )

  # --- 4. Upper Bound Biological Threshold Checks ---
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

  # --- 6. Formatting and Export Execution ---
  final_DMI <- results %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3))) %>%
    dplyr::select(
      dplyr::all_of(join_keys), diet_tag, animal_type, animal_subtype,
      GE_MJday, ED_kcalkg, poultry_ME_kcal_kg, swine_ME_kcal_kg, ME_total_kcal_day,
      DMI_kgday, DMI_bw_pct
    )

  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(final_DMI, "output/DMI_report.csv")
    message("\U0001f4be Saved DMI report to output/DMI_report.csv")
  }

  return(final_DMI)
}
