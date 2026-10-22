#' Calculate Volatile Solids (VS) for Animals
#'
#' Computes volatile solids (VS) based on Gross Energy (GE), Digestible Energy (DE),
#' and Ash content using IPCC Tier 2 methodology for ruminants, and DMI/Energy ratios for poultry.
#'
#' @param urinary_energy Numeric. Fraction of energy lost in urine. Default 0.04.
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @param data_dir Path to the directory containing input CSV/data files. Defaults to "user_data".
#' @param ge_data Optional precalculated Gross Energy dataframe.
#' @param dmi_data Optional precalculated DMI dataframe.
#' @param diet_char Optional precalculated weighted nutritional variables dataframe.
#' @return Tibble with VS for all animal categories.
#' @export
calculate_vs <- function(urinary_energy = 0.04, saveoutput = TRUE, data_dir = "user_data",
                         ge_data = NULL, dmi_data = NULL, diet_char = NULL) {

  message("Calculating Volatile Solids (VS)...")

  join_keys <- c("region", "subregion", "animal_tag", "class_flex", "animal_type", "animal_subtype")

  # --- 1. Load Dependencies and Model Components ---
  if (is.null(diet_char)) diet_char <- suppressMessages(calculate_weighted_variable(saveoutput = FALSE, data_dir = data_dir))
  if (is.null(ge_data))   ge_data   <- suppressMessages(calculate_ge(saveoutput = FALSE, data_dir = data_dir, de_df = diet_char))
  if (is.null(dmi_data))  dmi_data  <- suppressMessages(calculate_DMI(saveoutput = FALSE, data_dir = data_dir, ge_req = ge_data, diet_char = diet_char))

  # --- 2. Merge Assets and Clean Numeric Types ---
  results <- ge_data %>%
    dplyr::select(dplyr::all_of(join_keys), GE_MJday, DE_pct) %>%
    dplyr::left_join(
      diet_char %>% dplyr::select(dplyr::all_of(join_keys), ASH_pct, GE_feed_kcal_kg, poultry_ME_kcal_kg,  swine_DE_kcal_kg),
      by = join_keys
    ) %>%
    dplyr::left_join(
      dmi_data %>% dplyr::select(dplyr::all_of(join_keys), DMI_kgday),
      by = join_keys
    ) %>%
    dplyr::mutate(
      dplyr::across(
        c(GE_MJday, DE_pct, ASH_pct, GE_feed_kcal_kg, poultry_ME_kcal_kg, swine_DE_kcal_kg, DMI_kgday),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),

      # --- 3. Compute Volatile Solids (VS) via Category-Specific Methods ---
      VS_kgday = dplyr::case_when(
        # Poultry: VS derived via DMI and the proportion of non-metabolizable organic matter
        animal_type == "poultry" & GE_feed_kcal_kg > 0 ~
          (DMI_kgday * (1 - (poultry_ME_kcal_kg / GE_feed_kcal_kg)) * (1 - (ASH_pct / 100))),

        animal_type == "poultry" & GE_feed_kcal_kg <= 0 ~ 0,

        # Swine: VS derived via DMI, accounting for Digestible Energy and a 2% urinary loss adjustment
        animal_type == "swine" & GE_feed_kcal_kg > 0 ~
          (DMI_kgday * (1 - (swine_DE_kcal_kg / GE_feed_kcal_kg) + 0.02) * (1 - (ASH_pct / 100))),

        animal_type == "swine" & GE_feed_kcal_kg <= 0 ~ 0,

        # Ruminants (Default): IPCC Tier 2 Formula
        # Converts remaining energy (fecal + urinary loss) into mass using the 18.45 MJ/kg DM conversion factor
        TRUE ~ ((GE_MJday * (1 - DE_pct/100)) + (urinary_energy * GE_MJday)) * ((1 - ASH_pct/100) / 18.45)
      ),
      VS_kgday = pmax(0, VS_kgday)
    ) %>%

    # --- 4. Select, Format and Round Outputs ---
    dplyr::select(
      dplyr::all_of(join_keys),
      GE_MJday, DE_pct, ASH_pct, GE_feed_kcal_kg, poultry_ME_kcal_kg,  swine_DE_kcal_kg, DMI_kgday, VS_kgday
    ) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3)))

  # --- 5. Export Execution ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/VS.csv")
    message("[herdr] Saved output to output/VS.csv")
  }

  return(results)
}
