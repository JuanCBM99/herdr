#' Calculate Poultry Metabolic Energy Requirements (FEDNA)
#'
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @export
calculate_poultry_energy <- function(saveoutput = TRUE) {

  message("\U0001f4be Calculating Poultry Metabolizable Energy (kcal/day)...")

  poultry_csv <- readr::read_csv("user_data/monogastric_definitions.csv", show_col_types = FALSE)
  weights_csv <- readr::read_csv("user_data/livestock_weights.csv", show_col_types = FALSE)

  join_keys <- c("region", "subregion", "animal_tag", "class_flex", "animal_type", "animal_subtype")

  master_poultry <- poultry_csv %>%
    dplyr::left_join(
      weights_csv %>% dplyr::select(region, subregion, animal_tag, class_flex, initial_weight_kg, final_weight_kg, productive_period_days),
      by = c("region", "subregion", "animal_tag", "class_flex")
    ) %>%
    dplyr::filter(animal_type == "poultry")

  results <- master_poultry %>%
    dplyr::mutate(
      dplyr::across(
        c(initial_weight_kg, final_weight_kg, productive_period_days, frac_fat_pct, frac_protein_pct, egg_mass_g_day, cfi_maintenance),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),

      GMD_gday = dplyr::if_else(
        productive_period_days > 0 & final_weight_kg > initial_weight_kg,
        ((final_weight_kg - initial_weight_kg) / productive_period_days) * 1000,
        0
      ),

      W_mean = (initial_weight_kg + final_weight_kg) / 2,
      W_metabolic = W_mean^0.75,

      EM_mant_kcal = cfi_maintenance * W_metabolic,

      EM_crec_kcal = dplyr::case_when(
        animal_subtype == "meat" | (animal_subtype == "layer" & frac_protein_pct < 0.145) ~
          (13.4 * frac_fat_pct * GMD_gday) + (12.0 * frac_protein_pct * GMD_gday),

        animal_subtype == "layer" & frac_protein_pct >= 0.145 ~ 5 * GMD_gday,

        TRUE ~ 0
      ),

      EM_eggs_kcal = dplyr::case_when(
        egg_mass_g_day > 0 & productive_period_days > 0 ~
          2 * (egg_mass_g_day),
        TRUE ~ 0
      ),

      EM_total_kcal_day = EM_mant_kcal + EM_crec_kcal + EM_eggs_kcal
    ) %>%

    dplyr::select(
      dplyr::all_of(join_keys), diet_tag,
      initial_weight_kg, final_weight_kg, productive_period_days, GMD_gday, cfi_maintenance,
      EM_mant_kcal, EM_crec_kcal, EM_eggs_kcal, EM_total_kcal_day
    ) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4)))

  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/poultry_metabolizable_energy.csv")
    message("\U0001f4be Saved poultry metabolizable energy to output/poultry_metabolizable_energy.csv")
  }

  return(results)
}
