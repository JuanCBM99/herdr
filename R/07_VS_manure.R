#' Calculate Volatile Solids (VS) for Animals
#'
#' Computes volatile solids (VS) based on Gross Energy (GE), Digestible Energy (DE),
#' and Ash content using IPCC Tier 2 methodology for ruminants, and DMI/Energy ratios for poultry.
#'
#' @param urinary_energy Numeric. Fraction of energy lost in urine. Default 0.04.
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @return Tibble with VS for all animal categories.
#' @export
calculate_vs <- function(urinary_energy = 0.04, saveoutput = TRUE) {

  message("\U0001f7e2 Calculating Volatile Solids (VS)...")

  join_keys <- c("region", "subregion", "animal_tag", "class_flex", "animal_type", "animal_subtype")

  ge_data   <- calculate_ge(saveoutput = FALSE)
  dmi_data  <- calculate_DMI(saveoutput = FALSE)
  diet_char <- calculate_weighted_variable(saveoutput = FALSE)

  results <- ge_data %>%
    dplyr::select(dplyr::all_of(join_keys), GE_MJday, DE_pct) %>%
    dplyr::left_join(
      diet_char %>% dplyr::select(dplyr::all_of(join_keys), ASH_pct, ED_kcalkg, poultry_ME_kcal_kg),
      by = join_keys
    ) %>%
    dplyr::left_join(
      dmi_data %>% dplyr::select(dplyr::all_of(join_keys), DMI_kgday),
      by = join_keys
    ) %>%
    dplyr::mutate(
      dplyr::across(
        c(GE_MJday, DE_pct, ASH_pct, ED_kcalkg, poultry_ME_kcal_kg, DMI_kgday),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),

      VS_kgday = dplyr::case_when(
        animal_type == "poultry" & ED_kcalkg > 0 ~
          (DMI_kgday * (1 - (poultry_ME_kcal_kg / ED_kcalkg)) * (1 - (ASH_pct / 100))),

        animal_type == "poultry" & ED_kcalkg <= 0 ~ 0,

        TRUE ~ ((GE_MJday * (1 - DE_pct/100)) + (urinary_energy * GE_MJday)) * ((1 - ASH_pct/100) / 18.45)
      )
    ) %>%

    dplyr::select(
      dplyr::all_of(join_keys),
      GE_MJday, DE_pct, ASH_pct, ED_kcalkg, poultry_ME_kcal_kg, DMI_kgday, VS_kgday
    ) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3)))

  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/VS.csv")
    message("\U0001f4be Saved output to output/VS.csv")
  }

  return(results)
}
