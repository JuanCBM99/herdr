#' Calculate indirect N2O emissions from leaching
#'
#' Computes indirect N2O emissions derived from nitrogen leaching (IPCC Eq 10.27 and 10.29).
#' @param automatic_cycle Logical. If TRUE, uses the built-in model for automatic farm cycle calculation. Default is FALSE.
#' @param saveoutput Logical. If TRUE (default), results are saved to output folder.
#' @param data_dir Path to the directory containing input CSV/data files. Defaults to `"user_data"`.
#' @param direct_N2O_df Optional precalculated direct N2O manure dataframe.
#' @param pop_df Optional precalculated population dataframe.
#' @export
calculate_N2O_indirect_leaching <- function(automatic_cycle = FALSE, saveoutput = TRUE, data_dir = "user_data",
                                            direct_N2O_df = NULL, pop_df = NULL) {
  message("Calculating indirect N2O emissions (leaching)...")

  # --- 1. Data Loading ---
  user_manure <- readr::read_csv(file.path(data_dir, "manure_management.csv"), col_types = readr::cols(management_months = readr::col_character()), show_col_types = FALSE)
  ipcc_master <- readr::read_csv(file.path(data_dir, "ipcc_mm.csv"), col_types = readr::cols(management_months = readr::col_character()), show_col_types = FALSE)

  # Suppress verbose messages from internal calculation routines
  if (is.null(direct_N2O_df)) direct_N2O_df <- suppressMessages(calculate_N2O_direct_manure(automatic_cycle = automatic_cycle, saveoutput = FALSE, data_dir = data_dir))
  if (is.null(pop_df)) pop_df <- suppressMessages(calculate_population(automatic_cycle = automatic_cycle, saveoutput = FALSE, data_dir = data_dir))

  join_keys <- c("region", "subregion", "animal_tag", "class_flex", "animal_type", "animal_subtype")

  # --- 2. Validations ---
  herdr_validate_manure_systems(user_manure, ipcc_master, animal_metadata = direct_N2O_df)
  herdr_validate_manure_allocation(user_manure)
  herdr_validate_manure_coverage(pop_df, user_manure)

  # --- 2. Master Dataset Construction & Joins ---

  results <- direct_N2O_df %>%
    dplyr::select(dplyr::all_of(join_keys), N_excreted_kgheadyear) %>%
    dplyr::distinct() %>%
    dplyr::left_join(
      pop_df %>% dplyr::select(dplyr::all_of(join_keys), population),
      by = join_keys
    ) %>%
    dplyr::left_join(
      user_manure %>%
        dplyr::select(
          region, subregion, animal_tag, class_flex,
          system_base, management_months, system_climate,
          system_subclimate, climate_zone, system_variant,
          climate_moisture, allocation
        ),
      by = c("region", "subregion", "animal_tag", "class_flex")
    ) %>%
    # 2.3 Join Leaching Fraction (frac_leach) and Factor (EF5) from Master Table
    dplyr::left_join(
      ipcc_master %>%
        dplyr::select(
          system_base, management_months, system_climate,
          system_subclimate, climate_zone, system_variant,
          climate_moisture, animal_type, animal_subtype, frac_leach, EF5
        ), # EF5 kg N2O-N/kg N leached and runoff
      by = c(
        "system_base", "management_months", "system_climate",
        "system_subclimate", "climate_zone", "system_variant",
        "climate_moisture", "animal_type", "animal_subtype"
      )
    ) %>%
    # --- 3. Calculations (N Loss and N2O Conversion) ---
    dplyr::mutate(
      dplyr::across(
        c(population, N_excreted_kgheadyear, allocation, frac_leach, EF5),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),
      N_leaching_kg_year = population * N_excreted_kgheadyear * allocation * frac_leach,
      N2O_leach_kgyear = EF5 * N_leaching_kg_year * (44 / 28)
    ) %>%
    # --- 4. Final Selection and Cleanup ---
    dplyr::select(
      dplyr::all_of(join_keys),
      system_base, system_variant,
      N_excreted_kgheadyear, frac_leach, EF5,
      N_leaching_kg_year, N2O_leach_kgyear
    )

  # --- 5. Save Output ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/N2O_indirect_leaching.csv")
    message("[herdr] Saved output to output/N2O_indirect_leaching.csv")
  }

  return(results)
}
