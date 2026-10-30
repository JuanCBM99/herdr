#' Calculate CH4 Emissions from Manure Management
#'
#' Computes CH4 emissions from manure based on Volatile Solids (VS),
#' population, and management factors (B0, MCF, AWMS) using IPCC Eq 10.23.
#' @param automatic_cycle Logical. If TRUE, uses the built-in model for automatic farm cycle calculation. Default is FALSE.
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @param data_dir Path to the directory containing input CSV/data files. Defaults to "user_data".
#' @param vs_df Optional precalculated Volatile Solids dataframe.
#' @param pop_df Optional precalculated population dataframe.
#' @export
calculate_CH4_manure <- function(automatic_cycle = FALSE, saveoutput = TRUE, data_dir = "user_data",
                                 vs_df = NULL, pop_df = NULL) {
  message("Calculating CH4 emissions from manure management...")

  # --- 1. Data Loading ---
  user_manure <- readr::read_csv(file.path(data_dir, "manure_management.csv"), col_types = readr::cols(management_months = readr::col_character()), show_col_types = FALSE)
  ipcc_master <- readr::read_csv(file.path(data_dir, "ipcc_mm.csv"), col_types = readr::cols(management_months = readr::col_character()), show_col_types = FALSE)
  coefficients <- readr::read_csv(file.path(data_dir, "ipcc_coefficients.csv"), show_col_types = FALSE)

  join_keys <- c("farm_id", "region", "subregion", "animal_tag", "animal_type", "animal_subtype")

  if (is.null(vs_df)) {
    vs_df <- suppressMessages(calculate_vs(saveoutput = FALSE, data_dir = data_dir))
  }
  if (is.null(pop_df)) {
    pop_df <- suppressMessages(calculate_population(automatic_cycle = automatic_cycle, saveoutput = FALSE, data_dir = data_dir))
  }

  # --- 2. Validations ---
  herdr_validate_manure_systems(user_manure, ipcc_master, coefficients = coefficients, animal_metadata = vs_df)
  herdr_validate_manure_allocation(user_manure)
  herdr_validate_manure_coverage(pop_df, user_manure)

  # --- 3. Processing and Joins ---
  results <- vs_df %>%
    dplyr::select(dplyr::all_of(join_keys), VS_kgday) %>%
    dplyr::left_join(
      pop_df %>%
        dplyr::select(dplyr::all_of(join_keys), population),
      by = join_keys
    ) %>%
    dplyr::left_join(
      user_manure %>%
        dplyr::select(
          farm_id, region, subregion, animal_tag,
          system_base, management_months, system_climate,
          system_subclimate, climate_zone, system_variant,
          climate_moisture, b_0, allocation
        ),
      by = c("farm_id", "region", "subregion", "animal_tag")
    ) %>%
    dplyr::left_join(
      coefficients %>%
        dplyr::filter(tolower(coefficient) == "b_0") %>%
        dplyr::select(description, B0 = value),
      by = c("b_0" = "description")
    ) %>%
    dplyr::left_join(
      ipcc_master %>%
        dplyr::select(
          system_base, management_months, system_climate,
          system_subclimate, climate_zone, system_variant,
          climate_moisture, animal_type, animal_subtype, MCF_pct
        ),
      by = c(
        "system_base", "management_months", "system_climate",
        "system_subclimate", "climate_zone", "system_variant",
        "climate_moisture", "animal_type", "animal_subtype"
      )
    ) %>%
    # --- 4. Calculations (IPCC Eq 10.23) ---
    dplyr::mutate(
      dplyr::across(
        c(VS_kgday, population, allocation, B0, MCF_pct),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),
      EF_kgyear = (VS_kgday * 365) * (B0 * 0.67 * MCF_pct / 100 * allocation),
      total_CH4_mm_kgyear = (EF_kgyear * population)
    ) %>%
    # --- 5. Selection and Rounding ---
    dplyr::select(
      farm_id, region, subregion, animal_tag, animal_type, animal_subtype,
      system_base, system_variant, VS_kgday,
      B0_m3kg = B0, MCF_pct, allocation,
      EF_kgyear, population, total_CH4_mm_kgyear
    )

  # --- 6. Save Output ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/CH4_manure.csv")
    message("[herdr] Saved methane results to output/CH4_manure.csv")
  }

  return(results)
}
