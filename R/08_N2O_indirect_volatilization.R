#' Calculate indirect N2O emissions from volatilization
#'
#' Computes indirect N2O emissions derived from volatilization of excreted nitrogen (IPCC Eq 10.26 and 10.28).
#' @param automatic_cycle Logical. If TRUE, uses the built-in model for automatic farm cycle calculation. Default is FALSE.
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @export
calculate_N2O_indirect_volatilization <- function(automatic_cycle = FALSE, saveoutput = TRUE) {

  message("\U0001f4be Calculating indirect N2O emissions (volatilization)...")

  # --- 1. Data Loading ---
  user_manure <- readr::read_csv("user_data/manure_management.csv", show_col_types = FALSE)
  ipcc_master <- readr::read_csv("user_data/ipcc_mm.csv", show_col_types = FALSE)

  direct_n2o_df <- calculate_N2O_direct_manure(automatic_cycle = automatic_cycle, saveoutput = FALSE)
  pop_df        <- calculate_population(automatic_cycle = automatic_cycle, saveoutput = FALSE)

  # --- 2. Allocation Assertion ---
  allocation_sums <- user_manure %>%
    dplyr::group_by(region, subregion, animal_tag, class_flex) %>%
    dplyr::summarise(total_alloc = sum(allocation, na.rm = TRUE), .groups = "drop")

  assertthat::assert_that(all(allocation_sums$total_alloc <= 1.001),
                          msg = paste("Data Error: Manure allocation exceeds 1.0 (100%) for animals in:",
                                      paste(unique(allocation_sums$animal_tag[allocation_sums$total_alloc > 1.001]), collapse = ", ")))

  join_keys <- c("region", "subregion", "animal_tag", "class_flex", "animal_type", "animal_subtype")

  results <- direct_n2o_df %>%
    dplyr::select(dplyr::all_of(join_keys), N_excreted) %>%
    dplyr::distinct() %>%

    dplyr::left_join(
      pop_df %>% dplyr::select(dplyr::all_of(join_keys), population),
      by = join_keys
    ) %>%

    dplyr::left_join(
      user_manure %>%
        dplyr::select(region, subregion, animal_tag, class_flex,
                      system_base, management_months, system_climate,
                      system_subclimate, climate_zone, system_variant,
                      climate_moisture, animal_type, animal_subtype, allocation),
      by = c("region", "subregion", "animal_tag", "class_flex", "animal_type", "animal_subtype")
    ) %>%

    dplyr::left_join(
      ipcc_master %>%
        dplyr::select(system_base, management_months, system_climate,
                      system_subclimate, climate_zone, system_variant,
                      climate_moisture, animal_type, animal_subtype, frac_gas, EF4),
      by = c("system_base", "management_months", "system_climate",
             "system_subclimate", "climate_zone", "system_variant",
             "climate_moisture", "animal_type", "animal_subtype")
    ) %>%

    dplyr::mutate(
      dplyr::across(
        c(population, N_excreted, allocation, frac_gas, EF4),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),

      # N Loss due to Volatilization (Eq 10.26)
      n_volatilization_kg_year = population * N_excreted * allocation * frac_gas,

      # Indirect N2O Emissions (Eq 10.28)
      n2o_g = EF4 * n_volatilization_kg_year * (44 / 28)
    ) %>%

    dplyr::select(
      dplyr::all_of(join_keys),
      system_base, system_variant,
      N_excreted, frac_gas, EF4,
      n_volatilization_kg_year, n2o_g
    ) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4)))

  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/N2O_indirect_volatilization.csv")
    message("\U1F4BE Saved output to output/N2O_indirect_volatilization.csv")
  }

  return(results)
}
