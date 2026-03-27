#' Calculate indirect N2O emissions from leaching
#'
#' Computes indirect N2O emissions derived from nitrogen leaching (IPCC Eq 10.27 and 10.29).
#' @param automatic_cycle Logical. If TRUE, uses the built-in model for automatic farm cycle calculation. Default is FALSE.
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @export
calculate_N2O_indirect_leaching <- function(automatic_cycle = FALSE, saveoutput = TRUE) {

  message("\U0001f4be Calculating indirect N2O emissions (leaching)...")

  # --- 1. Data Loading ---
  user_manure <- readr::read_csv("user_data/manure_management.csv", show_col_types = FALSE)
  ipcc_master <- readr::read_csv("user_data/ipcc_mm.csv", show_col_types = FALSE)

  direct_N2O_df <- calculate_N2O_direct_manure(automatic_cycle = automatic_cycle, saveoutput = FALSE)
  pop_df        <- calculate_population(automatic_cycle = automatic_cycle, saveoutput = FALSE)

  # --- 2. Validations (Asserts) ---

  # 2.1 Combinations Integrity Check (Validates against ipcc_mm.csv)
  # Filter out rows where key fields are missing to avoid false positives with NAs
  check_data <- user_manure %>%
    dplyr::filter(!is.na(system_base))

  if (nrow(check_data) > 0) {
    user_keys <- check_data %>%
      dplyr::mutate(key = paste(system_base, management_months, system_climate, system_subclimate, system_variant, climate_zone, climate_moisture, sep = " | ")) %>%
      dplyr::pull(key) %>%
      unique()

    master_keys <- ipcc_master %>%
      dplyr::mutate(key = paste(system_base, management_months, system_climate, system_subclimate, system_variant, climate_zone, climate_moisture, sep = " | ")) %>%
      dplyr::pull(key)

    invalid_combos <- user_keys[!user_keys %in% master_keys]

    assertthat::assert_that(
      length(invalid_combos) == 0,
      msg = paste0(
        "\u274C Error: Invalid system/climate combinations detected in 'manure_management.csv':\n",
        paste("- ", invalid_combos, collapse = "\n"),
        "\n\nPlease consult the 'Manure System Guide' and ensure names match the internal library exactly."
      )
    )
  }

  # 2.2 Allocation Assertion
  # Verify that the sum of allocations for each unique animal group does not exceed 100%
  allocation_sums <- user_manure %>%
    dplyr::filter(!is.na(allocation)) %>%
    dplyr::group_by(region, subregion, animal_tag, class_flex) %>%
    dplyr::summarise(total_alloc = sum(allocation, na.rm = TRUE), .groups = "drop")

  if (nrow(allocation_sums) > 0) {
    assertthat::assert_that(
      all(allocation_sums$total_alloc <= 1.001),
      msg = paste("Data Error: Manure allocation exceeds 1.0 (100%) for animals in:",
                  paste(unique(allocation_sums$animal_tag[allocation_sums$total_alloc > 1.001]), collapse = ", "))
    )
  }


  join_keys <- c("region", "subregion", "animal_tag", "class_flex", "animal_type", "animal_subtype")

  # --- 2. Master Dataset Construction & Joins ---

  results <- direct_N2O_df %>%
    dplyr::select(dplyr::all_of(join_keys), N_excreted_kgheadday) %>%
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

    # 2.3 Join Leaching Fraction (frac_leach) and Factor (EF5) from Master Table
    dplyr::left_join(
      ipcc_master %>%
        dplyr::select(system_base, management_months, system_climate,
                      system_subclimate, climate_zone, system_variant,
                      climate_moisture, animal_type, animal_subtype, frac_leach, EF5), #EF5 kg N2O-N/kg N leached and runoff
      by = c("system_base", "management_months", "system_climate",
             "system_subclimate", "climate_zone", "system_variant",
             "climate_moisture", "animal_type", "animal_subtype")
    ) %>%

    # --- 3. Calculations (N Loss and N2O Conversion) ---
    dplyr::mutate(
      dplyr::across(
        c(population, N_excreted_kgheadday, allocation, frac_leach, EF5),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),

      N_leaching_kg_year = population * N_excreted_kgheadday * allocation * frac_leach,

      N2O_leach_kgyear = EF5 * N_leaching_kg_year * (44 / 28)
    ) %>%

    # --- 4. Final Selection and Cleanup ---
    dplyr::select(
      dplyr::all_of(join_keys),
      system_base, system_variant,
      N_excreted_kgheadday, frac_leach, EF5,
      N_leaching_kg_year, N2O_leach_kgyear
    ) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4)))

  # --- 5. Save Output ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/N2O_indirect_leaching.csv")
    message("\U1F4BE Saved output to output/N2O_indirect_leaching.csv")
  }

  return(results)
}
