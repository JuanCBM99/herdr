#' Calculate CH4 Emissions from Manure Management
#'
#' Computes CH4 emissions from manure based on Volatile Solids (VS),
#' population, and management factors (B0, MCF, AWMS) using IPCC Eq 10.23.
#' @param automatic_cycle Logical. If TRUE, uses the built-in model for automatic farm cycle calculation. Default is FALSE.
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @export
calculate_CH4_manure <- function(automatic_cycle = FALSE, saveoutput = TRUE) {

  message("\U0001f4be Calculating CH4 emissions from manure management...")

  # --- 1. Data Loading ---
  user_manure  <- readr::read_csv("user_data/manure_management.csv", show_col_types = FALSE)
  ipcc_master  <- readr::read_csv("user_data/ipcc_mm.csv", show_col_types = FALSE)
  coefficients <- readr::read_csv("user_data/ipcc_coefficients.csv", show_col_types = FALSE)

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

  # --- 3. Processing and Joins ---

  results <- calculate_vs(saveoutput = FALSE) %>%
    dplyr::select(dplyr::all_of(join_keys), VS_kgday) %>%

    dplyr::left_join(
      calculate_population(automatic_cycle = automatic_cycle, saveoutput = FALSE) %>%
        dplyr::select(dplyr::all_of(join_keys), population),
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
      coefficients %>%
        dplyr::filter(tolower(coefficient) == "b_0") %>%
        dplyr::select(animal_type, animal_subtype, B0 = value),
      by = c("animal_type", "animal_subtype")
    ) %>%

    dplyr::left_join(
      ipcc_master %>%
        dplyr::select(system_base, management_months, system_climate,
                      system_subclimate, climate_zone, system_variant,
                      climate_moisture, animal_type, animal_subtype, MCF_pct),
      by = c("system_base", "management_months", "system_climate",
             "system_subclimate", "climate_zone", "system_variant",
             "climate_moisture", "animal_type", "animal_subtype")
    ) %>%

    # --- 4. Calculations (IPCC Eq 10.23) ---
    dplyr::mutate(
      dplyr::across(
        c(VS_kgday, population, allocation, B0, MCF_pct),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),

      EF_kgyear = (VS_kgday * 365) * (B0 * 0.67 * MCF_pct/100 * allocation),
      total_CH4_mm_Ggyear = (EF_kgyear * population)
    ) %>%

    # --- 5. Selection and Rounding ---
    dplyr::select(
      region, subregion, animal_tag, class_flex, animal_type, animal_subtype,
      system_base, system_variant, VS_kgday, B0_m3kg = B0, MCF_pct, allocation,
      EF_kgyear, population, total_CH4_mm_Ggyear
    ) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 6)))

  # --- 6. Save Output ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/CH4_manure.csv")
    message("\U0001f4be Saved methane results to output/CH4_manure.csv")
  }

  return(results)
}
