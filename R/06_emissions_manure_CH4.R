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

  # --- 2. Allocation Assertion (using assertthat) ---
  # We group by the unique identity of the animal/system and verify the sum
  allocation_sums <- user_manure %>%
    dplyr::group_by(region, subregion, animal_tag, class_flex) %>%
    dplyr::summarise(total_alloc = sum(allocation, na.rm = TRUE), .groups = "drop")

  assertthat::assert_that(all(allocation_sums$total_alloc <= 1.001),
                          msg = paste("Data Error: Manure allocation exceeds 1.0 (100%) for animals in:",
                                      paste(unique(allocation_sums$animal_tag[allocation_sums$total_alloc > 1.001]), collapse = ", ")))

  # Identity keys for consistent joins
  join_keys <- c("region", "subregion", "animal_tag", "class_flex", "animal_type", "animal_subtype")

  # --- 3. Processing and Joins ---

  # 3.1 Fetch VS and Population data
  results <- calculate_vs(saveoutput = FALSE) %>%
    dplyr::select(dplyr::all_of(join_keys), vs) %>%

    dplyr::left_join(
      calculate_population(automatic_cycle = automatic_cycle, saveoutput = FALSE) %>%
        dplyr::select(dplyr::all_of(join_keys), population),
      by = join_keys
    ) %>%

    # 3.2 Join User Configuration (System and Climate)
    dplyr::left_join(
      user_manure %>%
        dplyr::select(region, subregion, animal_tag, class_flex,
                      system_base, management_months, system_climate,
                      system_subclimate, climate_zone, system_variant,
                      climate_moisture, animal_type, animal_subtype, allocation),
      by = c("region", "subregion", "animal_tag", "class_flex", "animal_type", "animal_subtype")
    ) %>%

    # 3.3 Join B0 Coefficient
    dplyr::left_join(
      coefficients %>%
        dplyr::filter(tolower(coefficient) == "b_0") %>%
        dplyr::select(animal_type, animal_subtype, B0 = value),
      by = c("animal_type", "animal_subtype")
    ) %>%

    # 3.4 Join MCF Factors from Master Table
    dplyr::left_join(
      ipcc_master %>%
        dplyr::select(system_base, management_months, system_climate,
                      system_subclimate, climate_zone, system_variant,
                      climate_moisture, animal_type, animal_subtype, mcf),
      by = c("system_base", "management_months", "system_climate",
             "system_subclimate", "climate_zone", "system_variant",
             "climate_moisture", "animal_type", "animal_subtype")
    ) %>%

    # --- 4. Calculations (IPCC Eq 10.23) ---
    dplyr::mutate(
      dplyr::across(
        c(vs, population, allocation, B0, mcf),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),

      # EF_CH4: kg CH4 / animal / year
      # Formula: EF = (VS * 365) * (B0 * 0.67 * MCF * Allocation)
      ef_ch4_kg_year = (vs * 365) * (B0 * 0.67 * mcf * allocation),

      # Total Emissions (Gg CH4 / year)
      Emissions_CH4_Gg_year = (ef_ch4_kg_year * population)
    ) %>%

    # --- 5. Selection and Rounding ---
    dplyr::select(
      region, subregion, animal_tag, class_flex, animal_type, animal_subtype,
      system_base, system_variant, vs, B0, mcf, allocation,
      ef_ch4_kg_year, population, Emissions_CH4_Gg_year
    ) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4)))

  # --- 6. Save Output ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/CH4_manure.csv")
    message("\U0001f4be Saved methane results to output/CH4_manure.csv")
  }

  return(results)
}
