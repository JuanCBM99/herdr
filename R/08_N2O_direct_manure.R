#' Calculate direct N2O emissions from manure
#'
#' Computes direct N2O emissions based on nitrogen excretion logic,
#' emission factors, management system, and climate (IPCC Eq 10.25).
#' @param automatic_cycle Logical. If TRUE, uses the built-in model for automatic farm cycle calculation. Default is FALSE.
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @export
calculate_N2O_direct_manure <- function(automatic_cycle = FALSE, saveoutput = TRUE) {

  message("\U0001f4be Calculating direct N2O emissions from manure...")

  # --- 1. Data Loading ---
  cat_csv      <- readr::read_csv("user_data/livestock_definitions.csv", show_col_types = FALSE)
  weights_csv  <- readr::read_csv("user_data/livestock_weights.csv", show_col_types = FALSE)
  user_manure <- readr::read_csv("user_data/manure_management.csv", col_types = readr::cols(management_months = readr::col_character()), show_col_types = FALSE)
  ipcc_master  <- readr::read_csv("user_data/ipcc_mm.csv", col_types = readr::cols(management_months = readr::col_character()), show_col_types = FALSE)

  ge_df  <- calculate_ge(saveoutput = FALSE)
  cp_df  <- calculate_weighted_variable(saveoutput = FALSE)
  pop_df <- calculate_population(automatic_cycle = automatic_cycle, saveoutput = FALSE)
  neg_df <- calculate_NEg(saveoutput = FALSE)

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

  results <- ge_df %>%
    dplyr::select(dplyr::all_of(join_keys), GE_MJday) %>%

    dplyr::left_join(
      cp_df %>% dplyr::select(dplyr::all_of(join_keys), CP_pct),
      by = join_keys
    ) %>%
    dplyr::left_join(
      pop_df %>% dplyr::select(dplyr::all_of(join_keys), population),
      by = join_keys
    ) %>%

    dplyr::left_join(
      cat_csv %>%
        dplyr::select(region, subregion, animal_tag, class_flex, milk_yield_kg_year, fat_content_pct),
      by = c("region", "subregion", "animal_tag", "class_flex")
    ) %>%

    dplyr::left_join(
      weights_csv %>%
        dplyr::select(region, subregion, animal_tag, class_flex, initial_weight_kg, final_weight_kg, productive_period_days),
      by = c("region", "subregion", "animal_tag", "class_flex")
    ) %>%
    dplyr::left_join(
      neg_df %>%
        dplyr::select(region, subregion, animal_tag, class_flex, NEg_MJday),
      by = c("region", "subregion", "animal_tag", "class_flex")
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
                      climate_moisture, animal_type, animal_subtype, EF3), #EF3 kg N2O-N/kg N
      by = c("system_base", "management_months", "system_climate",
             "system_subclimate", "climate_zone", "system_variant",
             "climate_moisture", "animal_type", "animal_subtype")
    ) %>%

    # --- 3. Calculations ---
    dplyr::mutate(
      dplyr::across(
        c(GE_MJday, CP_pct, population, milk_yield_kg_year, fat_content_pct, initial_weight_kg, final_weight_kg, productive_period_days, NEg_MJday, allocation, EF3),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),

      milk_protein = 1.9 + 0.4 * fat_content_pct,

      N_retention = dplyr::case_when(
        animal_type %in% c("sheep", "goat") ~ 0.1,
        ((final_weight_kg - initial_weight_kg)/productive_period_days) > 0 & NEg_MJday > 0  ~ ((milk_yield_kg_year * milk_protein) / 6.38) +
          ((((final_weight_kg - initial_weight_kg)/productive_period_days) * (268 - (7.03 * NEg_MJday / ((final_weight_kg - initial_weight_kg)/productive_period_days))) / 1000) / 6.25),
        TRUE ~ 0
      ),

      N_intake_kgheadday   = (GE_MJday / 18.45) * (CP_pct / 100 / 6.25),
      N_excreted_kgheadday = dplyr::if_else(
        animal_type %in% c("sheep", "goat"),
        (N_intake_kgheadday * (1 - N_retention)) * 365,
        (N_intake_kgheadday - N_retention) * 365
      ),

      direct_N2O_kgyear = population * N_excreted_kgheadday * allocation * EF3 * (44 / 28)
    ) %>%

    # --- 4. Final Formatting ---
    dplyr::select(
      dplyr::all_of(join_keys),
      system_base, system_variant, climate_moisture, climate_zone,
      N_intake_kgheadday, N_retention, N_excreted_kgheadday, EF3, population, direct_N2O_kgyear
    ) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4)))

  # --- 5. Save Output ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/N2O_direct_manure.csv")
    message("\U1F4BE Saved output to output/N2O_direct_manure.csv")
  }

  return(results)
}
