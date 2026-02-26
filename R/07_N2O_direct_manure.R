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
  # Load reference tables
  cat_csv      <- readr::read_csv("user_data/livestock_definitions.csv", show_col_types = FALSE)
  weights_csv  <- readr::read_csv("user_data/livestock_weights.csv", show_col_types = FALSE)
  user_manure  <- readr::read_csv("user_data/manure_management.csv", show_col_types = FALSE)
  ipcc_master  <- readr::read_csv("user_data/ipcc_mm.csv", show_col_types = FALSE)

  # Calling internal modules (dependencies)
  ge_df  <- calculate_ge(saveoutput = FALSE)
  cp_df  <- calculate_weighted_variable(saveoutput = FALSE)
  pop_df <- calculate_population(automatic_cycle = automatic_cycle, saveoutput = FALSE)
  neg_df <- calculate_NEg(saveoutput = FALSE)

  # --- 2. Allocation Assertion (using assertthat) ---
  # We group by the unique identity of the animal/system and verify the sum
  allocation_sums <- user_manure %>%
    dplyr::group_by(region, subregion, animal_tag, class_flex) %>%
    dplyr::summarise(total_alloc = sum(allocation, na.rm = TRUE), .groups = "drop")

  assertthat::assert_that(all(allocation_sums$total_alloc <= 1.001),
                          msg = paste("Data Error: Manure allocation exceeds 1.0 (100%) for animals in:",
                                      paste(unique(allocation_sums$animal_tag[allocation_sums$total_alloc > 1.001]), collapse = ", ")))

  # Mandatory keys for merging
  join_keys <- c("region", "subregion", "animal_tag", "class_flex", "animal_type", "animal_subtype")

  # --- 2. Master Dataset Construction & Joins ---

  results <- ge_df %>%
    dplyr::select(dplyr::all_of(join_keys), ge) %>%

    # 2.1 Join Nutritional (CP) and Population Data
    dplyr::left_join(
      cp_df %>% dplyr::select(dplyr::all_of(join_keys), cp),
      by = join_keys
    ) %>%
    dplyr::left_join(
      pop_df %>% dplyr::select(dplyr::all_of(join_keys), population),
      by = join_keys
    ) %>%

    # 2.2 Join categories data (Milk & Fat)
    dplyr::left_join(
      cat_csv %>%
        dplyr::select(region, subregion, animal_tag, class_flex, milk_yield, fat_content),
      by = c("region", "subregion", "animal_tag", "class_flex")
    ) %>%

    # 2.3 Join weight gain and NEg
    dplyr::left_join(
      weights_csv %>%
        dplyr::select(region, subregion, animal_tag, class_flex, weight_gain),
      by = c("region", "subregion", "animal_tag", "class_flex")
    ) %>%
    dplyr::left_join(
      neg_df %>%
        dplyr::select(region, subregion, animal_tag, class_flex, NEg),
      by = c("region", "subregion", "animal_tag", "class_flex")
    ) %>%

    # 2.4 Join Management Configuration (User Data)
    dplyr::left_join(
      user_manure %>%
        dplyr::select(region, subregion, animal_tag, class_flex,
                      system_base, management_months, system_climate,
                      system_subclimate, climate_zone, system_variant,
                      climate_moisture, animal_type, animal_subtype, allocation),
      by = c("region", "subregion", "animal_tag", "class_flex", "animal_type", "animal_subtype")
    ) %>%

    # 2.5 Join Direct Emission Factor (EF3) from Master Table
    dplyr::left_join(
      ipcc_master %>%
        dplyr::select(system_base, management_months, system_climate,
                      system_subclimate, climate_zone, system_variant,
                      climate_moisture, animal_type, animal_subtype, EF3),
      by = c("system_base", "management_months", "system_climate",
             "system_subclimate", "climate_zone", "system_variant",
             "climate_moisture", "animal_type", "animal_subtype")
    ) %>%

    # --- 3. Calculations ---
    dplyr::mutate(
      # Numeric Safety
      dplyr::across(
        c(ge, cp, population, milk_yield, fat_content, weight_gain, NEg, allocation, EF3),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),

      # Nitrogen Balance logic (IPCC)
      milk_protein = 1.9 + 0.4 * fat_content,

      # N Retention
      N_retention = dplyr::case_when(
        animal_type %in% c("sheep", "goat") ~ 0.1,
        weight_gain > 0 & NEg > 0  ~ ((milk_yield * milk_protein) / 6.38) +
          ((weight_gain * (268 - (7.03 * NEg / weight_gain)) / 1000) / 6.25),
        TRUE ~ 0
      ),

      # N Intake and Excretion (IPCC Eqs 10.31 & 10.32)
      N_intake   = (ge / 18.45) * (cp / 100 / 6.25),
      N_excreted = dplyr::if_else(
        animal_type %in% c("sheep", "goat"),
        (N_intake * (1 - N_retention)) * 365,
        (N_intake - N_retention) * 365
      ),

      # Direct N2O Emissions (IPCC Eq 10.25)
      # 44/28 is the conversion factor from N2O-N to N2O
      N2O_emissions = population * N_excreted * allocation * EF3 * (44 / 28)
    ) %>%

    # --- 4. Final Formatting ---
    dplyr::select(
      dplyr::all_of(join_keys),
      system_base, system_variant, climate_moisture, climate_zone,
      N_intake, N_retention, N_excreted, EF3, population, N2O_emissions
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
