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
  user_manure  <- readr::read_csv("user_data/manure_management.csv", col_types = readr::cols(management_months = readr::col_character()), show_col_types = FALSE)
  ipcc_master  <- readr::read_csv("user_data/ipcc_mm.csv", col_types = readr::cols(management_months = readr::col_character()), show_col_types = FALSE)
  mono_csv     <- readr::read_csv("user_data/monogastric_definitions.csv", show_col_types = FALSE)

  ge_df  <- calculate_ge(saveoutput = FALSE)
  cp_df  <- calculate_weighted_variable(saveoutput = FALSE)
  pop_df <- calculate_population(automatic_cycle = automatic_cycle, saveoutput = FALSE)
  neg_df <- calculate_NEg(saveoutput = FALSE)
  dmi_df <- calculate_DMI(saveoutput = FALSE)

  # --- 2. Validations (Asserts) ---

  # 2.1 Combinations Integrity Check (Validates against ipcc_mm.csv)
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

  # --- 3. Master Dataset Construction & Joins ---

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
      dmi_df %>% dplyr::select(dplyr::all_of(join_keys), DMI_kgday),
      by = join_keys
    ) %>%

    dplyr::left_join(
      cat_csv %>%
        dplyr::select(region, subregion, animal_tag, class_flex, milk_yield_kg_year, fat_content_pct),
      by = c("region", "subregion", "animal_tag", "class_flex")
    ) %>%

    # Read structural fields matching your exact livestock_weights.csv
    dplyr::left_join(
      weights_csv %>%
        dplyr::select(region, subregion, animal_tag, class_flex, initial_weight_kg, final_weight_kg,
                      productive_period_days, sows_gestation_days, sows_lactation_days,
                      piglet_birth_weight_kg, piglet_weaning_weight_kg, sow_reserve_gain_kg),
      by = c("region", "subregion", "animal_tag", "class_flex")
    ) %>%

    # Read biological fields matching your exact monogastric_definitions.csv
    dplyr::left_join(
      mono_csv %>%
        dplyr::select(region, subregion, animal_tag, class_flex, egg_mass_g_day, piglets_born, piglets_suckling),
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
                      climate_moisture, allocation),
      by = c("region", "subregion", "animal_tag", "class_flex")
    ) %>%

    dplyr::left_join(
      ipcc_master %>%
        dplyr::select(system_base, management_months, system_climate,
                      system_subclimate, climate_zone, system_variant,
                      climate_moisture, animal_type, animal_subtype, EF3),
      by = c("system_base", "management_months", "system_climate",
             "system_subclimate", "climate_zone", "system_variant",
             "climate_moisture", "animal_type", "animal_subtype")
    ) %>%

    # --- 4. Calculations ---
    dplyr::mutate(
      dplyr::across(
        c(GE_MJday, CP_pct, population, DMI_kgday, milk_yield_kg_year, fat_content_pct,
          initial_weight_kg, final_weight_kg, productive_period_days, sows_gestation_days, sows_lactation_days,
          piglets_born, egg_mass_g_day, piglet_birth_weight_kg, piglet_weaning_weight_kg, sow_reserve_gain_kg, NEg_MJday, allocation, EF3),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),

      daily_milk = milk_yield_kg_year / 365,

      daily_gain = dplyr::if_else(
        productive_period_days > 0,
        (final_weight_kg - initial_weight_kg) / productive_period_days,
        0
      ),

      milk_protein = 1.9 + 0.4 * fat_content_pct,

      # [IPCC 2019] Dynamic Ngain growth factor selector based on final body weight (Table 10.20B)
      ipcc_swine_n_gain_factor = dplyr::case_when(
        animal_type == "swine" & final_weight_kg <= 7  ~ 0.031,
        animal_type == "swine" & final_weight_kg <= 20 ~ 0.028,
        animal_type == "swine" & final_weight_kg <= 40 ~ 0.025,
        animal_type == "swine" & final_weight_kg <= 80 ~ 0.024,
        animal_type == "swine" & final_weight_kg > 80  ~ 0.021,
        TRUE ~ 0
      ),

      # [IPCC 2019] Annualized Fertility Rate (FR) based on productive timeline days
      ipcc_swine_fr = dplyr::if_else(productive_period_days > 0, 365 / productive_period_days, 0),

      # [IPCC 2019] Sow body weight change from parturition to parturition (Skg)
      ipcc_swine_Skg = dplyr::if_else(
        animal_type == "swine" & piglets_born > 0,
        ((piglets_born * piglet_birth_weight_kg) / 0.806),
        0
      ),

      # [IPCC 2019] Eq 10.33a: Mother sow body tissue annual nitrogen retention (kg N/animal/year)
      ipcc_swine_N_gain_sow = 0.025 * ipcc_swine_fr * ipcc_swine_Skg,

      # [IPCC 2019] Eq 10.33b: Exported nitrogen content inside weaned piglets annual pool (kg N/animal/year)
      ipcc_swine_N_weaned_piglets = dplyr::if_else(
        animal_type == "swine" & piglets_born > 0,
        (0.025 * piglets_born * ipcc_swine_fr * ((piglet_weaning_weight_kg - piglet_birth_weight_kg)) / 0.98),
        0
      ),

      # --- Unified Daily Nitrogen Retention Switch (kg N / animal / day) ---
      N_retention_kg_day = dplyr::case_when(

        # Poultry lifecycle retention rate (Layers)
        animal_type == "poultry" &  egg_mass_g_day > 0 ~
          (0.028 * daily_gain) + ((0.0185 * egg_mass_g_day) / 1000),

        # Poultry lifecycle retention rate (Pullets and Broilers)
        animal_type == "poultry" & productive_period_days > 0 ~
          ((final_weight_kg - initial_weight_kg) * 0.028) / productive_period_days,

        # Small Ruminants baseline retention rate (IPCC constant fraction fallback)
        animal_type %in% c("sheep", "goat") ~ 0.1,

        # IPCC 2019 SWINE Scenario 1: Active Reproductora Mothers (Switch activated by gestation/lactation days)
        animal_type == "swine" & (sows_gestation_days > 0 | sows_lactation_days > 0) ~
          (ipcc_swine_N_gain_sow + ipcc_swine_N_weaned_piglets) / 365,

        # IPCC 2019 SWINE Scenario 2: Standard Growing Animals (Switch activated when lifecycle phase days are 0)
        animal_type == "swine" & productive_period_days > 0 & sows_gestation_days == 0 & sows_lactation_days == 0 ~
          ((final_weight_kg - initial_weight_kg) * ipcc_swine_n_gain_factor) / productive_period_days,

        # Cattle Scenario 1: Lactating AND growing concurrently
        animal_type == "cattle" & daily_gain > 0 & NEg_MJday > 0 ~
          ((daily_milk * (milk_protein / 100)) / 6.38) +
          ((daily_gain * (268 - (7.03 * NEg_MJday / daily_gain)) / 1000) / 6.25),

        # Cattle Scenario 2: Mature adult milking animals (No active growth)
        animal_type == "cattle" & daily_milk > 0 & (daily_gain <= 0 | NEg_MJday <= 0) ~
          ((daily_milk * (milk_protein / 100)) / 6.38),

        # Cattle Scenario 3: Young growing animals (No milking)
        animal_type == "cattle" & daily_milk == 0 & daily_gain > 0 & NEg_MJday > 0 ~
          ((daily_gain * (268 - (7.03 * NEg_MJday / daily_gain)) / 1000) / 6.25),

        TRUE ~ 0
      ),

      # Daily Nitrogen Feed Intake (kg N / animal / day)
      N_intake_kgheadday = dplyr::if_else(
        animal_type %in% c("poultry", "swine"),
        DMI_kgday * (CP_pct / 100 / 6.25),
        (GE_MJday / 18.45) * (CP_pct / 100 / 6.25)
      ),

      # Annual Nitrogen Excretion (kg N / animal / year)
      N_excreted_kgheadyear = dplyr::if_else(
        animal_type %in% c("sheep", "goat"),
        (N_intake_kgheadday * (1 - N_retention_kg_day)) * 365,
        (N_intake_kgheadday - N_retention_kg_day) * 365
      ),

      # Direct nitrous oxide emissions per active block system (kg N2O / year)
      direct_N2O_kgyear = population * N_excreted_kgheadyear * allocation * EF3 * (44 / 28)
    ) %>%

    # --- 5. Final Formatting ---
    dplyr::select(
      dplyr::all_of(join_keys),
      system_base, system_variant, climate_moisture, climate_zone, system_climate,
      system_subclimate, allocation, ipcc_swine_N_weaned_piglets, ipcc_swine_Skg, ipcc_swine_N_gain_sow, ipcc_swine_fr,
      N_intake_kgheadday, N_retention_kg_day, N_excreted_kgheadyear, EF3, population, direct_N2O_kgyear
    ) %>%
    dplyr::mutate(
      dplyr::across(where(is.numeric), ~ round(.x, 4))
    )

  # --- 6. Save Output ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/N2O_direct_manure.csv")
    message("\U0001f4be Saved output to output/N2O_direct_manure.csv")
  }

  return(results)
}
