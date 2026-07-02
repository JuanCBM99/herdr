#' Calculate Monogastric Metabolizable Energy Requirements (FEDNA)
#'
#' Calculates daily metabolizable energy requirements (kcal/day) for poultry and swine
#' using direct mathematical switches.
#'
#' @param saveoutput If TRUE (default), saves the results to `output/monogastric_metabolizable_energy.csv`.
#' @export
calculate_monogastric_energy <- function(saveoutput = TRUE) {

  message("\U0001f4be Calculating Monogastric Metabolizable Energy (kcal/day)...")

  # 1. Read input data files
  mono_csv <- readr::read_csv("user_data/monogastric_definitions.csv", show_col_types = FALSE)
  weights_csv <- readr::read_csv("user_data/livestock_weights.csv", show_col_types = FALSE)

  # Platform standard joining keys
  join_keys <- c("region", "subregion", "animal_tag", "class_flex", "animal_type", "animal_subtype")

  # 2. Join definition and weight files
  master <- mono_csv %>%
    dplyr::left_join(weights_csv, by = c("region", "subregion", "animal_tag", "class_flex")) %>%
    dplyr::filter(animal_type %in% c("poultry", "swine"))

  # 3. Process daily energy equations
  results <- master %>%
    dplyr::mutate(
      # Handle missing values globally (NA to 0)
      dplyr::across(
        c(
          initial_weight_kg, final_weight_kg, productive_period_days,
          sows_gestation_days, sows_lactation_days,
          cfi_maintenance, alpha, frac_fat_pct, frac_protein_pct,
          egg_mass_g_day, piglets_born, piglets_suckling,
          piglet_birth_weight_kg, piglet_weaning_weight_kg, sow_reserve_gain_kg
        ),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),

      # --- Weight and maintenance parameters ---
      adg_gday = dplyr::if_else(
        productive_period_days > 0 & final_weight_kg > initial_weight_kg,
        ((final_weight_kg - initial_weight_kg) / productive_period_days) * 1000,
        0
      ),
      W_mean = (initial_weight_kg + final_weight_kg) / 2,
      W_metabolic = dplyr::case_when(
        animal_type == "poultry" ~ W_mean^0.75,
        animal_type == "swine"   ~ W_mean^alpha,
        TRUE                     ~ W_mean^0.75
      ),
      ME_maint_kcal_day = cfi_maintenance * W_metabolic,

      # --- Gestation phase requirements ---
      gestation_adg_gday = dplyr::if_else(
        sows_gestation_days > 0 & final_weight_kg > initial_weight_kg,
        ((final_weight_kg - initial_weight_kg) / sows_gestation_days) * 1000,
        0
      ),
      ME_growth_gestation_daily = ((12.8 * frac_fat_pct * gestation_adg_gday) + (12.1 * frac_protein_pct * gestation_adg_gday)),
      ME_conceptus_daily = dplyr::if_else(
        sows_gestation_days > 0,
        ((2600 * piglet_birth_weight_kg * piglets_born) / sows_gestation_days) + 55.175,
        0
      ),
      ME_maternal_reserves_daily = dplyr::if_else(
        sows_gestation_days > 0,
        (sow_reserve_gain_kg * 4800) / sows_gestation_days,
        0
      ),
      ME_gestation_phase_daily = ME_growth_gestation_daily + ME_conceptus_daily + ME_maternal_reserves_daily,

      # --- Lactation phase requirements ---
      piglet_adg_gday = dplyr::if_else(
        sows_lactation_days > 0,
        ((piglet_weaning_weight_kg - piglet_birth_weight_kg) / sows_lactation_days) * 1000,
        0
      ),
      ME_milk_daily = dplyr::if_else(
        sows_lactation_days > 0,
        ((6.83 * piglet_adg_gday) - 125) * piglets_suckling,
        0
      ),
      ME_mobilization_daily = dplyr::if_else(
        sows_lactation_days > 0,
        -28183 / sows_lactation_days,
        0
      ),
      ME_lactation_phase_daily = ME_milk_daily + ME_mobilization_daily,

      # --- Additional requirements (Growth and eggs) ---
      ME_standard_growth_day = dplyr::case_when(
        animal_type == "poultry" ~
          (13.4 * frac_fat_pct * adg_gday) + (12.0 * frac_protein_pct * adg_gday),
        animal_type == "swine" & sows_gestation_days == 0 & sows_lactation_days == 0 ~
          (12.8 * frac_fat_pct * adg_gday) + (12.1 * frac_protein_pct * adg_gday),
        TRUE ~ 0
      ),
      ME_eggs_day = 2 * egg_mass_g_day,

      # --- Total daily energy integration ---
      ME_total_kcal_day = ME_maint_kcal_day + ME_standard_growth_day + ME_eggs_day + ME_gestation_phase_daily + ME_lactation_phase_daily,

      # Map outputs directly to report columns
      ME_mant_kcal      = ME_maint_kcal_day,
      ME_crec_kcal      = dplyr::if_else(sows_gestation_days > 0, ME_growth_gestation_daily, ME_standard_growth_day),
      ME_eggs_kcal      = ME_eggs_day,
      ME_gestation_kcal = ME_gestation_phase_daily,
      ME_lactation_kcal = ME_lactation_phase_daily
    ) %>%

    # Select final tidy report schema
    dplyr::select(
      dplyr::all_of(join_keys), diet_tag, initial_weight_kg, final_weight_kg, productive_period_days,
      adg_gday, alpha, cfi_maintenance, ME_mant_kcal, ME_crec_kcal, ME_eggs_kcal,
      ME_gestation_kcal, ME_lactation_kcal, ME_total_kcal_day
    ) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4)))

  # 4. Save results to disk
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/monogastric_metabolizable_energy.csv")
    message("\U0001f4be Saved phase-isolated monogastric energy to output/monogastric_metabolizable_energy.csv")
  }

  return(results)
}
