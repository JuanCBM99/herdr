#' Summarize CH4, N2O Emissions, and Land Use
#'
#' @param automatic_cycle Logical. TRUE for built-in model, FALSE for manual livestock_census.csv.
#' @param region Character/Numeric vector to filter.
#' @param subregion Character vector to filter.
#' @param animal Livestock type (animal_type).
#' @param type Livestock subtype (animal_subtype).
#' @param class_flex Management class (e.g., 'grazing', 'stall').
#' @param saveoutput If TRUE saves to output folder.
#' @param group_by_identification If TRUE returns by animal_tag.
#' @param farm_country Character. The country of the farm/study (e.g., "Spain"). Default is "Spain".
#' @param year Numeric. The reference year for FAO trade data calculation if origins are missing. Default is 2022.
#' @param gwp_report Character string or named numeric vector. IPCC Assessment Report version for Global Warming Potential (GWP100) factors. Options are `"AR5"` (default, CH4=28, N2O=265), `"AR6"` (CH4=27, N2O=273), `"AR4"` (CH4=25, N2O=298), or `"SAR"` (CH4=21, N2O=310). Alternatively, a custom named vector like `c(CH4 = 27, N2O = 273)` can be provided.
#' @param ar Optional shorthand alias for `gwp_report` (e.g., `ar = "AR6"`).
#' @param data_dir Path to the directory containing input CSV/data files. Defaults to `"user_data"`.
#' @export
generate_impact_assessment <- function(automatic_cycle = FALSE,
                                       region = NULL, subregion = NULL,
                                       animal = NULL, type = NULL, class_flex = NULL,
                                       saveoutput = TRUE, group_by_identification = TRUE,
                                       farm_country = "Spain", year = 2024,
                                       gwp_report = "AR5",
                                       ar = NULL,
                                       data_dir = "user_data") {

  # 0. Validate GWP report upfront
  gwp_presets <- list(
    AR6 = c(CH4 = 27, N2O = 273),
    AR5 = c(CH4 = 28, N2O = 265),
    AR4 = c(CH4 = 25, N2O = 298),
    SAR = c(CH4 = 21, N2O = 310)
  )

  effective_gwp <- if (!is.null(ar)) ar else gwp_report

  if (is.character(effective_gwp)) {
    gwp_choice <- toupper(effective_gwp[1])
    if (!gwp_choice %in% names(gwp_presets)) {
      stop(sprintf("Invalid GWP report '%s'. Must be one of: %s, or a named vector c(CH4 = ..., N2O = ...)",
                   effective_gwp[1], paste(names(gwp_presets), collapse = ", ")))
    }
    gwp_vals <- gwp_presets[[gwp_choice]]
  } else if (is.numeric(effective_gwp) && all(c("CH4", "N2O") %in% names(effective_gwp))) {
    gwp_choice <- "Custom"
    gwp_vals <- effective_gwp
  } else {
    stop("Invalid 'gwp_report'. Must be a string ('AR6', 'AR5', 'AR4', 'SAR') or a named numeric vector c(CH4 = ..., N2O = ...)")
  }

  ch4_factor <- as.numeric(gwp_vals[["CH4"]])
  n2o_factor <- as.numeric(gwp_vals[["N2O"]])

  message("\U0001f7e2 Starting impact assessment summary (GWP standard: ", gwp_choice, ")...")
  join_keys <- c("region", "subregion", "animal_tag", "class_flex", "animal_type", "animal_subtype")

  # 1. Pipeline calls and unit standardization (Gg)
  CH4_ent <- calculate_emissions_enteric(automatic_cycle = automatic_cycle, saveoutput = FALSE, data_dir = data_dir) %>%
    dplyr::group_by(across(all_of(join_keys))) %>%
    dplyr::summarise(CH4_enteric_Gg = sum(total_CH4_enteric_Ggyear, na.rm = TRUE), .groups = "drop")

  CH4_man <- calculate_CH4_manure(automatic_cycle = automatic_cycle, saveoutput = FALSE, data_dir = data_dir) %>%
    dplyr::group_by(across(all_of(join_keys))) %>%
    dplyr::summarise(CH4_manure_Gg = sum(total_CH4_mm_kgyear / 1e6, na.rm = TRUE), .groups = "drop")

  N2O_dir <- calculate_N2O_direct_manure(automatic_cycle = automatic_cycle, saveoutput = FALSE, data_dir = data_dir) %>%
    dplyr::group_by(across(all_of(join_keys))) %>%
    dplyr::summarise(N2O_direct_Gg = sum(direct_N2O_kgyear, na.rm = TRUE) / 1e6, .groups = "drop")

  N2O_vol <- calculate_N2O_indirect_volatilization(automatic_cycle = automatic_cycle, saveoutput = FALSE, data_dir = data_dir) %>%
    dplyr::group_by(across(all_of(join_keys))) %>%
    dplyr::summarise(N2O_vol_Gg = sum(N2O_vol_kgyear, na.rm = TRUE) / 1e6, .groups = "drop")

  N2O_lea <- calculate_N2O_indirect_leaching(automatic_cycle = automatic_cycle, saveoutput = FALSE, data_dir = data_dir) %>%
    dplyr::group_by(across(all_of(join_keys))) %>%
    dplyr::summarise(N2O_lea_Gg = sum(N2O_leach_kgyear, na.rm = TRUE) / 1e6, .groups = "drop")

  land_u  <- calculate_land_use(
    automatic_cycle = automatic_cycle,
    saveoutput = FALSE,
    farm_country = farm_country,
    year = year,
    data_dir = data_dir
  ) %>%
    dplyr::group_by(across(all_of(join_keys))) %>%
    dplyr::summarise(
      Land_m2                         = sum(total_land_use_m2, na.rm = TRUE),
      Land_cropland_m2                = sum(total_land_use_m2[!is.na(land_type) & land_type == "cropland"], na.rm = TRUE),
      Land_grassland_convertible_m2   = sum(total_land_use_m2[!is.na(land_type) & land_type == "grassland_convertible"], na.rm = TRUE),
      Land_grassland_unconvertible_m2 = sum(total_land_use_m2[!is.na(land_type) & land_type == "grassland_unconvertible"], na.rm = TRUE),
      Land_other_m2                   = pmax(0, sum(total_land_use_m2, na.rm = TRUE) - (
        sum(total_land_use_m2[!is.na(land_type) & land_type == "cropland"], na.rm = TRUE) +
        sum(total_land_use_m2[!is.na(land_type) & land_type == "grassland_convertible"], na.rm = TRUE) +
        sum(total_land_use_m2[!is.na(land_type) & land_type == "grassland_unconvertible"], na.rm = TRUE)
      )),
      .groups = "drop"
    )

  # Nutritional variables (CP and Cropland CP)
  cp_data <- suppressMessages(calculate_weighted_variable(saveoutput = FALSE, data_dir = data_dir)) %>%
    dplyr::group_by(across(all_of(join_keys))) %>%
    dplyr::summarise(
      CP_pct          = mean(CP_pct, na.rm = TRUE),
      CP_cropland_pct = mean(CP_cropland_pct, na.rm = TRUE),
      .groups = "drop"
    )

  # Dry matter intake (DMI) and feed consumption
  dmi_data <- suppressMessages(calculate_DMI(saveoutput = FALSE, data_dir = data_dir)) %>%
    dplyr::group_by(across(all_of(join_keys))) %>%
    dplyr::summarise(DMI_kgday = mean(DMI_kgday, na.rm = TRUE), .groups = "drop")

  # Production outputs for functional units and feed conversion
  prod_data <- suppressMessages(calculate_production(automatic_cycle = automatic_cycle, saveoutput = FALSE, data_dir = data_dir)) %>%
    dplyr::group_by(across(all_of(join_keys))) %>%
    dplyr::summarise(
      population             = sum(population, na.rm = TRUE),
      milk_fresh_kg          = sum(milk_fresh_kg, na.rm = TRUE),
      milk_FPCM_kg           = sum(milk_FPCM_kg, na.rm = TRUE),
      meat_live_weight_kg    = sum(meat_live_weight_kg, na.rm = TRUE),
      meat_carcass_weight_kg = sum(meat_carcass_weight_kg, na.rm = TRUE),
      egg_fresh_kg           = sum(egg_fresh_kg, na.rm = TRUE),
      wool_kg                = sum(wool_kg, na.rm = TRUE),
      total_protein_kg       = sum(total_protein_kg, na.rm = TRUE),
      fat_content_pct        = mean(fat_content_pct, na.rm = TRUE),
      production_role        = dplyr::first(production_role),
      .groups = "drop"
    ) %>%
    dplyr::left_join(dmi_data, by = join_keys) %>%
    dplyr::left_join(cp_data, by = join_keys) %>%
    dplyr::mutate(
      DMI_kgday           = tidyr::replace_na(DMI_kgday, 0),
      CP_pct              = tidyr::replace_na(CP_pct, 0),
      CP_cropland_pct     = tidyr::replace_na(CP_cropland_pct, 0),
      feed_intake_kg      = population * DMI_kgday * 365,
      feed_CP_total_kg    = feed_intake_kg * (CP_pct / 100),
      feed_CP_cropland_kg = feed_intake_kg * (CP_cropland_pct / 100),

      # Gross Biophysical Energies (IDF Bulletin 520 / 2022 & IPCC 2019)
      # 1. Milk Net Energy for Lactation (NEL, IDF Bulletin 520 p. 86):
      # 3.1 MJ / kg FPCM for cattle; 4.6 MJ/kg for dairy sheep; 3.0 MJ/kg for dairy goats.
      energy_milk_mj = dplyr::case_when(
        animal_type == "sheep" ~ 4.6 * milk_fresh_kg,
        animal_type == "goat"  ~ 3.0 * milk_fresh_kg,
        TRUE                   ~ 3.1 * milk_FPCM_kg
      ),

      # 2. Meat Net Energy for Growth/Weight (NEG, IDF Bulletin 520 p. 86):
      # 15.0 MJ/kg Live Weight for mature cull animals; 11.0 MJ/kg Live Weight for fattening/young animals.
      # For laying hens (egg_fresh_kg > 0), meat is not counted (100% burden goes to eggs).
      neg_mj_kg = dplyr::case_when(
        production_role == "mature" ~ 15.0,
        TRUE                        ~ 11.0
      ),
      energy_meat_mj = dplyr::if_else(
        egg_fresh_kg > 0,
        0.0,
        neg_mj_kg * meat_live_weight_kg
      ),

      # 3. Wool Net Energy (IPCC Eq 10.12 = 24.0 MJ/kg greasy wool):
      # Allocated ONLY for meat sheep breeds. For dairy sheep and goats, wool is a non-commercial byproduct.
      energy_wool_mj = dplyr::if_else(
        animal_type == "sheep" & animal_subtype == "meat",
        24.0 * wool_kg,
        0.0
      )
    ) %>%
    dplyr::select(-DMI_kgday, -neg_mj_kg, -fat_content_pct, -production_role, -CP_pct, -CP_cropland_pct)

  # 2. Consolidation
  complete_summary <- list(CH4_ent, CH4_man, N2O_dir, N2O_vol, N2O_lea, land_u, prod_data) %>%
    purrr::reduce(dplyr::full_join, by = join_keys) %>%
    dplyr::mutate(across(where(is.numeric), ~ tidyr::replace_na(., 0)))

  # 3. Apply Filters
  final_summary <- complete_summary
  if (!is.null(region))     final_summary <- final_summary %>% dplyr::filter(region %in% .env$region)
  if (!is.null(subregion))  final_summary <- final_summary %>% dplyr::filter(subregion %in% .env$subregion)
  if (!is.null(animal))     final_summary <- final_summary %>% dplyr::filter(animal_type %in% .env$animal)
  if (!is.null(type))       final_summary <- final_summary %>% dplyr::filter(animal_subtype %in% .env$type)
  if (!is.null(class_flex)) final_summary <- final_summary %>% dplyr::filter(class_flex %in% .env$class_flex)

  # 4. Aggregation
  if (!group_by_identification) {
    final_summary <- final_summary %>%
      dplyr::group_by(region, subregion, class_flex, animal_type, animal_subtype) %>%
      dplyr::summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
  }

  # Calculate average daily DMI per head (kg DM / head / day)
  final_summary <- final_summary %>%
    dplyr::mutate(
      DMI_kgday = dplyr::if_else(population > 0, round(feed_intake_kg / (population * 365), 3), 0)
    )

  # 5. Calculate CO2eq and Carbon Footprint
  final_summary <- final_summary %>%
    dplyr::mutate(
      CO2eq_enteric      = CH4_enteric_Gg * ch4_factor,
      CO2eq_manure       = CH4_manure_Gg * ch4_factor,
      CO2eq_N2O_direct   = N2O_direct_Gg * n2o_factor,
      CO2eq_N2O_indirect = (N2O_vol_Gg + N2O_lea_Gg) * n2o_factor,
      CO2eq_N2O          = CO2eq_N2O_direct + CO2eq_N2O_indirect,
      CO2eq_Total_Gg     = CO2eq_enteric + CO2eq_manure + CO2eq_N2O
    )

  # 5b. Functional Units and Emission Intensities
  # Conversion: 1 Gg CO2e = 1e6 kg CO2e
  co2e_total_kg <- final_summary$CO2eq_Total_Gg * 1e6
  land_total_m2 <- final_summary$Land_m2
  feed_total_kg <- final_summary$feed_intake_kg

  milk_kg      <- final_summary$milk_FPCM_kg
  meat_kg      <- final_summary$meat_carcass_weight_kg
  meat_live_kg <- final_summary$meat_live_weight_kg
  wool_kg      <- final_summary$wool_kg
  egg_kg       <- final_summary$egg_fresh_kg

  # Flag non-milking dairy phases (e.g. dry_phase, replacement heifers)
  is_non_milking_dairy <- (final_summary$animal_subtype %in% "dairy") & (milk_kg == 0)

  # 1) Per kg of Edible Protein (Universal nutritional FU)
  prot_kg      <- final_summary$total_protein_kg
  ghg_protein  <- dplyr::if_else(prot_kg > 0 & !is_non_milking_dairy, round(co2e_total_kg / prot_kg, 3), NA_real_)
  land_protein <- dplyr::if_else(prot_kg > 0 & !is_non_milking_dairy, round(land_total_m2 / prot_kg, 2), NA_real_)

  # 2) Per Animal Head / Year (Per capita physiological FU)
  pop_n     <- final_summary$population
  ghg_head  <- dplyr::if_else(pop_n > 0, round(co2e_total_kg / pop_n, 2), NA_real_)
  land_head <- dplyr::if_else(pop_n > 0, round(land_total_m2 / pop_n, 1), NA_real_)

  # 3) Per Commercial Product (IDF Bulletin 520 / 2022 Biophysical Allocation)
  # Total biophysical energy across co-products in multi-output systems (MJ)
  energy_total_mj <- final_summary$energy_milk_mj +
                     final_summary$energy_meat_mj +
                     final_summary$energy_wool_mj

  has_energy <- energy_total_mj > 0

  # Allocation Factors (AF):
  # In laying hens, 100% of the burden goes to eggs (meat is non-commercial/not counted)
  af_egg <- dplyr::if_else(egg_kg > 0, 1.0, 0.0)

  # For ruminants and non-egg systems:
  af_milk <- dplyr::case_when(
    is_non_milking_dairy     ~ 0.0,
    has_energy & egg_kg == 0 ~ final_summary$energy_milk_mj / energy_total_mj,
    TRUE                     ~ 0.0
  )
  af_meat <- dplyr::case_when(
    egg_kg > 0           ~ 0.0,
    is_non_milking_dairy ~ 0.0,
    has_energy           ~ final_summary$energy_meat_mj / energy_total_mj,
    meat_kg > 0          ~ 1.0,
    TRUE                 ~ 0.0
  )
  af_wool <- dplyr::if_else(has_energy & egg_kg == 0 & !is_non_milking_dairy, final_summary$energy_wool_mj / energy_total_mj, 0.0)

  # Dedicated Product Intensities
  # A) Milk
  ghg_milk  <- dplyr::if_else(milk_kg > 0 & !is_non_milking_dairy, round((co2e_total_kg * af_milk) / milk_kg, 3), NA_real_)
  land_milk <- dplyr::if_else(milk_kg > 0 & !is_non_milking_dairy, round((land_total_m2 * af_milk) / milk_kg, 2), NA_real_)

  # B) Meat
  ghg_meat  <- dplyr::if_else(meat_kg > 0 & !is_non_milking_dairy, round((co2e_total_kg * af_meat) / meat_kg, 3), NA_real_)
  land_meat <- dplyr::if_else(meat_kg > 0 & !is_non_milking_dairy, round((land_total_m2 * af_meat) / meat_kg, 2), NA_real_)

  # C) Wool (only meat sheep breeds)
  ghg_wool  <- dplyr::if_else(wool_kg > 0 & af_wool > 0, round((co2e_total_kg * af_wool) / wool_kg, 3), NA_real_)
  land_wool <- dplyr::if_else(wool_kg > 0 & af_wool > 0, round((land_total_m2 * af_wool) / wool_kg, 2), NA_real_)

  # D) Eggs
  ghg_egg   <- dplyr::if_else(egg_kg > 0, round((co2e_total_kg * af_egg) / egg_kg, 3), NA_real_)
  land_egg  <- dplyr::if_else(egg_kg > 0, round((land_total_m2 * af_egg) / egg_kg, 2), NA_real_)

  # E) Primary Product (for backwards compatibility)
  primary_prod <- dplyr::case_when(
    milk_kg > 0 ~ "kg FPCM Milk",
    is_non_milking_dairy ~ "None (Dry/Rearing)",
    egg_kg > 0 ~ "kg Fresh Eggs",
    meat_kg > 0 ~ "kg Carcass Meat",
    wool_kg > 0 & af_wool > 0 ~ "kg Greasy Wool",
    TRUE ~ "None"
  )

  ghg_product <- dplyr::case_when(
    is_non_milking_dairy      ~ NA_real_,
    milk_kg > 0               ~ ghg_milk,
    egg_kg > 0                ~ ghg_egg,
    meat_kg > 0               ~ ghg_meat,
    wool_kg > 0 & af_wool > 0 ~ ghg_wool,
    TRUE                      ~ NA_real_
  )

  land_product <- dplyr::case_when(
    is_non_milking_dairy      ~ NA_real_,
    milk_kg > 0               ~ land_milk,
    egg_kg > 0                ~ land_egg,
    meat_kg > 0               ~ land_meat,
    wool_kg > 0 & af_wool > 0 ~ land_wool,
    TRUE                      ~ NA_real_
  )

  # 4) Protein Feed Conversion and Human Food Security (Mottet et al. 2017)
  feed_cp_tot  <- final_summary$feed_CP_total_kg
  feed_cp_crop <- final_summary$feed_CP_cropland_kg

  valid_prot <- prot_kg > 0 & !is_non_milking_dairy

  protein_fcr_total    <- dplyr::if_else(valid_prot, round(feed_cp_tot / prot_kg, 2), NA_real_)
  protein_fcr_cropland <- dplyr::if_else(valid_prot, round(feed_cp_crop / prot_kg, 2), NA_real_)

  final_summary <- final_summary %>%
    dplyr::mutate(
      primary_product        = primary_prod,
      feed_intake_kg         = round(feed_total_kg, 1),
      feed_CP_total_kg       = round(feed_cp_tot, 1),
      feed_CP_cropland_kg    = round(feed_cp_crop, 1),

      # Allocation factors
      AF_milk                = round(af_milk, 4),
      AF_meat                = round(af_meat, 4),
      AF_wool                = round(af_wool, 4),
      AF_egg                 = round(af_egg, 4),

      # Product-specific intensities
      GHG_intensity_milk     = ghg_milk,
      Land_intensity_milk    = land_milk,

      GHG_intensity_meat     = ghg_meat,
      Land_intensity_meat    = land_meat,

      GHG_intensity_wool     = ghg_wool,
      Land_intensity_wool    = land_wool,

      GHG_intensity_egg      = ghg_egg,
      Land_intensity_egg     = land_egg,

      # Primary product intensities
      GHG_intensity_product  = ghg_product,
      Land_intensity_product = land_product,

      # Universal Nutritional and Per-Head FUs
      GHG_intensity_protein  = ghg_protein,
      Land_intensity_protein = land_protein,
      GHG_intensity_head     = ghg_head,
      Land_intensity_head    = land_head,

      # Protein Feed Conversion and Human Food Security (Mottet et al. 2017)
      Protein_FCR_total      = protein_fcr_total,
      Protein_FCR_cropland   = protein_fcr_cropland
    ) %>%
    dplyr::select(-dplyr::any_of(c("energy_milk_mj", "energy_meat_mj", "energy_wool_mj")))

  attr(final_summary, "gwp_report") <- gwp_choice
  attr(final_summary, "gwp_factors") <- gwp_vals

  # 6. Save Output
  if (saveoutput) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(final_summary, "output/impact_assessment_summary.csv")
    message("\U1F4BE Report saved to output/impact_assessment_summary.csv")
  }

  return(final_summary)
}
