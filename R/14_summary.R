#' Summarize CH4, N2O Emissions, Land Use, and Farm Product Intensities
#'
#' @description Integrates all upstream Tier 2 GHG emissions, agroecological land footprints,
#' and GLEAM production outputs. Generates two distinct reports: an absolute environmental inventory (totals)
#' and farm-level product, nutritional, and per-head environmental intensities.
#'
#' Product intensities (Table 2) are obtained by allocating the whole enterprise footprint
#' (GHG, land, water) among its co-products using a biophysical (energy-based) allocation
#' factor: \code{AF_k = Energy_k / Energy_total}.
#'
#' @param automatic_cycle Logical. TRUE for built-in demographic models, FALSE for manual livestock_census.csv.
#' @param farm_id Character vector to filter by farm_id. Default is NULL.
#' @param region Character/Numeric vector to filter by geographical region (e.g., country or province). Together with \code{subregion}, defines the spatial location of the farm; management or housing variants within the same farm must be distinguished via \code{animal_tag}, not \code{subregion}. Default is NULL.
#' @param subregion Character vector to filter by geographical subregion (e.g., municipality or district). Default is NULL.
#' @param animal Livestock type (animal_type). Default is NULL.
#' @param type Livestock subtype (animal_subtype). Default is NULL.
#' @param saveoutput If TRUE saves to output folder: 'output/total_emissions_and_land_use.csv' and 'output/product_intensities.csv'.
#' @param group_by_identification If TRUE returns the inventory totals by individual animal_tag cohort. If FALSE aggregates totals by farm_id, region, subregion, animal_type, animal_subtype.
#' @param farm_country Character. The country of the farm/study (e.g., "Spain"). Default is "Spain".
#' @param year Numeric. The reference year for FAO trade data calculation if origins are missing. Default is 2024.
#' @param gwp_report Character string or named numeric vector. IPCC Assessment Report version for Global Warming Potential (GWP100) factors. Options are `"AR5"` (default, CH4=28, N2O=265), `"AR6"` (CH4=27, N2O=273), `"AR4"` (CH4=25, N2O=298), or `"SAR"` (CH4=21, N2O=310). Alternatively, a custom named vector like `c(CH4 = 27, N2O = 273)` can be provided.
#' @param ar Optional shorthand alias for `gwp_report` (e.g., `ar = "AR6"`).
#' @param data_dir Path to the directory containing input CSV/data files. Defaults to `"user_data"`.
#' @return A named list containing two dataframes:
#'   \item{totals}{Absolute environmental inventory of emissions (Gg CH4, Gg N2O, Gg CO2eq), feed (kg/year), land (m2) and water (m3).}
#'   \item{intensities}{Farm-level product (per kg FPCM milk, carcass meat, eggs, wool or edible protein), nutritional (kg feed protein / kg product protein), and per-head environmental intensities.}
#' @examples
#' \dontrun{
#' res <- generate_impact_assessment(
#'   farm_country = "Spain", year = 2022,
#'   saveoutput = FALSE, ar = "AR6"
#' )
#' res$totals
#' res$intensities
#' }
#' @export
generate_impact_assessment <- function(automatic_cycle = FALSE,
                                       farm_id = NULL,
                                       region = NULL, subregion = NULL,
                                       animal = NULL, type = NULL,
                                       saveoutput = TRUE, group_by_identification = TRUE,
                                       farm_country = "Spain", year = 2024,
                                       gwp_report = "AR5",
                                       ar = NULL,
                                       data_dir = "user_data") {
  # 0. Validate inputs and resolve GWP factors upfront
  assertthat::assert_that(
    assertthat::is.flag(automatic_cycle), assertthat::is.flag(saveoutput),
    assertthat::is.flag(group_by_identification),
    assertthat::is.string(farm_country), is.numeric(year), length(year) == 1,
    assertthat::is.string(data_dir)
  )

  gwp <- resolve_gwp(if (!is.null(ar)) ar else gwp_report)
  gwp_choice <- gwp$choice
  gwp_vals <- gwp$values
  ch4_factor <- as.numeric(gwp_vals[["CH4"]])
  n2o_factor <- as.numeric(gwp_vals[["N2O"]])

  message("[herdr] Starting impact assessment summary (GWP standard: ", gwp_choice, ")...")

  # Cohort identifier used to join every upstream result. `animal_tag` is part of the key
  # so that cohorts of the same species/subtype stay separate until the enterprise-level
  # allocation in Table 2.
  join_keys <- c("farm_id", "region", "subregion", "animal_tag", "animal_type", "animal_subtype")

  # 1. Base components computed ONCE in memory
  pop_data <- suppressMessages(calculate_population(automatic_cycle = automatic_cycle, saveoutput = FALSE, data_dir = data_dir))
  cp_data <- suppressMessages(calculate_weighted_variable(saveoutput = FALSE, data_dir = data_dir))
  ge_data <- suppressMessages(calculate_ge(saveoutput = FALSE, data_dir = data_dir, de_df = cp_data))
  mono_data <- suppressMessages(calculate_monogastric_energy(saveoutput = FALSE, data_dir = data_dir))
  dmi_data <- suppressMessages(calculate_DMI(saveoutput = FALSE, data_dir = data_dir, ge_req = ge_data, diet_char = cp_data, mono = mono_data))
  vs_data <- suppressMessages(calculate_vs(saveoutput = FALSE, data_dir = data_dir, ge_data = ge_data, dmi_data = dmi_data, diet_char = cp_data))

  CH4_ent <- calculate_emissions_enteric(
    automatic_cycle = automatic_cycle, saveoutput = FALSE, data_dir = data_dir,
    diet_vars = cp_data, ge_df = ge_data, pop_df = pop_data
  ) %>%
    dplyr::group_by(across(all_of(join_keys))) %>%
    dplyr::summarise(CH4_enteric_Gg = sum(total_CH4_enteric_Ggyear, na.rm = TRUE), .groups = "drop")

  CH4_man <- calculate_CH4_manure(
    automatic_cycle = automatic_cycle, saveoutput = FALSE, data_dir = data_dir,
    vs_df = vs_data, pop_df = pop_data
  ) %>%
    dplyr::group_by(across(all_of(join_keys))) %>%
    dplyr::summarise(CH4_manure_Gg = sum(total_CH4_mm_kgyear / 1e6, na.rm = TRUE), .groups = "drop")

  N2O_dir_raw <- calculate_N2O_direct_manure(
    automatic_cycle = automatic_cycle, saveoutput = FALSE, data_dir = data_dir,
    ge_df = ge_data, cp_df = cp_data, pop_df = pop_data, neg_df = ge_data, dmi_df = dmi_data
  )

  N2O_dir <- N2O_dir_raw %>%
    dplyr::group_by(across(all_of(join_keys))) %>%
    dplyr::summarise(N2O_direct_Gg = sum(direct_N2O_kgyear, na.rm = TRUE) / 1e6, .groups = "drop")

  ipcc_mm_data <- herdr_read_manure_table(file.path(data_dir, "ipcc_mm.csv"))

  N2O_vol <- herdr_calculate_N2O_indirect(
    mode = "volatilization",
    automatic_cycle = automatic_cycle, saveoutput = FALSE, data_dir = data_dir,
    direct_N2O_df = N2O_dir_raw, pop_df = pop_data, ipcc_master = ipcc_mm_data
  ) %>%
    dplyr::group_by(across(all_of(join_keys))) %>%
    dplyr::summarise(N2O_vol_Gg = sum(N2O_vol_kgyear, na.rm = TRUE) / 1e6, .groups = "drop")

  N2O_lea <- herdr_calculate_N2O_indirect(
    mode = "leaching",
    automatic_cycle = automatic_cycle, saveoutput = FALSE, data_dir = data_dir,
    direct_N2O_df = N2O_dir_raw, pop_df = pop_data, ipcc_master = ipcc_mm_data
  ) %>%
    dplyr::group_by(across(all_of(join_keys))) %>%
    dplyr::summarise(N2O_lea_Gg = sum(N2O_leach_kgyear, na.rm = TRUE) / 1e6, .groups = "drop")

  land_u <- calculate_land_use(
    automatic_cycle = automatic_cycle,
    saveoutput = FALSE,
    farm_country = farm_country,
    year = year,
    data_dir = data_dir,
    DMI_df = dmi_data,
    population_df = pop_data
  ) %>%
    dplyr::group_by(across(all_of(join_keys))) %>%
    dplyr::summarise(
      Land_m2 = sum(total_land_use_m2, na.rm = TRUE),
      Land_cropland_m2 = sum(total_land_use_m2[!is.na(land_type) & land_type == "cropland"], na.rm = TRUE),
      Land_grassland_convertible_m2 = sum(total_land_use_m2[!is.na(land_type) & land_type == "grassland_convertible"], na.rm = TRUE),
      Land_grassland_unconvertible_m2 = sum(total_land_use_m2[!is.na(land_type) & land_type == "grassland_unconvertible"], na.rm = TRUE),
      # Residual land not classified as cropland or grassland (never negative)
      Land_other_m2 = pmax(0, Land_m2 - Land_cropland_m2 - Land_grassland_convertible_m2 - Land_grassland_unconvertible_m2),
      .groups = "drop"
    )

  feed_emiss <- calculate_feed_emissions(
    automatic_cycle = automatic_cycle,
    saveoutput = FALSE,
    data_dir = data_dir,
    DMI_df = dmi_data,
    population_df = pop_data
  ) %>%
    dplyr::group_by(across(all_of(join_keys))) %>%
    dplyr::summarise(
      CO2eq_feed_Gg = sum(feed_CO2e_total_kg_year, na.rm = TRUE) / 1e6,
      CO2eq_feed_fossil_Gg = sum(feed_CO2e_fossil_kg_year, na.rm = TRUE) / 1e6,
      CO2eq_feed_luc_Gg = sum(feed_CO2e_luc_kg_year, na.rm = TRUE) / 1e6,
      Water_feed_m3 = sum(feed_water_m3_year, na.rm = TRUE),
      .groups = "drop"
    )

  # Nutritional variables (CP and Cropland CP)
  cp_summary <- cp_data %>%
    dplyr::group_by(across(all_of(join_keys))) %>%
    dplyr::summarise(
      CP_pct = mean(CP_pct, na.rm = TRUE),
      CP_cropland_pct = mean(CP_cropland_pct, na.rm = TRUE),
      .groups = "drop"
    )

  # Dry matter intake (DMI) per head
  dmi_summary <- dmi_data %>%
    dplyr::group_by(across(all_of(join_keys))) %>%
    dplyr::summarise(DMI_kgday = mean(DMI_kgday, na.rm = TRUE), .groups = "drop")

  # Production outputs. DMI and CP are only needed here to derive the cohort feed intake
  # (kg/year) and feed protein (kg/year); the per-head DMI is recomputed from feed_intake_kg
  # at the cohort stage below.
  prod_data <- suppressMessages(calculate_production(
    automatic_cycle = automatic_cycle, saveoutput = FALSE, data_dir = data_dir,
    pop_df = pop_data
  )) %>%
    dplyr::group_by(across(all_of(join_keys))) %>%
    dplyr::summarise(
      population = sum(population, na.rm = TRUE),
      milk_fresh_kg = sum(milk_fresh_kg, na.rm = TRUE),
      milk_FPCM_kg = sum(milk_FPCM_kg, na.rm = TRUE),
      meat_live_weight_kg = sum(meat_live_weight_kg, na.rm = TRUE),
      meat_carcass_weight_kg = sum(meat_carcass_weight_kg, na.rm = TRUE),
      egg_fresh_kg = sum(egg_fresh_kg, na.rm = TRUE),
      wool_kg = sum(wool_kg, na.rm = TRUE),
      total_protein_kg = sum(total_protein_kg, na.rm = TRUE),
      production_role = dplyr::first(production_role),
      .groups = "drop"
    ) %>%
    dplyr::left_join(dmi_summary, by = join_keys) %>%
    dplyr::left_join(cp_summary, by = join_keys) %>%
    dplyr::mutate(
      DMI_kgday = tidyr::replace_na(DMI_kgday, 0),
      CP_pct = tidyr::replace_na(CP_pct, 0),
      CP_cropland_pct = tidyr::replace_na(CP_cropland_pct, 0),
      feed_intake_kg = population * DMI_kgday * 365,
      feed_CP_total_kg = feed_intake_kg * (CP_pct / 100),
      feed_CP_cropland_kg = feed_intake_kg * (CP_cropland_pct / 100)
    ) %>%
    dplyr::select(-DMI_kgday, -CP_pct, -CP_cropland_pct)

  # Authoritative production_role from the definition files
  role_lookup <- load_production_roles(data_dir)

  complete_summary <- list(CH4_ent, CH4_man, N2O_dir, N2O_vol, N2O_lea, land_u, feed_emiss, prod_data) %>%
    purrr::reduce(dplyr::full_join, by = join_keys) %>%
    dplyr::mutate(across(dplyr::where(is.numeric), ~ tidyr::replace_na(., 0))) %>%
    dplyr::mutate(
      animal_subtype = trimws(as.character(animal_subtype))
    )

  if (nrow(role_lookup) > 0) {
    complete_summary <- complete_summary %>%
      dplyr::select(-dplyr::any_of("production_role")) %>%
      dplyr::left_join(role_lookup, by = c("farm_id", "region", "subregion", "animal_tag"))
  }
  complete_summary <- complete_summary %>%
    dplyr::mutate(production_role = tidyr::replace_na(production_role, "mature"))

  # Preserve the user's cohort order from livestock_census.csv / pop_data
  if (nrow(pop_data) > 0) {
    pop_order_df <- pop_data %>%
      dplyr::mutate(.pop_row_id = dplyr::row_number()) %>%
      dplyr::select(farm_id, region, subregion, animal_tag, .pop_row_id) %>%
      dplyr::distinct(farm_id, region, subregion, animal_tag, .keep_all = TRUE)
    complete_summary <- complete_summary %>%
      dplyr::left_join(pop_order_df, by = c("farm_id", "region", "subregion", "animal_tag")) %>%
      dplyr::arrange(.pop_row_id) %>%
      dplyr::select(-.pop_row_id)
  }

  # 3. Apply User Filters
  filtered_summary <- complete_summary
  if (!is.null(farm_id)) filtered_summary <- filtered_summary %>% dplyr::filter(farm_id %in% .env$farm_id)
  if (!is.null(region)) filtered_summary <- filtered_summary %>% dplyr::filter(region %in% .env$region)
  if (!is.null(subregion)) filtered_summary <- filtered_summary %>% dplyr::filter(subregion %in% .env$subregion)
  if (!is.null(animal)) filtered_summary <- filtered_summary %>% dplyr::filter(animal_type %in% .env$animal)
  if (!is.null(type)) filtered_summary <- filtered_summary %>% dplyr::filter(animal_subtype %in% .env$type)

  # 4. Cohort Level Calculations
  cohort_summary <- filtered_summary %>%
    dplyr::mutate(
      DMI_kgday                 = dplyr::if_else(population > 0, round(feed_intake_kg / (population * 365), 3), 0),
      N2O_indirect_Gg           = N2O_vol_Gg + N2O_lea_Gg,
      CO2eq_enteric             = CH4_enteric_Gg * ch4_factor,
      CO2eq_manure              = CH4_manure_Gg * ch4_factor,
      CO2eq_N2O_direct          = N2O_direct_Gg * n2o_factor,
      CO2eq_N2O_indirect        = N2O_indirect_Gg * n2o_factor,
      CO2eq_N2O                 = CO2eq_N2O_direct + CO2eq_N2O_indirect,
      CO2eq_Total_Gg            = CO2eq_enteric + CO2eq_manure + CO2eq_N2O,
      CO2eq_cradle_to_farm_Gg   = CO2eq_Total_Gg + CO2eq_feed_Gg,
      CO2eq_Total_head          = dplyr::if_else(population > 0, round((CO2eq_Total_Gg * 1e6) / population, 2), 0),
      CO2eq_cradle_to_farm_head = dplyr::if_else(population > 0, round((CO2eq_cradle_to_farm_Gg * 1e6) / population, 2), 0),
      Land_head                 = dplyr::if_else(population > 0, round(Land_m2 / population, 2), 0),
      Water_head                = dplyr::if_else(population > 0, round(Water_feed_m3 / population, 2), 0)
    )

  # ==============================================================================
  # TABLE 1: TOTAL EMISSIONS AND LAND USE (Absolute Bio-Environmental Inventory)
  # ==============================================================================
  # Additive columns: summed when cohorts are aggregated to enterprise level.
  additive_cols <- c(
    "population", "feed_intake_kg", "feed_CP_total_kg", "feed_CP_cropland_kg",
    "CH4_enteric_Gg", "CH4_manure_Gg", "N2O_direct_Gg", "N2O_vol_Gg", "N2O_lea_Gg", "N2O_indirect_Gg",
    "CO2eq_enteric", "CO2eq_manure", "CO2eq_N2O_direct", "CO2eq_N2O_indirect", "CO2eq_N2O",
    "CO2eq_feed_fossil_Gg", "CO2eq_feed_luc_Gg", "CO2eq_feed_Gg",
    "CO2eq_Total_Gg", "CO2eq_cradle_to_farm_Gg",
    "Land_cropland_m2", "Land_grassland_convertible_m2", "Land_grassland_unconvertible_m2", "Land_other_m2", "Land_m2",
    "Water_feed_m3"
  )

  if (group_by_identification) {
    totals_df <- cohort_summary %>%
      dplyr::select(
        farm_id, region, subregion, animal_tag, animal_type, animal_subtype, production_role,
        population, DMI_kgday, dplyr::all_of(additive_cols[-1]),
        CO2eq_Total_head, CO2eq_cradle_to_farm_head, Land_head, Water_head
      )
  } else {
    totals_df <- cohort_summary %>%
      dplyr::mutate(.orig_row_id = dplyr::row_number()) %>%
      dplyr::group_by(farm_id, region, subregion, animal_type, animal_subtype) %>%
      dplyr::summarise(
        .orig_row_id = min(.orig_row_id, na.rm = TRUE),
        across(all_of(additive_cols), ~ sum(.x, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      dplyr::arrange(.orig_row_id) %>%
      dplyr::select(-.orig_row_id) %>%
      dplyr::mutate(
        DMI_kgday                 = dplyr::if_else(population > 0, round(feed_intake_kg / (population * 365), 3), 0),
        CO2eq_Total_head          = dplyr::if_else(population > 0, round((CO2eq_Total_Gg * 1e6) / population, 2), 0),
        CO2eq_cradle_to_farm_head = dplyr::if_else(population > 0, round((CO2eq_cradle_to_farm_Gg * 1e6) / population, 2), 0),
        Land_head                 = dplyr::if_else(population > 0, round(Land_m2 / population, 2), 0),
        Water_head                = dplyr::if_else(population > 0, round(Water_feed_m3 / population, 2), 0)
      ) %>%
      dplyr::relocate(DMI_kgday, .after = population)
  }

  # ==============================================================================
  # TABLE 2: PRODUCT INTENSITIES (Functional Units & Allocation per Cohort)
  # ==============================================================================
  intensities_df <- cohort_summary %>%
    dplyr::mutate(.orig_row_id = dplyr::row_number()) %>%
    dplyr::group_by(farm_id, region, subregion, animal_type, animal_subtype) %>%
    dplyr::group_modify(~ {
      df_enterprise <- .x
      mat_idx <- df_enterprise$production_role == "mature"
      slau_idx <- df_enterprise$production_role == "slaughter"

      # Total enterprise footprint pool (Scope 1, Cradle-to-Farm, Land, Water)
      pool_farm <- list(
        s1    = sum(df_enterprise$CO2eq_Total_Gg, na.rm = TRUE) * 1e6,
        c2f   = sum(df_enterprise$CO2eq_cradle_to_farm_Gg, na.rm = TRUE) * 1e6,
        land  = sum(df_enterprise$Land_m2, na.rm = TRUE),
        water = sum(df_enterprise$Water_feed_m3, na.rm = TRUE)
      )

      # Enterprise outputs by product stream. "cull" = meat from mature animals,
      # "young" = meat from slaughter cohorts.
      enterprise <- list(
        milk_fpcm  = sum(df_enterprise$milk_FPCM_kg, na.rm = TRUE),
        milk_fresh = sum(df_enterprise$milk_fresh_kg, na.rm = TRUE),
        egg_kg     = sum(df_enterprise$egg_fresh_kg, na.rm = TRUE),
        wool_kg    = sum(df_enterprise$wool_kg, na.rm = TRUE),
        cull_cw    = sum(df_enterprise$meat_carcass_weight_kg[mat_idx], na.rm = TRUE),
        cull_lw    = sum(df_enterprise$meat_live_weight_kg[mat_idx], na.rm = TRUE),
        young_cw   = sum(df_enterprise$meat_carcass_weight_kg[slau_idx], na.rm = TRUE),
        young_lw   = sum(df_enterprise$meat_live_weight_kg[slau_idx], na.rm = TRUE),
        # Edible protein (milk + meat + eggs) of the cohorts that deliver products
        protein_kg = sum(df_enterprise$total_protein_kg[mat_idx | slau_idx], na.rm = TRUE)
      )

      # Enterprise master allocation factors (shared by every cohort of the enterprise)
      af_sys <- calc_biophysical_af(as.character(.y$animal_type), enterprise)

      dplyr::bind_rows(lapply(seq_len(nrow(df_enterprise)), function(i) {
        calc_cohort_intensities(df_enterprise[i, ], af_sys, pool_farm, enterprise)
      }))
    }) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.orig_row_id) %>%
    dplyr::select(-.orig_row_id) %>%
    dplyr::relocate(subregion, animal_tag, animal_type, animal_subtype, production_role, population, .after = region)

  attr(totals_df, "gwp_report") <- gwp_choice
  attr(totals_df, "gwp_factors") <- gwp_vals
  attr(intensities_df, "gwp_report") <- gwp_choice
  attr(intensities_df, "gwp_factors") <- gwp_vals

  # 5. Save Output Files
  if (saveoutput) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(totals_df, "output/total_emissions_and_land_use.csv")
    readr::write_csv(intensities_df, "output/product_intensities.csv")
    message("[herdr] Reports saved to output/total_emissions_and_land_use.csv and output/product_intensities.csv")
  }

  results <- list(
    totals = totals_df,
    intensities = intensities_df
  )
  attr(results, "gwp_report") <- gwp_choice
  attr(results, "gwp_factors") <- gwp_vals

  return(results)
}

# ==============================================================================
# INTERNAL HELPERS (not exported)
# ==============================================================================

# Net biophysical energy content of each farm output (MJ per physical unit):
# - milk_other: 3.1 MJ/kg FPCM (IDF Bulletin 520/2022, Section 10.5; Nemecek & Thoma)
# - milk_sheep: 4.6 MJ/kg fresh milk at 7% fat (IPCC 2019 Refinement, Vol 4, Ch 10; AFRC 1993, 1995)
# - milk_goat:  3.0 MJ/kg fresh milk at 3.8% fat (IPCC 2019 Refinement, Vol 4, Ch 10; AFRC 1998)
# - meat_cull:  15.0 MJ/kg live weight for culled mature breeding animals (IDF Bulletin 520/2022)
# - meat_young: 11.0 MJ/kg live weight for slaughter cohorts & bred heifers (IDF Bulletin 520/2022)
# - wool:       24.0 MJ/kg greasy wool before scouring (IPCC 2019 Refinement, Vol 4, Ch 10, Eq. 10.20; AFRC 1995)
# - egg:        6.0 MJ/kg fresh eggs (FAO LEAP Poultry Guidelines / GLEAM v3.0 default)
herdr_energy_coef <- list(
  milk_sheep = 4.6,  # MJ/kg fresh sheep milk
  milk_goat  = 3.0,  # MJ/kg fresh goat milk
  milk_other = 3.1,  # MJ/kg FPCM (cattle, buffalo, etc.)
  meat_cull  = 15.0, # MJ/kg live weight, mature (culled) animals
  meat_young = 11.0, # MJ/kg live weight, slaughter cohorts
  wool       = 24.0, # MJ/kg greasy wool
  egg        = 6.0   # MJ/kg fresh egg
)

# Intensity vector returned when a product has no valid allocation or denominator.
herdr_empty_intensity <- c(ghg_s1 = NA_real_, ghg_c2f = NA_real_, land = NA_real_, water = NA_real_)

#' Resolve GWP100 factors from a preset name or custom named vector.
#' @param gwp_input Character preset ("AR4", "AR5", "AR6", "SAR") or named numeric c(CH4=, N2O=).
#' @return List with `choice` (label) and `values` (named numeric CH4/N2O factors).
#' @noRd
resolve_gwp <- function(gwp_input) {
  gwp_presets <- list(
    AR6 = c(CH4 = 27, N2O = 273),
    AR5 = c(CH4 = 28, N2O = 265),
    AR4 = c(CH4 = 25, N2O = 298),
    SAR = c(CH4 = 21, N2O = 310)
  )

  if (is.character(gwp_input)) {
    choice <- toupper(gwp_input[1])
    if (!choice %in% names(gwp_presets)) {
      stop(sprintf(
        "Invalid GWP report '%s'. Must be one of: %s, or a named vector c(CH4 = ..., N2O = ...)",
        gwp_input[1], paste(names(gwp_presets), collapse = ", ")
      ))
    }
    return(list(choice = choice, values = gwp_presets[[choice]]))
  }
  if (is.numeric(gwp_input) && all(c("CH4", "N2O") %in% names(gwp_input))) {
    return(list(choice = "Custom", values = gwp_input))
  }
  stop("Invalid 'gwp_report'. Must be a string ('AR6', 'AR5', 'AR4', 'SAR') or a named numeric vector c(CH4 = ..., N2O = ...)")
}

#' Read the authoritative production_role of each cohort from the definition files.
#' @param data_dir Directory containing ruminant_definitions.csv / monogastric_definitions.csv.
#' @return Tibble with farm_id, region, subregion, animal_tag, production_role (lower case).
#' @noRd
load_production_roles <- function(data_dir) {
  role_cols <- c("farm_id", "region", "subregion", "animal_tag", "production_role")
  roles_list <- list()
  for (def_file in c("ruminant_definitions.csv", "monogastric_definitions.csv")) {
    def_path <- file.path(data_dir, def_file)
    if (!file.exists(def_path)) next
    def_df <- suppressMessages(readr::read_csv(def_path, show_col_types = FALSE))
    if ("production_role" %in% names(def_df)) {
      roles_list[[length(roles_list) + 1]] <- dplyr::select(def_df, dplyr::all_of(role_cols))
    }
  }

  if (length(roles_list) == 0) {
    return(tibble::tibble(
      farm_id = character(), region = character(), subregion = character(),
      animal_tag = character(), production_role = character()
    ))
  }
  dplyr::bind_rows(roles_list) %>%
    dplyr::mutate(production_role = tolower(trimws(as.character(production_role)))) %>%
    dplyr::distinct()
}

#' Biophysical (energy-based) allocation factors for an enterprise.
#'
#' AF_k = Energy_k / Energy_total, where Energy_k = coefficient_k * quantity_k (MJ).
#'
#' @param cur_type Animal type (e.g. "sheep", "goat", "cattle").
#' @param enterprise List of enterprise outputs (kg): milk_fpcm, milk_fresh, egg_kg, wool_kg,
#'   cull_cw, cull_lw, young_cw, young_lw, protein_kg.
#' @return Named list of allocation factors (0-1): af_milk, af_cull, af_young, af_wool, af_egg
#'   (sum to 1) and af_protein (= 1 - af_wool). If no energy is produced, all meat is assigned
#'   by carcass-weight share.
#' @noRd
calc_biophysical_af <- function(cur_type, enterprise) {
  coef <- herdr_energy_coef
  e_milk_coef <- dplyr::case_when(
    cur_type == "sheep" ~ coef$milk_sheep,
    cur_type == "goat" ~ coef$milk_goat,
    TRUE ~ coef$milk_other
  )
  # Small ruminants report fresh milk; all others report FPCM
  milk_qty <- if (cur_type %in% c("sheep", "goat")) enterprise$milk_fresh else enterprise$milk_fpcm

  non_neg <- function(x) max(0, tidyr::replace_na(as.numeric(x), 0))

  # Net biophysical energy produced by each output (MJ)
  e_milk  <- e_milk_coef * non_neg(milk_qty)
  e_cull  <- coef$meat_cull * non_neg(enterprise$cull_lw)
  e_young <- coef$meat_young * non_neg(enterprise$young_lw)
  e_wool  <- coef$wool * non_neg(enterprise$wool_kg)
  e_egg   <- coef$egg * non_neg(enterprise$egg_kg)

  # Master universal denominator
  e_total <- e_milk + e_cull + e_young + e_wool + e_egg

  if (e_total > 0) {
    return(list(
      af_milk    = e_milk / e_total,
      af_cull    = e_cull / e_total,
      af_young   = e_young / e_total,
      af_wool    = e_wool / e_total,
      af_egg     = e_egg / e_total,
      # Edible protein comes from milk, meat and eggs (everything except wool)
      af_protein = (e_milk + e_cull + e_young + e_egg) / e_total
    ))
  }

  # Fallback: no energy-bearing output, so meat takes the whole footprint split by carcass weight
  sys_meat_cw <- enterprise$cull_cw + enterprise$young_cw
  list(
    af_milk  = 0,
    af_cull  = if (sys_meat_cw > 0) enterprise$cull_cw / sys_meat_cw else 0,
    af_young = if (sys_meat_cw > 0) enterprise$young_cw / sys_meat_cw else 0,
    af_wool  = 0,
    af_egg   = 0,
    af_protein = 1
  )
}

#' Allocate the enterprise footprint to one product.
#' @param pool List with s1, c2f (kg CO2eq), land (m2) and water (m3) of the enterprise.
#' @param alloc_factor Allocation factor (0-1) of the product.
#' @param denom Total enterprise (or cohort) production of the product (kg).
#' @return Named numeric vector (ghg_s1, ghg_c2f, land, water) per kg of product, or NAs
#'   if the allocation factor or denominator is missing or not positive.
#' @noRd
calc_intensities <- function(pool, alloc_factor, denom) {
  if (is.na(denom) || denom <= 0 || is.na(alloc_factor) || alloc_factor <= 0) {
    return(herdr_empty_intensity)
  }
  c(
    ghg_s1  = round((pool$s1 * alloc_factor) / denom, 3),
    ghg_c2f = round((pool$c2f * alloc_factor) / denom, 3),
    land    = round((pool$land * alloc_factor) / denom, 3),
    water   = round((pool$water * alloc_factor) / denom, 3)
  )
}

#' Product intensities and protein FCR for one cohort (one row of an enterprise).
#'
#' Allocation by production role:
#' \itemize{
#'   \item mature: milk (af_milk), eggs (af_egg), wool (af_wool), cull meat (af_cull).
#'   \item slaughter: young meat (af_young), wool (af_wool).
#'   \item mature and slaughter: edible protein (af_protein = all products except wool),
#'     divided by the enterprise total protein, so every protein-producing cohort of an
#'     enterprise gets the same protein intensity.
#'   \item other roles (e.g. replacement): no commercial intensities.
#' }
#'
#' @param row_i One-row data frame of the cohort (from `cohort_summary`).
#' @param af_sys Enterprise allocation factors from [calc_biophysical_af()].
#' @param pool_farm Enterprise footprint pool (see [calc_intensities()]).
#' @param enterprise Enterprise output totals (see [calc_biophysical_af()]).
#' @return One-row tibble with outputs, intensities and Protein FCR for the cohort.
#' @noRd
calc_cohort_intensities <- function(row_i, af_sys, pool_farm, enterprise) {
  role_i <- row_i$production_role
  pop_i <- row_i$population

  # Default outputs and intensities
  milk_fpcm_i <- 0
  meat_cw_i <- 0
  egg_kg_i <- 0
  wool_kg_i <- 0
  prot_kg_i <- 0

  int_milk <- int_meat <- int_egg <- int_wool <- int_prot <- herdr_empty_intensity
  fcr_tot <- NA_real_
  fcr_crop <- NA_real_

  if (pop_i > 0) {
    if (role_i %in% c("mature", "slaughter")) {
      prot_kg_i <- row_i$total_protein_kg
      if (prot_kg_i > 0) {
        # Protein FCR (Mottet et al. 2017): kg feed protein per kg edible protein produced
        fcr_tot <- round(row_i$feed_CP_total_kg / prot_kg_i, 2)
        fcr_crop <- round(row_i$feed_CP_cropland_kg / prot_kg_i, 2)
        # Same rule for every cohort: protein share of the footprint / enterprise protein
        int_prot <- calc_intensities(pool_farm, af_sys$af_protein, enterprise$protein_kg)
      }
    }

    if (role_i == "mature") {
      # Commercial outputs from mature breeding animals
      milk_fpcm_i <- row_i$milk_FPCM_kg
      meat_cw_i <- row_i$meat_carcass_weight_kg
      egg_kg_i <- row_i$egg_fresh_kg
      wool_kg_i <- row_i$wool_kg

      # Product-driven intensities (homogeneous across all species)
      if (milk_fpcm_i > 0) int_milk <- calc_intensities(pool_farm, af_sys$af_milk, enterprise$milk_fpcm)
      if (egg_kg_i > 0) int_egg <- calc_intensities(pool_farm, af_sys$af_egg, enterprise$egg_kg)
      if (wool_kg_i > 0) int_wool <- calc_intensities(pool_farm, af_sys$af_wool, enterprise$wool_kg)
      if (meat_cw_i > 0) int_meat <- calc_intensities(pool_farm, af_sys$af_cull, enterprise$cull_cw)
    } else if (role_i == "slaughter") {
      meat_cw_i <- row_i$meat_carcass_weight_kg
      wool_kg_i <- row_i$wool_kg

      if (meat_cw_i > 0) int_meat <- calc_intensities(pool_farm, af_sys$af_young, enterprise$young_cw)
      if (wool_kg_i > 0) int_wool <- calc_intensities(pool_farm, af_sys$af_wool, enterprise$wool_kg)
    }
  }

  tibble::tibble(
    animal_tag                  = row_i$animal_tag,
    production_role             = role_i,
    population                  = pop_i,

    # Commercial & Nutritional Product Outputs
    milk_FPCM_kg                = milk_fpcm_i,
    meat_carcass_weight_kg      = meat_cw_i,
    egg_fresh_kg                = egg_kg_i,
    wool_kg                     = wool_kg_i,
    total_protein_kg            = prot_kg_i,

    # Product Intensities: Milk (per kg FPCM)
    GHG_intensity_milk          = int_milk[["ghg_s1"]],
    GHG_cradle_to_farm_milk     = int_milk[["ghg_c2f"]],
    Land_intensity_milk         = int_milk[["land"]],
    Water_intensity_milk        = int_milk[["water"]],

    # Product Intensities: Meat (per kg carcass weight)
    GHG_intensity_meat          = int_meat[["ghg_s1"]],
    GHG_cradle_to_farm_meat     = int_meat[["ghg_c2f"]],
    Land_intensity_meat         = int_meat[["land"]],
    Water_intensity_meat        = int_meat[["water"]],

    # Product Intensities: Eggs (per kg fresh eggs)
    GHG_intensity_egg           = int_egg[["ghg_s1"]],
    GHG_cradle_to_farm_egg      = int_egg[["ghg_c2f"]],
    Land_intensity_egg          = int_egg[["land"]],
    Water_intensity_egg         = int_egg[["water"]],

    # Product Intensities: Wool (per kg greasy wool)
    GHG_intensity_wool          = int_wool[["ghg_s1"]],
    GHG_cradle_to_farm_wool     = int_wool[["ghg_c2f"]],
    Land_intensity_wool         = int_wool[["land"]],
    Water_intensity_wool        = int_wool[["water"]],

    # Nutritional Intensities: Edible Protein (per kg edible protein)
    GHG_intensity_protein       = int_prot[["ghg_s1"]],
    GHG_cradle_to_farm_protein  = int_prot[["ghg_c2f"]],
    Land_intensity_protein      = int_prot[["land"]],
    Water_intensity_protein     = int_prot[["water"]],

    # Protein Feed Conversion Ratio (Mottet et al. 2017)
    Protein_FCR_total           = fcr_tot,
    Protein_FCR_cropland        = fcr_crop,
    .orig_row_id                = row_i$.orig_row_id
  )
}
