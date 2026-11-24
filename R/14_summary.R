#' Summarize CH4, N2O Emissions, Land Use, and Farm Product Intensities
#'
#' @description Integrates all upstream Tier 2 GHG emissions, agroecological land footprints,
#' and GLEAM production outputs. Generates two distinct reports: an absolute environmental inventory (totals)
#' and farm-level product, nutritional, and per-head environmental intensities.
#'
#' Product intensities and Protein FCR (Table 2) are obtained by allocating the whole enterprise pool
#' (GHG, land, water, feed protein) among its co-products using a biophysical (energy-based) allocation
#' factor: \code{AF_k = Energy_k / Energy_total}. Specifically, \code{Protein_FCR_total} divides whole-herd
#' total feed crude protein by edible animal protein (allocated by \code{af_protein}),
#' \code{Protein_FCR_human_edible} weights feed crude protein by the explicit \code{human_edible} column in
#' \code{mapping.csv} (default binary \code{0/1} classification anchored on arable non-forage concentrates
#' with \code{economic_allocation > 0.33}, excluding inedible/waste items and including commercial food-grade
#' dairy feeds), and \code{EPCR_DIAAS} adjusts both human-edible feed protein (\code{diaas_pct} in
#' \code{mapping.csv}) and animal product protein (Ertl et al., 2016b, Table 5) by their Digestible
#' Indispensable Amino Acid Score (FAO 2013 0.5-3 yr child pattern, untruncated), alongside the
#' Protein Quality Ratio (\code{PQR = DIAAS_output / DIAAS_input}).
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
#'   \item{totals}{Absolute environmental inventory of emissions (Gg CH4, Gg N2O, Gg CO2eq), feed (kg/year, including \code{feed_CP_total_kg}, \code{feed_CP_human_edible_kg}, and \code{feed_HDP_kg}), land (m2) and water (m3).}
#'   \item{intensities}{Farm-level product (per kg FPCM milk, carcass meat, eggs, wool or edible protein) and enterprise-level nutritional intensities (\code{Protein_FCR_total} across all feed CP; \code{Protein_FCR_human_edible} weighted by \code{human_edible} in \code{mapping.csv}; \code{EPCR_DIAAS} adjusting both feed and animal protein by DIAAS under FAO 2013 0.5-3 yr pattern; and \code{PQR = DIAAS_output / DIAAS_input}).}
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
  # 1. Validate inputs & resolve GWP100 factors
  assertthat::assert_that(
    assertthat::is.flag(automatic_cycle), assertthat::is.flag(saveoutput),
    assertthat::is.flag(group_by_identification),
    assertthat::is.string(farm_country), is.numeric(year), length(year) == 1,
    assertthat::is.string(data_dir)
  )

  gwp <- resolve_gwp(if (!is.null(ar)) ar else gwp_report)
  gwp_choice <- gwp$choice
  gwp_vals <- gwp$values

  message("[herdr] Starting impact assessment summary (GWP standard: ", gwp_choice, ")...")

  # 2. Run upstream modules and assemble the cohort-level inventory
  cohort_data <- assemble_cohort_inventory(
    automatic_cycle = automatic_cycle,
    farm_country = farm_country,
    year = year,
    data_dir = data_dir,
    ch4_factor = as.numeric(gwp_vals[["CH4"]]),
    n2o_factor = as.numeric(gwp_vals[["N2O"]])
  )

  # 3. Apply user filters
  if (!is.null(farm_id))   cohort_data <- dplyr::filter(cohort_data, farm_id %in% .env$farm_id)
  if (!is.null(region))    cohort_data <- dplyr::filter(cohort_data, region %in% .env$region)
  if (!is.null(subregion)) cohort_data <- dplyr::filter(cohort_data, subregion %in% .env$subregion)
  if (!is.null(animal))    cohort_data <- dplyr::filter(cohort_data, animal_type %in% .env$animal)
  if (!is.null(type))      cohort_data <- dplyr::filter(cohort_data, animal_subtype %in% .env$type)

  # 4. Build Table 1 (Absolute Totals) & Table 2 (Product Intensities + Protein FCR)
  totals_df <- build_totals_table(cohort_data, group_by_identification = group_by_identification)
  intensities_df <- build_intensities_table(cohort_data)

  # 5. Attach GWP metadata, optionally export CSVs, and return
  attr(totals_df, "gwp_report") <- gwp_choice
  attr(totals_df, "gwp_factors") <- gwp_vals
  attr(intensities_df, "gwp_report") <- gwp_choice
  attr(intensities_df, "gwp_factors") <- gwp_vals

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
# SECTION 1: UPSTREAM INVENTORY ASSEMBLY & TABLE 1 (ABSOLUTE TOTALS)
# ==============================================================================

#' Run upstream herdr modules once and assemble a unified cohort-level inventory.
#' @noRd
assemble_cohort_inventory <- function(automatic_cycle, farm_country, year, data_dir,
                                      ch4_factor, n2o_factor) {
  # Cohort key keeps animal_tag separate until enterprise-level allocation in Table 2
  join_keys <- c("farm_id", "region", "subregion", "animal_tag", "animal_type", "animal_subtype")

  # --- A. Shared base components (computed ONCE in memory) ---
  pop_data <- suppressMessages(calculate_population(automatic_cycle = automatic_cycle, saveoutput = FALSE, data_dir = data_dir))
  cp_data <- suppressMessages(calculate_weighted_variable(saveoutput = FALSE, data_dir = data_dir))
  ge_data <- suppressMessages(calculate_ge(saveoutput = FALSE, data_dir = data_dir, de_df = cp_data))
  mono_data <- suppressMessages(calculate_monogastric_energy(saveoutput = FALSE, data_dir = data_dir))
  dmi_data <- suppressMessages(calculate_DMI(saveoutput = FALSE, data_dir = data_dir, ge_req = ge_data, diet_char = cp_data, mono = mono_data))
  vs_data <- suppressMessages(calculate_vs(saveoutput = FALSE, data_dir = data_dir, ge_data = ge_data, dmi_data = dmi_data, diet_char = cp_data))

  # --- B. Emissions (Scope 1: Enteric CH4, Manure CH4, Direct & Indirect N2O) ---
  CH4_ent <- calculate_emissions_enteric(
    automatic_cycle = automatic_cycle, saveoutput = FALSE, data_dir = data_dir,
    diet_vars = cp_data, ge_df = ge_data, pop_df = pop_data
  ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(join_keys))) %>%
    dplyr::summarise(CH4_enteric_Gg = sum(total_CH4_enteric_Ggyear, na.rm = TRUE), .groups = "drop")

  CH4_man <- calculate_CH4_manure(
    automatic_cycle = automatic_cycle, saveoutput = FALSE, data_dir = data_dir,
    vs_df = vs_data, pop_df = pop_data
  ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(join_keys))) %>%
    dplyr::summarise(CH4_manure_Gg = sum(total_CH4_mm_kgyear / 1e6, na.rm = TRUE), .groups = "drop")

  N2O_dir_raw <- calculate_N2O_direct_manure(
    automatic_cycle = automatic_cycle, saveoutput = FALSE, data_dir = data_dir,
    ge_df = ge_data, cp_df = cp_data, pop_df = pop_data, neg_df = ge_data, dmi_df = dmi_data
  )

  N2O_dir <- N2O_dir_raw %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(join_keys))) %>%
    dplyr::summarise(N2O_direct_Gg = sum(direct_N2O_kgyear, na.rm = TRUE) / 1e6, .groups = "drop")

  ipcc_mm_data <- herdr_read_manure_table(file.path(data_dir, "ipcc_mm.csv"))

  N2O_vol <- herdr_calculate_N2O_indirect(
    mode = "volatilization",
    automatic_cycle = automatic_cycle, saveoutput = FALSE, data_dir = data_dir,
    direct_N2O_df = N2O_dir_raw, pop_df = pop_data, ipcc_master = ipcc_mm_data
  ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(join_keys))) %>%
    dplyr::summarise(N2O_vol_Gg = sum(N2O_vol_kgyear, na.rm = TRUE) / 1e6, .groups = "drop")

  N2O_lea <- herdr_calculate_N2O_indirect(
    mode = "leaching",
    automatic_cycle = automatic_cycle, saveoutput = FALSE, data_dir = data_dir,
    direct_N2O_df = N2O_dir_raw, pop_df = pop_data, ipcc_master = ipcc_mm_data
  ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(join_keys))) %>%
    dplyr::summarise(N2O_lea_Gg = sum(N2O_leach_kgyear, na.rm = TRUE) / 1e6, .groups = "drop")

  # --- C. Upstream Feed Footprints (Land & Feed GHG/Water) ---
  land_u <- calculate_land_use(
    automatic_cycle = automatic_cycle, saveoutput = FALSE,
    farm_country = farm_country, year = year, data_dir = data_dir,
    DMI_df = dmi_data, population_df = pop_data
  ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(join_keys))) %>%
    dplyr::summarise(
      Land_m2 = sum(total_land_use_m2, na.rm = TRUE),
      Land_cropland_m2 = sum(total_land_use_m2[!is.na(land_type) & land_type == "cropland"], na.rm = TRUE),
      Land_grassland_convertible_m2 = sum(total_land_use_m2[!is.na(land_type) & land_type == "grassland_convertible"], na.rm = TRUE),
      Land_grassland_unconvertible_m2 = sum(total_land_use_m2[!is.na(land_type) & land_type == "grassland_unconvertible"], na.rm = TRUE),
      Land_other_m2 = pmax(0, Land_m2 - Land_cropland_m2 - Land_grassland_convertible_m2 - Land_grassland_unconvertible_m2),
      .groups = "drop"
    )

  feed_emiss <- calculate_feed_emissions(
    automatic_cycle = automatic_cycle, saveoutput = FALSE, data_dir = data_dir,
    DMI_df = dmi_data, population_df = pop_data
  ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(join_keys))) %>%
    dplyr::summarise(
      CO2eq_feed_Gg = sum(feed_CO2e_total_kg_year, na.rm = TRUE) / 1e6,
      CO2eq_feed_fossil_Gg = sum(feed_CO2e_fossil_kg_year, na.rm = TRUE) / 1e6,
      CO2eq_feed_luc_Gg = sum(feed_CO2e_luc_kg_year, na.rm = TRUE) / 1e6,
      Water_feed_m3 = sum(feed_water_m3_year, na.rm = TRUE),
      .groups = "drop"
    )

  # --- D. Production & Annual Feed Protein Intake ---
  cp_summary <- cp_data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(join_keys))) %>%
    dplyr::summarise(
      CP_pct = mean(CP_pct, na.rm = TRUE),
      CP_human_edible_pct = mean(CP_human_edible_pct, na.rm = TRUE),
      HDP_feed_pct = mean(HDP_feed_pct, na.rm = FALSE),
      .groups = "drop"
    )

  dmi_summary <- dmi_data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(join_keys))) %>%
    dplyr::summarise(DMI_kgday = mean(DMI_kgday, na.rm = TRUE), .groups = "drop")

  prod_data <- suppressMessages(calculate_production(
    automatic_cycle = automatic_cycle, saveoutput = FALSE, data_dir = data_dir,
    pop_df = pop_data
  )) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(join_keys))) %>%
    dplyr::summarise(
      population = sum(population, na.rm = TRUE),
      milk_fresh_kg = sum(milk_fresh_kg, na.rm = TRUE),
      milk_FPCM_kg = sum(milk_FPCM_kg, na.rm = TRUE),
      meat_live_weight_kg = sum(meat_live_weight_kg, na.rm = TRUE),
      meat_carcass_weight_kg = sum(meat_carcass_weight_kg, na.rm = TRUE),
      egg_fresh_kg = sum(egg_fresh_kg, na.rm = TRUE),
      wool_kg = sum(wool_kg, na.rm = TRUE),
      total_protein_kg = sum(total_protein_kg, na.rm = TRUE),
      total_HDP_kg = sum(total_HDP_kg, na.rm = TRUE),
      production_role = dplyr::first(production_role),
      .groups = "drop"
    ) %>%
    dplyr::left_join(dmi_summary, by = join_keys) %>%
    dplyr::left_join(cp_summary, by = join_keys) %>%
    dplyr::mutate(
      DMI_kgday = tidyr::replace_na(DMI_kgday, 0),
      CP_pct = tidyr::replace_na(CP_pct, 0),
      CP_human_edible_pct = tidyr::replace_na(CP_human_edible_pct, 0),
      feed_intake_kg = population * DMI_kgday * 365,
      feed_CP_total_kg = feed_intake_kg * (CP_pct / 100),
      feed_CP_human_edible_kg = feed_intake_kg * (CP_human_edible_pct / 100),
      feed_HDP_kg = dplyr::if_else(
        feed_CP_human_edible_kg == 0,
        0,
        feed_intake_kg * (HDP_feed_pct / 100)
      )
    ) %>%
    dplyr::select(-DMI_kgday, -CP_pct, -CP_human_edible_pct, -HDP_feed_pct)

  # --- E. Merge all components, attach authoritative roles & preserve census order ---
  cohort_df <- list(CH4_ent, CH4_man, N2O_dir, N2O_vol, N2O_lea, land_u, feed_emiss, prod_data) %>%
    purrr::reduce(dplyr::full_join, by = join_keys) %>%
    dplyr::mutate(
      dplyr::across(dplyr::where(is.numeric) & !dplyr::any_of("feed_HDP_kg"), ~ tidyr::replace_na(., 0)),
      feed_HDP_kg = dplyr::if_else(feed_CP_human_edible_kg == 0 & is.na(feed_HDP_kg), 0, feed_HDP_kg),
      animal_subtype = trimws(as.character(animal_subtype))
    )

  role_lookup <- load_production_roles(data_dir)
  if (nrow(role_lookup) > 0) {
    cohort_df <- cohort_df %>%
      dplyr::select(-dplyr::any_of("production_role")) %>%
      dplyr::left_join(role_lookup, by = c("farm_id", "region", "subregion", "animal_tag"))
  }
  cohort_df <- cohort_df %>%
    dplyr::mutate(production_role = tidyr::replace_na(production_role, "mature"))

  if (nrow(pop_data) > 0) {
    pop_order_df <- pop_data %>%
      dplyr::mutate(.pop_row_id = dplyr::row_number()) %>%
      dplyr::select(farm_id, region, subregion, animal_tag, .pop_row_id) %>%
      dplyr::distinct(farm_id, region, subregion, animal_tag, .keep_all = TRUE)

    cohort_df <- cohort_df %>%
      dplyr::left_join(pop_order_df, by = c("farm_id", "region", "subregion", "animal_tag")) %>%
      dplyr::arrange(.pop_row_id) %>%
      dplyr::select(-.pop_row_id)
  }

  # --- F. Convert gases to CO2eq and compute per-head metrics ---
  cohort_df %>%
    dplyr::mutate(
      N2O_indirect_Gg         = N2O_vol_Gg + N2O_lea_Gg,
      CO2eq_enteric           = CH4_enteric_Gg * ch4_factor,
      CO2eq_manure            = CH4_manure_Gg * ch4_factor,
      CO2eq_N2O_direct        = N2O_direct_Gg * n2o_factor,
      CO2eq_N2O_indirect      = N2O_indirect_Gg * n2o_factor,
      CO2eq_N2O               = CO2eq_N2O_direct + CO2eq_N2O_indirect,
      CO2eq_Total_Gg          = CO2eq_enteric + CO2eq_manure + CO2eq_N2O,
      CO2eq_cradle_to_farm_Gg = CO2eq_Total_Gg + CO2eq_feed_Gg
    ) %>%
    add_per_head_metrics()
}

#' Derive daily DMI and per-head environmental indicators from inventory totals.
#' @noRd
add_per_head_metrics <- function(df) {
  df %>%
    dplyr::mutate(
      DMI_kgday                 = dplyr::if_else(population > 0, round(feed_intake_kg / (population * 365), 3), 0),
      CO2eq_Total_head          = dplyr::if_else(population > 0, round((CO2eq_Total_Gg * 1e6) / population, 2), 0),
      CO2eq_cradle_to_farm_head = dplyr::if_else(population > 0, round((CO2eq_cradle_to_farm_Gg * 1e6) / population, 2), 0),
      Land_head                 = dplyr::if_else(population > 0, round(Land_m2 / population, 2), 0),
      Water_head                = dplyr::if_else(population > 0, round(Water_feed_m3 / population, 2), 0)
    )
}

#' Build Table 1: Absolute environmental & feed inventory (by cohort or enterprise).
#' @noRd
build_totals_table <- function(cohort_df, group_by_identification = TRUE) {
  additive_cols <- c(
    "population", "feed_intake_kg", "feed_CP_total_kg", "feed_CP_human_edible_kg", "feed_HDP_kg",
    "CH4_enteric_Gg", "CH4_manure_Gg", "N2O_direct_Gg", "N2O_vol_Gg", "N2O_lea_Gg", "N2O_indirect_Gg",
    "CO2eq_enteric", "CO2eq_manure", "CO2eq_N2O_direct", "CO2eq_N2O_indirect", "CO2eq_N2O",
    "CO2eq_feed_fossil_Gg", "CO2eq_feed_luc_Gg", "CO2eq_feed_Gg",
    "CO2eq_Total_Gg", "CO2eq_cradle_to_farm_Gg",
    "Land_cropland_m2", "Land_grassland_convertible_m2", "Land_grassland_unconvertible_m2", "Land_other_m2", "Land_m2",
    "Water_feed_m3"
  )

  if (group_by_identification) {
    return(
      cohort_df %>%
        dplyr::select(
          farm_id, region, subregion, animal_tag, animal_type, animal_subtype, production_role,
          population, DMI_kgday, dplyr::all_of(additive_cols[-1]),
          CO2eq_Total_head, CO2eq_cradle_to_farm_head, Land_head, Water_head
        )
    )
  }

  cohort_df %>%
    dplyr::mutate(.orig_row_id = dplyr::row_number()) %>%
    dplyr::group_by(farm_id, region, subregion, animal_type, animal_subtype) %>%
    dplyr::summarise(
      .orig_row_id = min(.orig_row_id, na.rm = TRUE),
      # Compute feed_HDP_kg BEFORE across() overwrites feed_CP_human_edible_kg with its scalar sum
      feed_HDP_kg = if (any(feed_CP_human_edible_kg > 0 & is.na(feed_HDP_kg))) NA_real_ else sum(feed_HDP_kg, na.rm = TRUE),
      dplyr::across(dplyr::all_of(setdiff(additive_cols, "feed_HDP_kg")), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.orig_row_id) %>%
    dplyr::select(-.orig_row_id) %>%
    add_per_head_metrics() %>%
    dplyr::relocate(DMI_kgday, .after = population) %>%
    dplyr::relocate(feed_HDP_kg, .after = feed_CP_human_edible_kg)
}

# ==============================================================================
# SECTION 2: BIOPHYSICAL ALLOCATION & TABLE 2 (INTENSITIES + PROTEIN FCR)
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
  milk_sheep = 4.6,
  milk_goat  = 3.0,
  milk_other = 3.1,
  meat_cull  = 15.0,
  meat_young = 11.0,
  wool       = 24.0,
  egg        = 6.0
)

# Intensity vector returned when a product has no valid allocation or denominator.
herdr_empty_intensity <- c(
  ghg_s1 = NA_real_, ghg_c2f = NA_real_, land = NA_real_, water = NA_real_,
  fcr_tot = NA_real_, fcr_he = NA_real_
)

#' Build Table 2: Enterprise-allocated product intensities and Protein FCR per cohort.
#' @noRd
build_intensities_table <- function(cohort_df) {
  cohort_df %>%
    dplyr::mutate(.orig_row_id = dplyr::row_number()) %>%
    dplyr::group_by(farm_id, region, subregion, animal_type, animal_subtype) %>%
    dplyr::group_modify(~ build_enterprise_intensities(.x, as.character(.y$animal_type))) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.orig_row_id) %>%
    dplyr::select(-.orig_row_id) %>%
    dplyr::relocate(subregion, animal_tag, animal_type, animal_subtype, production_role, population, .after = region)
}

#' Compute enterprise pool, biophysical allocation factors, and assign intensities to cohorts.
#'
#' Allocation rules by production_role:
#' - mature: milk (af_milk), eggs (af_egg), wool (af_wool), cull meat (af_cull).
#' - slaughter: young meat (af_young), wool (af_wool).
#' - mature & slaughter: edible protein intensities (GHG, land, water), Protein FCR
#'   (\code{Protein_FCR_total}, \code{Protein_FCR_human_edible}; Mottet et al. 2017),
#'   and DIAAS-adjusted protein metrics (\code{EPCR_DIAAS}, \code{PQR};
#'   Ertl et al. 2016a, 2016b) use the unified whole-enterprise pool * af_protein.
#' - replacement (or other non-producing roles): commercial outputs = 0, intensities = NA.
#' @noRd
build_enterprise_intensities <- function(df_enterprise, animal_type) {
  n <- nrow(df_enterprise)
  is_mature <- df_enterprise$production_role == "mature"
  is_slaughter <- df_enterprise$production_role == "slaughter"
  has_pop <- df_enterprise$population > 0

  active_mature <- has_pop & is_mature
  active_slaughter <- has_pop & is_slaughter
  active_prod <- active_mature | active_slaughter

  # 1. Whole-enterprise input pool (integrates mature + replacement + slaughter)
  hdp_feed_missing <- any(df_enterprise$feed_CP_human_edible_kg > 0 & is.na(df_enterprise$feed_HDP_kg))
  pool_farm <- list(
    s1       = sum(df_enterprise$CO2eq_Total_Gg, na.rm = TRUE) * 1e6,
    c2f      = sum(df_enterprise$CO2eq_cradle_to_farm_Gg, na.rm = TRUE) * 1e6,
    land     = sum(df_enterprise$Land_m2, na.rm = TRUE),
    water    = sum(df_enterprise$Water_feed_m3, na.rm = TRUE),
    cp_tot   = sum(df_enterprise$feed_CP_total_kg, na.rm = TRUE),
    cp_he    = sum(df_enterprise$feed_CP_human_edible_kg, na.rm = TRUE),
    hdp_feed = if (hdp_feed_missing) NA_real_ else sum(df_enterprise$feed_HDP_kg, na.rm = TRUE)
  )

  # 2. Whole-enterprise product outputs
  enterprise <- list(
    milk_fpcm  = sum(df_enterprise$milk_FPCM_kg[is_mature], na.rm = TRUE),
    milk_fresh = sum(df_enterprise$milk_fresh_kg[is_mature], na.rm = TRUE),
    egg_kg     = sum(df_enterprise$egg_fresh_kg[is_mature], na.rm = TRUE),
    wool_kg    = sum(df_enterprise$wool_kg[is_mature | is_slaughter], na.rm = TRUE),
    cull_cw    = sum(df_enterprise$meat_carcass_weight_kg[is_mature], na.rm = TRUE),
    cull_lw    = sum(df_enterprise$meat_live_weight_kg[is_mature], na.rm = TRUE),
    young_cw   = sum(df_enterprise$meat_carcass_weight_kg[is_slaughter], na.rm = TRUE),
    young_lw   = sum(df_enterprise$meat_live_weight_kg[is_slaughter], na.rm = TRUE),
    protein_kg = sum(df_enterprise$total_protein_kg[is_mature | is_slaughter], na.rm = TRUE),
    hdp_animal = sum(df_enterprise$total_HDP_kg[is_mature | is_slaughter], na.rm = TRUE)
  )

  # 3. Biophysical allocation factors & enterprise intensities (computed ONCE per product)
  af <- calc_biophysical_af(animal_type, enterprise)

  int_milk  <- calc_intensities(pool_farm, af$af_milk,    enterprise$milk_fpcm)
  int_cull  <- calc_intensities(pool_farm, af$af_cull,    enterprise$cull_cw)
  int_young <- calc_intensities(pool_farm, af$af_young,   enterprise$young_cw)
  int_egg   <- calc_intensities(pool_farm, af$af_egg,     enterprise$egg_kg)
  int_wool  <- calc_intensities(pool_farm, af$af_wool,    enterprise$wool_kg)
  int_prot  <- calc_intensities(pool_farm, af$af_protein, enterprise$protein_kg)

  # DIAAS-adjusted Edible Protein Conversion Ratio (EPCR_DIAAS) and Protein Quality Ratio (PQR)
  # Ertl et al. (2016a, 2016b): EPCR_DIAAS = (HDP_feed * af_protein) / HDP_animal; PQR = DIAAS_out / DIAAS_in
  epcr_diaas_val <- if (
    !is.na(pool_farm$hdp_feed) && !is.na(enterprise$hdp_animal) &&
      enterprise$hdp_animal > 0 && !is.na(af$af_protein) && af$af_protein > 0
  ) {
    round((pool_farm$hdp_feed * af$af_protein) / enterprise$hdp_animal, 2)
  } else {
    NA_real_
  }

  pqr_val <- if (
    !is.na(pool_farm$hdp_feed) && pool_farm$hdp_feed > 0 &&
      !is.na(pool_farm$cp_he) && pool_farm$cp_he > 0 &&
      !is.na(enterprise$hdp_animal) && enterprise$hdp_animal > 0 &&
      !is.na(enterprise$protein_kg) && enterprise$protein_kg > 0
  ) {
    diaas_out <- enterprise$hdp_animal / enterprise$protein_kg
    diaas_in  <- pool_farm$hdp_feed / pool_farm$cp_he
    round(diaas_out / diaas_in, 2)
  } else {
    NA_real_
  }

  # 4. Cohort commercial outputs (zero for non-producing roles such as replacement)
  milk_fpcm <- dplyr::if_else(active_mature, df_enterprise$milk_FPCM_kg, 0)
  meat_cw   <- dplyr::if_else(active_prod,   df_enterprise$meat_carcass_weight_kg, 0)
  egg_kg    <- dplyr::if_else(active_mature, df_enterprise$egg_fresh_kg, 0)
  wool_kg   <- dplyr::if_else(active_prod,   df_enterprise$wool_kg, 0)
  prot_kg   <- dplyr::if_else(active_prod,   df_enterprise$total_protein_kg, 0)
  hdp_kg    <- dplyr::if_else(active_prod,   df_enterprise$total_HDP_kg, 0)

  # Helpers to assign enterprise intensities to active cohort rows
  assign_val <- function(cond, val) {
    dplyr::if_else(cond, rep(as.numeric(val), n), NA_real_)
  }
  assign_meat <- function(metric) {
    dplyr::case_when(
      active_mature & meat_cw > 0    ~ as.numeric(int_cull[[metric]]),
      active_slaughter & meat_cw > 0 ~ as.numeric(int_young[[metric]]),
      TRUE                           ~ NA_real_
    )
  }

  tibble::tibble(
    animal_tag                  = df_enterprise$animal_tag,
    production_role             = df_enterprise$production_role,
    population                  = df_enterprise$population,

    # Commercial & Nutritional Product Outputs
    milk_FPCM_kg                = milk_fpcm,
    meat_carcass_weight_kg      = meat_cw,
    egg_fresh_kg                = egg_kg,
    wool_kg                     = wool_kg,
    total_protein_kg            = prot_kg,
    total_HDP_kg                = hdp_kg,

    # Product Intensities: Milk (per kg FPCM / fresh for small ruminants)
    GHG_intensity_milk          = assign_val(milk_fpcm > 0, int_milk[["ghg_s1"]]),
    GHG_cradle_to_farm_milk     = assign_val(milk_fpcm > 0, int_milk[["ghg_c2f"]]),
    Land_intensity_milk         = assign_val(milk_fpcm > 0, int_milk[["land"]]),
    Water_intensity_milk        = assign_val(milk_fpcm > 0, int_milk[["water"]]),

    # Product Intensities: Meat (per kg carcass weight; mature = cull, slaughter = young)
    GHG_intensity_meat          = assign_meat("ghg_s1"),
    GHG_cradle_to_farm_meat     = assign_meat("ghg_c2f"),
    Land_intensity_meat         = assign_meat("land"),
    Water_intensity_meat        = assign_meat("water"),

    # Product Intensities: Eggs (per kg fresh eggs)
    GHG_intensity_egg           = assign_val(egg_kg > 0, int_egg[["ghg_s1"]]),
    GHG_cradle_to_farm_egg      = assign_val(egg_kg > 0, int_egg[["ghg_c2f"]]),
    Land_intensity_egg          = assign_val(egg_kg > 0, int_egg[["land"]]),
    Water_intensity_egg         = assign_val(egg_kg > 0, int_egg[["water"]]),

    # Product Intensities: Wool (per kg greasy wool)
    GHG_intensity_wool          = assign_val(wool_kg > 0, int_wool[["ghg_s1"]]),
    GHG_cradle_to_farm_wool     = assign_val(wool_kg > 0, int_wool[["ghg_c2f"]]),
    Land_intensity_wool         = assign_val(wool_kg > 0, int_wool[["land"]]),
    Water_intensity_wool        = assign_val(wool_kg > 0, int_wool[["water"]]),

    # Nutritional Intensities & Protein FCR (per kg edible protein, whole-enterprise pool)
    GHG_intensity_protein       = assign_val(prot_kg > 0, int_prot[["ghg_s1"]]),
    GHG_cradle_to_farm_protein  = assign_val(prot_kg > 0, int_prot[["ghg_c2f"]]),
    Land_intensity_protein      = assign_val(prot_kg > 0, int_prot[["land"]]),
    Water_intensity_protein     = assign_val(prot_kg > 0, int_prot[["water"]]),
    Protein_FCR_total           = assign_val(prot_kg > 0, int_prot[["fcr_tot"]]),
    Protein_FCR_human_edible    = assign_val(prot_kg > 0, int_prot[["fcr_he"]]),
    EPCR_DIAAS                  = assign_val(prot_kg > 0, epcr_diaas_val),
    PQR                         = assign_val(prot_kg > 0, pqr_val),

    .orig_row_id                = df_enterprise$.orig_row_id
  )
}

#' Biophysical (energy-based) allocation factors for an enterprise.
#'
#' AF_k = Energy_k / Energy_total, where Energy_k = coefficient_k * quantity_k (MJ).
#'
#' @param cur_type Animal type (e.g. "sheep", "goat", "cattle").
#' @param enterprise List of enterprise outputs (kg).
#' @return Named list of allocation factors (0-1): af_milk, af_cull, af_young, af_wool, af_egg
#'   (sum to 1) and af_protein (= 1 - af_wool).
#' @noRd
calc_biophysical_af <- function(cur_type, enterprise) {
  coef <- herdr_energy_coef
  e_milk_coef <- dplyr::case_when(
    cur_type == "sheep" ~ coef$milk_sheep,
    cur_type == "goat"  ~ coef$milk_goat,
    TRUE                ~ coef$milk_other
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
  e_total <- e_milk + e_cull + e_young + e_wool + e_egg

  if (e_total > 0) {
    return(list(
      af_milk    = e_milk / e_total,
      af_cull    = e_cull / e_total,
      af_young   = e_young / e_total,
      af_wool    = e_wool / e_total,
      af_egg     = e_egg / e_total,
      af_protein = (e_milk + e_cull + e_young + e_egg) / e_total
    ))
  }

  # Fallback: no energy-bearing output, split meat footprint by carcass weight
  sys_meat_cw <- enterprise$cull_cw + enterprise$young_cw
  list(
    af_milk    = 0,
    af_cull    = if (sys_meat_cw > 0) enterprise$cull_cw / sys_meat_cw else 0,
    af_young   = if (sys_meat_cw > 0) enterprise$young_cw / sys_meat_cw else 0,
    af_wool    = 0,
    af_egg     = 0,
    af_protein = 1
  )
}

#' Allocate the enterprise footprint and feed-protein pool to one product.
#' @param pool List with s1, c2f (kg CO2eq), land (m2), water (m3), cp_tot and cp_he (kg CP).
#' @param alloc_factor Allocation factor (0-1) of the product.
#' @param denom Total enterprise production of the product (kg).
#' @return Named numeric vector (ghg_s1, ghg_c2f, land, water, fcr_tot, fcr_he) per kg of product.
#' @noRd
calc_intensities <- function(pool, alloc_factor, denom) {
  if (is.na(denom) || denom <= 0 || is.na(alloc_factor) || alloc_factor <= 0) {
    return(herdr_empty_intensity)
  }
  c(
    ghg_s1   = round((pool$s1 * alloc_factor) / denom, 3),
    ghg_c2f  = round((pool$c2f * alloc_factor) / denom, 3),
    land     = round((pool$land * alloc_factor) / denom, 3),
    water    = round((pool$water * alloc_factor) / denom, 3),
    fcr_tot  = round((pool$cp_tot * alloc_factor) / denom, 2),
    fcr_he   = round((pool$cp_he * alloc_factor) / denom, 2)
  )
}

# ==============================================================================
# SECTION 3: UTILITY HELPERS
# ==============================================================================

#' Resolve GWP100 factors from a preset name or custom named vector.
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
