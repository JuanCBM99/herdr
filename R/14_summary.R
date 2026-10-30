#' Summarize CH4, N2O Emissions, Land Use, and Farm Product Intensities
#'
#' @description Integrates all upstream Tier 2 GHG emissions, agroecological land footprints,
#' and GLEAM production outputs. Generates two distinct reports: an absolute environmental inventory (totals)
#' and farm-level product, nutritional, and per-head environmental intensities.
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
#'   \item{totals}{Absolute environmental inventory of emissions, feed, and land use.}
#'   \item{intensities}{Farm-level product, nutritional, and per-head environmental intensities.}
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
      stop(sprintf(
        "Invalid GWP report '%s'. Must be one of: %s, or a named vector c(CH4 = ..., N2O = ...)",
        effective_gwp[1], paste(names(gwp_presets), collapse = ", ")
      ))
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

  message("[herdr] Starting impact assessment summary (GWP standard: ", gwp_choice, ")...")
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

  neg_data <- suppressMessages(calculate_NEg(saveoutput = FALSE, data_dir = data_dir))
  N2O_dir_raw <- calculate_N2O_direct_manure(
    automatic_cycle = automatic_cycle, saveoutput = FALSE, data_dir = data_dir,
    ge_df = ge_data, cp_df = cp_data, pop_df = pop_data, neg_df = neg_data, dmi_df = dmi_data
  )

  N2O_dir <- N2O_dir_raw %>%
    dplyr::group_by(across(all_of(join_keys))) %>%
    dplyr::summarise(N2O_direct_Gg = sum(direct_N2O_kgyear, na.rm = TRUE) / 1e6, .groups = "drop")

  N2O_vol <- calculate_N2O_indirect_volatilization(
    automatic_cycle = automatic_cycle, saveoutput = FALSE, data_dir = data_dir,
    direct_N2O_df = N2O_dir_raw, pop_df = pop_data
  ) %>%
    dplyr::group_by(across(all_of(join_keys))) %>%
    dplyr::summarise(N2O_vol_Gg = sum(N2O_vol_kgyear, na.rm = TRUE) / 1e6, .groups = "drop")

  N2O_lea <- calculate_N2O_indirect_leaching(
    automatic_cycle = automatic_cycle, saveoutput = FALSE, data_dir = data_dir,
    direct_N2O_df = N2O_dir_raw, pop_df = pop_data
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
      Land_other_m2 = pmax(0, sum(total_land_use_m2, na.rm = TRUE) - (
        sum(total_land_use_m2[!is.na(land_type) & land_type == "cropland"], na.rm = TRUE) +
          sum(total_land_use_m2[!is.na(land_type) & land_type == "grassland_convertible"], na.rm = TRUE) +
          sum(total_land_use_m2[!is.na(land_type) & land_type == "grassland_unconvertible"], na.rm = TRUE)
      )),
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
      feed_intake_fresh_kg = sum(feed_fresh_kg_year, na.rm = TRUE),
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

  # Dry matter intake (DMI) and feed consumption
  dmi_summary <- dmi_data %>%
    dplyr::group_by(across(all_of(join_keys))) %>%
    dplyr::summarise(DMI_kgday = mean(DMI_kgday, na.rm = TRUE), .groups = "drop")

  # Production outputs
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
      fat_content_pct = mean(fat_content_pct, na.rm = TRUE),
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
    dplyr::select(-DMI_kgday, -fat_content_pct, -CP_pct, -CP_cropland_pct)

  # Authoritative production_role from definitions
  rum_def_path <- file.path(data_dir, "ruminant_definitions.csv")
  mono_def_path <- file.path(data_dir, "monogastric_definitions.csv")
  roles_list <- list()
  if (file.exists(rum_def_path)) {
    r_df <- suppressMessages(readr::read_csv(rum_def_path, show_col_types = FALSE))
    if ("production_role" %in% names(r_df)) {
      roles_list[[length(roles_list) + 1]] <- r_df %>%
        dplyr::select(farm_id, region, subregion, animal_tag, production_role)
    }
  }
  if (file.exists(mono_def_path)) {
    m_df <- suppressMessages(readr::read_csv(mono_def_path, show_col_types = FALSE))
    if ("production_role" %in% names(m_df)) {
      roles_list[[length(roles_list) + 1]] <- m_df %>%
        dplyr::select(farm_id, region, subregion, animal_tag, production_role)
    }
  }
  role_lookup <- if (length(roles_list) > 0) {
    dplyr::bind_rows(roles_list) %>%
      dplyr::mutate(production_role = tolower(trimws(as.character(production_role)))) %>%
      dplyr::distinct()
  } else {
    tibble::tibble(farm_id = character(), region = character(), subregion = character(), animal_tag = character(), production_role = character())
  }

  complete_summary <- list(CH4_ent, CH4_man, N2O_dir, N2O_vol, N2O_lea, land_u, feed_emiss, prod_data) %>%
    purrr::reduce(dplyr::full_join, by = join_keys) %>%
    dplyr::mutate(across(dplyr::where(is.numeric), ~ tidyr::replace_na(., 0))) %>%
    dplyr::mutate(
      animal_subtype = trimws(as.character(animal_subtype))
    )

  if (nrow(role_lookup) > 0) {
    complete_summary <- complete_summary %>%
      dplyr::select(-dplyr::any_of("production_role")) %>%
      dplyr::left_join(role_lookup, by = c("farm_id", "region", "subregion", "animal_tag")) %>%
      dplyr::mutate(production_role = tidyr::replace_na(production_role, "mature"))
  } else {
    complete_summary <- complete_summary %>%
      dplyr::mutate(production_role = tidyr::replace_na(production_role, "mature"))
  }

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
  if (group_by_identification) {
    totals_df <- cohort_summary %>%
      dplyr::select(
        farm_id, region, subregion, animal_tag, animal_type, animal_subtype, production_role,
        population, DMI_kgday, feed_intake_kg, feed_CP_total_kg, feed_CP_cropland_kg,
        CH4_enteric_Gg, CH4_manure_Gg, N2O_direct_Gg, N2O_vol_Gg, N2O_lea_Gg, N2O_indirect_Gg,
        CO2eq_enteric, CO2eq_manure, CO2eq_N2O_direct, CO2eq_N2O_indirect, CO2eq_N2O,
        CO2eq_feed_fossil_Gg, CO2eq_feed_luc_Gg, CO2eq_feed_Gg,
        CO2eq_Total_Gg, CO2eq_cradle_to_farm_Gg,
        Land_cropland_m2, Land_grassland_convertible_m2, Land_grassland_unconvertible_m2, Land_other_m2, Land_m2,
        Water_feed_m3,
        CO2eq_Total_head, CO2eq_cradle_to_farm_head, Land_head, Water_head
      )
  } else {
    totals_df <- cohort_summary %>%
      dplyr::mutate(.orig_row_id = dplyr::row_number()) %>%
      dplyr::group_by(farm_id, region, subregion, animal_type, animal_subtype) %>%
      dplyr::summarise(
        .orig_row_id = min(.orig_row_id, na.rm = TRUE),
        population = sum(population, na.rm = TRUE),
        feed_intake_kg = sum(feed_intake_kg, na.rm = TRUE),
        feed_CP_total_kg = sum(feed_CP_total_kg, na.rm = TRUE),
        feed_CP_cropland_kg = sum(feed_CP_cropland_kg, na.rm = TRUE),
        CH4_enteric_Gg = sum(CH4_enteric_Gg, na.rm = TRUE),
        CH4_manure_Gg = sum(CH4_manure_Gg, na.rm = TRUE),
        N2O_direct_Gg = sum(N2O_direct_Gg, na.rm = TRUE),
        N2O_vol_Gg = sum(N2O_vol_Gg, na.rm = TRUE),
        N2O_lea_Gg = sum(N2O_lea_Gg, na.rm = TRUE),
        N2O_indirect_Gg = sum(N2O_indirect_Gg, na.rm = TRUE),
        CO2eq_enteric = sum(CO2eq_enteric, na.rm = TRUE),
        CO2eq_manure = sum(CO2eq_manure, na.rm = TRUE),
        CO2eq_N2O_direct = sum(CO2eq_N2O_direct, na.rm = TRUE),
        CO2eq_N2O_indirect = sum(CO2eq_N2O_indirect, na.rm = TRUE),
        CO2eq_N2O = sum(CO2eq_N2O, na.rm = TRUE),
        CO2eq_feed_fossil_Gg = sum(CO2eq_feed_fossil_Gg, na.rm = TRUE),
        CO2eq_feed_luc_Gg = sum(CO2eq_feed_luc_Gg, na.rm = TRUE),
        CO2eq_feed_Gg = sum(CO2eq_feed_Gg, na.rm = TRUE),
        CO2eq_Total_Gg = sum(CO2eq_Total_Gg, na.rm = TRUE),
        CO2eq_cradle_to_farm_Gg = sum(CO2eq_cradle_to_farm_Gg, na.rm = TRUE),
        Land_cropland_m2 = sum(Land_cropland_m2, na.rm = TRUE),
        Land_grassland_convertible_m2 = sum(Land_grassland_convertible_m2, na.rm = TRUE),
        Land_grassland_unconvertible_m2 = sum(Land_grassland_unconvertible_m2, na.rm = TRUE),
        Land_other_m2 = sum(Land_other_m2, na.rm = TRUE),
        Land_m2 = sum(Land_m2, na.rm = TRUE),
        Water_feed_m3 = sum(Water_feed_m3, na.rm = TRUE),
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
  # All cohorts listed: producing cohorts attribute enterprise footprints
  # (mature dairy attributes mature + replacement herd; slaughter attributes meat)
  # ==============================================================================
  intensities_df <- cohort_summary %>%
    dplyr::mutate(.orig_row_id = dplyr::row_number()) %>%
    dplyr::group_by(farm_id, region, subregion, animal_type, animal_subtype) %>%
    dplyr::group_modify(~ {
      df_enterprise <- .x
      cur_type <- as.character(.y$animal_type)
      cur_subtype <- tolower(trimws(as.character(.y$animal_subtype)))

      is_dairy <- cur_subtype == "dairy"
      is_layer <- cur_subtype == "layer" || (cur_type == "poultry" && cur_subtype != "meat" && any(df_enterprise$egg_fresh_kg > 0))
      is_meat <- !is_dairy && !is_layer

      # Enterprise breeding herd totals (mature + replacement cohorts)
      breed_idx <- df_enterprise$production_role %in% c("mature", "replacement")
      repl_idx <- df_enterprise$production_role == "replacement"
      mat_idx <- df_enterprise$production_role == "mature"
      slau_idx <- df_enterprise$production_role == "slaughter"
      has_breeding <- any(breed_idx & df_enterprise$population > 0)
      breed_s1 <- sum(df_enterprise$CO2eq_Total_Gg[breed_idx], na.rm = TRUE) * 1e6 # kg CO2e
      breed_c2f <- sum(df_enterprise$CO2eq_cradle_to_farm_Gg[breed_idx], na.rm = TRUE) * 1e6
      breed_land <- sum(df_enterprise$Land_m2[breed_idx], na.rm = TRUE)
      breed_water <- sum(df_enterprise$Water_feed_m3[breed_idx], na.rm = TRUE)

      repl_s1 <- sum(df_enterprise$CO2eq_Total_Gg[repl_idx], na.rm = TRUE) * 1e6
      repl_c2f <- sum(df_enterprise$CO2eq_cradle_to_farm_Gg[repl_idx], na.rm = TRUE) * 1e6
      repl_land <- sum(df_enterprise$Land_m2[repl_idx], na.rm = TRUE)
      repl_water <- sum(df_enterprise$Water_feed_m3[repl_idx], na.rm = TRUE)
      tot_mat_pop <- sum(df_enterprise$population[mat_idx], na.rm = TRUE)
      tot_slau_cw <- sum(df_enterprise$meat_carcass_weight_kg[slau_idx], na.rm = TRUE)
      tot_slau_pop <- sum(df_enterprise$population[slau_idx], na.rm = TRUE)
      n_dairy_milk_cohorts <- sum(mat_idx & df_enterprise$milk_FPCM_kg > 0, na.rm = TRUE)
      n_slau_cohorts <- sum(slau_idx & df_enterprise$population > 0, na.rm = TRUE)

      # Mature breeding outputs across this enterprise
      tot_milk_fpcm <- sum(df_enterprise$milk_FPCM_kg[breed_idx], na.rm = TRUE)
      tot_milk_fresh <- sum(df_enterprise$milk_fresh_kg[breed_idx], na.rm = TRUE)
      tot_cull_cw <- sum(df_enterprise$meat_carcass_weight_kg[breed_idx], na.rm = TRUE)
      tot_cull_lw <- sum(df_enterprise$meat_live_weight_kg[breed_idx], na.rm = TRUE)
      tot_egg_kg <- sum(df_enterprise$egg_fresh_kg[breed_idx], na.rm = TRUE)
      tot_wool_kg <- sum(df_enterprise$wool_kg[breed_idx], na.rm = TRUE)
      tot_breed_prot <- sum(df_enterprise$total_protein_kg[breed_idx], na.rm = TRUE)

      # Integrated meat enterprise totals (mature + replacement + slaughter)
      sys_s1 <- sum(df_enterprise$CO2eq_Total_Gg, na.rm = TRUE) * 1e6
      sys_c2f <- sum(df_enterprise$CO2eq_cradle_to_farm_Gg, na.rm = TRUE) * 1e6
      sys_land <- sum(df_enterprise$Land_m2, na.rm = TRUE)
      sys_water <- sum(df_enterprise$Water_feed_m3, na.rm = TRUE)
      sys_meat_cw <- sum(df_enterprise$meat_carcass_weight_kg, na.rm = TRUE)
      sys_meat_lw <- sum(df_enterprise$meat_live_weight_kg, na.rm = TRUE)
      sys_wool_kg <- sum(df_enterprise$wool_kg, na.rm = TRUE)
      sys_prot_kg <- sum(df_enterprise$total_protein_kg, na.rm = TRUE)

      if (cur_type == "sheep" && sys_wool_kg > 0 && sys_meat_lw > 0) {
        ewool_sys <- 24.0 * sys_wool_kg
        emeat_sys <- 11.0 * sys_meat_lw
        etot_sys <- ewool_sys + emeat_sys
        af_wool_sys <- round(ewool_sys / etot_sys, 4)
        af_meat_sys <- round(emeat_sys / etot_sys, 4)
      } else {
        af_wool_sys <- 0.0
        af_meat_sys <- 1.0
      }

      # Biophysical allocation factors for dairy breeding enterprise (IDF 2022)
      if (is_dairy) {
        energy_milk_mj <- dplyr::case_when(
          cur_type == "sheep" ~ 4.6 * tot_milk_fresh,
          cur_type == "goat" ~ 3.0 * tot_milk_fresh,
          TRUE ~ 3.1 * tot_milk_fpcm
        )
        energy_cull_mj <- 15.0 * tot_cull_lw
        energy_dairy_tot <- energy_milk_mj + energy_cull_mj

        if (energy_dairy_tot > 0 && tot_milk_fpcm > 0) {
          af_milk_dairy <- energy_milk_mj / energy_dairy_tot
          af_cull_dairy <- energy_cull_mj / energy_dairy_tot
        } else {
          af_milk_dairy <- 0.0
          af_cull_dairy <- 0.0
        }
      }

      # Compute row by row for each animal cohort
      n_rows <- nrow(df_enterprise)
      out_list <- vector("list", n_rows)

      for (i in seq_len(n_rows)) {
        row_i <- df_enterprise[i, ]
        role_i <- row_i$production_role
        pop_i <- row_i$population

        # Default outputs and intensities
        milk_fpcm_i <- 0
        meat_cw_i <- 0
        egg_kg_i <- 0
        wool_kg_i <- 0
        prot_kg_i <- 0

        ghg_milk_s1 <- NA_real_
        ghg_milk_c2f <- NA_real_
        land_milk <- NA_real_
        water_milk <- NA_real_

        ghg_meat_s1 <- NA_real_
        ghg_meat_c2f <- NA_real_
        land_meat <- NA_real_
        water_meat <- NA_real_

        ghg_egg_s1 <- NA_real_
        ghg_egg_c2f <- NA_real_
        land_egg <- NA_real_
        water_egg <- NA_real_

        ghg_wool_s1 <- NA_real_
        ghg_wool_c2f <- NA_real_
        land_wool <- NA_real_
        water_wool <- NA_real_

        ghg_prot_s1 <- NA_real_
        ghg_prot_c2f <- NA_real_
        land_prot <- NA_real_
        water_prot <- NA_real_

        fcr_tot <- NA_real_
        fcr_crop <- NA_real_

        if (pop_i > 0) {
          if (role_i %in% c("mature", "slaughter")) {
            prot_kg_i <- row_i$total_protein_kg
            if (prot_kg_i > 0) {
              fcr_tot <- round(row_i$feed_CP_total_kg / prot_kg_i, 2)
              fcr_crop <- round(row_i$feed_CP_cropland_kg / prot_kg_i, 2)
            }
          }

          if (role_i == "mature") {
            if (is_dairy) {
              # Mature dairy: milk and cull meat (desvieje).
              # If multiple milking cohorts exist in the same farm (e.g. stabled vs grazing cows),
              # each milking cohort retains its own direct footprint + proportional share of non-milking breeding/replacement stock.
              milk_fpcm_i <- row_i$milk_FPCM_kg
              meat_cw_i <- row_i$meat_carcass_weight_kg

              if (n_dairy_milk_cohorts > 1 && milk_fpcm_i > 0) {
                non_milk_idx <- breed_idx & (df_enterprise$milk_FPCM_kg == 0)
                nm_s1 <- sum(df_enterprise$CO2eq_Total_Gg[non_milk_idx], na.rm = TRUE) * 1e6
                nm_c2f <- sum(df_enterprise$CO2eq_cradle_to_farm_Gg[non_milk_idx], na.rm = TRUE) * 1e6
                nm_land <- sum(df_enterprise$Land_m2[non_milk_idx], na.rm = TRUE)
                nm_water <- sum(df_enterprise$Water_feed_m3[non_milk_idx], na.rm = TRUE)
                nm_cull_cw <- sum(df_enterprise$meat_carcass_weight_kg[non_milk_idx], na.rm = TRUE)
                nm_cull_lw <- sum(df_enterprise$meat_live_weight_kg[non_milk_idx], na.rm = TRUE)
                nm_prot <- sum(df_enterprise$total_protein_kg[non_milk_idx], na.rm = TRUE)

                tot_milk_pop <- sum(df_enterprise$population[mat_idx & df_enterprise$milk_FPCM_kg > 0], na.rm = TRUE)
                sh_d <- if (tot_milk_pop > 0) pop_i / tot_milk_pop else 0

                d_s1_i <- (row_i$CO2eq_Total_Gg * 1e6) + (nm_s1 * sh_d)
                d_c2f_i <- (row_i$CO2eq_cradle_to_farm_Gg * 1e6) + (nm_c2f * sh_d)
                d_land_i <- row_i$Land_m2 + (nm_land * sh_d)
                d_water_i <- row_i$Water_feed_m3 + (nm_water * sh_d)
                d_cull_cw_i <- meat_cw_i + (nm_cull_cw * sh_d)
                d_cull_lw_i <- row_i$meat_live_weight_kg + (nm_cull_lw * sh_d)
                d_prot_i <- prot_kg_i + (nm_prot * sh_d)

                emilk_i <- dplyr::case_when(
                  cur_type == "sheep" ~ 4.6 * row_i$milk_fresh_kg,
                  cur_type == "goat" ~ 3.0 * row_i$milk_fresh_kg,
                  TRUE ~ 3.1 * milk_fpcm_i
                )
                ecull_i <- 15.0 * d_cull_lw_i
                etot_i <- emilk_i + ecull_i
                af_m_i <- if (etot_i > 0) emilk_i / etot_i else 0
                af_c_i <- if (etot_i > 0) ecull_i / etot_i else 0

                ghg_milk_s1 <- round((d_s1_i * af_m_i) / milk_fpcm_i, 3)
                ghg_milk_c2f <- round((d_c2f_i * af_m_i) / milk_fpcm_i, 3)
                land_milk <- round((d_land_i * af_m_i) / milk_fpcm_i, 3)
                water_milk <- round((d_water_i * af_m_i) / milk_fpcm_i, 3)

                if (d_cull_cw_i > 0) {
                  ghg_meat_s1 <- round((d_s1_i * af_c_i) / d_cull_cw_i, 3)
                  ghg_meat_c2f <- round((d_c2f_i * af_c_i) / d_cull_cw_i, 3)
                  land_meat <- round((d_land_i * af_c_i) / d_cull_cw_i, 3)
                  water_meat <- round((d_water_i * af_c_i) / d_cull_cw_i, 3)
                }
                if (d_prot_i > 0) {
                  ghg_prot_s1 <- round(d_s1_i / d_prot_i, 3)
                  ghg_prot_c2f <- round(d_c2f_i / d_prot_i, 3)
                  land_prot <- round(d_land_i / d_prot_i, 3)
                  water_prot <- round(d_water_i / d_prot_i, 3)
                }
              } else {
                if (tot_milk_fpcm > 0) {
                  ghg_milk_s1 <- round((breed_s1 * af_milk_dairy) / tot_milk_fpcm, 3)
                  ghg_milk_c2f <- round((breed_c2f * af_milk_dairy) / tot_milk_fpcm, 3)
                  land_milk <- round((breed_land * af_milk_dairy) / tot_milk_fpcm, 3)
                  water_milk <- round((breed_water * af_milk_dairy) / tot_milk_fpcm, 3)
                }
                if (tot_cull_cw > 0) {
                  ghg_meat_s1 <- round((breed_s1 * af_cull_dairy) / tot_cull_cw, 3)
                  ghg_meat_c2f <- round((breed_c2f * af_cull_dairy) / tot_cull_cw, 3)
                  land_meat <- round((breed_land * af_cull_dairy) / tot_cull_cw, 3)
                  water_meat <- round((breed_water * af_cull_dairy) / tot_cull_cw, 3)
                }
                if (tot_breed_prot > 0 && prot_kg_i > 0) {
                  ghg_prot_s1 <- round(breed_s1 / tot_breed_prot, 3)
                  ghg_prot_c2f <- round(breed_c2f / tot_breed_prot, 3)
                  land_prot <- round(breed_land / tot_breed_prot, 3)
                  water_prot <- round(breed_water / tot_breed_prot, 3)
                }
              }
            } else if (is_layer) {
              # Mature layer: each laying cohort retains its own direct footprint + proportional share of replacement pullets
              egg_kg_i <- row_i$egg_fresh_kg
              meat_cw_i <- row_i$meat_carcass_weight_kg
              repl_share_i <- if (tot_mat_pop > 0) pop_i / tot_mat_pop else 0

              layer_s1_i <- (row_i$CO2eq_Total_Gg * 1e6) + (repl_s1 * repl_share_i)
              layer_c2f_i <- (row_i$CO2eq_cradle_to_farm_Gg * 1e6) + (repl_c2f * repl_share_i)
              layer_land_i <- row_i$Land_m2 + (repl_land * repl_share_i)
              layer_water_i <- row_i$Water_feed_m3 + (repl_water * repl_share_i)

              if (egg_kg_i > 0) {
                ghg_egg_s1 <- round(layer_s1_i / egg_kg_i, 3)
                ghg_egg_c2f <- round(layer_c2f_i / egg_kg_i, 3)
                land_egg <- round(layer_land_i / egg_kg_i, 3)
                water_egg <- round(layer_water_i / egg_kg_i, 3)
              }
              if (prot_kg_i > 0) {
                ghg_prot_s1 <- round(layer_s1_i / prot_kg_i, 3)
                ghg_prot_c2f <- round(layer_c2f_i / prot_kg_i, 3)
                land_prot <- round(layer_land_i / prot_kg_i, 3)
                water_prot <- round(layer_water_i / prot_kg_i, 3)
              }
            } else {
              # Mature meat breeding stock (suckler cows, breeder sows, breeder hens, meat ewes/does):
              # Integrates entire meat enterprise (mature + replacement + slaughter) over total meat & wool
              meat_cw_i <- row_i$meat_carcass_weight_kg
              wool_kg_i <- row_i$wool_kg

              if (sys_meat_cw > 0 && meat_cw_i > 0) {
                ghg_meat_s1 <- round((sys_s1 * af_meat_sys) / sys_meat_cw, 3)
                ghg_meat_c2f <- round((sys_c2f * af_meat_sys) / sys_meat_cw, 3)
                land_meat <- round((sys_land * af_meat_sys) / sys_meat_cw, 3)
                water_meat <- round((sys_water * af_meat_sys) / sys_meat_cw, 3)
              }
              if (sys_wool_kg > 0 && af_wool_sys > 0 && wool_kg_i > 0) {
                ghg_wool_s1 <- round((sys_s1 * af_wool_sys) / sys_wool_kg, 3)
                ghg_wool_c2f <- round((sys_c2f * af_wool_sys) / sys_wool_kg, 3)
                land_wool <- round((sys_land * af_wool_sys) / sys_wool_kg, 3)
                water_wool <- round((sys_water * af_wool_sys) / sys_wool_kg, 3)
              }
              if (sys_prot_kg > 0 && prot_kg_i > 0) {
                ghg_prot_s1 <- round((sys_s1 * af_meat_sys) / sys_prot_kg, 3)
                ghg_prot_c2f <- round((sys_c2f * af_meat_sys) / sys_prot_kg, 3)
                land_prot <- round((sys_land * af_meat_sys) / sys_prot_kg, 3)
                water_prot <- round((sys_water * af_meat_sys) / sys_prot_kg, 3)
              }
            }
          } else if (role_i == "slaughter") {
            meat_cw_i <- row_i$meat_carcass_weight_kg
            wool_kg_i <- row_i$wool_kg

            if (is_meat && has_breeding) {
              # Integrated meat enterprise (breeding herd + fattening cohorts):
              # Each slaughter cohort retains its own direct fattening footprint + proportional share of breeding herd (mature + replacement)
              slau_share_i <- if (tot_slau_cw > 0) {
                meat_cw_i / tot_slau_cw
              } else if (tot_slau_pop > 0) {
                pop_i / tot_slau_pop
              } else {
                1.0
              }

              slau_s1_i <- (row_i$CO2eq_Total_Gg * 1e6) + (breed_s1 * slau_share_i)
              slau_c2f_i <- (row_i$CO2eq_cradle_to_farm_Gg * 1e6) + (breed_c2f * slau_share_i)
              slau_land_i <- row_i$Land_m2 + (breed_land * slau_share_i)
              slau_water_i <- row_i$Water_feed_m3 + (breed_water * slau_share_i)

              slau_cw_tot_i <- meat_cw_i + (tot_cull_cw * slau_share_i)
              slau_lw_tot_i <- row_i$meat_live_weight_kg + (tot_cull_lw * slau_share_i)
              slau_wool_tot_i <- wool_kg_i + (tot_wool_kg * slau_share_i)
              slau_prot_tot_i <- prot_kg_i + (tot_breed_prot * slau_share_i)

              if (cur_type == "sheep" && slau_wool_tot_i > 0 && slau_lw_tot_i > 0) {
                ew_i <- 24.0 * slau_wool_tot_i
                em_i <- 11.0 * slau_lw_tot_i
                et_i <- ew_i + em_i
                af_wool_slau <- round(ew_i / et_i, 4)
                af_meat_slau <- round(em_i / et_i, 4)
              } else {
                af_wool_slau <- 0.0
                af_meat_slau <- 1.0
              }

              if (slau_cw_tot_i > 0 && meat_cw_i > 0) {
                ghg_meat_s1 <- round((slau_s1_i * af_meat_slau) / slau_cw_tot_i, 3)
                ghg_meat_c2f <- round((slau_c2f_i * af_meat_slau) / slau_cw_tot_i, 3)
                land_meat <- round((slau_land_i * af_meat_slau) / slau_cw_tot_i, 3)
                water_meat <- round((slau_water_i * af_meat_slau) / slau_cw_tot_i, 3)
              }
              if (slau_wool_tot_i > 0 && af_wool_slau > 0 && wool_kg_i > 0) {
                ghg_wool_s1 <- round((slau_s1_i * af_wool_slau) / slau_wool_tot_i, 3)
                ghg_wool_c2f <- round((slau_c2f_i * af_wool_slau) / slau_wool_tot_i, 3)
                land_wool <- round((slau_land_i * af_wool_slau) / slau_wool_tot_i, 3)
                water_wool <- round((slau_water_i * af_wool_slau) / slau_wool_tot_i, 3)
              }
              if (slau_prot_tot_i > 0 && prot_kg_i > 0) {
                ghg_prot_s1 <- round((slau_s1_i * af_meat_slau) / slau_prot_tot_i, 3)
                ghg_prot_c2f <- round((slau_c2f_i * af_meat_slau) / slau_prot_tot_i, 3)
                land_prot <- round((slau_land_i * af_meat_slau) / slau_prot_tot_i, 3)
                water_prot <- round((slau_water_i * af_meat_slau) / slau_prot_tot_i, 3)
              }
            } else {
              # Pure fattening / feedlot (no breeding herd) or dairy/layer surplus calves/lambs:
              # Evaluated on its own direct fattening footprint
              s1_i <- row_i$CO2eq_Total_Gg * 1e6
              c2f_i <- row_i$CO2eq_cradle_to_farm_Gg * 1e6
              land_i <- row_i$Land_m2
              water_i <- row_i$Water_feed_m3

              if (cur_type == "sheep" && wool_kg_i > 0 && row_i$meat_live_weight_kg > 0) {
                ewool <- 24.0 * wool_kg_i
                emeat <- 11.0 * row_i$meat_live_weight_kg
                etot <- ewool + emeat
                af_wool_i <- round(ewool / etot, 4)
                af_meat_i <- round(emeat / etot, 4)
              } else {
                af_wool_i <- 0.0
                af_meat_i <- 1.0
              }

              if (meat_cw_i > 0) {
                ghg_meat_s1 <- round((s1_i * af_meat_i) / meat_cw_i, 3)
                ghg_meat_c2f <- round((c2f_i * af_meat_i) / meat_cw_i, 3)
                land_meat <- round((land_i * af_meat_i) / meat_cw_i, 3)
                water_meat <- round((water_i * af_meat_i) / meat_cw_i, 3)
              }
              if (wool_kg_i > 0 && af_wool_i > 0) {
                ghg_wool_s1 <- round((s1_i * af_wool_i) / wool_kg_i, 3)
                ghg_wool_c2f <- round((c2f_i * af_wool_i) / wool_kg_i, 3)
                land_wool <- round((land_i * af_wool_i) / wool_kg_i, 3)
                water_wool <- round((water_i * af_wool_i) / wool_kg_i, 3)
              }
              if (prot_kg_i > 0) {
                ghg_prot_s1 <- round((s1_i * af_meat_i) / prot_kg_i, 3)
                ghg_prot_c2f <- round((c2f_i * af_meat_i) / prot_kg_i, 3)
                land_prot <- round((land_i * af_meat_i) / prot_kg_i, 3)
                water_prot <- round((water_i * af_meat_i) / prot_kg_i, 3)
              }
            }
          } else {
            # Replacement cohorts: non-commercial rearing stock, intensities NA
          }
        }

        out_list[[i]] <- tibble::tibble(
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
          GHG_intensity_milk          = ghg_milk_s1,
          GHG_cradle_to_farm_milk     = ghg_milk_c2f,
          Land_intensity_milk         = land_milk,
          Water_intensity_milk        = water_milk,

          # Product Intensities: Meat (per kg carcass weight)
          GHG_intensity_meat          = ghg_meat_s1,
          GHG_cradle_to_farm_meat     = ghg_meat_c2f,
          Land_intensity_meat         = land_meat,
          Water_intensity_meat        = water_meat,

          # Product Intensities: Eggs (per kg fresh eggs)
          GHG_intensity_egg           = ghg_egg_s1,
          GHG_cradle_to_farm_egg      = ghg_egg_c2f,
          Land_intensity_egg          = land_egg,
          Water_intensity_egg         = water_egg,

          # Product Intensities: Wool (per kg greasy wool)
          GHG_intensity_wool          = ghg_wool_s1,
          GHG_cradle_to_farm_wool     = ghg_wool_c2f,
          Land_intensity_wool         = land_wool,
          Water_intensity_wool        = water_wool,

          # Nutritional Intensities: Edible Protein (per kg edible protein)
          GHG_intensity_protein       = ghg_prot_s1,
          GHG_cradle_to_farm_protein  = ghg_prot_c2f,
          Land_intensity_protein      = land_prot,
          Water_intensity_protein     = water_prot,

          # Protein Feed Conversion Ratio (Mottet et al. 2017)
          Protein_FCR_total           = fcr_tot,
          Protein_FCR_cropland        = fcr_crop,
          .orig_row_id                = row_i$.orig_row_id
        )
      }

      dplyr::bind_rows(out_list)
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
