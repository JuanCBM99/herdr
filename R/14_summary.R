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

  # Here we pass the country and year parameters to calculate_land_use
  land_u  <- calculate_land_use(
    automatic_cycle = automatic_cycle,
    saveoutput = FALSE,
    farm_country = farm_country,
    year = year,
    data_dir = data_dir
  ) %>%
    dplyr::group_by(across(all_of(join_keys))) %>%
    dplyr::summarise(Land_m2 = sum(total_land_use_m2, na.rm = TRUE), .groups = "drop")

  # Production outputs for functional units
  prod_data <- suppressMessages(calculate_production(automatic_cycle = automatic_cycle, saveoutput = FALSE, data_dir = data_dir)) %>%
    dplyr::group_by(across(all_of(join_keys))) %>%
    dplyr::summarise(
      population             = sum(population, na.rm = TRUE),
      milk_FPCM_kg           = sum(milk_FPCM_kg, na.rm = TRUE),
      meat_carcass_weight_kg = sum(meat_carcass_weight_kg, na.rm = TRUE),
      egg_fresh_kg           = sum(egg_fresh_kg, na.rm = TRUE),
      total_protein_kg       = sum(total_protein_kg, na.rm = TRUE),
      .groups = "drop"
    )

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
      dplyr::summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop")
  }

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

  # 1) Per kg of Edible Protein (Universal nutritional FU)
  prot_kg <- final_summary$total_protein_kg
  ghg_protein <- dplyr::if_else(prot_kg > 0, round(co2e_total_kg / prot_kg, 3), NA_real_)
  land_protein <- dplyr::if_else(prot_kg > 0, round(land_total_m2 / prot_kg, 2), NA_real_)

  # 2) Per Animal Head / Year (Per capita physiological FU)
  pop_n <- final_summary$population
  ghg_head <- dplyr::if_else(pop_n > 0, round(co2e_total_kg / pop_n, 2), NA_real_)
  land_head <- dplyr::if_else(pop_n > 0, round(land_total_m2 / pop_n, 1), NA_real_)

  # 3) Per Commercial Product (IDF biophysical allocation for dairy, 100% for meat/egg)
  milk_kg <- final_summary$milk_FPCM_kg
  meat_kg <- final_summary$meat_carcass_weight_kg
  egg_kg  <- final_summary$egg_fresh_kg

  af_milk <- dplyr::case_when(
    milk_kg > 0 & meat_kg > 0 ~ pmax(0.5, pmin(1.0, 1 - 5.7717 * (meat_kg / milk_kg))),
    milk_kg > 0 ~ 1.0,
    TRUE ~ 0.0
  )

  primary_prod <- dplyr::case_when(
    milk_kg > 0 ~ "kg FPCM Milk",
    egg_kg > 0 ~ "kg Fresh Eggs",
    meat_kg > 0 ~ "kg Carcass Meat",
    TRUE ~ "None"
  )

  prod_yield_kg <- dplyr::case_when(
    milk_kg > 0 ~ milk_kg,
    egg_kg > 0 ~ egg_kg,
    meat_kg > 0 ~ meat_kg,
    TRUE ~ 0.0
  )

  af_prod <- dplyr::case_when(
    milk_kg > 0 ~ af_milk,
    prod_yield_kg > 0 ~ 1.0,
    TRUE ~ 0.0
  )

  ghg_product <- dplyr::if_else(prod_yield_kg > 0, round((co2e_total_kg * af_prod) / prod_yield_kg, 3), NA_real_)
  land_product <- dplyr::if_else(prod_yield_kg > 0, round((land_total_m2 * af_prod) / prod_yield_kg, 2), NA_real_)

  final_summary <- final_summary %>%
    dplyr::mutate(
      primary_product        = primary_prod,
      GHG_intensity_protein  = ghg_protein,
      Land_intensity_protein = land_protein,
      GHG_intensity_head     = ghg_head,
      Land_intensity_head    = land_head,
      GHG_intensity_product  = ghg_product,
      Land_intensity_product = land_product
    )

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
