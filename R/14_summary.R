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

  # 2. Consolidation
  complete_summary <- list(CH4_ent, CH4_man, N2O_dir, N2O_vol, N2O_lea, land_u) %>%
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
      CO2eq_Total_Gg     = CO2eq_enteric + CO2eq_manure + CO2eq_N2O)

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
