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
#' @export
generate_impact_assessment <- function(automatic_cycle = FALSE,
                                       region = NULL, subregion = NULL,
                                       animal = NULL, type = NULL, class_flex = NULL,
                                       saveoutput = TRUE, group_by_identification = TRUE) {

  message("\U0001f4be Starting impact assessment summary...")
  join_keys <- c("region", "subregion", "animal_tag", "class_flex", "animal_type", "animal_subtype")

  # 1. Pipeline calls and unit standardization (Gg)
  ch4_ent <- calculate_emissions_enteric(automatic_cycle = automatic_cycle, saveoutput = FALSE) %>%
    dplyr::group_by(across(all_of(join_keys))) %>%
    dplyr::summarise(CH4_enteric_Gg = sum(emissions_total, na.rm = TRUE), .groups = "drop")

  ch4_man <- calculate_CH4_manure(automatic_cycle = automatic_cycle, saveoutput = FALSE) %>%
    dplyr::group_by(across(all_of(join_keys))) %>%
    dplyr::summarise(CH4_manure_Gg = sum(Emissions_CH4_Gg_year / 1e6, na.rm = TRUE), .groups = "drop")

  n2o_dir <- calculate_N2O_direct_manure(automatic_cycle = automatic_cycle, saveoutput = FALSE) %>%
    dplyr::group_by(across(all_of(join_keys))) %>%
    dplyr::summarise(N2O_direct_Gg = sum(N2O_emissions, na.rm = TRUE) / 1e6, .groups = "drop") # Convierte kg a Gg

  n2o_vol <- calculate_N2O_indirect_volatilization(automatic_cycle = automatic_cycle, saveoutput = FALSE) %>%
    dplyr::group_by(across(all_of(join_keys))) %>%
    dplyr::summarise(N2O_vol_Gg = sum(n2o_g, na.rm = TRUE) / 1e6, .groups = "drop") # Convierte kg a Gg

  n2o_lea <- calculate_N2O_indirect_leaching(automatic_cycle = automatic_cycle, saveoutput = FALSE) %>%
    dplyr::group_by(across(all_of(join_keys))) %>%
    dplyr::summarise(N2O_lea_Gg = sum(n2o_l, na.rm = TRUE) / 1e6, .groups = "drop") # Convierte kg a Gg

  land_u  <- calculate_land_use(automatic_cycle = automatic_cycle, saveoutput = FALSE) %>%
    dplyr::group_by(across(all_of(join_keys))) %>%
    dplyr::summarise(Land_m2 = sum(Land_use_Total_m2, na.rm = TRUE), .groups = "drop")

  # 2. Consolidation
  complete_summary <- list(ch4_ent, ch4_man, n2o_dir, n2o_vol, n2o_lea, land_u) %>%
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

  # 5. Calculate CO2eq and Carbon Footprint (AR5 GWP: CH4=28, N2O=265)
  final_summary <- final_summary %>%
    dplyr::mutate(
      CO2eq_enteric  = CH4_enteric_Gg * 28,
      CO2eq_manure   = CH4_manure_Gg * 28,
      CO2eq_n2o      = (N2O_direct_Gg + N2O_vol_Gg + N2O_lea_Gg) * 265,
      CO2eq_Total_Gg = CO2eq_enteric + CO2eq_manure + CO2eq_n2o,

      # Intensity: kg CO2eq per m2
      # (Gg * 1e6 converts to kg)
      Carbon_Footprint_m2 = (CO2eq_Total_Gg * 1e6) / Land_m2
    )

  # 6. Save Output
  if (saveoutput) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(final_summary, "output/impact_assessment_summary.csv")
    message("\U1F4BE Report saved to output/impact_assessment_summary.csv")
  }

  return(final_summary)
}
