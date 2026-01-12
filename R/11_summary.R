#' Summarize CH₄, N₂O Emissions, and Land Use
#'
#' Calculates a summary of CH₄ and N₂O emissions (direct and indirect) and land use,
#' grouped by code, animal type, subtype, and zone. Optionally allows for
#' aggregation to a more general level.
#'
#' @param group Character/Numeric vector (optional). Groups to filter. Default `NULL`.
#' @param zone Character vector (optional). Zones to filter. Default `NULL`.
#' @param animal Character string (optional). Livestock type (`animal_type`). Default `NULL`.
#' @param type Character string (optional). Livestock subtype (`animal_subtype`). Default `NULL`.
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @param group_by_identification Logical. If `TRUE`, returns results at the `identification` level. If `FALSE`, aggregates all `identification`s.
#' @return A `tibble` with columns for each emission type and land use.
#' @export
generate_impact_assessment <- function(group = NULL, zone = NULL, animal = NULL, type = NULL,
                                       saveoutput = TRUE, group_by_identification = TRUE) {

  message("\U0001f4be Starting impact assessment summary...")

  # --- 1. Data Validation Step ---
  message(" -> 1/3: Validating data consistency (census vs. diet)...")

  # Load data ONLY for validation purposes.
  tryCatch({
    validation_pop <- calculate_population(saveoutput = FALSE)
    validation_diet <- calculate_weighted_variable(saveoutput = FALSE)

    # Call the dedicated validation function
    validate_data(pop_df = validation_pop, diet_df = validation_diet)

  }, error = function(e) {
    # Capture and display error if validate_data() returns a stop()
    stop("Data validation error! Please check your CSV files.\n",
         "Original error: ", e$message)
  })

  message(" -> 2/3: Calculating all emissions and land use.")
  # --- End of Validation ---


  # --- 2. Core Calculation and Aggregation ---

  # Calculate and aggregate CH4 Enteric Emissions
  ch4_enteric <- calculate_emissions_enteric(saveoutput = FALSE) %>%
    dplyr::group_by(group, zone, identification, animal_type, animal_subtype) %>%
    dplyr::summarise(Emissions_CH4_enteric = sum(emissions_total, na.rm = TRUE), .groups = "drop")

  # Calculate and aggregate CH4 Manure Emissions
  ch4_manure <- calculate_CH4_manure(saveoutput = FALSE) %>%
    dplyr::group_by(group, zone, identification, animal_type, animal_subtype) %>%
    dplyr::summarise(Emissions_CH4_manure = sum(Emissions_CH4_Mg_year, na.rm = TRUE), .groups = "drop")

  # Calculate and aggregate Direct N2O Emissions
  direct_n2o <- calculate_N2O_direct_manure(saveoutput = FALSE) %>%
    dplyr::group_by(group, zone, identification, animal_type, animal_subtype) %>%
    dplyr::summarise(Emissions_N2O_direct = sum(N2O_emissions, na.rm = TRUE), .groups = "drop")

  # Calculate and aggregate Indirect N2O (Volatilization)
  n2o_indirect_vol <- calculate_N2O_indirect_volatilization(saveoutput = FALSE) %>%
    dplyr::group_by(group, zone, identification, animal_type, animal_subtype) %>%
    dplyr::summarise(Emissions_N2O_indirect_vol = sum(n2o_g, na.rm = TRUE), .groups = "drop")

  # Calculate and aggregate Indirect N2O (Leaching)
  n2o_indirect_leach <- calculate_N2O_indirect_leaching(saveoutput = FALSE) %>%
    dplyr::group_by(group, zone, identification, animal_type, animal_subtype) %>%
    dplyr::summarise(Emissions_N2O_indirect_leach = sum(N2O_L, na.rm = TRUE), .groups = "drop")

  # Calculate and aggregate Land Use
  land_use <- calculate_land_use(saveoutput = FALSE) %>%
    dplyr::group_by(group, zone, identification, animal_type, animal_subtype) %>%
    dplyr::summarise(Land_use_m2 = sum(Land_use_Total_m2, na.rm = TRUE), .groups = "drop")

  # --- 3. Consolidate and Filter Results ---
  message(" -> 3/3: Consolidating and filtering results.")

  # Combine all results using full_join
  complete_summary <- list(ch4_enteric, ch4_manure, direct_n2o,
                           n2o_indirect_vol, n2o_indirect_leach, land_use) %>%
    purrr::reduce(dplyr::full_join,
                  by = c("group", "zone", "identification", "animal_type", "animal_subtype"),
                  na_matches = "na") %>%
    # Replace any residual NAs with 0
    dplyr::mutate(dplyr::across(where(is.numeric), ~ tidyr::replace_na(., 0)))

  # Filter results based on user arguments (group, zone, animal, type)
  filtered_summary <- complete_summary

  if (!is.null(group)) {
    filtered_summary <- filtered_summary %>%
      dplyr::filter(group %in% .env$group)
  }
  if (!is.null(zone)) {
    if (any(is.na(zone))) {
      filtered_summary <- filtered_summary %>%
        dplyr::filter(zone %in% .env$zone | is.na(zone))
    } else {
      filtered_summary <- filtered_summary %>%
        dplyr::filter(zone %in% .env$zone)
    }
  }
  if (!is.null(animal)) {
    filtered_summary <- filtered_summary %>%
      dplyr::filter(animal_type %in% .env$animal)
  }
  if (!is.null(type)) {
    filtered_summary <- filtered_summary %>%
      dplyr::filter(animal_subtype %in% .env$type)
  }

  # Optional aggregation (if group_by_identification is FALSE)
  if (!group_by_identification) {
    final_summary <- filtered_summary %>%
      dplyr::group_by(group, zone, animal_type, animal_subtype) %>%
      dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop")
  } else {
    final_summary <- filtered_summary
  }

  # Save Final CSV
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    readr::write_csv(final_summary, "output/generate_impact_assessment.csv")
    message("\U0001f4be Saved output to output/generate_impact_assessment.csv")
  }

  message("\u2705Summary completed.")
  return(final_summary)
}
