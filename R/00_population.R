#' Calculate total animal population (Refactored)
#'
#' Orchestrates the population calculation by loading census data and dispatching
#' sub-calculations for each animal type (cattle, sheep, goat).
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @export
calculate_population <- function(saveoutput = TRUE) {

  message("\U0001f7e2 Calculating Total Population...")

  # --- 1. Data Loading ---
  census_base <- load_dataset("census")
  rate_parameters <- load_dataset("rate_parameters")
  categories <- load_dataset("categories")

  # Minimum validation to prevent cascading errors
  stopifnot("animal_type" %in% names(census_base))

  # --- 2. Dispatcher ---
  # Create a list to store partial results
  results_list <- list()

  # A) CATTLE
  # Requires special logic (passes 'categories' to handle empty/NA zones)
  df_cattle <- census_base %>% dplyr::filter(tolower(animal_type) == "cattle")
  if (nrow(df_cattle) > 0) {
    message(" -> Processing Cattle...")
    results_list$cattle <- calculate_population_cattle(
      census_cattle = df_cattle,
      rate_parameters = rate_parameters,
      categories = categories
    )
  }

  # B) SHEEP
  df_sheep <- census_base %>% dplyr::filter(tolower(animal_type) == "sheep")
  if (nrow(df_sheep) > 0) {
    message(" -> Processing Sheep...")
    results_list$sheep <- calculate_population_sheep(
      census_sheep = df_sheep,
      rate_parameters = rate_parameters
    )
  }

  # C) GOATS
  df_goat <- census_base %>% dplyr::filter(tolower(animal_type) == "goat")
  if (nrow(df_goat) > 0) {
    message(" -> Processing Goats...")
    results_list$goat <- calculate_population_goat(
      census_goat = df_goat,
      rate_parameters = rate_parameters
    )
  }

  # --- 3. Unification and Metadata ---
  message(" -> Combining and attaching metadata...")

  if (length(results_list) == 0) {
    warning("\u26a0 No data found for any known animal type.")
    return(tibble::tibble())
  }

  final_result <- dplyr::bind_rows(results_list) %>%
    # Join with categories to retrieve descriptive info (subtype, etc.)
    # Use the 'identification' column as the unique key
    dplyr::left_join(
      categories %>% dplyr::select(identification, animal_type, animal_subtype),
      by = "identification"
    ) %>%
    # Final column selection and order
    dplyr::select(
      group, zone, identification, animal_type, animal_subtype, population
    ) %>%
    # Final cleanup: ensure population is numeric and order
    dplyr::mutate(population = as.numeric(population)) %>%
    dplyr::arrange(group, zone, identification)

  # --- 4. Save Output ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(final_result, "output/population_result.csv")
    message("\U1F4BE Saved output to output/population_result.csv")
  }

  return(final_result)
}
