#' Calculate Total Animal Population
#'
#' Orchestrates the population calculation. Choose between manual census
#' or automatic farm cycle models based on mature animal counts.
#'
#' @param automatic_cycle Logical. If TRUE, uses species-specific birth models.
#' @param saveoutput If TRUE, saves results to the output folder.
#' @export
calculate_population <- function(automatic_cycle = FALSE, saveoutput = TRUE) {

  message("\U0001f7e2 Calculating Total Population...")

  # --- 1. Load Data ---
  census_raw      <- readr::read_csv("user_data/livestock_census.csv", show_col_types = FALSE)
  definitions     <- readr::read_csv("user_data/livestock_definitions.csv", show_col_types = FALSE)
  rate_parameters <- readr::read_csv("user_data/reproduction_parameters.csv", show_col_types = FALSE)

  # --- 2. Quick Integrity Check ---
  if (any(census_raw$population < 0, na.rm = TRUE)) {
    stop("Critical Error: Negative population values found in 'livestock_census.csv'.")
  }

  # --- 3. Automatic Cycle Validation ---
  if (automatic_cycle) {
    # Define the required mature base tags for the 3 helper functions
    required_bases <- c("mature_dairy_cattle", "mature_beef_cattle", "mature_beef_bull",
                        "mature_sheep_female_dairy", "mature_sheep_female_meat",
                        "mature_goat_female_dairy", "mature_goat_female_meat")

    present_tags <- census_raw$animal_tag[census_raw$population > 0]
    missing_tags <- setdiff(required_bases, present_tags)

    if (length(missing_tags) > 0) {
      message("\u26A0  Watch out! You have removed or set to zero these mature animals: ",
              paste(missing_tags, collapse = ", "),
              ". Their respective kids/offspring will NOT appear in the results.")
    }
  }

  # --- 4. Processing ---
  census_base <- census_raw %>%
    dplyr::left_join(
      definitions %>% dplyr::select(animal_tag, region, subregion, class_flex, animal_type),
      by = c("animal_tag", "region", "subregion", "class_flex")
    )

  if (!automatic_cycle) {
    final_pop_raw <- census_base
  } else {
    message(" -> Mode: Automatic. Processing Species Helpers...")
    results_list <- list()

    # Cattle
    df_cattle <- census_base %>% dplyr::filter(tolower(animal_type) == "cattle")
    if (nrow(df_cattle) > 0) {
      results_list$cattle <- calculate_population_cattle(df_cattle, rate_parameters, definitions)
    }

    # Sheep
    df_sheep <- census_base %>% dplyr::filter(tolower(animal_type) == "sheep")
    if (nrow(df_sheep) > 0) {
      results_list$sheep <- calculate_population_sheep(df_sheep, rate_parameters)
    }

    # Goat
    df_goat <- census_base %>% dplyr::filter(tolower(animal_type) == "goat")
    if (nrow(df_goat) > 0) {
      results_list$goat <- calculate_population_goat(df_goat, rate_parameters)
    }

    if (length(results_list) == 0) return(tibble::tibble())

    final_pop_raw <- results_list %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(across(any_of(c("region", "subregion", "class_flex")), as.character))
  }

  # --- 5. Final Assembly ---
  final_result <- final_pop_raw %>%
    dplyr::select(-dplyr::any_of(c("animal_type", "animal_subtype"))) %>%
    dplyr::left_join(
      definitions %>% dplyr::select(animal_tag, animal_type, animal_subtype) %>% dplyr::distinct(),
      by = "animal_tag"
    ) %>%
    dplyr::select(region, subregion, animal_tag, class_flex, animal_type, animal_subtype, population)

  if (saveoutput) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(final_result, "output/population_result.csv")
    message("\U0001f4be Results saved to output/population_result.csv")
  }

  return(final_result)
}
