#' Calculate Total Animal Population
#'
#' Orchestrates the animal population calculation for the farm. Provides two distinct modes:
#' \itemize{
#'   \item \strong{Manual Census (\code{automatic_cycle = FALSE})}: Directly uses the animal counts
#'         provided by the user in \code{livestock_census.csv}. Allows arbitrary custom categories
#'         as long as they are declared in the definitions and parameter tables.
#'   \item \strong{Automatic Farm Cycle (\code{automatic_cycle = TRUE})}: Models the complete herd
#'         or flock structure (offspring, replacements, fattening/slaughter cohorts) automatically
#'         from mature breeding animals using biological reproduction and replacement dynamics.
#' }
#'
#' @section Automatic Demographic Modeling:
#' When \code{automatic_cycle = TRUE}, the model requires standard mature categories (e.g.,
#' \code{mature_dairy_cattle}, \code{mature_beef_cattle}, \code{mature_sheep_female_dairy},
#' \code{mature_goat_female_dairy}, \code{breeder_sows}, \code{breeder_meat_hens}, \code{laying_hens}).
#' All generated offspring automatically inherit the \code{region} and \code{subregion} of their
#' parent cohorts, ensuring seamless downstream integration with diets, weights, and manure management.
#'
#' @param automatic_cycle Logical. If \code{TRUE}, runs the built-in biological demographic model
#'   for each species. Default is \code{FALSE}.
#' @param saveoutput Logical. If \code{TRUE}, exports the final population dataset to
#'   \code{output/population_result.csv}. Default is \code{TRUE}.
#'
#' @return A tibble containing the complete animal population structure with columns:
#'   \code{region}, \code{subregion}, \code{animal_tag}, \code{class_flex},
#'   \code{animal_type}, \code{animal_subtype}, and \code{population}.
#' @export
calculate_population <- function(automatic_cycle = FALSE, saveoutput = TRUE) {

  message("\U0001F7E2 Calculating Total Population...")

  # =========================================================================
  # Step 1: Load Input Tables Safely
  # =========================================================================
  census_raw <- readr::read_csv(
    "user_data/livestock_census.csv",
    col_types = readr::cols(subregion = "c", class_flex = "c"),
    show_col_types = FALSE
  )

  definitions_ruminant <- readr::read_csv(
    "user_data/ruminant_definitions.csv",
    col_types = readr::cols(subregion = "c", class_flex = "c"),
    show_col_types = FALSE
  )

  definitions_monogastric <- readr::read_csv(
    "user_data/monogastric_definitions.csv",
    col_types = readr::cols(subregion = "c", class_flex = "c"),
    show_col_types = FALSE
  )

  rate_parameters <- if (file.exists("user_data/reproduction_parameters.csv")) {
    readr::read_csv("user_data/reproduction_parameters.csv", show_col_types = FALSE)
  } else {
    tibble::tibble(animal_tag = character(), parameter = character(), value = numeric())
  }

  # =========================================================================
  # Step 2: Validate Data Integrity and Unify Definitions
  # =========================================================================
  if (any(census_raw$population < 0, na.rm = TRUE)) {
    stop("Critical Error: Negative population values found in 'livestock_census.csv'.")
  }

  unified_definitions <- dplyr::bind_rows(definitions_ruminant, definitions_monogastric)

  census_base <- census_raw %>%
    dplyr::left_join(
      unified_definitions %>% dplyr::select(animal_tag, region, subregion, class_flex, animal_type),
      by = c("animal_tag", "region", "subregion", "class_flex")
    )

  # =========================================================================
  # Step 3: Handle Demographic Modeling Dispatch
  # =========================================================================
  if (!automatic_cycle) {
    # Manual Mode: pass through census data as declared by the user
    final_pop_raw <- census_base

  } else {
    # Automatic Mode: check base mature presence and dispatch by species
    message(" -> Mode: Automatic. Processing Species Helpers...")

    present_types  <- tolower(unique(census_base$animal_type[!is.na(census_base$animal_type)]))
    required_bases <- c()
    if ("cattle" %in% present_types) required_bases <- c(required_bases, "mature_dairy_cattle", "mature_beef_cattle", "mature_beef_bull")
    if ("sheep"  %in% present_types) required_bases <- c(required_bases, "mature_sheep_female_dairy", "mature_sheep_female_meat")
    if ("goat"   %in% present_types) required_bases <- c(required_bases, "mature_goat_female_dairy", "mature_goat_female_meat")
    if ("swine"  %in% present_types) required_bases <- c(required_bases, "breeder_sows")

    present_tags <- census_raw$animal_tag[census_raw$population > 0]
    missing_tags <- setdiff(required_bases, present_tags)

    if (length(missing_tags) > 0) {
      message("\u26A0  Watch out! You have removed or set to zero these mature animals: ",
              paste(missing_tags, collapse = ", "),
              ". Their respective kids/offspring will NOT appear in the results.")
    }

    results_list <- list()

    # 3a. Cattle demographic modeling
    df_cattle <- census_base %>% dplyr::filter(tolower(animal_type) == "cattle")
    if (nrow(df_cattle) > 0) {
      message("\U0001F403 Calculating populations for CATTLE...")
      results_list$cattle <- calculate_population_cattle(df_cattle, rate_parameters, definitions_ruminant)
    }

    # 3b. Sheep demographic modeling
    df_sheep <- census_base %>% dplyr::filter(tolower(animal_type) == "sheep")
    if (nrow(df_sheep) > 0) {
      message("\U0001F411 Calculating populations for SHEEP...")
      results_list$sheep <- calculate_population_sheep(df_sheep, rate_parameters, definitions_ruminant)
    }

    # 3c. Goat demographic modeling
    df_goat <- census_base %>% dplyr::filter(tolower(animal_type) == "goat")
    if (nrow(df_goat) > 0) {
      message("\U0001F410 Calculating populations for GOAT...")
      results_list$goat <- calculate_population_goat(df_goat, rate_parameters, definitions_ruminant)
    }

    # 3d. Swine demographic modeling
    df_swine <- census_base %>% dplyr::filter(tolower(animal_type) == "swine")
    if (nrow(df_swine) > 0) {
      message("\U0001F416 Calculating populations for SWINE...")
      results_list$swine <- calculate_population_swine(df_swine, rate_parameters, definitions_monogastric)
    }

    # 3e. Poultry demographic modeling
    df_poultry <- census_base %>% dplyr::filter(tolower(animal_type) == "poultry")
    if (nrow(df_poultry) > 0) {
      message("\U0001F426 Calculating populations for POULTRY...")
      results_list$poultry <- calculate_population_poultry(df_poultry, rate_parameters, definitions_monogastric)
    }

    if (length(results_list) == 0) return(tibble::tibble())

    final_pop_raw <- dplyr::bind_rows(results_list)
  }

  # =========================================================================
  # Step 4: Consolidate Metadata (animal_type, animal_subtype)
  # =========================================================================
  # Canonical definitions guarantee consistent metadata for auto-modeled cohorts
  canonical_metadata <- tibble::tribble(
    ~animal_tag, ~animal_type, ~animal_subtype,
    # Cattle
    "mature_dairy_cattle", "cattle", "dairy",
    "mature_beef_cattle", "cattle", "beef",
    "mature_beef_bull", "cattle", "beef",
    "dairy_calves_female_replacement", "cattle", "dairy",
    "dairy_yearlings_female_replacement", "cattle", "dairy",
    "feedlot_calves_male", "cattle", "dairy",
    "feedlot_calves_female", "cattle", "dairy",
    "beef_calves_male", "cattle", "beef",
    "beef_calves_female", "cattle", "beef",
    "beef_calves_male_replacement", "cattle", "beef",
    "beef_calves_female_replacement", "cattle", "beef",
    "beef_yearlings_male_replacement", "cattle", "beef",
    "beef_yearlings_female_replacement", "cattle", "beef",
    # Sheep
    "mature_sheep_female_dairy", "sheep", "dairy",
    "mature_sheep_female_meat", "sheep", "meat",
    "mature_sheep_male_dairy", "sheep", "dairy",
    "mature_sheep_male_meat", "sheep", "meat",
    "lamb_female_dairy_replacement", "sheep", "dairy",
    "lamb_male_dairy_replacement", "sheep", "dairy",
    "lamb_female_meat_replacement", "sheep", "meat",
    "lamb_male_meat_replacement", "sheep", "meat",
    "lamb_dairy_slaughter", "sheep", "dairy",
    "lamb_meat_slaughter", "sheep", "meat",
    # Goat
    "mature_goat_female_dairy", "goat", "dairy",
    "mature_goat_female_meat", "goat", "meat",
    "mature_goat_male_dairy", "goat", "dairy",
    "mature_goat_male_meat", "goat", "meat",
    "kid_goat_female_dairy_replacement", "goat", "dairy",
    "kid_goat_male_dairy_replacement", "goat", "dairy",
    "kid_goat_female_meat_replacement", "goat", "meat",
    "kid_goat_male_meat_replacement", "goat", "meat",
    "kid_goat_dairy_slaughter", "goat", "dairy",
    "kid_goat_meat_slaughter", "goat", "meat",
    # Swine
    "breeder_sows", "swine", "breeder",
    "boars", "swine", "breeder",
    "replacement_sows", "swine", "replacement",
    "fattening_pigs", "swine", "fattening",
    # Poultry
    "breeder_meat_hens", "poultry", "meat",
    "replacement_meat_pullets", "poultry", "meat",
    "broilers", "poultry", "meat",
    "laying_hens", "poultry", "layer",
    "replacement_layer_pullets", "poultry", "layer"
  )

  defs_metadata <- unified_definitions %>%
    dplyr::select(animal_tag, animal_type, animal_subtype) %>%
    dplyr::filter(!is.na(animal_type)) %>%
    dplyr::distinct()

  all_metadata <- dplyr::bind_rows(defs_metadata, canonical_metadata) %>%
    dplyr::distinct(animal_tag, .keep_all = TRUE)

  final_result <- final_pop_raw %>%
    dplyr::select(-dplyr::any_of(c("animal_type", "animal_subtype"))) %>%
    dplyr::left_join(all_metadata, by = "animal_tag") %>%
    dplyr::select(region, subregion, animal_tag, class_flex, animal_type, animal_subtype, population) %>%
    dplyr::filter(population > 0)

  # =========================================================================
  # Step 5: Export and Return
  # =========================================================================
  if (saveoutput) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(final_result, "output/population_result.csv")
    message("\U0001F4BE Results saved to output/population_result.csv")
  }

  return(final_result)
}
