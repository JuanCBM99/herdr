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
#' \itemize{
#'   \item \code{farm_id}: Farm or production system identifier.
#'   \item \code{region} and \code{subregion}: Strictly geographical location identifiers (e.g., country/province/municipality) that define the spatial unit of the farm. Different housing or management variants within the same geographical farm must be distinguished via \code{animal_tag} (e.g., \code{laying_hens} vs. \code{free_range_laying_hens}), not via \code{subregion}.
#' }
#'
#' @param automatic_cycle Logical. If \code{TRUE}, runs the built-in biological demographic model
#'   for each species. Default is \code{FALSE}.
#' @param saveoutput Logical. If \code{TRUE}, exports the final population dataset to
#'   \code{output/population_result.csv}. Default is \code{TRUE}.
#'
#' @param data_dir Character. Path to the folder containing input CSV files. Default is \code{"user_data"}.
#' @return A tibble containing the complete animal population structure with columns:
#'   \code{farm_id}, \code{region}, \code{subregion}, \code{animal_tag},
#'   \code{animal_type}, \code{animal_subtype}, and \code{population}.
#' @export
calculate_population <- function(automatic_cycle = FALSE, saveoutput = TRUE, data_dir = "user_data") {

  message("Calculating Total Population...")

  # =========================================================================
  # Step 1: Load Input Tables Safely
  # =========================================================================
  census_raw <- readr::read_csv(
    file.path(data_dir, "livestock_census.csv"),
    col_types = readr::cols(farm_id = "c", subregion = "c"),
    show_col_types = FALSE
  )

  rum_file <- file.path(data_dir, "ruminant_definitions.csv")
  definitions_ruminant <- if (file.exists(rum_file)) {
    readr::read_csv(
      rum_file,
      col_types = readr::cols(farm_id = "c", subregion = "c"),
      show_col_types = FALSE
    )
  } else {
    tibble::tibble(farm_id = character(), animal_tag = character(), region = character(), subregion = character(), animal_type = character(), animal_subtype = character())
  }

  if ("replacement_rate" %in% names(definitions_ruminant)) {
    definitions_ruminant$replacement_rate <- suppressWarnings(as.numeric(definitions_ruminant$replacement_rate))
  }

  mono_file <- file.path(data_dir, "monogastric_definitions.csv")
  definitions_monogastric <- if (file.exists(mono_file)) {
    readr::read_csv(
      mono_file,
      col_types = readr::cols(farm_id = "c", subregion = "c"),
      show_col_types = FALSE
    )
  } else {
    tibble::tibble(farm_id = character(), animal_tag = character(), region = character(), subregion = character(), animal_type = character(), animal_subtype = character())
  }
  if ("replacement_rate" %in% names(definitions_monogastric)) {
    definitions_monogastric$replacement_rate <- suppressWarnings(as.numeric(definitions_monogastric$replacement_rate))
  }

  weights_file <- file.path(data_dir, "livestock_weights.csv")
  weights_df <- if (file.exists(weights_file)) {
    readr::read_csv(weights_file, show_col_types = FALSE)
  } else {
    NULL
  }

  # =========================================================================
  # Step 2: Validate Data Integrity and Unify Definitions
  # =========================================================================
  if (any(census_raw$population < 0, na.rm = TRUE)) {
    stop("Critical Error: Negative population values found in 'livestock_census.csv'.")
  }

  def_cols <- c("farm_id", "animal_tag", "region", "subregion", "animal_type", "animal_subtype")
  unified_definitions <- dplyr::bind_rows(
    definitions_ruminant %>% dplyr::select(dplyr::any_of(def_cols)),
    definitions_monogastric %>% dplyr::select(dplyr::any_of(def_cols))
  )

  census_base <- census_raw %>%
    dplyr::left_join(
      unified_definitions %>% dplyr::select(dplyr::any_of(c("farm_id", "animal_tag", "region", "subregion", "animal_type"))),
      by = c("farm_id", "animal_tag", "region", "subregion")
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
      message("[herdr] Notice: Mature breeding animals with zero count or omitted: ",
              paste(missing_tags, collapse = ", "),
              ". Corresponding offspring cohorts will not be modeled.")
    }

    results_list <- list()

    # 3a. Cattle demographic modeling
    df_cattle <- census_base %>% dplyr::filter(tolower(animal_type) == "cattle")
    if (nrow(df_cattle) > 0) {
      message("Calculating populations for CATTLE...")
      results_list$cattle <- calculate_population_cattle(df_cattle, definitions_ruminant, weights = weights_df, data_dir = data_dir)
    }

    # 3b. Sheep demographic modeling
    df_sheep <- census_base %>% dplyr::filter(tolower(animal_type) == "sheep")
    if (nrow(df_sheep) > 0) {
      message("Calculating populations for SHEEP...")
      results_list$sheep <- calculate_population_sheep(df_sheep, definitions_ruminant, weights = weights_df, data_dir = data_dir)
    }

    # 3c. Goat demographic modeling
    df_goat <- census_base %>% dplyr::filter(tolower(animal_type) == "goat")
    if (nrow(df_goat) > 0) {
      message("Calculating populations for GOAT...")
      results_list$goat <- calculate_population_goat(df_goat, definitions_ruminant, weights = weights_df, data_dir = data_dir)
    }

    # 3d. Swine demographic modeling
    df_swine <- census_base %>% dplyr::filter(tolower(animal_type) == "swine")
    if (nrow(df_swine) > 0) {
      message("Calculating populations for SWINE...")
      results_list$swine <- calculate_population_swine(df_swine, definitions_monogastric, weights = weights_df, data_dir = data_dir)
    }

    # 3e. Poultry demographic modeling
    df_poultry <- census_base %>% dplyr::filter(tolower(animal_type) == "poultry")
    if (nrow(df_poultry) > 0) {
      message("Calculating populations for POULTRY...")
      results_list$poultry <- calculate_population_poultry(df_poultry, definitions_monogastric, weights = weights_df, data_dir = data_dir)
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
    "mature_beef_cattle", "cattle", "meat",
    "mature_beef_bull", "cattle", "meat",
    "dairy_calves_female_replacement", "cattle", "dairy",
    "dairy_yearlings_female_replacement", "cattle", "dairy",
    "feedlot_calves_male", "cattle", "dairy",
    "feedlot_calves_female", "cattle", "dairy",
    "beef_calves_male", "cattle", "meat",
    "beef_calves_female", "cattle", "meat",
    "beef_calves_male_replacement", "cattle", "meat",
    "beef_calves_female_replacement", "cattle", "meat",
    "beef_yearlings_male_replacement", "cattle", "meat",
    "beef_yearlings_female_replacement", "cattle", "meat",
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
    "breeder_sows", "swine", "meat",
    "boars", "swine", "meat",
    "replacement_sows", "swine", "meat",
    "fattening_pigs", "swine", "meat",
    # Poultry
    "breeder_meat_hens", "poultry", "meat",
    "replacement_meat_pullets", "poultry", "meat",
    "broilers", "poultry", "meat",
    "laying_hens", "poultry", "layer",
    "free_range_laying_hens", "poultry", "layer",
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
    dplyr::select(farm_id, region, subregion, animal_tag, animal_type, animal_subtype, population) %>%
    dplyr::filter(population > 0)

  # =========================================================================
  # Step 5: Export and Return
  # =========================================================================
  if (saveoutput) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(final_result, "output/population_result.csv")
    message("[herdr] Results saved to output/population_result.csv")
  }

  return(final_result)
}
