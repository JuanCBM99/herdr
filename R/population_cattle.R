#' Calculate Cattle Population Structure
#'
#' Internal helper to model the cattle herd based on mature population and biological rates.
#'
#' @param census_cattle Filtered census data for cattle.
#' @param rate_parameters Reproduction and replacement rates.
#' @param categories Definition table for cross-referencing.
#' @export
calculate_population_cattle <- function(census_cattle, rate_parameters, categories) {

  message("\U0001F9EE Calculating populations for CATTLE...")

  # --- 1. Setup and Rate Retrieval ---
  base_tags <- c("mature_beef_bull", "mature_dairy_cattle", "mature_beef_cattle")

  # Helper to fetch specific parameters safely
  get_rate <- function(param, tag) {
    val <- rate_parameters %>%
      dplyr::filter(parameter == param, animal_tag == tag) %>%
      dplyr::pull(value)
    return(if (length(val) > 0) val[1] else 0)
  }

  rates <- list(
    dairy_calving     = get_rate("calving_rate",     "mature_dairy_cattle"),
    beef_calving      = get_rate("calving_rate",      "mature_beef_cattle"),
    beef_male_repl    = get_rate("replacement_rate", "mature_beef_bull"),
    beef_female_repl  = get_rate("replacement_rate", "mature_beef_cattle"),
    dairy_female_repl = get_rate("replacement_rate", "mature_dairy_cattle")
  )

  # --- 2. Pivot Base Population ---
  # Aggregate by class_flex to ensure productivity phases are preserved
  base_pops_wide <- census_cattle %>%
    dplyr::filter(animal_tag %in% base_tags) %>%
    dplyr::group_by(region, subregion, animal_tag, class_flex) %>%
    dplyr::summarise(population = sum(population, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = animal_tag, values_from = population, values_fill = 0)

  # Ensure all expected columns exist even if 0
  for(tag in base_tags) if(!(tag %in% names(base_pops_wide))) base_pops_wide[[tag]] <- 0

  # --- 3. Modeling Offspring Logic ---


  calculated_pops <- base_pops_wide %>%
    dplyr::mutate(
      # Replacements
      pop_beef_calves_male_repl   = mature_beef_bull   * rates$beef_male_repl,
      pop_beef_calves_female_repl = mature_beef_cattle * rates$beef_female_repl,
      pop_dairy_calves_female_repl = mature_dairy_cattle * rates$dairy_female_repl,

      # Births (divided by 2 for sex ratio)
      dairy_births_half = (mature_dairy_cattle * rates$dairy_calving) / 2,
      beef_births_half  = (mature_beef_cattle  * rates$beef_calving)  / 2,

      # Slaughter/Market Animals (Born - Replacements)
      pop_feedlot_calves_male   = dairy_births_half,
      pop_feedlot_calves_female = dairy_births_half - pop_dairy_calves_female_repl,
      pop_beef_calves_male      = beef_births_half  - pop_beef_calves_male_repl,
      pop_beef_calves_female    = beef_births_half  - pop_beef_calves_female_repl
    )

  # --- 4. Long Format and Class Management ---
  all_pops_long <- calculated_pops %>%
    dplyr::select(
      region, subregion, class_flex,
      mature_beef_bull, mature_dairy_cattle, mature_beef_cattle,
      feedlot_calves_male             = pop_feedlot_calves_male,
      feedlot_calves_female           = pop_feedlot_calves_female,
      beef_calves_male                = pop_beef_calves_male,
      beef_calves_female              = pop_beef_calves_female,
      beef_calves_male_replacement    = pop_beef_calves_male_repl,
      beef_calves_female_replacement  = pop_beef_calves_female_repl,
      dairy_calves_female_replacement = pop_dairy_calves_female_repl
    ) %>%
    # Add Yearlings (mapped 1:1 from calves)
    dplyr::mutate(
      beef_yearlings_male_replacement    = beef_calves_male_replacement,
      beef_yearlings_female_replacement  = beef_calves_female_replacement,
      dairy_yearlings_female_replacement = dairy_calves_female_replacement
    ) %>%
    tidyr::pivot_longer(
      cols = -c(region, subregion, class_flex),
      names_to = "animal_tag",
      values_to = "population"
    ) %>%
    # Offspring do NOT inherit productivity phases (class_flex) from parents
    dplyr::mutate(
      class_flex = dplyr::if_else(animal_tag %in% base_tags, class_flex, NA_character_)
    )

  # --- 5. Subregion and Final Aggregation ---
  final_cattle_pop <- all_pops_long %>%
    dplyr::mutate(
      # Feedlot animals are often managed at a regional level (NA subregion)
      subregion = dplyr::if_else(
        animal_tag %in% c("feedlot_calves_male", "feedlot_calves_female"),
        NA_character_,
        subregion
      )
    ) %>%
    dplyr::group_by(region, subregion, animal_tag, class_flex) %>%
    dplyr::summarise(population = sum(population, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(round(population, 5) > 0)

  return(final_cattle_pop)
}
