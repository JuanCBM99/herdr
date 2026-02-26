#' Calculates the full goat population structure (Internal Helper)
#' @param census_goat Edit in csv
#' @param rate_parameters Edit in csv
#' @export
calculate_population_goat <- function(census_goat, rate_parameters) {

  message("\U0001F9EE Calculating populations for GOAT...")

  # (Assume these are the base 'animal_tag' for goats)
  req_identifications <- c("mature_goat_male_dairy", "mature_goat_male_meat",
                           "mature_goat_female_dairy", "mature_goat_female_meat")

  # --- Helper to get rates safely ---
  get_rate <- function(param, tag) {
    rate_parameters %>%
      dplyr::filter(parameter == param, animal_tag == tag) %>%
      dplyr::pull(value) %>%
      .[1] # Coge el primer valor que encuentre
  }

  # --- Goat Rates ---
  # (Ensure 'kidding_rate' exists in your 'reproduction_parameters.csv')
  rate_dairy_kidding     <- get_rate("kidding_rate", "mature_goat_female_dairy")
  rate_meat_kidding      <- get_rate("kidding_rate", "mature_goat_female_meat")
  rate_dairy_male_repl   <- get_rate("replacement_rate", "mature_goat_male_dairy")
  rate_dairy_female_repl <- get_rate("replacement_rate", "mature_goat_female_dairy")
  rate_meat_male_repl    <- get_rate("replacement_rate", "mature_goat_male_meat")
  rate_meat_female_repl  <- get_rate("replacement_rate", "mature_goat_female_meat")

  # --- Goat Calculation ---
  base_pops_agg <- census_goat %>%
    dplyr::filter(animal_tag %in% req_identifications) %>%
    dplyr::group_by(region, subregion, class_flex, animal_tag) %>%
    dplyr::summarise(population = sum(population, na.rm = TRUE), .groups = "drop")

  base_pops_wide <- base_pops_agg %>%
    tidyr::pivot_wider(
      names_from = animal_tag,
      values_from = population,
      values_fill = 0
    )

  calculated_pops <- base_pops_wide %>%
    dplyr::group_by(region, subregion, class_flex) %>%
    dplyr::mutate(
      # Replacements (ONLY calculated offspring)
      pop_kid_goat_female_dairy_replacement = mature_goat_female_dairy * rate_dairy_female_repl,
      pop_kid_goat_male_dairy_replacement = mature_goat_male_dairy * rate_dairy_male_repl,
      pop_kid_goat_female_meat_replacement = mature_goat_female_meat * rate_meat_female_repl,
      pop_kid_goat_male_meat_replacement = mature_goat_male_meat * rate_meat_male_repl

      # (No 'total_births' or 'slaughter')
    ) %>%
    dplyr::ungroup()

  # --- Goat Assembly ---
  all_populations_long <- calculated_pops %>%
    dplyr::select(
      region, subregion, class_flex,
      # Parents
      mature_goat_male_dairy, mature_goat_male_meat,
      mature_goat_female_dairy, mature_goat_female_meat,
      # Offspring (Replacement Only)
      kid_goat_female_dairy_replacement = pop_kid_goat_female_dairy_replacement,
      kid_goat_male_dairy_replacement = pop_kid_goat_male_dairy_replacement,
      kid_goat_female_meat_replacement = pop_kid_goat_female_meat_replacement,
      kid_goat_male_meat_replacement = pop_kid_goat_male_meat_replacement
    ) %>%
    tidyr::pivot_longer(
      cols = -c(region, subregion, class_flex),
      names_to = "animal_tag",
      values_to = "population"
    ) %>%
    dplyr::filter(round(population, 5) > 0)

  return(all_populations_long)
}
