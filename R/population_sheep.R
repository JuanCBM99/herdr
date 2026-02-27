#' Calculates the full sheep population structure (Internal Helper)
#' @param census_sheep Edit in csv
#' @param rate_parameters Edit in csv
#' @export
calculate_population_sheep <- function(census_sheep, rate_parameters) {

  message("\U0001F9EE Calculating populations for SHEEP...")

  req_identifications <- c("mature_sheep_male_dairy", "mature_sheep_male_meat",
                           "mature_sheep_female_dairy", "mature_sheep_female_meat")

  get_rate <- function(param, tag) {
    rate_parameters %>%
      dplyr::filter(parameter == param, animal_tag == tag) %>%
      dplyr::pull(value) %>%
      .[1]
  }

  rate_dairy_lambing     <- get_rate("lambing_rate", "mature_sheep_female_dairy")
  rate_meat_lambing      <- get_rate("lambing_rate", "mature_sheep_female_meat")
  rate_dairy_male_repl   <- get_rate("replacement_rate", "mature_sheep_male_dairy")
  rate_dairy_female_repl <- get_rate("replacement_rate", "mature_sheep_female_dairy")
  rate_meat_male_repl    <- get_rate("replacement_rate", "mature_sheep_male_meat")
  rate_meat_female_repl  <- get_rate("replacement_rate", "mature_sheep_female_meat")

  base_pops_agg <- census_sheep %>%
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
      pop_lamb_female_dairy_replacement = mature_sheep_female_dairy * rate_dairy_female_repl,
      pop_lamb_male_dairy_replacement = mature_sheep_male_dairy * rate_dairy_male_repl,
      pop_lamb_female_meat_replacement = mature_sheep_female_meat * rate_meat_female_repl,
      pop_lamb_male_meat_replacement = mature_sheep_male_meat * rate_meat_male_repl,

      total_dairy_births = mature_sheep_female_dairy * rate_dairy_lambing,
      total_meat_births  = mature_sheep_female_meat * rate_meat_lambing
    ) %>%
    dplyr::mutate(
      pop_lamb_dairy_slaughter = total_dairy_births - pop_lamb_female_dairy_replacement - pop_lamb_male_dairy_replacement,
      pop_lamb_meat_slaughter = total_meat_births - pop_lamb_female_meat_replacement - pop_lamb_male_meat_replacement
    ) %>%
    dplyr::ungroup()

  all_populations_long <- calculated_pops %>%
    dplyr::select(
      region, subregion, class_flex,
      mature_sheep_male_dairy, mature_sheep_male_meat,
      mature_sheep_female_dairy, mature_sheep_female_meat,
      lamb_female_dairy_replacement = pop_lamb_female_dairy_replacement,
      lamb_male_dairy_replacement = pop_lamb_male_dairy_replacement,
      lamb_female_meat_replacement = pop_lamb_female_meat_replacement,
      lamb_male_meat_replacement = pop_lamb_male_meat_replacement,
      lamb_dairy_slaughter = pop_lamb_dairy_slaughter,
      lamb_meat_slaughter = pop_lamb_meat_slaughter
    ) %>%
    tidyr::pivot_longer(
      cols = -c(region, subregion, class_flex),
      names_to = "animal_tag",
      values_to = "population"
    ) %>%
    dplyr::filter(round(population, 5) > 0)

  return(all_populations_long)
}
