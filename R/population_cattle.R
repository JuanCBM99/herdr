#' Calculates the full cattle population structure (Internal Helper)
#' @export
calculate_population_cattle <- function(census_cattle, rate_parameters, categories) {

  message("🧮 Calculating populations for CATTLE...")

  req_identifications <- c("mature_beef_bull", "mature_dairy_cattle", "mature_beef_cattle")
  if (!all(req_identifications %in% census_cattle$identification)) {
    stop("Error: 'census.csv' (cattle) must contain base identifications.")
  }

  get_rate <- function(param, subtype, sex_val = NA) {
    df <- rate_parameters %>%
      dplyr::filter(animal_type == "cattle",
                    parameter == param,
                    animal_subtype == subtype)
    if (!is.na(sex_val)) df <- df %>% dplyr::filter(sex == sex_val)
    val <- df %>% dplyr::pull(value)
    if (length(val) == 0) {
      warning(paste("Rate not found for (cattle):", param, subtype, sex_val))
      return(NA_real_)
    }
    val[1]
  }

  rate_dairy_calving <- get_rate("calving_rate", "dairy")
  rate_beef_calving  <- get_rate("calving_rate", "beef")
  rate_beef_male_repl    <- get_rate("replacement_rate", "beef", "male")
  rate_beef_female_repl  <- get_rate("replacement_rate", "beef", "female")
  rate_dairy_female_repl <- get_rate("replacement_rate", "dairy", "female")

  base_pops_agg <- census_cattle %>%
    dplyr::filter(identification %in% req_identifications) %>%
    dplyr::group_by(group, zone, identification) %>%
    dplyr::summarise(population = sum(population, na.rm = TRUE), .groups = "drop")

  base_pops_wide <- base_pops_agg %>%
    tidyr::pivot_wider(
      names_from = identification,
      values_from = population,
      values_fill = 0
    )

  calculated_pops <- base_pops_wide %>%
    dplyr::group_by(group, zone) %>%
    dplyr::mutate(
      pop_beef_calves_male_replacement = mature_beef_bull * rate_beef_male_repl,
      pop_beef_calves_female_replacement = mature_beef_cattle * rate_beef_female_repl,
      pop_dairy_calves_female_replacement = mature_dairy_cattle * rate_dairy_female_repl,
      total_dairy_births = mature_dairy_cattle * rate_dairy_calving / 2,
      total_beef_births  = mature_beef_cattle * rate_beef_calving / 2,
      pop_feedlot_calves_male = total_dairy_births,
      pop_feedlot_calves_female = total_dairy_births - pop_dairy_calves_female_replacement,
      pop_beef_calves_male = total_beef_births - pop_beef_calves_male_replacement,
      pop_beef_calves_female = total_beef_births - pop_beef_calves_female_replacement
    ) %>%
    dplyr::ungroup()

  all_populations_long <- calculated_pops %>%
    dplyr::select(
      group,
      zone_parent = zone,
      mature_beef_bull, mature_dairy_cattle, mature_beef_cattle,
      feedlot_calves_male = pop_feedlot_calves_male,
      feedlot_calves_female = pop_feedlot_calves_female,
      beef_calves_male = pop_beef_calves_male,
      beef_calves_female = pop_beef_calves_female,
      beef_calves_male_replacement = pop_beef_calves_male_replacement,
      beef_calves_female_replacement = pop_beef_calves_female_replacement,
      dairy_calves_female_replacement = pop_dairy_calves_female_replacement
    ) %>%
    dplyr::mutate(
      beef_yearlings_male_replacement = beef_calves_male_replacement,
      beef_yearlings_female_replacement = beef_calves_female_replacement,
      dairy_yearlings_female_replacement = dairy_calves_female_replacement
    ) %>%
    tidyr::pivot_longer(
      cols = -c(group, zone_parent),
      names_to = "identification",
      values_to = "population"
    )

  # --- 7. Reassign zones for feedlot animals (¡ARREGLADO!) ---
  all_populations_agg <- all_populations_long %>%
    # Ya no necesitamos el 'left_join' a 'categories' aquí
    dplyr::mutate(
      zone = dplyr::if_else(
        # Usamos la lógica de 'identification'
        identification %in% c("feedlot_calves_male", "feedlot_calves_female"),
        NA_character_,
        zone_parent
      )
    ) %>%
    dplyr::group_by(group, zone, identification) %>%
    dplyr::summarise(population = sum(population, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(round(population, 5) > 0)

  return(all_populations_agg)
}
