#' Calculates the full goat population structure (Internal Helper)
#' @noRd # <-- ¡Importante! No se exporta
calculate_population_goat <- function(census_goat, rate_parameters) {

  message("🧮 Calculating populations for GOAT...")

  # (Asumimos que estos son los 'identification' base para cabras)
  req_identifications <- c("mature_goat_male_dairy", "mature_goat_male_meat",
                           "mature_goat_female_dairy", "mature_goat_female_meat")
  if (!all(req_identifications %in% census_goat$identification)) {
    stop("Error: 'census.csv' (goat) must contain all 4 base identifications.")
  }

  # --- Helper to get rates safely ---
  get_rate <- function(param, subtype, sex_val = NA) {
    df <- rate_parameters %>%
      dplyr::filter(animal_type == "goat", # <-- Filtrar por goat
                    parameter == param,
                    animal_subtype == subtype)
    if (!is.na(sex_val)) df <- df %>% dplyr::filter(sex == sex_val)
    val <- df %>% dplyr::pull(value)
    if (length(val) == 0) {
      warning(paste("Rate not found for (goat):", param, subtype, sex_val))
      return(NA_real_)
    }
    val[1]
  }

  # --- Tasas de Cabras ---
  # (Asegúrate de que 'kidding_rate' existe en tu 'rate_parameters.csv')
  rate_dairy_kidding <- get_rate("kidding_rate", "dairy", sex_val = NA)
  rate_meat_kidding  <- get_rate("kidding_rate", "meat", sex_val = NA)
  rate_dairy_male_repl   <- get_rate("replacement_rate", "dairy", "male")
  rate_dairy_female_repl <- get_rate("replacement_rate", "dairy", "female")
  rate_meat_male_repl    <- get_rate("replacement_rate", "meat", "male")
  rate_meat_female_repl  <- get_rate("replacement_rate", "meat", "female")

  # --- Cálculo de Cabras ---
  base_pops_agg <- census_goat %>%
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
      # Reemplazos (¡ÚNICOS HIJOS CALCULADOS!)
      pop_kid_goat_female_dairy_replacement = mature_goat_female_dairy * rate_dairy_female_repl,
      pop_kid_goat_male_dairy_replacement = mature_goat_male_dairy * rate_dairy_male_repl,
      pop_kid_goat_female_meat_replacement = mature_goat_female_meat * rate_meat_female_repl,
      pop_kid_goat_male_meat_replacement = mature_goat_male_meat * rate_meat_male_repl

      # (No hay 'total_births' ni 'slaughter' según tu lógica)
    ) %>%
    dplyr::ungroup()

  # --- Ensamblado de Cabras ---
  all_populations_long <- calculated_pops %>%
    dplyr::select(
      group, zone,
      # Padres
      mature_goat_male_dairy, mature_goat_male_meat,
      mature_goat_female_dairy, mature_goat_female_meat,
      # Hijos (Solo Reemplazo)
      kid_goat_female_dairy_replacement = pop_kid_goat_female_dairy_replacement,
      kid_goat_male_dairy_replacement = pop_kid_goat_male_dairy_replacement,
      kid_goat_female_meat_replacement = pop_kid_goat_female_meat_replacement,
      kid_goat_male_meat_replacement = pop_kid_goat_male_meat_replacement
    ) %>%
    tidyr::pivot_longer(
      cols = -c(group, zone),
      names_to = "identification",
      values_to = "population"
    ) %>%
    dplyr::filter(round(population, 5) > 0)

  return(all_populations_long)
}
