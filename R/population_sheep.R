#' Calculates the full sheep population structure (Internal Helper)
#' @export
calculate_population_sheep <- function(census_sheep, rate_parameters) {

  message("🧮 Calculating populations for SHEEP...")

  # (Asumimos que estos son los 'identification' base para ovejas)
  req_identifications <- c("mature_sheep_male_dairy", "mature_sheep_male_meat",
                           "mature_sheep_female_dairy", "mature_sheep_female_meat")
  if (!all(req_identifications %in% census_sheep$identification)) {
    stop("Error: 'census.csv' (sheep) must contain all 4 base identifications.")
  }

  # --- Helper to get rates safely ---
  get_rate <- function(param, subtype, sex_val = NA) {
    df <- rate_parameters %>%
      dplyr::filter(animal_type == "sheep", # <-- Filtrar por sheep
                    parameter == param,
                    animal_subtype == subtype)
    if (!is.na(sex_val)) df <- df %>% dplyr::filter(sex == sex_val)
    val <- df %>% dplyr::pull(value)
    if (length(val) == 0) {
      warning(paste("Rate not found for (sheep):", param, subtype, sex_val))
      return(NA_real_)
    }
    val[1]
  }

  # --- Tasas de Ovejas ---
  rate_dairy_lambing <- get_rate("lambing_rate", "dairy", sex_val = NA)
  rate_meat_lambing  <- get_rate("lambing_rate", "meat", sex_val = NA)
  rate_dairy_male_repl   <- get_rate("replacement_rate", "dairy", "male")
  rate_dairy_female_repl <- get_rate("replacement_rate", "dairy", "female")
  rate_meat_male_repl    <- get_rate("replacement_rate", "meat", "male")
  rate_meat_female_repl  <- get_rate("replacement_rate", "meat", "female")

  # --- Cálculo de Ovejas ---
  base_pops_agg <- census_sheep %>%
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
      # Reemplazos
      pop_lamb_female_dairy_replacement = mature_sheep_female_dairy * rate_dairy_female_repl,
      pop_lamb_male_dairy_replacement = mature_sheep_male_dairy * rate_dairy_male_repl,
      pop_lamb_female_meat_replacement = mature_sheep_female_meat * rate_meat_female_repl,
      pop_lamb_male_meat_replacement = mature_sheep_male_meat * rate_meat_male_repl,

      # Nacimientos
      total_dairy_births = mature_sheep_female_dairy * rate_dairy_lambing,
      total_meat_births  = mature_sheep_female_meat * rate_meat_lambing
    ) %>%
    dplyr::mutate(
      # Sacrificio (Nacidos - Reemplazos)
      pop_lamb_dairy_slaughter = total_dairy_births - pop_lamb_female_dairy_replacement - pop_lamb_male_dairy_replacement,
      pop_lamb_meat_slaughter = total_meat_births - pop_lamb_female_meat_replacement - pop_lamb_male_meat_replacement
    ) %>%
    dplyr::ungroup()

  # --- Ensamblado de Ovejas ---
  all_populations_long <- calculated_pops %>%
    dplyr::select(
      group, zone,
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
      cols = -c(group, zone),
      names_to = "identification",
      values_to = "population"
    ) %>%
    dplyr::filter(round(population, 5) > 0)

  return(all_populations_long)
}
