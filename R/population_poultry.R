#' Calculate Poultry Population Structure
#'
#' Models the complete poultry flock structure from mature breeding or laying hens
#' based on biological laying cycles, incubation fertility, and rearing durations.
#'
#' @section Demographic Flow Logic:
#' \itemize{
#'   \item \strong{Mature Layers / Breeders}: \code{laying_hens},
#'         \code{breeder_meat_hens}.
#'   \item \strong{Replacement Pullets}: Rearing cohorts (\code{replacement_layer_pullets},
#'         \code{replacement_meat_pullets}) generated from the all-in all-out cycle turnover
#'         and rearing days (\code{days / 365}).
#'   \item \strong{Broilers}: Meat chickens generated from breeder meat hens based on daily egg mass
#'         (\code{egg_mass_g_day}), average egg weight (\code{egg_weight_g}), incubation hatchability
#'         (\code{fertility_rate}), and fattening duration (\code{broiler_days}).
#'   \item \strong{Manual Broiler Protection}: If the user explicitly enters a positive count for
#'         \code{broilers} in the census (e.g. an independent commercial grow-out farm), their manual
#'         figure is preserved intact without being overwritten.
#' }
#'
#' @param census_poultry Filtered census data for poultry.
#' @param rate_parameters Reproduction and replacement rates table.
#' @param definitions Optional unified or monogastric definitions table.
#' @param weights Optional weights table (e.g., livestock_weights) containing \code{productive_period_days}.
#'
#' @return A tibble with the modeled poultry population structure.
#' @export
calculate_population_poultry <- function(census_poultry, rate_parameters, definitions = NULL, weights = NULL) {

  message("\U0001F426 Calculating populations for POULTRY...")

  # =========================================================================
  # Step 1: Identify Base Mature Cohorts & Valid Tags
  # =========================================================================
  valid_poultry_tags <- if (!is.null(definitions) && "animal_type" %in% names(definitions)) {
    definitions %>%
      dplyr::filter(tolower(animal_type) == "poultry") %>%
      dplyr::pull(animal_tag) %>%
      unique()
  } else {
    character()
  }

  if (length(valid_poultry_tags) == 0) {
    valid_poultry_tags <- unique(census_poultry$animal_tag)
  }

  # Backward compatibility: migrate legacy 'free_range_laying_hens' to 'laying_hens'
  if ("free_range_laying_hens" %in% census_poultry$animal_tag) {
    census_poultry <- census_poultry %>%
      dplyr::mutate(
        class_flex = dplyr::if_else(animal_tag == "free_range_laying_hens" & (is.na(class_flex) | class_flex == ""),
                                    "free_range", class_flex),
        animal_tag = dplyr::if_else(animal_tag == "free_range_laying_hens",
                                    "laying_hens", animal_tag)
      )
  }

  base_tags <- c("breeder_meat_hens", "laying_hens")
  present_bases <- intersect(base_tags, census_poultry$animal_tag[census_poultry$population > 0])

  if (length(present_bases) == 0) {
    return(census_poultry %>% dplyr::filter(animal_tag %in% valid_poultry_tags))
  }

  # =========================================================================
  # Step 2: Retrieve Biological & Zootechnical Parameters
  # =========================================================================
  # 2a. Parameter retrieval helper closures
  get_def_val <- function(col, tag, default_val = 0) {
    if (is.null(definitions) || !col %in% names(definitions)) return(default_val)
    val <- definitions %>%
      dplyr::filter(animal_tag == tag) %>%
      dplyr::pull(!!rlang::sym(col))
    if (length(val) > 0 && !is.na(val[1])) {
      num_val <- suppressWarnings(as.numeric(val[1]))
      if (!is.na(num_val) && num_val > 0) return(num_val)
    }
    return(default_val)
  }

  get_rate <- function(param, tag, default_val) {
    if (is.null(rate_parameters) || nrow(rate_parameters) == 0) return(default_val)
    val <- rate_parameters %>%
      dplyr::filter(tolower(parameter) == tolower(param), animal_tag == tag) %>%
      dplyr::pull(value)
    if (length(val) > 0 && !is.na(val[1])) as.numeric(val[1]) else default_val
  }

  weights_df <- if (!is.null(weights)) {
    weights
  } else if (file.exists("user_data/livestock_weights.csv")) {
    suppressMessages(readr::read_csv("user_data/livestock_weights.csv", show_col_types = FALSE))
  } else {
    NULL
  }

  get_period_days <- function(tag, default_days = 365) {
    if (is.null(weights_df)) return(default_days)
    val <- weights_df %>%
      dplyr::filter(animal_tag == tag) %>%
      dplyr::pull(productive_period_days)
    if (length(val) > 0 && !is.na(val[1]) && as.numeric(val[1]) > 0) as.numeric(val[1]) else default_days
  }

  # 2b. Durations of productive laying cycles and rearing periods
  pullet_meat_days  <- get_period_days("replacement_meat_pullets", default_days = 140)
  pullet_layer_days <- get_period_days("replacement_layer_pullets", default_days = 119)
  hen_meat_days     <- get_period_days("breeder_meat_hens", default_days = 301)
  laying_days       <- get_period_days("laying_hens", default_days = 511)
  broiler_days      <- get_period_days("broilers", default_days = 42)

  # 2c. Breeder meat hen oviposition and fertility parameters
  egg_mass_breeder  <- get_def_val("egg_mass_g_day", "breeder_meat_hens", default_val = 37.12)
  egg_wt_breeder    <- get_def_val("egg_weight_g", "breeder_meat_hens", default_val = 64.0)
  fertility_def     <- get_def_val("fertility_rate", "breeder_meat_hens", default_val = 0)
  fertility_breeder <- if (fertility_def > 0) {
    fertility_def
  } else {
    get_rate("fertility_rate", "breeder_meat_hens", default_val = 0.83)
  }
  if (fertility_breeder > 1) fertility_breeder <- fertility_breeder / 100

  # 2d. Replacement rate dynamics: user-specified value takes precedence,
  # otherwise estimated from all-in all-out cycle turnover (365 / productive_period_days)
  get_repl_rate <- function(tag, cycle_days) {
    user_val <- get_rate("replacement_rate", tag, default_val = NA_real_)
    if (!is.na(user_val) && user_val > 0) return(user_val)
    if (cycle_days > 0) (365 / cycle_days) else 1.0
  }

  breeder_meat_repl_rate <- get_repl_rate("breeder_meat_hens", hen_meat_days)
  laying_repl_rate       <- get_repl_rate("laying_hens", laying_days)

  # =========================================================================
  # Step 3: Pivot Mature Base Populations (Wide Format)
  # =========================================================================
  adult_census <- census_poultry %>%
    dplyr::filter(animal_tag %in% base_tags)

  # Check if non-base cohorts (e.g. broilers) exist in input census with positive count
  non_base_census <- census_poultry %>%
    dplyr::filter(!animal_tag %in% c(base_tags, "replacement_meat_pullets", "replacement_layer_pullets"),
                  population > 0)

  user_has_broilers       <- any(non_base_census$animal_tag == "broilers")
  broilers_in_definitions <- "broilers" %in% valid_poultry_tags
  auto_model_broilers     <- !user_has_broilers && broilers_in_definitions

  grouped_keys <- c("region", "subregion", "class_flex")

  base_wide <- adult_census %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouped_keys)), animal_tag) %>%
    dplyr::summarise(population = sum(population, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = animal_tag, values_from = population, values_fill = 0)

  for (tag in base_tags) {
    if (!tag %in% names(base_wide)) base_wide[[tag]] <- 0
  }

  eggs_per_day <- if (egg_wt_breeder > 0) (egg_mass_breeder / egg_wt_breeder) else (37.12 / 64.0)

  # =========================================================================
  # Step 4: Demographic Modeling Equations
  # =========================================================================
  calculated <- base_wide %>%
    dplyr::mutate(
      # 4a. Annual replacement pullets required for laying and breeding flocks
      annual_meat_pullets  = breeder_meat_hens * breeder_meat_repl_rate,
      annual_layer_pullets = laying_hens * laying_repl_rate,

      # 4b. Standing population of replacement pullets (IPCC Eq. 10.1 AAP)
      pop_replacement_meat_pullets  = annual_meat_pullets * (pullet_meat_days / 365),
      pop_replacement_layer_pullets = annual_layer_pullets * (pullet_layer_days / 365),

      # 4c. Standing population of broilers auto-modeled from breeder hens (IPCC Eq. 10.1 AAP)
      pop_broilers = dplyr::if_else(
        auto_model_broilers & breeder_meat_hens > 0,
        breeder_meat_hens * eggs_per_day * fertility_breeder * broiler_days,
        0
      )
    )

  # =========================================================================
  # Step 5: Format, Assemble & Return (Long Format)
  # =========================================================================
  gen_select_cols <- c(
    grouped_keys,
    base_tags,
    "replacement_meat_pullets",
    "replacement_layer_pullets"
  )
  if (auto_model_broilers) {
    gen_select_cols <- c(gen_select_cols, "broilers")
  }

  generated_long <- calculated %>%
    dplyr::rename(
      replacement_meat_pullets  = pop_replacement_meat_pullets,
      replacement_layer_pullets = pop_replacement_layer_pullets,
      broilers                  = pop_broilers
    ) %>%
    dplyr::select(dplyr::all_of(gen_select_cols)) %>%
    tidyr::pivot_longer(
      cols = -dplyr::all_of(grouped_keys),
      names_to = "animal_tag",
      values_to = "population"
    ) %>%
    # Offspring do not inherit productivity phases (class_flex) from parents
    dplyr::mutate(
      class_flex = dplyr::if_else(animal_tag %in% base_tags, class_flex, NA_character_)
    )

  # Combine auto-modeled cohorts with preserved user cohorts (e.g. manual commercial broilers)
  combined_pops <- dplyr::bind_rows(generated_long, non_base_census)

  final_poultry_pop <- combined_pops %>%
    dplyr::group_by(region, subregion, animal_tag, class_flex) %>%
    dplyr::summarise(population = sum(population, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(animal_tag %in% valid_poultry_tags, round(population, 5) > 0)

  # Informative console summary messages
  tot_layers        <- sum(final_poultry_pop$population[final_poultry_pop$animal_tag == "laying_hens"], na.rm = TRUE)
  tot_breeders      <- sum(final_poultry_pop$population[final_poultry_pop$animal_tag == "breeder_meat_hens"], na.rm = TRUE)
  tot_layer_pullets <- sum(final_poultry_pop$population[final_poultry_pop$animal_tag == "replacement_layer_pullets"], na.rm = TRUE)
  tot_meat_pullets  <- sum(final_poultry_pop$population[final_poultry_pop$animal_tag == "replacement_meat_pullets"], na.rm = TRUE)
  tot_broilers      <- sum(final_poultry_pop$population[final_poultry_pop$animal_tag == "broilers"], na.rm = TRUE)

  if (tot_broilers > 0) {
    annual_broiler_harvest <- tot_broilers * (365 / broiler_days)
    if (user_has_broilers) {
      message(sprintf("   \u2022 Broilers: %s standing barn places (%.0f days feed -> %s finished/yr)",
                      format(round(tot_broilers), big.mark = ","),
                      broiler_days,
                      format(round(annual_broiler_harvest), big.mark = ",")))
    } else if (auto_model_broilers && any(calculated$pop_broilers > 0)) {
      message(sprintf("   \u2022 Broilers: %s standing barn places (%.0f days feed -> %s finished/yr) (auto-modeled from breeder hens: %.2f eggs/day, %.1f%% hatch)",
                      format(round(tot_broilers), big.mark = ","),
                      broiler_days,
                      format(round(annual_broiler_harvest), big.mark = ","),
                      eggs_per_day,
                      fertility_breeder * 100))
    }
  }

  if (tot_layers > 0) {
    message(sprintf("   \u2022 Layer hens: %s head (cycle %.0f days, repl %.1f%%) -> %s standing replacement pullets (%.0f days rearing)",
                    format(round(tot_layers), big.mark = ","),
                    laying_days,
                    laying_repl_rate * 100,
                    format(round(tot_layer_pullets, 1), big.mark = ","),
                    pullet_layer_days))
  }

  if (tot_breeders > 0) {
    message(sprintf("   \u2022 Breeder hens: %s head (cycle %.0f days, repl %.1f%%) -> %s standing replacement pullets (%.0f days rearing)",
                    format(round(tot_breeders), big.mark = ","),
                    hen_meat_days,
                    breeder_meat_repl_rate * 100,
                    format(round(tot_meat_pullets, 1), big.mark = ","),
                    pullet_meat_days))
  }

  return(final_poultry_pop)
}
