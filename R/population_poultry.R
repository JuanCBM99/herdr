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
#'   \item \strong{Broilers}: Meat chickens generated from breeder meat hens based on annual egg production
#'         (\code{eggs_per_year}), average egg weight (\code{egg_weight_g}), incubation hatchability
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
#' @param data_dir Character. Path to the folder containing input CSV files. Default is \code{"user_data"}.
#'
#' @return A tibble with the modeled poultry population structure.
#' @export
calculate_population_poultry <- function(census_poultry, rate_parameters = NULL, definitions = NULL, weights = NULL, data_dir = "user_data") {

  message("Calculating populations for POULTRY...")

  # Handle flexible parameter positioning
  if (!is.null(rate_parameters)) {
    if (!("parameter" %in% names(rate_parameters) && "value" %in% names(rate_parameters))) {
      if (is.null(definitions)) {
        definitions <- rate_parameters
      }
      rate_parameters <- NULL
    }
  }

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
  # Step 2: Pivot Mature Base Populations (Wide Format)
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

  if (nrow(base_wide) == 0 ||
      all(c(base_wide$breeder_meat_hens, base_wide$laying_hens) == 0)) {
    return(census_poultry %>% dplyr::filter(animal_tag %in% valid_poultry_tags))
  }

  # =========================================================================
  # Step 3: Retrieve Biological & Zootechnical Parameters (Stratum-Aware)
  # =========================================================================
  weights_df <- if (!is.null(weights)) {
    weights
  } else {
    weights_path <- file.path(data_dir, "livestock_weights.csv")
    if (file.exists(weights_path)) {
      suppressMessages(readr::read_csv(weights_path, show_col_types = FALSE))
    } else {
      NULL
    }
  }

  # 3a. Durations of productive laying cycles and rearing periods
  pullet_meat_days  <- herdr_resolve_period_days(base_wide, "replacement_meat_pullets",  weights = weights_df, default_days = 140)
  pullet_layer_days <- herdr_resolve_period_days(base_wide, "replacement_layer_pullets", weights = weights_df, default_days = 119)
  hen_meat_days     <- herdr_resolve_period_days(base_wide, "breeder_meat_hens",         weights = weights_df, default_days = 301)
  laying_days       <- herdr_resolve_period_days(base_wide, "laying_hens",               weights = weights_df, default_days = 511)
  broiler_days      <- herdr_resolve_period_days(base_wide, "broilers",                  weights = weights_df, default_days = 42)

  # 3b. Breeder meat hen oviposition and fertility parameters
  eggs_yr_breeder   <- herdr_resolve_demographic_param(base_wide, "breeder_meat_hens", "eggs_per_year",
                                                       definitions = definitions, rate_parameters = rate_parameters, default_val = 211.7)
  egg_wt_breeder    <- herdr_resolve_demographic_param(base_wide, "breeder_meat_hens", "egg_weight_g",
                                                       definitions = definitions, rate_parameters = rate_parameters, default_val = 64.0)
  fertility_def     <- herdr_resolve_demographic_param(base_wide, "breeder_meat_hens", "fertility_rate",
                                                       definitions = definitions, rate_parameters = rate_parameters, default_val = 0.83)
  fertility_breeder <- ifelse(fertility_def > 1, fertility_def / 100, fertility_def)
  eggs_per_day      <- ifelse(eggs_yr_breeder > 0, eggs_yr_breeder / 365, 211.7 / 365)

  # 3c. Replacement rate dynamics: user-specified value takes precedence,
  # otherwise estimated from all-in all-out cycle turnover (365 / productive_period_days)
  breeder_meat_repl_rate_def <- herdr_resolve_demographic_param(base_wide, "breeder_meat_hens", "replacement_rate",
                                                                definitions = definitions, rate_parameters = rate_parameters, default_val = -1)
  breeder_meat_repl_rate     <- ifelse(breeder_meat_repl_rate_def >= 0, breeder_meat_repl_rate_def,
                                       ifelse(hen_meat_days > 0, 365 / hen_meat_days, 1.0))

  laying_repl_rate_def       <- herdr_resolve_demographic_param(base_wide, "laying_hens", "replacement_rate",
                                                                definitions = definitions, rate_parameters = rate_parameters, default_val = -1)
  laying_repl_rate           <- ifelse(laying_repl_rate_def >= 0, laying_repl_rate_def,
                                       ifelse(laying_days > 0, 365 / laying_days, 1.0))

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
    annual_broiler_harvest <- tot_broilers * (365 / broiler_days[1])
    if (user_has_broilers) {
      message(sprintf("   \u2022 Broilers: %s standing barn places (%.0f days feed -> %s finished/yr)",
                      format(round(tot_broilers), big.mark = ","),
                      broiler_days[1],
                      format(round(annual_broiler_harvest), big.mark = ",")))
    } else if (auto_model_broilers && any(calculated$pop_broilers > 0)) {
      message(sprintf("   \u2022 Broilers: %s standing barn places (%.0f days feed -> %s finished/yr) (auto-modeled from breeder hens: %.2f eggs/day, %.1f%% hatch)",
                      format(round(tot_broilers), big.mark = ","),
                      broiler_days[1],
                      format(round(annual_broiler_harvest), big.mark = ","),
                      eggs_per_day[1],
                      fertility_breeder[1] * 100))
    }
  }

  if (tot_layers > 0) {
    message(sprintf("   \u2022 Layer hens: %s head (cycle %.0f days, repl %.1f%%) -> %s standing replacement pullets (%.0f days rearing)",
                    format(round(tot_layers), big.mark = ","),
                    laying_days[1],
                    laying_repl_rate[1] * 100,
                    format(round(tot_layer_pullets, 1), big.mark = ","),
                    pullet_layer_days[1]))
  }

  if (tot_breeders > 0) {
    message(sprintf("   \u2022 Breeder hens: %s head (cycle %.0f days, repl %.1f%%) -> %s standing replacement pullets (%.0f days rearing)",
                    format(round(tot_breeders), big.mark = ","),
                    hen_meat_days[1],
                    breeder_meat_repl_rate[1] * 100,
                    format(round(tot_meat_pullets, 1), big.mark = ","),
                    pullet_meat_days[1]))
  }

  return(final_poultry_pop)
}
