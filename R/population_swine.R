#' Calculate Swine Population Structure
#'
#' Models the complete swine herd structure from mature breeding animals using
#' farrowing rates, litter sizes, sow replacement rates, and fattening durations.
#'
#' @section Demographic Flow Logic:
#' \itemize{
#'   \item \strong{Mature Parents}: \code{breeder_sows}, \code{boars}.
#'   \item \strong{Annual Piglets}: \code{breeder_sows * farrowing_rate * litter_size}.
#'   \item \strong{Replacement Sows}: \code{breeder_sows * replacement_rate} retained annually.
#'   \item \strong{Fattening Pigs}: Slaughter pigs (\code{Annual Piglets - Replacements})
#'         scaled to Average Annual Population (AAP, IPCC Eq. 10.1) based on fattening days
#'         (\code{days / 365}).
#' }
#'
#' @param census_swine Filtered census data for swine.
#' @param rate_parameters Reproduction and replacement rates table.
#' @param definitions Optional unified or monogastric definitions table.
#' @param weights Optional weights table (e.g., livestock_weights) containing \code{productive_period_days}.
#' @param data_dir Character. Path to the folder containing input CSV files. Default is \code{"user_data"}.
#'
#' @return A tibble with the modeled swine population structure.
#' @export
calculate_population_swine <- function(census_swine, rate_parameters = NULL, definitions = NULL, weights = NULL, data_dir = "user_data") {

  message("Calculating populations for SWINE...")

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
  valid_swine_tags <- if (!is.null(definitions) && "animal_type" %in% names(definitions)) {
    definitions %>%
      dplyr::filter(tolower(animal_type) == "swine") %>%
      dplyr::pull(animal_tag) %>%
      unique()
  } else {
    character()
  }

  if (length(valid_swine_tags) == 0) {
    valid_swine_tags <- unique(census_swine$animal_tag)
  }

  base_tags <- c("breeder_sows", "boars")
  present_bases <- intersect(base_tags, census_swine$animal_tag[census_swine$population > 0])

  if (!"breeder_sows" %in% present_bases) {
    return(census_swine %>% dplyr::filter(animal_tag %in% valid_swine_tags))
  }

  # =========================================================================
  # Step 2: Pivot Mature Base Populations (Wide Format)
  # =========================================================================
  base_pops_wide <- census_swine %>%
    dplyr::filter(animal_tag %in% base_tags) %>%
    dplyr::group_by(farm_id, region, subregion, animal_tag) %>%
    dplyr::summarise(population = sum(population, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = animal_tag, values_from = population, values_fill = 0)

  for (tag in base_tags) {
    if (!tag %in% names(base_pops_wide)) base_pops_wide[[tag]] <- 0
  }

  if (all(c(base_pops_wide$breeder_sows, base_pops_wide$boars) == 0)) {
    return(census_swine %>% dplyr::filter(animal_tag %in% valid_swine_tags))
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

  # 3a. Durations of reproductive cycle and fattening period
  sow_cycle_days <- herdr_resolve_period_days(base_pops_wide, "breeder_sows",   weights = weights_df, default_days = 148.9)
  fattening_days <- herdr_resolve_period_days(base_pops_wide, "fattening_pigs", weights = weights_df, default_days = 110)

  # 3b. Sow replacement rate (priority: definitions -> rate_parameters -> default 25%)
  sow_replacement_rate <- herdr_resolve_demographic_param(base_pops_wide, "breeder_sows", "replacement_rate",
                                                          definitions = definitions, rate_parameters = rate_parameters, default_val = 0.25)

  # 3c. Farrowing index (litters per sow per year: reproductive cycle turnover 365 / productive_period_days)
  farrowing_rate <- ifelse(sow_cycle_days > 0, 365 / sow_cycle_days, 2.45)

  # 3d. Litter size (weaned piglets per litter: definitions piglets_suckling -> piglets_born * 0.85 -> default 12.0)
  suckling_def <- herdr_resolve_demographic_param(base_pops_wide, "breeder_sows", "piglets_suckling",
                                                  definitions = definitions, rate_parameters = rate_parameters, default_val = 0)
  born_def     <- herdr_resolve_demographic_param(base_pops_wide, "breeder_sows", "piglets_born",
                                                  definitions = definitions, rate_parameters = rate_parameters, default_val = 0)
  litter_size  <- ifelse(suckling_def > 0, suckling_def, ifelse(born_def > 0, born_def * 0.85, 12.0))

  # =========================================================================
  # Step 4: Demographic Modeling Equations
  # =========================================================================
  calculated_pops <- base_pops_wide %>%
    dplyr::mutate(
      # 4a. Annual weaned piglets pool produced by breeder sows
      annual_piglets = breeder_sows * farrowing_rate * litter_size,

      # 4b. Annual replacement gilts/sows retained
      pop_replacement_sows = breeder_sows * sow_replacement_rate,

      # 4c. Annual commercial slaughter pigs (Born - Replacements, clamped at 0)
      annual_slaughter = pmax(0, annual_piglets - pop_replacement_sows),

      # 4d. Standing herd census of fattening pigs (Average Annual Population, IPCC Eq. 10.1)
      pop_fattening_pigs = annual_slaughter * (fattening_days / 365)
    )

  # Demographic integrity warning (replacements exceeding available piglets)
  if (any(calculated_pops$pop_replacement_sows > calculated_pops$annual_piglets + 1e-6, na.rm = TRUE)) {
    warning("Demographic Warning (Swine): Replacement demand exceeds annual piglets produced. Check litter size (piglets_born/piglets_suckling) vs replacement_rate.")
  }

  # =========================================================================
  # Step 5: Format, Assemble & Return (Long Format)
  # =========================================================================
  all_pops_long <- calculated_pops %>%
    dplyr::select(
      farm_id, region, subregion,
      breeder_sows,
      boars,
      replacement_sows = pop_replacement_sows,
      fattening_pigs   = pop_fattening_pigs
    ) %>%
    tidyr::pivot_longer(
      cols = -c(farm_id, region, subregion),
      names_to = "animal_tag",
      values_to = "population"
    )

  final_swine_pop <- all_pops_long %>%
    dplyr::group_by(farm_id, region, subregion, animal_tag) %>%
    dplyr::summarise(population = sum(population, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(animal_tag %in% valid_swine_tags, round(population, 5) > 0)

  # Informative console summary messages
  tot_sows        <- sum(final_swine_pop$population[final_swine_pop$animal_tag == "breeder_sows"], na.rm = TRUE)
  tot_repl_sows   <- sum(final_swine_pop$population[final_swine_pop$animal_tag == "replacement_sows"], na.rm = TRUE)
  tot_fattening   <- sum(final_swine_pop$population[final_swine_pop$animal_tag == "fattening_pigs"], na.rm = TRUE)
  total_slaughter <- sum(calculated_pops$annual_slaughter, na.rm = TRUE)

  if (tot_sows > 0) {
    message(sprintf("   \u2022 Breeder sows: %s head (cycle %.1f days = %.2f litters/yr, %.1f piglets/litter)",
                    format(round(tot_sows), big.mark = ","),
                    sow_cycle_days[1],
                    farrowing_rate[1],
                    litter_size[1]))
    message(sprintf("   \u2022 Replacement sows: %s head (repl rate %.1f%%)",
                    format(round(tot_repl_sows), big.mark = ","),
                    sow_replacement_rate[1] * 100))
    if (tot_fattening > 0) {
      message(sprintf("   \u2022 Fattening pigs: %s standing barn places (%.0f days fattening -> %s slaughter pigs/yr)",
                      format(round(tot_fattening), big.mark = ","),
                      fattening_days[1],
                      format(round(total_slaughter), big.mark = ",")))
    }
  }

  return(final_swine_pop)
}
