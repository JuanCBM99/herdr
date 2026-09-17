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
#'
#' @return A tibble with the modeled swine population structure.
#' @export
calculate_population_swine <- function(census_swine, rate_parameters, definitions = NULL, weights = NULL) {

  message("\U0001F416 Calculating populations for SWINE...")

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

  get_rate <- function(param, tag, default_val = 0) {
    if (is.null(rate_parameters) || nrow(rate_parameters) == 0) return(default_val)
    val <- rate_parameters %>%
      dplyr::filter(tolower(parameter) == tolower(param), animal_tag == tag) %>%
      dplyr::pull(value)
    if (length(val) > 0 && !is.na(val[1])) {
      num_val <- suppressWarnings(as.numeric(val[1]))
      if (!is.na(num_val) && num_val > 0) return(num_val)
    }
    return(default_val)
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
    if (length(val) > 0 && !is.na(val[1])) {
      num_val <- suppressWarnings(as.numeric(val[1]))
      if (!is.na(num_val) && num_val > 0) return(num_val)
    }
    return(default_days)
  }

  # 2b. Durations of reproductive cycle and fattening period
  sow_cycle_days <- get_period_days("breeder_sows", default_days = 148.9)
  fattening_days <- get_period_days("fattening_pigs", default_days = 110)

  # 2c. Sow replacement rate (priority: rate_parameters -> default 25%)
  sow_replacement_rate <- get_rate("replacement_rate", "breeder_sows", default_val = 0.25)

  # 2d. Farrowing index (litters per sow per year: reproductive cycle turnover 365 / productive_period_days)
  farrowing_rate <- if (sow_cycle_days > 0) (365 / sow_cycle_days) else 2.45

  # 2e. Litter size (weaned piglets per litter: definitions piglets_suckling -> piglets_born * 0.85 -> default 12.0)
  suckling_def <- get_def_val("piglets_suckling", "breeder_sows", default_val = 0)
  born_def     <- get_def_val("piglets_born", "breeder_sows", default_val = 0)
  litter_size  <- if (suckling_def > 0) {
    suckling_def
  } else if (born_def > 0) {
    born_def * 0.85
  } else {
    12.0
  }

  # =========================================================================
  # Step 3: Pivot Mature Base Populations (Wide Format)
  # =========================================================================
  base_pops_wide <- census_swine %>%
    dplyr::filter(animal_tag %in% base_tags) %>%
    dplyr::group_by(region, subregion, animal_tag, class_flex) %>%
    dplyr::summarise(population = sum(population, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = animal_tag, values_from = population, values_fill = 0)

  for (tag in base_tags) {
    if (!tag %in% names(base_pops_wide)) base_pops_wide[[tag]] <- 0
  }

  if (all(c(base_pops_wide$breeder_sows, base_pops_wide$boars) == 0)) {
    return(census_swine %>% dplyr::filter(animal_tag %in% valid_swine_tags))
  }

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
    warning("\u26A0 Demographic Warning (Swine): Replacement demand exceeds annual piglets produced. Check litter size (piglets_born/piglets_suckling) vs replacement_rate.")
  }

  # =========================================================================
  # Step 5: Format, Assemble & Return (Long Format)
  # =========================================================================
  all_pops_long <- calculated_pops %>%
    dplyr::select(
      region, subregion, class_flex,
      breeder_sows,
      boars,
      replacement_sows = pop_replacement_sows,
      fattening_pigs   = pop_fattening_pigs
    ) %>%
    tidyr::pivot_longer(
      cols = -c(region, subregion, class_flex),
      names_to = "animal_tag",
      values_to = "population"
    ) %>%
    # Offspring do not inherit productivity phases (class_flex) from parents
    dplyr::mutate(
      class_flex = dplyr::if_else(animal_tag %in% base_tags, class_flex, NA_character_)
    )

  final_swine_pop <- all_pops_long %>%
    dplyr::group_by(region, subregion, animal_tag, class_flex) %>%
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
                    sow_cycle_days,
                    farrowing_rate,
                    litter_size))
    message(sprintf("   \u2022 Replacement sows: %s head (repl rate %.1f%%)",
                    format(round(tot_repl_sows), big.mark = ","),
                    sow_replacement_rate * 100))
    if (tot_fattening > 0) {
      message(sprintf("   \u2022 Fattening pigs: %s standing barn places (%.0f days fattening -> %s slaughter pigs/yr)",
                      format(round(tot_fattening), big.mark = ","),
                      fattening_days,
                      format(round(total_slaughter), big.mark = ",")))
    }
  }

  return(final_swine_pop)
}
