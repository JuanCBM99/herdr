#' Calculate Sheep Population Structure
#'
#' Models the complete sheep flock structure from mature breeding animals using
#' biological prolificacy rates, reproductive cycle duration (lambing interval),
#' replacement dynamics, and feeding period days for slaughter lambs.
#'
#' @section Demographic Flow Logic:
#' \itemize{
#'   \item \strong{Mature Parents}: \code{mature_sheep_female_dairy}, \code{mature_sheep_female_meat},
#'         \code{mature_sheep_male_dairy}, \code{mature_sheep_male_meat}.
#'   \item \strong{Lambing Frequency}: Number of lambings per ewe per year (\code{365 / productive_period_days}),
#'         supporting accelerated lambing systems (e.g. 240 days = 1.5 lambings/year).
#'   \item \strong{Replacements}: Ewe and ram lambs retained annually to sustain the breeding flock.
#'   \item \strong{Births}: Total lambs born per year (\code{ewes * lambing_frequency * prolificacy}).
#'   \item \strong{Slaughter Lambs}: Remaining offspring (\code{Births - Replacements}) scaled to
#'         Average Annual Population (AAP, IPCC Eq. 10.1) based on feeding duration (\code{days / 365}).
#' }
#'
#' @param census_sheep Filtered census data for sheep.
#' @param rate_parameters Reproduction and replacement rates table.
#' @param definitions Optional definitions table (e.g., ruminant_definitions) containing \code{pr_sheep_goat}.
#' @param weights Optional weights table (e.g., livestock_weights) containing \code{productive_period_days}.
#'
#' @return A tibble with the modeled sheep population structure.
#' @export
calculate_population_sheep <- function(census_sheep, rate_parameters, definitions = NULL, weights = NULL) {

  message("\U0001F9EE Calculating populations for SHEEP...")

  # =========================================================================
  # Step 1: Identify Base Mature Cohorts & Valid Tags
  # =========================================================================
  base_tags <- c("mature_sheep_male_dairy", "mature_sheep_male_meat",
                 "mature_sheep_female_dairy", "mature_sheep_female_meat")

  present_bases <- intersect(base_tags, census_sheep$animal_tag[census_sheep$population > 0])
  if (length(present_bases) == 0) {
    return(census_sheep)
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

  # 2b. Durations of reproductive cycle (lambing interval) and slaughter lamb feeding period
  dairy_ewe_cycle_days <- get_period_days("mature_sheep_female_dairy", default_days = 365)
  meat_ewe_cycle_days  <- get_period_days("mature_sheep_female_meat", default_days = 365)

  dairy_lambing_freq <- if (dairy_ewe_cycle_days > 0) (365 / dairy_ewe_cycle_days) else 1.0
  meat_lambing_freq  <- if (meat_ewe_cycle_days > 0) (365 / meat_ewe_cycle_days) else 1.0

  dairy_lamb_days <- get_period_days("lamb_dairy_slaughter", default_days = 365)
  meat_lamb_days  <- get_period_days("lamb_meat_slaughter", default_days = 365)

  # 2c. Prolificacy per parturition (priority: definitions pr_sheep_goat -> rate_parameters pr_sheep_goat -> default)
  dairy_lambing_def <- get_def_val("pr_sheep_goat", "mature_sheep_female_dairy", default_val = 0)
  dairy_lambing     <- if (dairy_lambing_def > 0) dairy_lambing_def else get_rate("pr_sheep_goat", "mature_sheep_female_dairy", default_val = 1.27)

  meat_lambing_def  <- get_def_val("pr_sheep_goat", "mature_sheep_female_meat", default_val = 0)
  meat_lambing      <- if (meat_lambing_def > 0) meat_lambing_def else get_rate("pr_sheep_goat", "mature_sheep_female_meat", default_val = 1.0)

  # 2d. Replacement rates (priority: rate_parameters -> definitions -> defaults)
  dairy_male_repl   <- get_rate("replacement_rate", "mature_sheep_male_dairy", default_val = get_def_val("replacement_rate", "mature_sheep_male_dairy", default_val = 0.23))
  dairy_female_repl <- get_rate("replacement_rate", "mature_sheep_female_dairy", default_val = get_def_val("replacement_rate", "mature_sheep_female_dairy", default_val = 0.30))
  meat_male_repl    <- get_rate("replacement_rate", "mature_sheep_male_meat", default_val = get_def_val("replacement_rate", "mature_sheep_male_meat", default_val = 0.25))
  meat_female_repl  <- get_rate("replacement_rate", "mature_sheep_female_meat", default_val = get_def_val("replacement_rate", "mature_sheep_female_meat", default_val = 0.30))

  # =========================================================================
  # Step 3: Pivot Mature Base Populations (Wide Format)
  # =========================================================================
  base_pops_wide <- census_sheep %>%
    dplyr::filter(animal_tag %in% base_tags) %>%
    dplyr::group_by(region, subregion, animal_tag, class_flex) %>%
    dplyr::summarise(population = sum(population, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = animal_tag, values_from = population, values_fill = 0)

  for (tag in base_tags) {
    if (!tag %in% names(base_pops_wide)) base_pops_wide[[tag]] <- 0
  }

  if (nrow(base_pops_wide) == 0 ||
      all(c(base_pops_wide$mature_sheep_male_dairy, base_pops_wide$mature_sheep_male_meat,
            base_pops_wide$mature_sheep_female_dairy, base_pops_wide$mature_sheep_female_meat) == 0)) {
    return(census_sheep)
  }

  # =========================================================================
  # Step 4: Demographic Modeling Equations
  # =========================================================================
  calculated_pops <- base_pops_wide %>%
    dplyr::mutate(
      # 4a. Annual replacement lambs required to maintain mature stock
      pop_lamb_female_dairy_repl = mature_sheep_female_dairy * dairy_female_repl,
      pop_lamb_male_dairy_repl   = mature_sheep_male_dairy   * dairy_male_repl,
      pop_lamb_female_meat_repl  = mature_sheep_female_meat  * meat_female_repl,
      pop_lamb_male_meat_repl    = mature_sheep_male_meat    * meat_male_repl,

      # 4b. Annual lambs born from breeding females (lambings/year * prolificacy)
      total_dairy_births = mature_sheep_female_dairy * dairy_lambing_freq * dairy_lambing,
      total_meat_births  = mature_sheep_female_meat  * meat_lambing_freq  * meat_lambing,

      # 4c. Annual commercial slaughter lambs pool (Born - Replacements, clamped at 0)
      annual_dairy_slaughter = pmax(0, total_dairy_births - pop_lamb_female_dairy_repl - pop_lamb_male_dairy_repl),
      annual_meat_slaughter  = pmax(0, total_meat_births  - pop_lamb_female_meat_repl  - pop_lamb_male_meat_repl),

      # 4d. Standing population of slaughter lambs (IPCC Eq. 10.1 Average Annual Population, AAP)
      pop_lamb_dairy_slaughter = annual_dairy_slaughter * (dairy_lamb_days / 365),
      pop_lamb_meat_slaughter  = annual_meat_slaughter  * (meat_lamb_days / 365)
    )

  # Demographic integrity warnings (replacements exceeding available births)
  if (any(calculated_pops$pop_lamb_female_dairy_repl + calculated_pops$pop_lamb_male_dairy_repl > calculated_pops$total_dairy_births + 1e-6, na.rm = TRUE)) {
    warning("\u26A0 Demographic Warning (Dairy Sheep): Replacement demand exceeds lambs born. Check pr_sheep_goat (prolificacy) vs replacement_rate.")
  }
  if (any(calculated_pops$pop_lamb_female_meat_repl + calculated_pops$pop_lamb_male_meat_repl > calculated_pops$total_meat_births + 1e-6, na.rm = TRUE)) {
    warning("\u26A0 Demographic Warning (Meat Sheep): Replacement demand exceeds lambs born. Check pr_sheep_goat (prolificacy) vs replacement_rate.")
  }

  # =========================================================================
  # Step 5: Format, Assemble & Return (Long Format)
  # =========================================================================
  all_pops_long <- calculated_pops %>%
    dplyr::select(
      region, subregion, class_flex,
      mature_sheep_male_dairy, mature_sheep_male_meat,
      mature_sheep_female_dairy, mature_sheep_female_meat,
      lamb_female_dairy_replacement = pop_lamb_female_dairy_repl,
      lamb_male_dairy_replacement   = pop_lamb_male_dairy_repl,
      lamb_female_meat_replacement  = pop_lamb_female_meat_repl,
      lamb_male_meat_replacement    = pop_lamb_male_meat_repl,
      lamb_dairy_slaughter          = pop_lamb_dairy_slaughter,
      lamb_meat_slaughter           = pop_lamb_meat_slaughter
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

  final_sheep_pop <- all_pops_long %>%
    dplyr::group_by(region, subregion, animal_tag, class_flex) %>%
    dplyr::summarise(population = sum(population, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(round(population, 5) > 0)

  # Informative console summary messages
  tot_dairy_ewes      <- sum(final_sheep_pop$population[final_sheep_pop$animal_tag == "mature_sheep_female_dairy"], na.rm = TRUE)
  tot_meat_ewes       <- sum(final_sheep_pop$population[final_sheep_pop$animal_tag == "mature_sheep_female_meat"], na.rm = TRUE)
  tot_dairy_slaughter <- sum(final_sheep_pop$population[final_sheep_pop$animal_tag == "lamb_dairy_slaughter"], na.rm = TRUE)
  tot_meat_slaughter  <- sum(final_sheep_pop$population[final_sheep_pop$animal_tag == "lamb_meat_slaughter"], na.rm = TRUE)
  ann_dairy_slaughter <- sum(calculated_pops$annual_dairy_slaughter, na.rm = TRUE)
  ann_meat_slaughter  <- sum(calculated_pops$annual_meat_slaughter, na.rm = TRUE)

  if (tot_dairy_ewes > 0) {
    freq_msg <- if (dairy_ewe_cycle_days < 365) sprintf(", cycle %.0f days = %.2f lambings/yr", dairy_ewe_cycle_days, dairy_lambing_freq) else ""
    message(sprintf("   \u2022 Dairy sheep: %s ewes (prolificacy %.2f, repl %.1f%%%s)",
                    format(round(tot_dairy_ewes), big.mark = ","),
                    dairy_lambing,
                    dairy_female_repl * 100,
                    freq_msg))
    if (tot_dairy_slaughter > 0 && dairy_lamb_days < 365) {
      message(sprintf("   \u2022 Dairy slaughter lambs: %s standing places (AAP, %.0f days feed -> %s finished/yr)",
                      format(round(tot_dairy_slaughter, 1), big.mark = ","),
                      dairy_lamb_days,
                      format(round(ann_dairy_slaughter), big.mark = ",")))
    }
  }

  if (tot_meat_ewes > 0) {
    freq_msg <- if (meat_ewe_cycle_days < 365) sprintf(", cycle %.0f days = %.2f lambings/yr", meat_ewe_cycle_days, meat_lambing_freq) else ""
    message(sprintf("   \u2022 Meat sheep: %s ewes (prolificacy %.2f, repl %.1f%%%s)",
                    format(round(tot_meat_ewes), big.mark = ","),
                    meat_lambing,
                    meat_female_repl * 100,
                    freq_msg))
    if (tot_meat_slaughter > 0 && meat_lamb_days < 365) {
      message(sprintf("   \u2022 Meat slaughter lambs: %s standing places (AAP, %.0f days feed -> %s finished/yr)",
                      format(round(tot_meat_slaughter, 1), big.mark = ","),
                      meat_lamb_days,
                      format(round(ann_meat_slaughter), big.mark = ",")))
    }
  }

  return(final_sheep_pop)
}
