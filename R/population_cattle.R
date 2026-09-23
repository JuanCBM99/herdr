#' Calculate Cattle Population Structure
#'
#' Models the complete cattle herd structure from mature breeding animals using
#' biological reproduction rates, replacement dynamics, and feeding period durations.
#'
#' @section Demographic Flow Logic:
#' \itemize{
#'   \item \strong{Mature Parents}: \code{mature_dairy_cattle}, \code{mature_beef_cattle}, \code{mature_beef_bull}.
#'   \item \strong{Replacements}: Female and male calves needed annually to maintain mature stock.
#'   \item \strong{Births}: Calculated assuming an equal 50:50 sex ratio (calves divided by 2).
#'   \item \strong{Slaughter / Feedlot Calves}: Non-replacement offspring scaled to Average Annual
#'         Population (AAP, IPCC Eq. 10.1) based on feeding duration (\code{days / 365}).
#'   \item \strong{Yearlings}: Retained replacement calves mapped 1:1 into yearling cohorts.
#' }
#'
#' @param census_cattle Filtered census data for cattle.
#' @param rate_parameters Reproduction and replacement rates table.
#' @param definitions Optional definitions table (e.g., ruminant_definitions) containing \code{pregnancy_rate}.
#' @param weights Optional weights table (e.g., livestock_weights) containing \code{productive_period_days}.
#'
#' @return A tibble with the modeled cattle population structure.
#' @export
calculate_population_cattle <- function(census_cattle, rate_parameters, definitions = NULL, weights = NULL) {

  message("\U0001F9EE Calculating populations for CATTLE...")

  # =========================================================================
  # Step 1: Identify Base Mature Cohorts & Valid Tags
  # =========================================================================
  base_tags <- c("mature_beef_bull", "mature_dairy_cattle", "mature_beef_cattle")

  present_bases <- intersect(base_tags, census_cattle$animal_tag[census_cattle$population > 0])
  if (length(present_bases) == 0) {
    return(census_cattle)
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
    # Check generic tag without sex suffix (e.g., feedlot_calves from feedlot_calves_male)
    generic_tag <- sub("_(male|female)$", "", tag)
    if (generic_tag != tag) {
      val_gen <- weights_df %>%
        dplyr::filter(animal_tag == generic_tag) %>%
        dplyr::pull(productive_period_days)
      if (length(val_gen) > 0 && !is.na(val_gen[1])) {
        num_gen <- suppressWarnings(as.numeric(val_gen[1]))
        if (!is.na(num_gen) && num_gen > 0) return(num_gen)
      }
    }
    return(default_days)
  }

  # 2b. Pregnancy rates (priority: definitions pregnancy_rate -> rate_parameters pregnancy_rate -> default)
  dairy_calving_def <- get_def_val("pregnancy_rate", "mature_dairy_cattle", default_val = 0)
  dairy_calving     <- if (dairy_calving_def > 0) dairy_calving_def else get_rate("pregnancy_rate", "mature_dairy_cattle", default_val = 0.86)

  beef_calving_def  <- get_def_val("pregnancy_rate", "mature_beef_cattle", default_val = 0)
  beef_calving      <- if (beef_calving_def > 0) beef_calving_def else get_rate("pregnancy_rate", "mature_beef_cattle", default_val = 0.79)

  # 2c. Replacement rates (priority: rate_parameters -> definitions -> defaults)
  beef_male_repl    <- get_rate("replacement_rate", "mature_beef_bull", default_val = get_def_val("replacement_rate", "mature_beef_bull", default_val = 0.125))
  beef_female_repl  <- get_rate("replacement_rate", "mature_beef_cattle", default_val = get_def_val("replacement_rate", "mature_beef_cattle", default_val = 0.15))
  dairy_female_repl <- get_rate("replacement_rate", "mature_dairy_cattle", default_val = get_def_val("replacement_rate", "mature_dairy_cattle", default_val = 0.27))

  # 2d. Feeding durations for IPCC Average Annual Population (AAP) scaling
  feedlot_days_male   <- get_period_days("feedlot_calves_male", default_days = 365)
  feedlot_days_female <- get_period_days("feedlot_calves_female", default_days = feedlot_days_male)
  beef_days_male      <- get_period_days("beef_calves_male", default_days = 365)
  beef_days_female    <- get_period_days("beef_calves_female", default_days = beef_days_male)

  # =========================================================================
  # Step 3: Pivot Mature Base Populations (Wide Format)
  # =========================================================================
  base_pops_wide <- census_cattle %>%
    dplyr::filter(animal_tag %in% base_tags) %>%
    dplyr::group_by(region, subregion, animal_tag, class_flex) %>%
    dplyr::summarise(population = sum(population, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = animal_tag, values_from = population, values_fill = 0)

  for (tag in base_tags) {
    if (!tag %in% names(base_pops_wide)) base_pops_wide[[tag]] <- 0
  }

  if (all(c(base_pops_wide$mature_beef_bull, base_pops_wide$mature_dairy_cattle, base_pops_wide$mature_beef_cattle) == 0)) {
    return(census_cattle)
  }

  # =========================================================================
  # Step 4: Demographic Modeling Equations
  # =========================================================================
  calculated_pops <- base_pops_wide %>%
    dplyr::mutate(
      # 4a. Annual replacement calves required to maintain mature stock
      pop_beef_calves_male_repl    = mature_beef_bull   * beef_male_repl,
      pop_beef_calves_female_repl  = mature_beef_cattle * beef_female_repl,
      pop_dairy_calves_female_repl = mature_dairy_cattle * dairy_female_repl,

      # 4b. Annual calves born by sex (assuming a 50:50 sex ratio)
      dairy_births_half = (mature_dairy_cattle * dairy_calving) / 2,
      beef_births_half  = (mature_beef_cattle  * beef_calving)  / 2,

      # 4c. Commercial feedlot and slaughter calves (Born - Replacements, scaled to IPCC AAP)
      pop_feedlot_calves_male   = dairy_births_half * (feedlot_days_male / 365),
      pop_feedlot_calves_female = pmax(0, dairy_births_half - pop_dairy_calves_female_repl) * (feedlot_days_female / 365),
      pop_beef_calves_male      = pmax(0, beef_births_half  - pop_beef_calves_male_repl)   * (beef_days_male / 365),
      pop_beef_calves_female    = pmax(0, beef_births_half  - pop_beef_calves_female_repl) * (beef_days_female / 365)
    )

  # Demographic integrity warnings (replacements exceeding available births)
  if (any(calculated_pops$pop_dairy_calves_female_repl > calculated_pops$dairy_births_half + 1e-6, na.rm = TRUE)) {
    warning("\u26A0 Demographic Warning (Dairy Cattle): Female replacement demand exceeds female calves born. Check pregnancy_rate vs replacement_rate.")
  }
  if (any(calculated_pops$pop_beef_calves_female_repl > calculated_pops$beef_births_half + 1e-6, na.rm = TRUE)) {
    warning("\u26A0 Demographic Warning (Beef Cattle): Female replacement demand exceeds female calves born. Check pregnancy_rate vs replacement_rate.")
  }
  if (any(calculated_pops$pop_beef_calves_male_repl > calculated_pops$beef_births_half + 1e-6, na.rm = TRUE)) {
    warning("\u26A0 Demographic Warning (Beef Bulls): Male replacement demand exceeds male calves born. Check pregnancy_rate vs replacement_rate.")
  }

  # =========================================================================
  # Step 5: Format, Assemble & Return (Long Format)
  # =========================================================================
  all_pops_long <- calculated_pops %>%
    dplyr::select(
      region, subregion, class_flex,
      mature_beef_bull, mature_dairy_cattle, mature_beef_cattle,
      feedlot_calves_male             = pop_feedlot_calves_male,
      feedlot_calves_female           = pop_feedlot_calves_female,
      beef_calves_male                = pop_beef_calves_male,
      beef_calves_female              = pop_beef_calves_female,
      beef_calves_male_replacement    = pop_beef_calves_male_repl,
      beef_calves_female_replacement  = pop_beef_calves_female_repl,
      dairy_calves_female_replacement = pop_dairy_calves_female_repl
    ) %>%
    # 5a. Map replacement yearlings 1:1 from replacement calves
    dplyr::mutate(
      beef_yearlings_male_replacement    = beef_calves_male_replacement,
      beef_yearlings_female_replacement  = beef_calves_female_replacement,
      dairy_yearlings_female_replacement = dairy_calves_female_replacement
    ) %>%
    tidyr::pivot_longer(
      cols = -c(region, subregion, class_flex),
      names_to = "animal_tag",
      values_to = "population"
    ) %>%
    # 5b. Offspring do not inherit productivity phases (class_flex) from parents
    dplyr::mutate(
      class_flex = dplyr::if_else(animal_tag %in% base_tags, class_flex, NA_character_)
    )

  final_cattle_pop <- all_pops_long %>%
    dplyr::group_by(region, subregion, animal_tag, class_flex) %>%
    dplyr::summarise(population = sum(population, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(round(population, 5) > 0)

  # Informative console summary messages
  tot_dairy          <- sum(final_cattle_pop$population[final_cattle_pop$animal_tag == "mature_dairy_cattle"], na.rm = TRUE)
  tot_beef_cows      <- sum(final_cattle_pop$population[final_cattle_pop$animal_tag == "mature_beef_cattle"], na.rm = TRUE)
  tot_feedlot_calves <- sum(final_cattle_pop$population[grepl("feedlot_calves", final_cattle_pop$animal_tag)], na.rm = TRUE)

  if (tot_dairy > 0) {
    feedlot_note <- if (feedlot_days_male < 365 || feedlot_days_female < 365) {
      sprintf(" -> %s standing feedlot calves (AAP, %.0f days)", format(round(tot_feedlot_calves, 1), big.mark = ","), feedlot_days_male)
    } else {
      sprintf(" -> %s feedlot calves to fattening", format(round(tot_feedlot_calves), big.mark = ","))
    }
    message(sprintf("   \u2022 Dairy cattle: %s cows (pregnancy rate %.1f%%, repl %.1f%%)%s",
                    format(round(tot_dairy), big.mark = ","),
                    dairy_calving * 100,
                    dairy_female_repl * 100,
                    feedlot_note))
  }

  if (tot_beef_cows > 0) {
    tot_beef_slaughter <- sum(final_cattle_pop$population[final_cattle_pop$animal_tag %in% c("beef_calves_male", "beef_calves_female")], na.rm = TRUE)
    beef_note <- if (beef_days_male < 365 || beef_days_female < 365) {
      sprintf(" -> %s standing beef calves (AAP, %.0f days)", format(round(tot_beef_slaughter, 1), big.mark = ","), beef_days_male)
    } else {
      sprintf(" -> %s beef calves to slaughter", format(round(tot_beef_slaughter), big.mark = ","))
    }
    message(sprintf("   \u2022 Beef cattle: %s cows (pregnancy rate %.1f%%, repl %.1f%%)%s",
                    format(round(tot_beef_cows), big.mark = ","),
                    beef_calving * 100,
                    beef_female_repl * 100,
                    beef_note))
  }

  return(final_cattle_pop)
}
