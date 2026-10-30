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
#' @param data_dir Character. Path to the folder containing input CSV files. Default is \code{"user_data"}.
#'
#' @return A tibble with the modeled cattle population structure.
#' @export
calculate_population_cattle <- function(census_cattle, rate_parameters = NULL, definitions = NULL, weights = NULL, data_dir = "user_data") {

  message("Calculating populations for CATTLE...")

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
  base_tags <- c("mature_beef_bull", "mature_dairy_cattle", "mature_beef_cattle")

  present_bases <- intersect(base_tags, census_cattle$animal_tag[census_cattle$population > 0])
  if (length(present_bases) == 0) {
    return(census_cattle)
  }

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

  # =========================================================================
  # Step 2: Pivot Mature Base Populations (Wide Format)
  # =========================================================================
  base_pops_wide <- census_cattle %>%
    dplyr::filter(animal_tag %in% base_tags) %>%
    dplyr::group_by(farm_id, region, subregion, animal_tag) %>%
    dplyr::summarise(population = sum(population, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = animal_tag, values_from = population, values_fill = 0)

  for (tag in base_tags) {
    if (!tag %in% names(base_pops_wide)) base_pops_wide[[tag]] <- 0
  }

  if (all(c(base_pops_wide$mature_beef_bull, base_pops_wide$mature_dairy_cattle, base_pops_wide$mature_beef_cattle) == 0)) {
    return(census_cattle)
  }

  # =========================================================================
  # Step 3: Retrieve Biological & Zootechnical Parameters (Stratum-Aware)
  # =========================================================================
  # 3a. Pregnancy rates (priority: definitions pregnancy_rate -> rate_parameters pregnancy_rate -> default)
  dairy_calving <- herdr_resolve_demographic_param(base_pops_wide, "mature_dairy_cattle", "pregnancy_rate",
                                                  definitions = definitions, rate_parameters = rate_parameters, default_val = 0.86)
  beef_calving  <- herdr_resolve_demographic_param(base_pops_wide, "mature_beef_cattle",  "pregnancy_rate",
                                                  definitions = definitions, rate_parameters = rate_parameters, default_val = 0.79)

  # 3b. Replacement rates (priority: definitions -> rate_parameters -> defaults)
  beef_male_repl    <- herdr_resolve_demographic_param(base_pops_wide, "mature_beef_bull",    "replacement_rate",
                                                      definitions = definitions, rate_parameters = rate_parameters, default_val = 0.125)
  beef_female_repl  <- herdr_resolve_demographic_param(base_pops_wide, "mature_beef_cattle",  "replacement_rate",
                                                      definitions = definitions, rate_parameters = rate_parameters, default_val = 0.15)
  dairy_female_repl <- herdr_resolve_demographic_param(base_pops_wide, "mature_dairy_cattle", "replacement_rate",
                                                      definitions = definitions, rate_parameters = rate_parameters, default_val = 0.27)

  # 3c. Feeding durations for IPCC Average Annual Population (AAP) scaling
  feedlot_days_male   <- herdr_resolve_period_days(base_pops_wide, "feedlot_calves_male", weights = weights_df, default_days = 365)
  feedlot_days_female <- herdr_resolve_period_days(base_pops_wide, "feedlot_calves_female", weights = weights_df, default_days = feedlot_days_male)
  beef_days_male      <- herdr_resolve_period_days(base_pops_wide, "beef_calves_male", weights = weights_df, default_days = 365)
  beef_days_female    <- herdr_resolve_period_days(base_pops_wide, "beef_calves_female", weights = weights_df, default_days = beef_days_male)

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
    warning("Demographic Warning (Dairy Cattle): Female replacement demand exceeds female calves born. Check pregnancy_rate vs replacement_rate.")
  }
  if (any(calculated_pops$pop_beef_calves_female_repl > calculated_pops$beef_births_half + 1e-6, na.rm = TRUE)) {
    warning("Demographic Warning (Beef Cattle): Female replacement demand exceeds female calves born. Check pregnancy_rate vs replacement_rate.")
  }
  if (any(calculated_pops$pop_beef_calves_male_repl > calculated_pops$beef_births_half + 1e-6, na.rm = TRUE)) {
    warning("Demographic Warning (Beef Bulls): Male replacement demand exceeds male calves born. Check pregnancy_rate vs replacement_rate.")
  }

  # =========================================================================
  # Step 5: Format, Assemble & Return (Long Format)
  # =========================================================================
  all_pops_long <- calculated_pops %>%
    dplyr::select(
      farm_id, region, subregion,
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
      cols = -c(farm_id, region, subregion),
      names_to = "animal_tag",
      values_to = "population"
    )

  final_cattle_pop <- all_pops_long %>%
    dplyr::group_by(farm_id, region, subregion, animal_tag) %>%
    dplyr::summarise(population = sum(population, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(round(population, 5) > 0)

  # Informative console summary messages
  tot_dairy          <- sum(final_cattle_pop$population[final_cattle_pop$animal_tag == "mature_dairy_cattle"], na.rm = TRUE)
  tot_beef_cows      <- sum(final_cattle_pop$population[final_cattle_pop$animal_tag == "mature_beef_cattle"], na.rm = TRUE)
  tot_feedlot_calves <- sum(final_cattle_pop$population[grepl("feedlot_calves", final_cattle_pop$animal_tag)], na.rm = TRUE)

  if (tot_dairy > 0) {
    avg_feedlot_days <- mean(feedlot_days_male[base_pops_wide$mature_dairy_cattle > 0], na.rm = TRUE)
    feedlot_note <- if (any(feedlot_days_male < 365) || any(feedlot_days_female < 365)) {
      sprintf(" -> %s standing feedlot calves (AAP, %.0f days)", format(round(tot_feedlot_calves, 1), big.mark = ","), avg_feedlot_days)
    } else {
      sprintf(" -> %s feedlot calves to fattening", format(round(tot_feedlot_calves), big.mark = ","))
    }
    calv_vals <- unique(round(dairy_calving[base_pops_wide$mature_dairy_cattle > 0] * 100, 1))
    repl_vals <- unique(round(dairy_female_repl[base_pops_wide$mature_dairy_cattle > 0] * 100, 1))
    calv_str <- if (length(calv_vals) == 1) sprintf("%.1f%%", calv_vals) else sprintf("%.1f%%-%.1f%%", min(calv_vals), max(calv_vals))
    repl_str <- if (length(repl_vals) == 1) sprintf("%.1f%%", repl_vals) else sprintf("%.1f%%-%.1f%%", min(repl_vals), max(repl_vals))
    message(sprintf("   \u2022 Dairy cattle: %s cows (pregnancy rate %s, repl %s)%s",
                    format(round(tot_dairy), big.mark = ","),
                    calv_str,
                    repl_str,
                    feedlot_note))
  }

  if (tot_beef_cows > 0) {
    tot_beef_slaughter <- sum(final_cattle_pop$population[final_cattle_pop$animal_tag %in% c("beef_calves_male", "beef_calves_female")], na.rm = TRUE)
    avg_beef_days <- mean(beef_days_male[base_pops_wide$mature_beef_cattle > 0], na.rm = TRUE)
    beef_note <- if (any(beef_days_male < 365) || any(beef_days_female < 365)) {
      sprintf(" -> %s standing beef calves (AAP, %.0f days)", format(round(tot_beef_slaughter, 1), big.mark = ","), avg_beef_days)
    } else {
      sprintf(" -> %s beef calves to slaughter", format(round(tot_beef_slaughter), big.mark = ","))
    }
    beef_calv_vals <- unique(round(beef_calving[base_pops_wide$mature_beef_cattle > 0] * 100, 1))
    beef_repl_vals <- unique(round(beef_female_repl[base_pops_wide$mature_beef_cattle > 0] * 100, 1))
    beef_calv_str <- if (length(beef_calv_vals) == 1) sprintf("%.1f%%", beef_calv_vals) else sprintf("%.1f%%-%.1f%%", min(beef_calv_vals), max(beef_calv_vals))
    beef_repl_str <- if (length(beef_repl_vals) == 1) sprintf("%.1f%%", beef_repl_vals) else sprintf("%.1f%%-%.1f%%", min(beef_repl_vals), max(beef_repl_vals))
    message(sprintf("   \u2022 Beef cattle: %s cows (pregnancy rate %s, repl %s)%s",
                    format(round(tot_beef_cows), big.mark = ","),
                    beef_calv_str,
                    beef_repl_str,
                    beef_note))
  }

  return(final_cattle_pop)
}
