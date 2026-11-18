#' Calculate Small Ruminant Population Structure (Sheep and Goats)
#'
#' Models the complete sheep or goat flock/herd structure from mature breeding animals
#' using biological prolificacy rates, reproductive cycle duration (lambing or kidding
#' interval), replacement dynamics, and feeding period days for slaughter offspring.
#'
#' @section Demographic Flow Logic:
#' \itemize{
#'   \item \strong{Mature Parents}: \code{mature_<species>_female_dairy}, \code{mature_<species>_female_meat},
#'         \code{mature_<species>_male_dairy}, \code{mature_<species>_male_meat}
#'         (where \code{<species>} is \code{"sheep"} or \code{"goat"}).
#'   \item \strong{Parturition Frequency}: Number of lambings/kiddings per breeding female per year
#'         (\code{365 / productive_period_days}), supporting accelerated reproductive systems
#'         (e.g., 240 days = 1.52 parturitions/year).
#'   \item \strong{Replacements}: Female and male offspring retained annually to sustain the breeding herd.
#'   \item \strong{Births}: Total offspring born per year (\code{females * frequency * prolificacy}).
#'   \item \strong{Slaughter Offspring}: Remaining offspring (\code{Births - Replacements}) scaled to
#'         Average Annual Population (AAP, IPCC Eq. 10.1) based on feeding duration (\code{days / 365}).
#' }
#'
#' @param census_df Data.frame with filtered census data (\code{farm_id}, \code{region},
#'   \code{subregion}, \code{animal_tag}, \code{population} in head).
#' @param rate_parameters Optional data.frame of reproduction and replacement rates in long format
#'   (\code{animal_tag}, \code{parameter}, \code{value}).
#' @param definitions Optional definitions data.frame (e.g., \code{ruminant_definitions})
#'   containing \code{pr_sheep_goat} (lambs/kids per parturition) and \code{replacement_rate}.
#' @param weights Optional weights data.frame (e.g., \code{livestock_weights}) containing
#'   \code{productive_period_days} (days).
#' @param data_dir Character. Path to the folder containing input CSV files. Default is \code{"user_data"}.
#' @param species Character. Small ruminant species: \code{"sheep"} or \code{"goat"}. When omitted,
#'   inferred automatically from \code{census_df} (\code{"goat"} if goat tags/types are present
#'   without sheep, otherwise \code{"sheep"}).
#'
#' @return A tibble with columns \code{farm_id}, \code{region}, \code{subregion},
#'   \code{animal_tag}, and \code{population} (head/year or AAP) for all non-zero cohorts.
#' @examples
#' \dontrun{
#' census <- tibble::tibble(
#'   farm_id = "farm_1",
#'   animal_tag = "mature_sheep_female_meat",
#'   region = "spain",
#'   subregion = "aragon",
#'   population = 1000
#' )
#' calculate_population_small_ruminants(census, species = "sheep")
#' }
#' @export
calculate_population_small_ruminants <- function(census_df,
                                                 rate_parameters = NULL,
                                                 definitions = NULL,
                                                 weights = NULL,
                                                 data_dir = "user_data",
                                                 species = c("sheep", "goat")) {
  if (missing(species) || length(species) > 1L) {
    tag_or_type <- c(
      if ("animal_tag" %in% names(census_df)) census_df$animal_tag,
      if ("animal_type" %in% names(census_df)) census_df$animal_type
    )
    has_goat  <- any(grepl("goat",  tag_or_type, ignore.case = TRUE))
    has_sheep <- any(grepl("sheep", tag_or_type, ignore.case = TRUE))
    species <- if (has_goat && !has_sheep) "goat" else "sheep"
  } else {
    species <- match.arg(tolower(as.character(species[1L])), c("sheep", "goat"))
  }

  is_sheep <- species == "sheep"

  species_upper    <- if (is_sheep) "SHEEP" else "GOAT"
  species_title    <- if (is_sheep) "Sheep" else "Goat"
  adult_female_lbl <- if (is_sheep) "ewes" else "does"
  offspring        <- if (is_sheep) "lamb" else "kid_goat"
  offspring_plural <- if (is_sheep) "lambs" else "kids"
  parturition_lbl  <- if (is_sheep) "lambings" else "kiddings"

  message(sprintf("Calculating populations for %s...", species_upper))

  norm_params <- herdr_normalize_rate_params(rate_parameters, definitions)
  rate_parameters <- norm_params$rate_parameters
  definitions <- norm_params$definitions

  tag_m_dairy      <- paste0("mature_", species, "_male_dairy")
  tag_m_meat       <- paste0("mature_", species, "_male_meat")
  tag_f_dairy      <- paste0("mature_", species, "_female_dairy")
  tag_f_meat       <- paste0("mature_", species, "_female_meat")
  tag_slau_dairy   <- paste0(offspring, "_dairy_slaughter")
  tag_slau_meat    <- paste0(offspring, "_meat_slaughter")
  tag_repl_f_dairy <- paste0(offspring, "_female_dairy_replacement")
  tag_repl_m_dairy <- paste0(offspring, "_male_dairy_replacement")
  tag_repl_f_meat  <- paste0(offspring, "_female_meat_replacement")
  tag_repl_m_meat  <- paste0(offspring, "_male_meat_replacement")

  base_tags <- c(tag_m_dairy, tag_m_meat, tag_f_dairy, tag_f_meat)
  present_bases <- intersect(base_tags, census_df$animal_tag[census_df$population > 0])
  if (length(present_bases) == 0) {
    return(census_df)
  }

  base_pops_wide <- census_df %>%
    dplyr::filter(animal_tag %in% base_tags) %>%
    dplyr::group_by(farm_id, region, subregion, animal_tag) %>%
    dplyr::summarise(population = sum(population, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = animal_tag, values_from = population, values_fill = 0)

  for (tag in base_tags) {
    if (!tag %in% names(base_pops_wide)) base_pops_wide[[tag]] <- 0
  }

  weights_df <- herdr_load_weights_df(weights, data_dir)

  dairy_cycle_days <- herdr_resolve_period_days(base_pops_wide, tag_f_dairy, weights = weights_df, default_days = 365)
  meat_cycle_days  <- herdr_resolve_period_days(base_pops_wide, tag_f_meat,  weights = weights_df, default_days = 365)

  dairy_freq <- ifelse(dairy_cycle_days > 0, 365 / dairy_cycle_days, 1.0)
  meat_freq  <- ifelse(meat_cycle_days > 0, 365 / meat_cycle_days, 1.0)

  dairy_offspring_days <- herdr_resolve_period_days(base_pops_wide, tag_slau_dairy, weights = weights_df, default_days = 365)
  meat_offspring_days  <- herdr_resolve_period_days(base_pops_wide, tag_slau_meat,  weights = weights_df, default_days = 365)

  dairy_prolificacy <- herdr_resolve_demographic_param(base_pops_wide, tag_f_dairy, "pr_sheep_goat",
                                                       definitions = definitions, rate_parameters = rate_parameters, default_val = 1.27)
  meat_prolificacy  <- herdr_resolve_demographic_param(base_pops_wide, tag_f_meat,  "pr_sheep_goat",
                                                       definitions = definitions, rate_parameters = rate_parameters, default_val = 1.0)

  dairy_male_repl   <- herdr_resolve_demographic_param(base_pops_wide, tag_m_dairy, "replacement_rate",
                                                       definitions = definitions, rate_parameters = rate_parameters, default_val = 0.23)
  dairy_female_repl <- herdr_resolve_demographic_param(base_pops_wide, tag_f_dairy, "replacement_rate",
                                                       definitions = definitions, rate_parameters = rate_parameters, default_val = 0.30)
  meat_male_repl    <- herdr_resolve_demographic_param(base_pops_wide, tag_m_meat,  "replacement_rate",
                                                       definitions = definitions, rate_parameters = rate_parameters, default_val = 0.25)
  meat_female_repl  <- herdr_resolve_demographic_param(base_pops_wide, tag_f_meat,  "replacement_rate",
                                                       definitions = definitions, rate_parameters = rate_parameters, default_val = 0.30)

  pop_f_dairy_repl <- base_pops_wide[[tag_f_dairy]] * dairy_female_repl
  pop_m_dairy_repl <- base_pops_wide[[tag_m_dairy]] * dairy_male_repl
  pop_f_meat_repl  <- base_pops_wide[[tag_f_meat]]  * meat_female_repl
  pop_m_meat_repl  <- base_pops_wide[[tag_m_meat]]  * meat_male_repl

  tot_dairy_births <- base_pops_wide[[tag_f_dairy]] * dairy_freq * dairy_prolificacy
  tot_meat_births  <- base_pops_wide[[tag_f_meat]]  * meat_freq  * meat_prolificacy

  ann_dairy_slaughter <- pmax(0, tot_dairy_births - pop_f_dairy_repl - pop_m_dairy_repl)
  ann_meat_slaughter  <- pmax(0, tot_meat_births  - pop_f_meat_repl  - pop_m_meat_repl)

  pop_dairy_slaughter <- ann_dairy_slaughter * (dairy_offspring_days / 365)
  pop_meat_slaughter  <- ann_meat_slaughter  * (meat_offspring_days / 365)

  if (any((pop_f_dairy_repl + pop_m_dairy_repl > tot_dairy_births + 1e-6) & dairy_prolificacy > 0, na.rm = TRUE)) {
    warning(sprintf(
      "Demographic Warning (Dairy %s): Replacement demand exceeds %s born. Check pr_sheep_goat (prolificacy) vs replacement_rate.",
      species_title, offspring_plural
    ))
  }
  if (any((pop_f_meat_repl + pop_m_meat_repl > tot_meat_births + 1e-6) & meat_prolificacy > 0, na.rm = TRUE)) {
    warning(sprintf(
      "Demographic Warning (Meat %s): Replacement demand exceeds %s born. Check pr_sheep_goat (prolificacy) vs replacement_rate.",
      species_title, offspring_plural
    ))
  }

  wide_out <- base_pops_wide[, c("farm_id", "region", "subregion", base_tags), drop = FALSE]
  wide_out[[tag_repl_f_dairy]] <- pop_f_dairy_repl
  wide_out[[tag_repl_m_dairy]] <- pop_m_dairy_repl
  wide_out[[tag_repl_f_meat]]  <- pop_f_meat_repl
  wide_out[[tag_repl_m_meat]]  <- pop_m_meat_repl
  wide_out[[tag_slau_dairy]]   <- pop_dairy_slaughter
  wide_out[[tag_slau_meat]]    <- pop_meat_slaughter

  final_pop <- wide_out %>%
    tidyr::pivot_longer(
      cols = -c(farm_id, region, subregion),
      names_to = "animal_tag",
      values_to = "population"
    ) %>%
    dplyr::group_by(farm_id, region, subregion, animal_tag) %>%
    dplyr::summarise(population = sum(population, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(round(population, 5) > 0)

  tot_dairy_f         <- sum(final_pop$population[final_pop$animal_tag == tag_f_dairy], na.rm = TRUE)
  tot_meat_f          <- sum(final_pop$population[final_pop$animal_tag == tag_f_meat], na.rm = TRUE)
  tot_dairy_slaughter <- sum(final_pop$population[final_pop$animal_tag == tag_slau_dairy], na.rm = TRUE)
  tot_meat_slaughter  <- sum(final_pop$population[final_pop$animal_tag == tag_slau_meat], na.rm = TRUE)
  sum_ann_dairy_slau  <- sum(ann_dairy_slaughter, na.rm = TRUE)
  sum_ann_meat_slau   <- sum(ann_meat_slaughter, na.rm = TRUE)

  if (tot_dairy_f > 0) {
    idx_d <- which(base_pops_wide[[tag_f_dairy]] > 0)[1]
    if (is.na(idx_d)) idx_d <- 1L
    freq_msg <- if (dairy_cycle_days[idx_d] < 365) sprintf(", cycle %.0f days = %.2f %s/yr", dairy_cycle_days[idx_d], dairy_freq[idx_d], parturition_lbl) else ""
    message(sprintf("   \u2022 Dairy %s: %s %s (prolificacy %.2f, repl %.1f%%%s)",
                    species,
                    format(round(tot_dairy_f), big.mark = ","),
                    adult_female_lbl,
                    dairy_prolificacy[idx_d],
                    dairy_female_repl[idx_d] * 100,
                    freq_msg))
    if (tot_dairy_slaughter > 0 && dairy_offspring_days[idx_d] < 365) {
      message(sprintf("   \u2022 Dairy slaughter %s: %s standing places (AAP, %.0f days feed -> %s finished/yr)",
                      offspring_plural,
                      format(round(tot_dairy_slaughter, 1), big.mark = ","),
                      dairy_offspring_days[idx_d],
                      format(round(sum_ann_dairy_slau), big.mark = ",")))
    }
  }

  if (tot_meat_f > 0) {
    idx_m <- which(base_pops_wide[[tag_f_meat]] > 0)[1]
    if (is.na(idx_m)) idx_m <- 1L
    freq_msg <- if (meat_cycle_days[idx_m] < 365) sprintf(", cycle %.0f days = %.2f %s/yr", meat_cycle_days[idx_m], meat_freq[idx_m], parturition_lbl) else ""
    message(sprintf("   \u2022 Meat %s: %s %s (prolificacy %.2f, repl %.1f%%%s)",
                    species,
                    format(round(tot_meat_f), big.mark = ","),
                    adult_female_lbl,
                    meat_prolificacy[idx_m],
                    meat_female_repl[idx_m] * 100,
                    freq_msg))
    if (tot_meat_slaughter > 0 && meat_offspring_days[idx_m] < 365) {
      message(sprintf("   \u2022 Meat slaughter %s: %s standing places (AAP, %.0f days feed -> %s finished/yr)",
                      offspring_plural,
                      format(round(tot_meat_slaughter, 1), big.mark = ","),
                      meat_offspring_days[idx_m],
                      format(round(sum_ann_meat_slau), big.mark = ",")))
    }
  }

  final_pop
}
