#' Calculate Weighted Nutritional Variables
#'
#' Computes weighted averages of nutritional variables (DE, CP, Cropland CP, NDF, ASH, GE, and monogastric DE/ME)
#' by mapping ingredients to diets and diets to animals, combining ruminant and monogastric (swine and poultry) definitions.
#'
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @param data_dir Character. Path to the folder containing input CSV files. Default is \code{"user_data"}.
#' @return Tibble with weighted nutritional variables for all animal cohorts.
#' @examples
#' \dontrun{
#' calculate_weighted_variable(saveoutput = FALSE, data_dir = "user_data")
#' }
#' @export
calculate_weighted_variable <- function(saveoutput = TRUE, data_dir = "user_data") {
  message("Calculating Weighted Nutritional Variables...")

  # --- 1. Load Data Assets ---
  diets <- readr::read_csv(file.path(data_dir, "diet_profiles.csv"), show_col_types = FALSE)
  ingredients <- readr::read_csv(file.path(data_dir, "diet_ingredients.csv"), show_col_types = FALSE)
  characteristics <- readr::read_csv(file.path(data_dir, "feed_characteristics.csv"), show_col_types = FALSE)
  definitions <- readr::read_csv(file.path(data_dir, "ruminant_definitions.csv"), show_col_types = FALSE)
  monogastric <- readr::read_csv(file.path(data_dir, "monogastric_definitions.csv"), show_col_types = FALSE)

  mapping_path <- file.path(data_dir, "mapping.csv")
  if (file.exists(mapping_path)) {
    mapping_raw <- readr::read_csv(mapping_path, show_col_types = FALSE)
    mapping_he <- mapping_raw %>%
      dplyr::transmute(
        ingredient,
        human_edible = if ("human_edible" %in% names(mapping_raw)) {
          suppressWarnings(as.numeric(human_edible))
        } else {
          0
        }
      ) %>%
      dplyr::distinct(ingredient, .keep_all = TRUE)
  } else {
    mapping_he <- dplyr::tibble(
      ingredient = character(),
      human_edible = numeric()
    )
  }

  norm_diets <- herdr_normalize_diets(diets, ingredients)
  diets <- norm_diets$diets
  ingredients <- norm_diets$ingredients
  diet_join_keys <- norm_diets$diet_join_keys

  characteristics <- characteristics %>%
    dplyr::distinct(ingredient, ingredient_type, .keep_all = TRUE)

  # --- 2. Consistency Validations (Shares must sum to 100%) ---
  herdr_validate_diet_shares(diets, ingredients, diet_join_keys = diet_join_keys)

  # --- 3. Compute Weighted Nutritional Profiles ---
  diet_profiles <- diets %>%
    dplyr::left_join(ingredients, by = diet_join_keys, relationship = "many-to-many") %>%
    dplyr::left_join(characteristics, by = c("ingredient", "ingredient_type")) %>%
    dplyr::left_join(mapping_he, by = "ingredient") %>%
    dplyr::mutate(
      dplyr::across(
        c(DE_pct, CP_pct, NDF_pct, ASH_pct, GE_feed_kcal_kg, swine_ME_kcal_kg, swine_DE_kcal_kg, poultry_ME_kcal_kg),
        ~ as.numeric(as.character(.))
      )
    ) %>%
    # Calculate absolute composition factor: (% ingredient within category * % category in diet) / 10000
    dplyr::mutate(
      weight_factor = (ingredient_share * dplyr::case_when(
        ingredient_type == "forage" ~ forage_share,
        ingredient_type == "concentrate" ~ concentrate_share,
        ingredient_type == "milk" ~ milk_share,
        ingredient_type == "milk_replacer" ~ milk_replacer_share,
        TRUE ~ 0
      )) / 10000
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(diet_join_keys))) %>%
    dplyr::summarise(
      forage_pct = dplyr::first(forage_share),
      # Human-edible feed CP (Mottet et al. 2017; Wilkinson 2011):
      # Must be computed BEFORE dplyr::across() overwrites CP_pct with the scalar diet average.
      # Uses `human_edible` (0/1 or fraction in [0, 1]) directly from mapping.csv.
      CP_cropland_pct = sum(
        CP_pct * pmin(pmax(dplyr::coalesce(human_edible, 0), 0), 1) * weight_factor,
        na.rm = TRUE
      ),
      dplyr::across(
        c(DE_pct, CP_pct, NDF_pct, ASH_pct, GE_feed_kcal_kg, swine_ME_kcal_kg, swine_DE_kcal_kg, poultry_ME_kcal_kg),
        ~ sum(. * weight_factor, na.rm = TRUE)
      ),
      .groups = "drop"
    )

  # --- 4. Consolidate Ruminant and Monogastric Mappings ---
  unified_definitions <- dplyr::bind_rows(
    definitions %>%
      dplyr::mutate(
        replacement_rate = as.numeric(as.character(replacement_rate))
      ),
    monogastric %>%
      dplyr::mutate(
        replacement_rate = as.numeric(as.character(replacement_rate))
      )
  )

  full_data <- unified_definitions %>%
    dplyr::select(farm_id, region, subregion, animal_tag, animal_type, animal_subtype, diet_tag, dplyr::any_of("production_role")) %>%
    dplyr::left_join(diet_profiles, by = diet_join_keys) %>%
    dplyr::filter(!is.na(region))

  if (!"production_role" %in% names(full_data)) {
    full_data$production_role <- NA_character_
  }

  # --- 5. Biological & Nutritional Threshold Checks (Mature Ruminants Only) ---
  mature_check <- full_data %>%
    dplyr::filter(
      tolower(animal_type) %in% c("cattle", "sheep", "goat"),
      tolower(trimws(as.character(production_role))) == "mature"
    )

  if (nrow(mature_check) > 0) {
    warn_forage_low <- mature_check %>% dplyr::filter(forage_pct < 30)
    if (nrow(warn_forage_low) > 0) {
      warning(paste0(
        "Warning (Forage): < 30% in: ", paste(unique(warn_forage_low$animal_tag), collapse = ", "),
        ". Risk of acidosis due to lack of structural fiber (Ref: NRC 2001)."
      ))
    }

    warn_forage_high <- mature_check %>% dplyr::filter(forage_pct > 80)
    if (nrow(warn_forage_high) > 0) {
      warning(paste0(
        "Warning (Forage): > 80% in: ", paste(unique(warn_forage_high$animal_tag), collapse = ", "),
        ". Likely to limit intake due to physical rumen fill (Ref: NRC 2001 / 1996)."
      ))
    }

    warn_cp_low <- mature_check %>% dplyr::filter(CP_pct < 7)
    if (nrow(warn_cp_low) > 0) {
      warning(paste0(
        "Warning (Protein): < 7% in: ", paste(unique(warn_cp_low$animal_tag), collapse = ", "),
        ". Insufficient to maintain basal rumen fermentation (Ref: CSIRO 2007)."
      ))
    }

    warn_cp_high <- mature_check %>% dplyr::filter(CP_pct > 20)
    if (nrow(warn_cp_high) > 0) {
      warning(paste0(
        "Warning (Protein): > 20% in: ", paste(unique(warn_cp_high$animal_tag), collapse = ", "),
        ". Excess causes high energy expenditure and reproductive/embryonic toxicity (Ref: NRC 2001)."
      ))
    }

    warn_ash <- mature_check %>% dplyr::filter(ASH_pct > 10)
    if (nrow(warn_ash) > 0) {
      warning(paste0(
        "Warning (Ash): > 10% in: ", paste(unique(warn_ash$animal_tag), collapse = ", "),
        ". Drastically increases water requirements due to osmotic stress (Ref: CSIRO 2007)."
      ))
    }
  }

  # --- 6. Formatting and File Export ---
  results <- full_data %>%
    dplyr::select(
      farm_id, region, subregion, animal_tag, animal_type, animal_subtype, diet_tag,
      DE_pct, CP_pct, CP_cropland_pct, NDF_pct, ASH_pct, GE_feed_kcal_kg, swine_ME_kcal_kg, swine_DE_kcal_kg, poultry_ME_kcal_kg
    )

  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/weighted_nutritional_variables.csv")
    message("[herdr] Saved nutritional variables to output/weighted_nutritional_variables.csv")
  }

  return(results)
}
