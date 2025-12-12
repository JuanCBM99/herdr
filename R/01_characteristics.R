#' Calculate Weighted Nutritional Variables (Refactored)
#'
#' Computes weighted averages of nutritional variables (DE, CP, NDF, Ash)
#' by mapping ingredients to diets, and diets to animals.
#' @export
calculate_weighted_variable <- function(saveoutput = TRUE) {

  message("🟢 Calculating Weighted Nutritional Variables...")

  # --- 1. Data Loading ---
  diet            <- load_dataset("diet")
  ingredients     <- load_dataset("ingredients")
  characteristics <- load_dataset("characteristics")
  categories      <- load_dataset("categories") # The mapping table

  # Quick validations
  stopifnot(
    all(c("group", "zone", "diet_tag", "forage_share", "feed_share") %in% names(diet)),
    all(c("diet_tag", "ingredient", "ingredient_share") %in% names(ingredients)),
    all(c("identification", "diet_tag") %in% names(categories))
  )

  # --- 2. Phase A: Calculate Nutritional Profile by Diet Tag ---
  # Goal: Obtain one row per diet with its weighted average values (DE, CP, etc.)

  diet_profiles <- diet %>%
    # Join ingredients and their nutritional characteristics
    dplyr::left_join(ingredients, by = c("group", "zone", "diet_tag")) %>%
    dplyr::left_join(characteristics, by = c("ingredient", "ingredient_type")) %>%

    dplyr::mutate(
      # Determine the percentage of the total diet this category represents
      category_share = dplyr::case_when(
        ingredient_type == "feed"          ~ feed_share,
        ingredient_type == "forage"        ~ forage_share,
        ingredient_type == "milk"          ~ milk_share,
        ingredient_type == "milk_replacer" ~ milk_replacer_share,
        TRUE                               ~ 0
      ),

      # Numeric cleanup (Convert to numeric and NAs to 0)
      across(
        c(de, cp, ash, ndf, ingredient_share, category_share),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),

      # Final Weighting Factor
      # (Ingredient Share % * Category Share %) / 10000
      weight_factor = (ingredient_share * category_share) / 10000
    ) %>%

    # Group by DIET (group, zone, diet_tag) to sum contributions
    dplyr::group_by(group, zone, diet_tag) %>%
    dplyr::summarise(
      de  = sum(de * weight_factor,  na.rm = TRUE),
      cp  = sum(cp * weight_factor,  na.rm = TRUE),
      ndf = sum(ndf * weight_factor, na.rm = TRUE),
      ash = sum(ash * weight_factor, na.rm = TRUE),
      .groups = "drop"
    )

  # --- 3. Phase B: Assign Diets to Animals ---
  # Goal: Expand the calculated profiles to each specific animal identification

  results <- categories %>%
    dplyr::select(identification, diet_tag, animal_type, animal_subtype) %>%

    # Join the calculated profiles
    dplyr::left_join(diet_profiles, by = "diet_tag") %>%

    # Filter out junk rows (if the join failed because group/zone was missing)
    dplyr::filter(!is.na(group)) %>%

    # Final Selection and Cleanup
    dplyr::select(
      group, zone, identification, animal_type, animal_subtype, diet_tag,
      de, cp, ndf, ash
    ) %>%
    dplyr::arrange(group, zone, identification) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))

  # --- 4. Save Output ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/variables.csv")
    message("💾 Saved output to output/variables.csv")
  }

  return(results)
}
