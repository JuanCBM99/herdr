#' Calculate Weighted Nutritional Variables
#'
#' Computes weighted averages of nutritional variables (DE, CP, NDF, Ash, EB)
#' by mapping ingredients to diets and diets to animals.
#'
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @export
calculate_weighted_variable <- function(saveoutput = TRUE) {

  message("\U0001f7e2 Calculating Weighted Nutritional Variables...")

  # --- 1. Load Data ---
  diets           <- readr::read_csv("user_data/diet_profiles.csv", show_col_types = FALSE)
  ingredients     <- readr::read_csv("user_data/diet_ingredients.csv", show_col_types = FALSE)
  characteristics <- readr::read_csv("user_data/feed_characteristics.csv", show_col_types = FALSE)
  definitions     <- readr::read_csv("user_data/livestock_definitions.csv", show_col_types = FALSE)

  # --- 2. Data Integrity Assertions ---

  # A. Check Diet Profiles sum to 100%
  diet_sum_check <- diets %>%
    dplyr::mutate(total_diet = forage_share + concentrate_share + milk_share + milk_replacer_share) %>%
    dplyr::filter(abs(total_diet - 100) > 0.1)

  assertthat::assert_that(nrow(diet_sum_check) == 0,
                          msg = paste("Diet Profile Error: Shares do not sum to 100% in 'diet_profiles.csv' for:",
                                      paste(diet_sum_check$diet_tag, collapse = ", ")))

  # B. Check Ingredient Shares within each category sum to 100%
  ing_sum_check <- ingredients %>%
    dplyr::group_by(diet_tag, region, subregion, class_flex, ingredient_type) %>%
    dplyr::summarise(total_ing = sum(ingredient_share, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(abs(total_ing - 100) > 0.1)

  assertthat::assert_that(nrow(ing_sum_check) == 0,
                          msg = paste("Ingredient Error: Shares do not sum to 100% within a category in 'diet_ingredients.csv' for:",
                                      paste(unique(ing_sum_check$diet_tag), collapse = ", ")))

  # --- 3. Build Diet Nutritional Profiles ---

  diet_profiles <- diets %>%
    dplyr::left_join(ingredients, by = c("region", "subregion", "class_flex", "diet_tag")) %>%
    dplyr::left_join(characteristics, by = c("ingredient", "ingredient_type")) %>%
    dplyr::mutate(
      # Weighting factor calculation:
      # (Ingredient % in category) * (Category % in Diet) / 10000
      weight_factor = (ingredient_share * dplyr::case_when(
        ingredient_type == "forage"      ~ forage_share,
        ingredient_type == "concentrate" ~ concentrate_share,
        ingredient_type == "milk"        ~ milk_share,
        ingredient_type == "replacer"    ~ milk_replacer_share,
        TRUE ~ 0
      )) / 10000
    ) %>%
    dplyr::group_by(region, subregion, class_flex, diet_tag) %>%
    dplyr::summarise(
      dplyr::across(c(de, cp, ndf, ash, eb), ~ sum(. * weight_factor, na.rm = TRUE)),
      .groups = "drop"
    )

  # --- 4. Map Diets to Animals ---
  results <- definitions %>%
    dplyr::select(region, subregion, animal_tag, class_flex, animal_type, animal_subtype, diet_tag) %>%
    dplyr::left_join(diet_profiles, by = c("region", "subregion", "class_flex", "diet_tag")) %>%
    dplyr::filter(!is.na(region)) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4))) %>%
    dplyr::select(
      region, subregion, animal_tag, class_flex, animal_type, animal_subtype, diet_tag,
      de, cp, ndf, ash, eb
    )

  # --- 5. Save Output ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/weighted_nutritional_variables.csv")
    message("\U0001f4be Saved nutritional variables to output/weighted_nutritional_variables.csv")
  }

  return(results)
}
