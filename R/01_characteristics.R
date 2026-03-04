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
      # Keep forage_share to validate later
      forage_pct = dplyr::first(forage_share),
      dplyr::across(c(de, cp, ndf, ash, eb), ~ sum(. * weight_factor, na.rm = TRUE)),
      .groups = "drop"
    )

  # --- 4. Map Diets to Animals & Validate MATURE only ---

  # Join with definitions to identify which diet belongs to which animal
  full_data <- definitions %>%
    dplyr::select(region, subregion, animal_tag, class_flex, animal_type, animal_subtype, diet_tag) %>%
    dplyr::left_join(diet_profiles, by = c("region", "subregion", "class_flex", "diet_tag")) %>%
    dplyr::filter(!is.na(region))

  # Create subset for MATURE validations
  mature_check <- full_data %>%
    dplyr::filter(grepl("mature", animal_tag, ignore.case = TRUE))

  if (nrow(mature_check) > 0) {

    # A. ASSERT: Forage Safety (< 30% - NRC 1996)
    bad_forage <- mature_check %>% dplyr::filter(forage_pct < 30)
    assertthat::assert_that(nrow(bad_forage) == 0,
                            msg = paste0("\u274C Biological Error (Mature): Forage share < 30% for: ",
                                         paste(unique(bad_forage$animal_tag), collapse = ", ")))

    # B. ASSERT: Ash (1-18% - NRC 1996)
    bad_ash <- mature_check %>% dplyr::filter(ash < 1 | ash > 18)
    assertthat::assert_that(nrow(bad_ash) == 0,
                            msg = paste0("\u274C Critical Error (Mature): Weighted Ash (1-18%) out of range for: ",
                                         paste(unique(bad_ash$animal_tag), collapse = ", ")))

    # C. ASSERT: Crude Protein (5-40% - NRC 1996)
    bad_cp <- mature_check %>% dplyr::filter(cp < 5 | cp > 40)
    assertthat::assert_that(nrow(bad_cp) == 0,
                            msg = paste0("\u274C Critical Error (Mature): Weighted CP (5-40%) out of range for: ",
                                         paste(unique(bad_cp$animal_tag), collapse = ", ")))

    # D. ASSERT: NDF Fiber (20-85% - NMSU/NRC)
    bad_ndf <- mature_check %>% dplyr::filter(ndf < 20 | ndf > 85)
    assertthat::assert_that(nrow(bad_ndf) == 0,
                            msg = paste0("\u274C Critical Error (Mature): Weighted NDF (20-85%) out of range for: ",
                                         paste(unique(bad_ndf$animal_tag), collapse = ", ")))

    # E. Productivity Warning (> 80% Forage - AFRC 1993)
    high_forage <- mature_check %>% dplyr::filter(forage_pct > 80)
    if (nrow(high_forage) > 0) {
      message(paste0("\u2139 Productivity Note (Mature): Forage > 80% for: ",
                     paste(unique(high_fvorage$animal_tag), collapse = ", "),
                     ". Rumen fill may limit intake."))
    }
  }

  # --- 5. Final Formatting ---
  results <- full_data %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4))) %>%
    dplyr::select(
      region, subregion, animal_tag, class_flex, animal_type, animal_subtype, diet_tag,
      de, cp, ndf, ash, eb
    )

  # --- 6. Save Output ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/weighted_nutritional_variables.csv")
    message("\U0001f4be Saved nutritional variables to output/weighted_nutritional_variables.csv")
  }

  return(results)
}
