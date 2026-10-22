#' Initialize herdr project folders and example data
#'
#' This function creates the 'user_data' and 'Examples' folders in your
#' current working directory and populates them with the default files from the package.
#'
#' @export
herdr_init <- function() {

  folders <- c("user_data", "Examples")

  message("\u2699\ufe0f Initializing herdr project structure...")

  for (f in folders) {
    if (!dir.exists(f)) {
      dir.create(f, showWarnings = FALSE)
      message("\u2705 Created folder: ", f)
    }
  }


  path_default_example <- system.file("Examples/Level1_Spain_Dairy_Cattle_2015", package = "herdr")
  if (path_default_example != "") {
    files_ud <- list.files(path_default_example, full.names = TRUE)

    file.copy(files_ud, "user_data", overwrite = FALSE)
    message("\U0001f4d1 Copied default template (Level1_Spain_Dairy_Cattle_2015) to 'user_data/'.")
  } else {
    message("\u26a0\ufe0f Note: Default example 'Level1_Spain_Dairy_Cattle_2015' not found in the package.")
  }


  path_examples <- system.file("Examples", package = "herdr")
  if (path_examples != "") {
    items_to_copy <- list.files(path_examples, full.names = TRUE)

    file.copy(items_to_copy, "Examples", recursive = TRUE, overwrite = FALSE)
    message("\U0001f4ca Copied reference examples (including subfolders) to 'Examples/'.")
  } else {
    message("\u2139\ufe0f Note: No 'Examples' folder found in the package.")
  }

  message("\n\u2728 Project initialized successfully!")
  message("\U0001f4a1 Check your working directory: ", getwd())
}

#' Validate Manure System Allocation Fractions
#'
#' Verifies that manure management system allocation fractions per animal cohort
#' sum to 1.0 (100%). Throws a fatal error if allocations do not sum to 1.0 within
#' the specified numerical tolerance.
#'
#' @param user_manure A data.frame containing user manure management data.
#' @param tolerance Numeric. Numerical tolerance for rounding differences. Default is 1e-3 (0.001).
#' @return Invisibly returns the aggregated allocation data.frame.
#' @noRd
herdr_validate_manure_allocation <- function(user_manure, tolerance = 1e-3) {
  if (is.null(user_manure) || nrow(user_manure) == 0 || !"allocation" %in% names(user_manure)) {
    return(invisible(NULL))
  }

  allocation_sums <- user_manure %>%
    dplyr::filter(!is.na(allocation)) %>%
    dplyr::group_by(region, subregion, animal_tag, class_flex) %>%
    dplyr::summarise(total_alloc = sum(as.numeric(allocation), na.rm = TRUE), .groups = "drop")

  if (nrow(allocation_sums) == 0) {
    return(invisible(allocation_sums))
  }

  # 1. Over-allocation check (> 100%)
  over_allocated <- allocation_sums %>% dplyr::filter(total_alloc > (1 + tolerance))
  if (nrow(over_allocated) > 0) {
    details <- paste0(over_allocated$animal_tag, " (sum = ", round(over_allocated$total_alloc, 3), ")", collapse = ", ")
    stop("Data Error: Manure allocation exceeds 1.0 (100%) for cohorts: ", details, call. = FALSE)
  }

  # 2. Under-allocation check (< 100%)
  under_allocated <- allocation_sums %>% dplyr::filter(total_alloc < (1 - tolerance))
  if (nrow(under_allocated) > 0) {
    details <- paste0(under_allocated$animal_tag, " (sum = ", round(under_allocated$total_alloc, 3), ")", collapse = ", ")
    stop("Data Error: Manure allocation is less than 1.0 (100%) for cohorts: ", details,
         ". Total manure allocation must sum to exactly 1.0.", call. = FALSE)
  }

  invisible(allocation_sums)
}

#' Validate Diet Profiles and Ingredient Shares
#'
#' Verifies that:
#' \enumerate{
#'   \item Category shares in \code{diet_profiles} (forage, concentrate, milk, milk replacer) sum to 100%.
#'   \item For every category with a share > 0%, matching ingredients exist in \code{diet_ingredients}
#'         and their shares sum to 100%.
#' }
#' Throws a fatal error with informative diagnostics if any constraint is violated.
#'
#' @param diets A data.frame containing diet profiles.
#' @param ingredients A data.frame containing diet ingredients.
#' @param diet_join_keys Character vector of grouping keys (e.g. \code{"diet_tag"}). Default is \code{"diet_tag"}.
#' @param tolerance Numeric. Maximum allowable difference from 100%. Default is 0.1.
#' @return Invisibly returns TRUE if all validations pass.
#' @noRd
herdr_validate_diet_shares <- function(diets, ingredients, diet_join_keys = "diet_tag", tolerance = 0.1) {
  if (is.null(diets) || nrow(diets) == 0 || is.null(ingredients) || nrow(ingredients) == 0) {
    return(invisible(TRUE))
  }

  category_cols <- c("forage_share", "concentrate_share", "milk_share", "milk_replacer_share")
  present_cats <- intersect(category_cols, names(diets))

  # 1. Check that diet profiles categories sum to 100%
  if (length(present_cats) > 0) {
    cat_matrix <- as.matrix(diets[present_cats])
    cat_matrix[is.na(cat_matrix)] <- 0
    diet_totals <- rowSums(cat_matrix)
    invalid_diets <- which(abs(diet_totals - 100) > tolerance)

    if (length(invalid_diets) > 0) {
      details <- paste0(diets$diet_tag[invalid_diets], " (sum = ", round(diet_totals[invalid_diets], 2), "%)", collapse = ", ")
      stop("Diet Profile Error: Shares do not sum to 100% in 'diet_profiles.csv' for: ", details, call. = FALSE)
    }
  }

  # 2. Check that ingredient shares within each category sum to 100%
  join_keys <- intersect(diet_join_keys, names(ingredients))
  if (length(join_keys) == 0) join_keys <- "diet_tag"

  ing_totals <- ingredients %>%
    dplyr::filter(!is.na(ingredient_share)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(join_keys, "ingredient_type")))) %>%
    dplyr::summarise(total_ing = sum(as.numeric(ingredient_share), na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(abs(total_ing - 100) > tolerance)

  if (nrow(ing_totals) > 0) {
    details <- paste0(ing_totals$diet_tag, " [", ing_totals$ingredient_type, "] (sum = ", round(ing_totals$total_ing, 2), "%)", collapse = ", ")
    stop("Ingredient Error: Shares do not sum to 100% within a category in 'diet_ingredients.csv' for: ", details, call. = FALSE)
  }

  # 3. Cross-validation: Ensure categories declared > 0% have matching ingredients
  type_map <- c(
    "forage" = "forage_share",
    "concentrate" = "concentrate_share",
    "milk" = "milk_share",
    "milk_replacer" = "milk_replacer_share"
  )

  missing_ingredients <- character()
  for (type_name in names(type_map)) {
    share_col <- type_map[[type_name]]
    if (share_col %in% names(diets)) {
      active_rows <- which(!is.na(diets[[share_col]]) & as.numeric(diets[[share_col]]) > tolerance)
      for (r in active_rows) {
        d_tag <- diets$diet_tag[r]
        has_ings <- any(ingredients$diet_tag == d_tag &
                        ingredients$ingredient_type == type_name &
                        !is.na(ingredients$ingredient_share) &
                        as.numeric(ingredients$ingredient_share) > 0)
        if (!has_ings) {
          missing_ingredients <- c(missing_ingredients, paste0(d_tag, " [", type_name, " = ", diets[[share_col]][r], "%]"))
        }
      }
    }
  }

  if (length(missing_ingredients) > 0) {
    stop("Ingredient Error: Active diet categories missing ingredients in 'diet_ingredients.csv': ",
         paste(missing_ingredients, collapse = ", "), call. = FALSE)
  }

  invisible(TRUE)
}
