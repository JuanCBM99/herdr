#' Validate Input Data Consistency
#'
#' Checks that every animal defined in the census (population) has a
#' corresponding diet (diet data) and vice versa.
#'
#' @param pop_df Population dataframe (from 'calculate_population()').
#' @param diet_df Diet dataframe (from 'calculate_weighted_variable()').
#'
#' @return NULL (invisible). Stops execution with 'stop()' if a critical error occurs.
#' @noRd

validate_data <- function(pop_df, diet_df) {

  message("  -> Validating data consistency (census vs. diet)...")

  # 1. Define Join Keys (Columns that must match)
  join_keys <- intersect(names(pop_df), names(diet_df))

  # 2. Extract Unique Keys from each dataframe
  pop_keys <- pop_df %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(join_keys)))

  diet_keys <- diet_df %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(join_keys)))

  # 3. Check 1: Missing Diets? (Critical Error)
  # (Animals present in population but NOT in diet data)
  missing_diets <- dplyr::anti_join(pop_keys, diet_keys, by = join_keys)

  if (nrow(missing_diets) > 0) {
    # If missing, create a clear error message
    error_msg <- "Validation Error! Missing diets for the following animals (defined in 'census.csv'):\n"

    # Format the first 5 missing rows for display
    error_details <- missing_diets %>%
      head(5) %>%
      utils::capture.output(print)

    stop(paste(error_msg, paste(error_details, collapse = "\n")))
  }

  # 4. Check 2: Missing Population? (Non-critical Warning)
  # (Animals that have a diet but NO corresponding census data)
  missing_pop <- dplyr::anti_join(diet_keys, pop_keys, by = join_keys)

  if (nrow(missing_pop) > 0) {
    # This triggers a Warning, not a Stop
    warning_msg <- "WARNING: The following diets have no corresponding population in 'census.csv' (they will be ignored):\n"

    # Format the first 5 missing rows for display
    warning_details <- missing_pop %>%
      head(5) %>%
      utils::capture.output(print)

    warning(paste(warning_msg, paste(warning_details, collapse = "\n")))
  }

  message("  -> Data validation successful.")
  return(invisible(NULL))
}
