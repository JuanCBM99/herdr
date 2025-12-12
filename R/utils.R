#' Load dataset from user_data or package default
#'
#' @param name Dataset name without extension, e.g., "diet", "categories"
#' @return Data frame
#' @export
load_dataset <- function(name) {
  # Folder where the user can place custom CSV files
  user_path <- file.path(getwd(), "user_data")
  csv_path <- file.path(user_path, paste0(name, ".csv"))

  if (file.exists(csv_path)) {
    # Success message for using user file
    message("    → Success! Using user file: user_data/", name, ".csv")

    # FIX: na.strings = "NA" (default) + "" (for blank cells)
    # This converts empty cells "" to NA
    df <- read.csv(csv_path, stringsAsFactors = FALSE, na.strings = c("NA", ""))

  } else {
    # Fallback to package default dataset

    if (dir.exists(user_path)) {
      message("    → WARNING: The 'user_data' folder exists, but '", name, ".csv' was not found.")
      message("    → Ensure the file name is correct.")
      message("    → Using package default dataset: ", name)
    } else {
      message("    → Using package default dataset: ", name)
    }

    # Load package default dataset (.rda in data/)
    # MODIFICATION: Apply the same NA logic to .rda files
    df_raw <- get(name, envir = asNamespace("AnimalGEILU"))

    if (is.data.frame(df_raw)) {
      # Replace "" with NA in all character columns
      df <- df_raw %>%
        dplyr::mutate(dplyr::across(where(is.character), ~ na_if(., "")))
    } else {
      df <- df_raw
    }
  }

  return(df)
}
