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

  path_user_data <- system.file("user_data", package = "herdr")
  if (path_user_data != "") {
    files_ud <- list.files(path_user_data, full.names = TRUE)

    file.copy(files_ud, "user_data", overwrite = FALSE)
    message("\u1f4d1 Copied default configuration files to 'user_data/'.")
  }

  path_examples <- system.file("Examples", package = "herdr")
  if (path_examples != "") {
    items_to_copy <- list.files(path_examples, full.names = TRUE)

    file.copy(items_to_copy, "Examples", recursive = TRUE, overwrite = FALSE)
    message("\u1f4ca Copied reference examples (including subfolders) to 'Examples/'.")
  } else {
    message("\u2139\ufe0f Note: No 'Examples' folder found in the package.")
  }

  message("\n\u2728 Project initialized successfully!")
  message("\u1f4a1 Check your working directory: ", getwd())
}
