#' Download Template Files from GitHub
#'
#' Downloads example data files (templates) from the GitHub repository and
#' saves them to a local folder.
#'
#' @param dest_folder The name of the folder where the templates will be saved.
#'   Defaults to "user_data".
#'
#' @return The absolute path to the `dest_folder` where the files were saved.
#' @export
#'
#' @examples
#' \dontrun{
#'  # Call the function to download the files
#'  data_path <- download_templates()
#'
#'  # View the downloaded files
#'  list.files(data_path)
#'
#'  # Read one of the files
#'  read.csv(file.path(data_path, "categories.csv"))
#' }
#'
download_templates <- function(dest_folder = "user_data") {

  message("Starting template download...")

  # --- 1. Define Files and Base URL ---

  # Base URL for direct content download from GitHub (raw content)
  base_url <- "https://raw.githubusercontent.com/JuanCBM99/Animal_GEI_LU/dev/inst/extdata/templates/"

  # List of all required template files for the package
  template_files <- c(
    "categories.csv",
    "ch4_mm.csv",
    "characteristics.csv",
    "crops.csv",
    "diet.csv",
    "ingredients.csv",
    "n2o_direct.csv",
    "n2o_indirect.csv",
    "weights.csv"
  )

  # --- 2. Create Destination Directory ---
  dir.create(dest_folder, showWarnings = FALSE, recursive = TRUE)
  download_path <- normalizePath(dest_folder)

  message(paste("Files will be saved to:", download_path))


  # --- 3. Download Loop ---

  for (file_name in template_files) {

    # Construct the full source URL
    source_url <- paste0(base_url, file_name)

    # Construct the destination file path
    dest_file_path <- file.path(download_path, file_name)

    message(paste(" -> Downloading", file_name, "..."))

    tryCatch({
      # Download file (mode 'wb' for binary write)
      download.file(url = source_url, destfile = dest_file_path, mode = "wb", quiet = FALSE)

    }, error = function(e) {
      # Warn if a file fails, but do not stop the function
      warning(paste("Error downloading", file_name, ":", e$message))
      warning(paste("URL attempted:", source_url))
    })
  }
  message(paste("Files are located at:", download_path))
  message("✅ Download completed.")

  # --- 4. Return Path ---
  return(download_path)
}
