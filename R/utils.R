#' Load dataset from user_data or package default
#'
#' @param name Dataset name without extension, e.g., "diet", "categories"
#' @return Data frame
#' @export
load_dataset <- function(name) {
  # Carpeta donde el usuario puede poner sus CSV personalizados
  user_path <- file.path(getwd(), "user_data")
  csv_path <- file.path(user_path, paste0(name, ".csv"))

  if (file.exists(csv_path)) {
    message("   → Using user_data/", name, ".csv")
    df <- read.csv(csv_path, stringsAsFactors = FALSE)
  } else {
    message("   → Using default dataset: ", name)
    # Cargar dataset por defecto del paquete (.rda en data/)
    df <- get(name, envir = asNamespace("AnimalGEILU"))
  }

  return(df)
}

