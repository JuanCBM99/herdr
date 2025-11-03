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
    # Mensaje de éxito claro
    message("   → ¡Éxito! Usando archivo de usuario: user_data/", name, ".csv")
    df <- read.csv(csv_path, stringsAsFactors = FALSE)
  } else {

    # --- INICIO DE LA MEJORA ---
    # Comprobamos si 'user_data' existe
    if (dir.exists(user_path)) {
      # Si existe, pero el archivo NO, avisamos al usuario.
      message("   → AVISO: La carpeta 'user_data' existe, pero no se encontró '", name, ".csv'.")
      message("   → Asegúrate de que el nombre del archivo es correcto.")
      message("   → Usando dataset por defecto del paquete: ", name)
    } else {
      # Si 'user_data' ni siquiera existe, es normal.
      message("   → Usando dataset por defecto del paquete: ", name)
    }
    # --- FIN DE LA MEJORA ---

    # Cargar dataset por defecto del paquete (.rda en data/)
    df <- get(name, envir = asNamespace("AnimalGEILU"))
  }

  return(df)
}
