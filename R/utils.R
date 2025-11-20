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
    message("    → ¡Éxito! Usando archivo de usuario: user_data/", name, ".csv")

    # --- ¡ARREGLO! ---
    # na.strings = "NA" (por defecto) + "" (para celdas en blanco)
    # Esto convierte las celdas vacías "" en NA
    df <- read.csv(csv_path, stringsAsFactors = FALSE, na.strings = c("NA", ""))

  } else {
    # (El resto de la función no cambia)
    if (dir.exists(user_path)) {
      message("    → AVISO: La carpeta 'user_data' existe, pero no se encontró '", name, ".csv'.")
      message("    → Asegúrate de que el nombre del archivo es correcto.")
      message("    → Usando dataset por defecto del paquete: ", name)
    } else {
      message("    → Usando dataset por defecto del paquete: ", name)
    }

    # Cargar dataset por defecto del paquete (.rda en data/)
    # ¡MODIFICACIÓN! Aplicamos la misma lógica de NA a los .rda
    df_raw <- get(name, envir = asNamespace("AnimalGEILU"))

    if (is.data.frame(df_raw)) {
      # Reemplaza "" por NA en todas las columnas de texto
      df <- df_raw %>%
        dplyr::mutate(dplyr::across(where(is.character), ~ na_if(., "")))
    } else {
      df <- df_raw
    }
  }

  return(df)
}
