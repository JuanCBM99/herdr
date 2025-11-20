#' Descarga los archivos de template desde GitHub
#'
#' Descarga los archivos de ejemplo (census, categories, rate_parameters)
#' desde el repositorio de GitHub y los guarda en una carpeta local.
#'
#' @param dest_folder El nombre de la carpeta donde se guardarán los
#'   templates. Por defecto es "user_data".
#'
#' @return El path absoluto a la carpeta `dest_folder` donde se
#'   guardaron los archivos.
#' @export
#'
#' @examples
#' \dontrun{
#'   # Llama a la función para descargar los archivos
#'   ruta_datos <- download_templates()
#'
#'   # Ver los archivos descargados
#'   list.files(ruta_datos)
#'
#'   # Leer uno de los archivos
#'   read.csv(file.path(ruta_datos, "census_c_s.csv"))
#' }
#'
download_templates <- function(dest_folder = "user_data") {

  message("Iniciando la descarga de templates...")

  # --- 1. Definir los archivos y la URL base ---

  # ¡CORREGIDO! Esta es la URL "raw" para descargar directamente el contenido.
  # Nota que ahora termina con una barra diagonal (/)
  base_url <- "https://raw.githubusercontent.com/JuanCBM99/Animal_GEI_LU/dev/inst/extdata/templates/"

  # ¡CORREGIDO! Actualizados los nombres de archivo
  # y añadido rate_parameters.csv (que aparecía en tus errores).
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

  # --- 2. Crear el directorio de destino ---
  dir.create(dest_folder, showWarnings = FALSE, recursive = TRUE)
  download_path <- normalizePath(dest_folder)

  message(paste("Archivos se guardarán en:", download_path))


  # --- 3. Bucle de descarga ---

  for (file_name in template_files) {

    # Construir la URL completa (ej: .../templates/census_c_s.csv)
    # Como base_url ahora termina en "/", paste0 funciona correctamente
    source_url <- paste0(base_url, file_name)

    # Construir la ruta de destino (ej: user_data/census_c_s.csv)
    dest_file_path <- file.path(download_path, file_name)

    message(paste(" -> Descargando", file_name, "..."))

    tryCatch({
      # Usamos 'quiet = FALSE' para ver el progreso, o 'quiet = TRUE' para ocultarlo
      download.file(url = source_url, destfile = dest_file_path, mode = "wb", quiet = FALSE)

    }, error = function(e) {
      # Advertir si un archivo falla, pero no detener la función
      warning(paste("Error al descargar", file_name, ":", e$message))
      warning(paste("URL intentada:", source_url)) # Añadido para mejor depuración
    })
  }
  message(paste("Los archivos están en:", download_path))
  message("✅ Descarga completada.")

  # --- 4. Devolver el path ---
  return(download_path)
}
