#' Launch the herdr Interactive Application
#'
#' @description
#' Opens the local herdr graphical user interface (GUI) in your default web browser.
#' This allows you to run impact assessments offline keeping your data private.
#'
#' @export
run_herdr_app <- function() {
  # 1. Buscamos el archivo app.R dentro del paquete instalado
  app_file <- system.file("app", "app.R", package = "herdr")

  if (app_file == "") {
    stop("Could not find app.R. Try re-installing `herdr`.", call. = FALSE)
  }

  message("Starting herdr local interface in your current working directory...")

  # 2. LA MAGIA: En lugar de darle la carpeta a Shiny, leemos la app como un
  # objeto en tu entorno actual. Así, la carpeta "user_data" se creará
  # exactamente en la carpeta de tu ordenador donde tengas abierto RStudio.
  app_obj <- source(app_file, local = new.env())$value

  # 3. Lanzamos el objeto
  shiny::runApp(app_obj, display.mode = "normal")
}
