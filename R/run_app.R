#' Launch the herdr Interactive Application
#'
#' @description
#' Opens the local herdr graphical user interface (GUI) in your default web browser.
#' This allows you to run impact assessments offline keeping your data private.
#'
#' @export
# nocov start
run_herdr_app <- function() {

  app_file <- system.file("app", "app.R", package = "herdr")

  if (app_file == "") {
    stop("Could not find app.R. Try re-installing `herdr`.", call. = FALSE)
  }

  message("Starting herdr local interface in your current working directory...")


  app_obj <- source(app_file, local = new.env())$value


  shiny::runApp(app_obj, display.mode = "normal")
}
# nocov end
