#' Calculate Weighted Nutritional Variables
#'
#' Computes weighted averages of nutritional variables (CP, DE, Ash, NDF)
#' for different livestock animals based on dietary composition.
#'
#' @param animal Character string specifying the animal type.
#'   Must be one of: `"vaca"`, `"oveja"`, or `"cabra"`.
#' @param type For `"vaca"` only: specify production system.
#'   Must be one of `"Dairy"`, `"Beef"`, or `"Feedlot"`.
#'   If `NULL`, all available systems are included.
#' @param zones Optional character vector of zones.
#'   Only applies when `animal = "vaca"` and `type` includes `"Dairy"` or `"Beef"`.
#' @param saveoutput Para guardar el resultado final. Por defecto TRUE.
#'
#' @return A tibble with weighted nutritional values (`CP [%]`, `DE [%]`,
#'   `Ash [%]`, `NDF [%]`) by `animal_type`, `zone`, and `Code`.
#'
#' @details
#' The function uses three datasets loaded by `load_all_data()`:
#' \itemize{
#'   \item `ingredients`
#'   \item `diet_characteristics`
#'   \item `diet`
#' }
#'
#' The weighting is based on dietary proportions of Feed, Forage, and Milk.
#'
#' @examples
#' \dontrun{
#' # For dairy cows, filtering by zone A
#' calculate_weighted_variable(animal = "vaca", type = "Dairy", zones = "A")
#'
#' # For sheep (no type or zone needed)
#' calculate_weighted_variable(animal = "oveja")
#' }
#'
#' @importFrom dplyr filter left_join mutate across group_by summarise arrange
#' @importFrom assertthat assert_that
#' @export
calculate_weighted_variable <- function(animal, type = NULL, zones = NULL, saveoutput = TRUE) {

  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Necesitas 'dplyr'")
  if (!requireNamespace("assertthat", quietly = TRUE)) stop("Necesitas 'assertthat'")

  # --- Validaciones con assertthat ---
  assertthat::assert_that(animal %in% c("Cattle", "Sheep", "Goat"),
                          msg = "animal debe ser 'Cattle', 'Sheep' o 'Goat'")

  if (!is.null(type)) {
    # Solo para vacas
    if (animal == "Cattle") {
      tipos_validos <- unique(diet$animal_subtype)
      assertthat::assert_that(type %in% tipos_validos,
                              msg = paste0("type debe estar en los valores disponibles: ", paste(tipos_validos, collapse = ", ")))
    } else {
      warning("El parámetro `type` solo aplica para vacas. Se ignorará para otros animales.")
    }
  }

  if (!is.null(zones)) {
    zonas_validas <- unique(diet$zone)
    assertthat::assert_that(all(zones %in% zonas_validas),
                            msg = "Alguna zona especificada no existe en los datos")
  }

  # --- Filtrado de datos ---
  if (animal == "Cattle") {
    if (!is.null(type)) diet <- dplyr::filter(diet, animal_subtype %in% type)
    if (!is.null(zones)) diet <- dplyr::filter(diet, zone %in% zones)
  } else {
    diet <- dplyr::filter(diet, animal_type == animal)
  }

  # --- Joins ---
  joined <- ingredients %>%
    dplyr::inner_join(characteristics, by = c("ingredient", "animal_type","animal_subtype", "ingredient_type")) %>%
    dplyr::inner_join(diet, by = c("code", "animal_type","animal_subtype", "zone"))


  # --- Variables a calcular ---
  vars <- intersect(c("cp", "de", "ash", "ndf"), names(joined))
  req <- c("ingredient_share", "feed_share", "forage_share", "milk_share")
  miss <- setdiff(req, names(joined))
  if (length(miss) > 0) stop("Faltan columnas: ", paste(miss, collapse = ", "))

  # --- Cálculo ponderado ---
  variables <- joined %>%
    dplyr::mutate(weight = dplyr::case_when(
      ingredient_type == "Feed"   ~ `feed_share`,
      ingredient_type == "Forage" ~ `forage_share`,
      ingredient_type == "Milk"   ~ `milk_share`,
      TRUE ~ 0
    )) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(vars),
                                ~ . * `ingredient_share` * weight / 10000,
                                .names = "{.col}"
    )) %>%
    dplyr::group_by(animal_type, animal_subtype, zone, code) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(vars), sum, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(animal_type, animal_subtype, zone, code)

  # --- Guardar salida ---
  if (saveoutput) {
    write.csv(variables, "output/variables.csv", row.names = FALSE)
  }

  return(invisible(variables))
}


