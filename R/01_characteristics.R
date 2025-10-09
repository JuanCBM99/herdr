#' Calculate Weighted Nutritional Variables
#'
#' Computes weighted averages of nutritional variables based on animal diets.
#' Allows using user-provided datasets in 'user_data/' or defaults from the package.
#'
#' @param animal Animal type to filter by (optional)
#' @param type Animal subtype (only for Cattle, optional)
#' @param zones Geographical zones to include (optional)
#' @param saveoutput Save results to CSV? Default TRUE
#'
#' @return A data frame with weighted nutritional values by animal category and zone
#'
#' @examples
#' \dontrun{
#' calculate_weighted_variable(animal = "Cattle", type = "Dairy")
#' }
#' @export
calculate_weighted_variable <- function(animal = NULL, type = NULL, zones = NULL, saveoutput = TRUE) {

  message("🟢 Loading input data...")

  # --- Cargar datasets ---
  diet <- load_dataset("diet")
  ingredients <- load_dataset("ingredients")
  characteristics <- load_dataset("characteristics")

  # --- Validaciones ---
  if (!is.null(animal)) {
    animales_validos <- unique(diet$animal_type)
    assertthat::assert_that(animal %in% animales_validos,
                            msg = paste0("El animal debe estar en los datos. Opciones: ",
                                         paste(animales_validos, collapse = ", ")))
  }

  if (!is.null(type) && !is.null(animal) && animal == "Cattle") {
    tipos_validos <- unique(diet$animal_subtype)
    assertthat::assert_that(type %in% tipos_validos,
                            msg = paste0("type debe estar en: ",
                                         paste(tipos_validos, collapse = ", ")))
  } else if (!is.null(type)) {
    warning("El parámetro `type` solo aplica para `Cattle`. Se ignorará para otros animales.")
  }

  if (!is.null(zones)) {
    zonas_validas <- unique(diet$zone)
    assertthat::assert_that(all(zones %in% zonas_validas),
                            msg = paste("Alguna zona no existe. Opciones: ",
                                        paste(zonas_validas, collapse = ", ")))
  }

  # --- Filtrado de diet ---
  diet_sub <- diet
  if (!is.null(animal)) {
    diet_sub <- dplyr::filter(diet_sub, animal_type == animal)
    if (!is.null(type))  diet_sub <- dplyr::filter(diet_sub, animal_subtype %in% type)
    if (!is.null(zones)) diet_sub <- dplyr::filter(diet_sub, zone %in% zones)
  }

  # --- Joins ---
  joined <- dplyr::inner_join(ingredients, characteristics,
                              by = c("ingredient", "animal_type", "animal_subtype", "ingredient_type")) %>%
    dplyr::inner_join(diet_sub,
                      by = c("code", "animal_type", "animal_subtype", "zone"))

  # --- Variables a calcular ---
  vars <- intersect(c("cp", "de", "ash", "ndf"), names(joined))
  req <- c("ingredient_share", "feed_share", "forage_share", "milk_share","milk_replacer_share")
  miss <- setdiff(req, names(joined))
  if (length(miss) > 0) stop("Faltan columnas: ", paste(miss, collapse = ", "))

  # --- Cálculo ponderado ---
  variables <- joined %>%
    dplyr::mutate(weight = dplyr::case_when(
      ingredient_type == "Feed"   ~ feed_share,
      ingredient_type == "Forage" ~ forage_share,
      ingredient_type == "Milk"   ~ milk_share,
      ingredient_type == "Lactoreemplazante" ~ milk_replacer_share,
      TRUE ~ 0
    )) %>%
    dplyr::mutate(dplyr::across(all_of(vars),
                                ~ . * ingredient_share * weight / 10000,
                                .names = "{.col}")) %>%
    dplyr::group_by(animal_type, animal_subtype, zone, code) %>%
    dplyr::summarise(dplyr::across(all_of(vars), sum, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(code, zone, animal_type, animal_subtype)

  # --- Guardar salida ---
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    write.csv(variables, "output/variables.csv", row.names = FALSE)
    message("💾 Saved output to output/variables.csv")
  }

  variables
}
