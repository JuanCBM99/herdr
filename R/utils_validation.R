#' Valida la consistencia de los datos de entrada
#'
#' Comprueba que cada animal definido en el censo (población) tenga una
#' dieta correspondiente (dieta) y viceversa.
#'
#' @param pop_df Dataframe de población (de 'calculate_population()').
#' @param diet_df Dataframe de dieta (de 'calculate_weighted_variable()').
#'
#' @return NULL (invisible). Para la ejecución con 'stop()' si hay un error.
#' @noRd

validate_data <- function(pop_df, diet_df) {

  message("  -> Validando consistencia de datos (censo vs. dieta)...")

  # 1. Definir las claves de unión (las columnas que deben coincidir)
  # Usamos 'intersect' para coger solo las columnas comunes (group, zone, code, etc.)
  join_keys <- intersect(names(pop_df), names(diet_df))

  # 2. Extraer claves únicas de cada dataframe
  pop_keys <- pop_df %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(join_keys)))

  diet_keys <- diet_df %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(join_keys)))

  # 3. Comprobación 1: ¿Faltan dietas?
  # (Animales que están en la población pero NO en la dieta)
  missing_diets <- dplyr::anti_join(pop_keys, diet_keys, by = join_keys)

  if (nrow(missing_diets) > 0) {
    # Si faltan, creamos un mensaje de error claro
    error_msg <- "¡Error de Validación! Faltan dietas para los siguientes animales (definidos en 'census.csv'):\n"

    # Formatear las primeras 5 filas que faltan para mostrarlas
    error_details <- missing_diets %>%
      head(5) %>%
      utils::capture.output(print) # Convierte el dataframe en texto

    stop(paste(error_msg, paste(error_details, collapse = "\n")))
  }

  # 4. Comprobación 2: ¿Sobra población? (Opcional, pero bueno)
  # (Animales que tienen dieta pero NO están en el censo)
  missing_pop <- dplyr::anti_join(diet_keys, pop_keys, by = join_keys)

  if (nrow(missing_pop) > 0) {
    # Esto es solo una advertencia (Warning), no un error
    warning_msg <- "ADVERTENCIA: Las siguientes dietas no tienen población correspondiente en 'census.csv' (serán ignoradas):\n"

    warning_details <- missing_pop %>%
      head(5) %>%
      utils::capture.output(print)

    warning(paste(warning_msg, paste(warning_details, collapse = "\n")))
  }

  message("  -> Validación de datos superada.")
  return(invisible(NULL))
}
