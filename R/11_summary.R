#' Resumen de emisiones de CH4, N2O y uso de tierra
#'
#' Calcula un resumen de emisiones de CH₄ y N₂O (directas e indirectas) y de uso de tierra
#' agrupado por código, tipo de animal, subtipo y zona. Permite opcionalmente
#' agrupar a un nivel más agregado.
#'
#' @param group Character/Numeric vector (opcional). Grupos a filtrar. Default `NULL`.
#' @param zone Character vector (opcional). Zonas a filtrar. Default `NULL`.
#' @param animal Character string (opcional). Tipo de ganado (`animal_type`). Default `NULL`.
#' @param type Character string (opcional). Subtipo de ganado (`animal_subtype`). Default `NULL`.
#' @param saveoutput Logical. Si `TRUE`, guarda el resultado en `"output/Resumen_emisiones_land_use.csv"`. Default `TRUE`.
#' @param group_by_identification Logical. Si `TRUE` devuelve resultados al nivel de `identification`. Si `FALSE`, agrega todos los `identification`.
#' @return Un `tibble` con columnas para cada tipo de emisión y uso de tierra.
#' @export
resumen_emisiones_land_use <- function(group = NULL, zone = NULL, animal = NULL, type = NULL,
                                       saveoutput = TRUE, group_by_identification = TRUE) {

  message("🟢 Iniciando resumen...")

  # --- ¡NUEVO PASO DE VALIDACIÓN! ---
  message("  -> 1/3: Validando consistencia de datos (censo vs. dieta)...")

  # Cargamos los datos SOLO para validarlos.
  # (Esta es la redundancia que aceptamos para mantener las funciones simples)
  tryCatch({
    validation_pop <- calculate_population(saveoutput = FALSE)
    validation_diet <- calculate_weighted_variable(saveoutput = FALSE)

    # Llamamos a la función de validación (que está en R/utils_validation.R)
    validate_data(pop_df = validation_pop, diet_df = validation_diet)

  }, error = function(e) {
    # Si validate_data() da un stop(), lo capturamos y lo mostramos
    stop("¡Error en la validación de datos! Por favor, revisa tus archivos CSV.\n",
         "Error original: ", e$message)
  })

  message("  -> 2/3: Calculando todas las emisiones y uso de tierra.")
  # --- FIN DE VALIDACIÓN ---


  # --- Cálculo principal ---
  # (Esta parte no cambia)

  ch4_enteric <- calculate_emissions_enteric(saveoutput = FALSE) %>%
    dplyr::group_by(group, zone, identification, animal_type, animal_subtype) %>%
    dplyr::summarise(Emissions_CH4_enteric = sum(emissions_total, na.rm = TRUE), .groups = "drop")

  ch4_manure <- calculate_CH4_manure(saveoutput = FALSE) %>%
    dplyr::group_by(group, zone, identification, animal_type, animal_subtype) %>%
    dplyr::summarise(Emissions_CH4_manure = sum(Emissions_CH4_Mg_year, na.rm = TRUE), .groups = "drop")

  direct_n2o <- calculate_N2O_direct_manure(saveoutput = FALSE) %>%
    dplyr::group_by(group, zone, identification, animal_type, animal_subtype) %>%
    dplyr::summarise(Emissions_N2O_direct = sum(Emisiones_N2O, na.rm = TRUE), .groups = "drop")

  n2o_indirect_vol <- calculate_N2O_indirect_volatilization(saveoutput = FALSE) %>%
    dplyr::group_by(group, zone, identification, animal_type, animal_subtype) %>%
    dplyr::summarise(Emissions_N2O_indirect_vol = sum(n2o_g, na.rm = TRUE), .groups = "drop")

  n2o_indirect_leach <- calculate_N2O_indirect_leaching(saveoutput = FALSE) %>%
    dplyr::group_by(group, zone, identification, animal_type, animal_subtype) %>%
    dplyr::summarise(Emissions_N2O_indirect_leach = sum(N2O_L, na.rm = TRUE), .groups = "drop")

  land_use <- calculate_land_use(saveoutput = FALSE) %>%
    dplyr::group_by(group, zone, identification, animal_type, animal_subtype) %>%
    dplyr::summarise(Land_use_m2 = sum(Land_use_Total_m2, na.rm = TRUE), .groups = "drop")

  # --- Combinar todos los resultados ---
  message("  -> 3/3: Consolidando resultados...")

  resumen_completo <- list(ch4_enteric, ch4_manure, direct_n2o,
                           n2o_indirect_vol, n2o_indirect_leach, land_use) %>%
    purrr::reduce(dplyr::full_join,
                  by = c("group", "zone", "identification", "animal_type", "animal_subtype"),
                  na_matches = "na") %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ tidyr::replace_na(., 0)))

  # --- ¡PASO CLAVE 2! FILTRAR ---
  # (Esta parte no cambia)
  resumen_filtrado <- resumen_completo

  if (!is.null(group)) {
    resumen_filtrado <- resumen_filtrado %>%
      dplyr::filter(group %in% .env$group)
  }
  if (!is.null(zone)) {
    if (any(is.na(zone))) {
      resumen_filtrado <- resumen_filtrado %>%
        dplyr::filter(zone %in% .env$zone | is.na(zone))
    } else {
      resumen_filtrado <- resumen_filtrado %>%
        dplyr::filter(zone %in% .env$zone)
    }
  }
  if (!is.null(animal)) {
    resumen_filtrado <- resumen_filtrado %>%
      dplyr::filter(animal_type %in% .env$animal)
  }
  if (!is.null(type)) {
    resumen_filtrado <- resumen_filtrado %>%
      dplyr::filter(animal_subtype %in% .env$type)
  }

  # --- Agregación opcional sin identification ---
  # (Esta parte no cambia)
  if (!group_by_identification) {
    final_resumen <- resumen_filtrado %>%
      dplyr::group_by(group, zone, animal_type, animal_subtype) %>%
      dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop")
  } else {
    final_resumen <- resumen_filtrado
  }

  # --- Guardar CSV ---
  # (Esta parte no cambia)
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    readr::write_csv(final_resumen, "output/Resumen_emisiones_land_use.csv")
    message("💾 Saved output to output/Resumen_emisiones_land_use.csv")
  }

  message("✅ Resumen completado.")
  return(final_resumen)
}
