#' Resumen de emisiones de CH4, N2O y uso de tierra
#'
#' Calcula un resumen de emisiones de CH₄ y N₂O (directas e indirectas) y de uso de tierra
#' agrupado por código, tipo de animal, subtipo y zona. Permite opcionalmente
#' agrupar a un nivel más agregado (por ejemplo, sumando todos los `code` de un mismo animal).
#'
#' @param animal Character string (opcional). Tipo de ganado (`animal_type`). Default `NULL` (todos).
#' @param type Character string (opcional). Subtipo de ganado (`animal_subtype`). Default `NULL` (todos).
#' @param zone Character vector (opcional). Zonas a filtrar (`zone`). Default `NULL` (todas).
#' @param saveoutput Logical. Si `TRUE`, guarda el resultado en `"output/Resumen_emisiones_land_use.csv"`. Default `TRUE`.
#' @param group_by_code Logical. Si `TRUE` (default) devuelve los resultados al nivel de
#' `code` + `animal_type` + `animal_subtype` + `zone`. Si `FALSE`, agrupa
#' únicamente por `animal_type` + `animal_subtype` + `zone` (sumando todos los `code`).
#'
#' @return Un `tibble` con columnas para cada tipo de emisión y uso de tierra.
#' Si `saveoutput = TRUE`, además guarda un CSV en `"output/Resumen_emisiones_land_use.csv"`.
#'
#' @details
#' Internamente llama a:
#' * `calculate_emissions_enteric()`
#' * `calculate_CH4_manure()`
#' * `calculate_N2O_direct_manure()`
#' * `calculate_N2O_indirect_volatilization()`
#' * `calculate_N2O_indirect_leaching()`
#' * `calculate_land_use()`
#'
#' Agrupa y suma los resultados por las variables seleccionadas.
#'
#' @examples
#' \donttest{
#' # Resumen al nivel de cada code:
#' resumen <- resumen_emisiones_land_use(animal = "Cattle", type = "Dairy", zone = c("A"))
#'
#' # Resumen agregando todos los code de Cattle juntos:
#' resumen2 <- resumen_emisiones_land_use(animal = "Cattle", group_by_code = FALSE)
#' print(resumen2)
#' }
#'
#' @export
resumen_emisiones_land_use <- function(animal = NULL, type = NULL, zone = NULL,
                                       saveoutput = TRUE, group_by_code = TRUE) {

  ch4_enteric <- calculate_emissions_enteric(animal = animal, type = type, zone = zone) %>%
    dplyr::group_by(code, animal_type, animal_subtype, zone) %>%
    dplyr::summarise(Emissions_CH4_enteric = sum(emissions_total, na.rm = TRUE), .groups = "drop")

  ch4_manure <- calculate_CH4_manure(animal = animal, type = type, zone = zone) %>%
    dplyr::group_by(code, animal_type, animal_subtype, zone) %>%
    dplyr::summarise(Emissions_CH4_manure = sum(Emissions_CH4_Mg_year, na.rm = TRUE), .groups = "drop")

  direct_n2o <- calculate_N2O_direct_manure(animal = animal, type = type, zone = zone) %>%
    dplyr::group_by(code, animal_type, animal_subtype, zone, management_system, climate, management_duration) %>%
    dplyr::summarise(Emissions_N2O_direct = sum(Emisiones_N2O, na.rm = TRUE), .groups = "drop")

  n2o_indirect_vol <- calculate_N2O_indirect_volatilization(animal = animal, type = type, zone = zone) %>%
    dplyr::group_by(code, animal_type, animal_subtype, zone) %>%
    dplyr::summarise(Emissions_N2O_indirect_vol = sum(n2o_g, na.rm = TRUE), .groups = "drop")

  n2o_indirect_leach <- calculate_N2O_indirect_leaching(animal = animal, type = type, zone = zone) %>%
    dplyr::group_by(code, animal_type, animal_subtype, zone) %>%
    dplyr::summarise(Emissions_N2O_indirect_leach = sum(N2O_L, na.rm = TRUE), .groups = "drop")

  land_use <- calculate_land_use(animal = animal, type = type, zone = zone) %>%
    dplyr::group_by(code, animal_type, animal_subtype, zone) %>%
    dplyr::summarise(Land_use_m2 = sum(Land_use, na.rm = TRUE), .groups = "drop")

  resumen <- list(ch4_enteric, ch4_manure, direct_n2o,
                  n2o_indirect_vol, n2o_indirect_leach, land_use) %>%
    purrr::reduce(dplyr::full_join, by = c("code", "animal_type", "animal_subtype", "zone")) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~tidyr::replace_na(., 0)))

  # reagrupación opcional
  if (!group_by_code) {
    resumen <- resumen %>%
      dplyr::group_by(animal_type, animal_subtype, zone) %>%
      dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop")
  }

  if (saveoutput) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(resumen, "output/Resumen_emisiones_land_use.csv")
  }

  return(resumen)
}

