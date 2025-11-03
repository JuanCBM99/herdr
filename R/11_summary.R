#' Resumen de emisiones de CH4, N2O y uso de tierra
#'
#' Calcula un resumen de emisiones de CH₄ y N₂O (directas e indirectas) y de uso de tierra
#' agrupado por código, tipo de animal, subtipo y zona. Permite opcionalmente
#' agrupar a un nivel más agregado.
#'
#' @param animal Character string (opcional). Tipo de ganado (`animal_type`). Default `NULL`.
#' @param type Character string (opcional). Subtipo de ganado (`animal_subtype`). Default `NULL`.
#' @param zone Character vector (opcional). Zonas a filtrar (`zone`). Default `NULL`.
#' @param saveoutput Logical. Si `TRUE`, guarda el resultado en `"output/Resumen_emisiones_land_use.csv"`. Default `TRUE`.
#' @param group_by_code Logical. Si `TRUE` devuelve resultados al nivel de `code`. Si `FALSE`, agrega todos los `code`.
#' @return Un `tibble` con columnas para cada tipo de emisión y uso de tierra.
#' @export
resumen_emisiones_land_use <- function(animal = NULL, type = NULL, zone = NULL,
                                       saveoutput = TRUE, group_by_code = TRUE) {

  message("🟢 Iniciando resumen... Se pedirá la población 1 vez.")

  # --- ¡PASO CLAVE 1! ---
  # Llamamos a calculate_population() UNA SOLA VEZ al principio.
  # El usuario introduce los números aquí.
  pop_data <- calculate_population(saveoutput = FALSE)

  message("✅ Población obtenida. Calculando todas las emisiones...")

  # --- ¡PASO CLAVE 2! ---
  # Ahora "pasamos" pop_data a todas las funciones que lo necesiten
  # usando el nuevo argumento 'population_df'.

  ch4_enteric <- calculate_emissions_enteric(
    animal = animal, type = type, zone = zone,
    population_df = pop_data # <-- ¡AÑADIDO!
  ) %>%
    dplyr::group_by(code, animal_type, animal_subtype, zone) %>%
    dplyr::summarise(Emissions_CH4_enteric = sum(emissions_total, na.rm = TRUE), .groups = "drop")

  ch4_manure <- calculate_CH4_manure(
    animal = animal, type = type, zone = zone,
    population_df = pop_data # <-- ¡AÑADIDO!
  ) %>%
    dplyr::group_by(code, animal_type, animal_subtype, zone) %>%
    dplyr::summarise(Emissions_CH4_manure = sum(Emissions_CH4_Mg_year, na.rm = TRUE), .groups = "drop")

  direct_n2o <- calculate_N2O_direct_manure(
    animal = animal, type = type, zone = zone,
    population_df = pop_data # <-- ¡AÑADIDO!
  ) %>%
    dplyr::group_by(code, animal_type, animal_subtype, zone) %>%
    dplyr::summarise(Emissions_N2O_direct = sum(Emisiones_N2O, na.rm = TRUE), .groups = "drop")

  n2o_indirect_vol <- calculate_N2O_indirect_volatilization(
    animal = animal, type = type, zone = zone,
    population_df = pop_data # <-- ¡AÑADIDO!
  ) %>%
    dplyr::group_by(code, animal_type, animal_subtype, zone) %>%
    dplyr::summarise(Emissions_N2O_indirect_vol = sum(n2o_g, na.rm = TRUE), .groups = "drop")

  n2o_indirect_leach <- calculate_N2O_indirect_leaching(
    animal = animal, type = type, zone = zone,
    population_df = pop_data # <-- ¡AÑADIDO!
  ) %>%
    dplyr::group_by(code, animal_type, animal_subtype, zone) %>%
    dplyr::summarise(Emissions_N2O_indirect_leach = sum(N2O_L, na.rm = TRUE), .groups = "drop")

  land_use <- calculate_land_use(
    animal = animal, type = type, zone = zone,
    population_df = pop_data # <-- ¡AÑADIDO!
  ) %>%
    dplyr::group_by(code, animal_type, animal_subtype, zone) %>%
    dplyr::summarise(Land_use_m2 = sum(Land_use, na.rm = TRUE), .groups = "drop")

  # --- Combinar todos los resultados ---
  # (Esta parte no cambia)
  resumen <- list(ch4_enteric, ch4_manure, direct_n2o,
                  n2o_indirect_vol, n2o_indirect_leach, land_use) %>%
    purrr::reduce(dplyr::full_join, by = c("code", "animal_type", "animal_subtype", "zone")) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), \(x) tidyr::replace_na(x, 0)))

  # --- Agregación opcional sin code ---
  # (Esta parte no cambia)
  if (!group_by_code) {
    resumen <- resumen %>%
      dplyr::group_by(animal_type, animal_subtype, zone) %>%
      dplyr::summarise(dplyr::across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop")
  }

  # --- Guardar CSV ---
  # (Esta parte no cambia)
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    readr::write_csv(resumen, "output/Resumen_emisiones_land_use.csv")
    message("💾 Saved output to output/Resumen_emisiones_land_use.csv")
  }

  resumen
}
