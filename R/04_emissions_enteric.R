#' Calculate methane emissions from enteric fermentation
#'
#' (Documentación ... sin cambios ...)
#'
#' @param animal character. Type of animal ("Cattle", "Sheep", "Goat").
#' @param type Optional character. Only for "Cattle" (e.g., "Dairy", "Beef").
#' @param zone Optional character vector. Only for "Cattle" when type is specified.
#' @param saveoutput Logical (optional). If TRUE, saves the result as CSV. Default FALSE.
#' @param population_df (Opcional) Un dataframe de población ya calculado.
#'   Si es NULL (por defecto), la función llamará a `calculate_population()`
#'   interactivamente.
#'
#' @return A tibble with columns:
#' (Columnas ... sin cambios ...)
#'
#' @export
calculate_emissions_enteric <- function(animal = NULL, type = NULL, zone = NULL, saveoutput = TRUE,
                                        population_df = NULL) { # <-- CAMBIO 1: Argumento añadido

  # --- 1. Calcular todos los inputs DE UNA VEZ ---
  message("🟢 Calculating nutritional data...")
  diet_vars <- calculate_weighted_variable(
    animal = animal, type = type, zone = zone, saveoutput = FALSE
  ) %>%
    dplyr::select(code, animal_type, animal_subtype, de, ndf, zone)

  if (nrow(diet_vars) == 0) {
    message("No diet data found for the specified filters. Returning empty result.")
    return(dplyr::tibble(
      code = character(), animal_type = character(), animal_subtype = character(),
      zone = character(), de = double(), ndf = double(), ge = double(), ym = double(),
      ef_kg_animal_year = double(), population = double(), emissions_total = double()
    ))
  }

  message("🟢 Calculating Gross Energy (GE)...")
  ge_df <- calculate_ge(
    animal = animal, type = type, zone = zone, saveoutput = FALSE
  ) %>%
    dplyr::select(code, ge, animal_type, animal_subtype, zone)

  # --- CAMBIO 3: Lógica de población modificada ---
  message("🟢 Calculating population data...")

  # Variable para guardar la población que usaremos
  pop_data_to_use <- NULL

  if (!is.null(population_df)) {
    # Opción 1: Usamos la población que nos han "pasado"
    message("♻️ Usando datos de población proporcionados.")
    pop_data_to_use <- population_df

  } else {
    # Opción 2: Nadie nos pasó los datos. Llamamos a la función interactiva
    message("-> No se proporcionaron datos de población. Calculando...")
    pop_data_to_use <- calculate_population(saveoutput = FALSE)
  }

  # El resto del código ahora usa 'pop_data_to_use'
  full_pop_df <- pop_data_to_use %>%
    dplyr::select(code, animal_type, animal_subtype, population)
  # --- Fin del Cambio 3 ---

  # --- 2. Unir y Calcular TODO EN UN SOLO PIPELINE ---
  # (Esta sección no cambia, ya que 'full_pop_df' está correctamente poblada)
  final <- diet_vars %>%
    dplyr::inner_join(ge_df, by = c("code", "animal_type", "animal_subtype", "zone")) %>%
    dplyr::inner_join(full_pop_df, by = c("code", "animal_type", "animal_subtype")) %>%
    dplyr::mutate(
      ym = dplyr::case_when(
        animal_type == "sheep" ~ 6.7,
        animal_type == "goat" ~ 5.5,
        animal_type == "cattle" & code == "k23" & de >= 70 & ndf <= 35 ~ 5.7,
        animal_type == "cattle" & code == "k23" & de >= 70 & ndf > 35  ~ 6.0,
        animal_type == "cattle" & code == "k23" & de >= 63 & de < 70 & ndf > 37 ~ 6.3,
        animal_type == "cattle" & code == "k23" & de <= 62 & ndf > 38 ~ 6.5,
        animal_type == "cattle" & code != "k23" & de >= 75 ~ 3.0,
        animal_type == "cattle" & code != "k23" & de >= 72 ~ 4.0,
        animal_type == "cattle" & code != "k23" & de >= 62 & de <= 71 ~ 6.3,
        animal_type == "cattle" & code != "k23" & de < 62 ~ 7.0,
        TRUE ~ NA_real_
      ),
      ef_kg_animal_year = (ge * (ym / 100) * 365) / 55.65,
      emissions_total = ef_kg_animal_year * (population / 1e6)
    ) %>%
    dplyr::select(
      code, animal_type, animal_subtype, zone, de, ndf, ge, ym,
      ef_kg_animal_year, population, emissions_total
    )

  # --- 3. Guardar Salida (Sin cambios) ---
  if (saveoutput && nrow(final) > 0) {
    dir.create("output", showWarnings = FALSE)
    write.csv(final, "output/enteric_emissions.csv", row.names = FALSE)
  }

  return(final)
}
