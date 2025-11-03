#' Calculate CH₄ Emissions from Manure Management
#'
#' (Documentación ... sin cambios ...)
#'
#' @param animal Character string (optional). Filter by animal type ("Cattle", "Sheep", "Goat"). Default NULL.
#' @param type Character string (optional). Only applies if `animal = "Cattle"` (e.g., "Dairy", "Beef"). Default NULL.
#' @param zone Character vector (optional). Filter by zone. Default NULL.
#' @param saveoutput Logical (optional). If TRUE, saves the result as CSV. Default FALSE.
#' @param population_df (Opcional) Un dataframe de población ya calculado.
#'   Si es NULL (por defecto), la función llamará a `calculate_population()`
#'   interactivamente.
#' @return Tibble with CH₄ emissions per animal category.
#' @export
calculate_CH4_manure <- function(animal = NULL, type = NULL, zone = NULL, saveoutput = TRUE,
                                 population_df = NULL) {

  message("🟢 Calculating CH₄ emissions from manure management...")

  # --- 1️⃣ Calcular Volatile Solids ---
  vs_df <- calculate_vs(animal = animal, type = type, zone = zone, saveoutput = FALSE) %>%
    dplyr::select(code, animal_type, animal_subtype, zone, vs)

  # --- 2️⃣ Cargar datasets usando load_dataset ---
  ch4_data <- load_dataset("ch4_mm") %>%
    dplyr::select(code, animal_category, animal_type, animal_subtype,
                  management_system, system_climate, management_duration)

  coefficients_df <- load_dataset("coefficients") %>%
    dplyr::filter(coefficient == "b_0") %>%
    dplyr::select(animal_category = description, B0 = value)

  MCF_data <- load_dataset("mcf") %>%
    dplyr::select(management_system, system_climate, mcf)

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
  pop_df <- pop_data_to_use %>%
    dplyr::select(code, animal_type, animal_subtype, population)
  # --- Fin del Cambio 3 ---


  # --- 3️⃣ Filtrar por animal / type ---
  if (!is.null(animal)) {
    ch4_data <- ch4_data %>% dplyr::filter(animal_type == animal)
    pop_df <- pop_df %>% dplyr::filter(animal_type == animal)
  }
  if (!is.null(type)) {
    ch4_data <- ch4_data %>% dplyr::filter(animal_subtype == type)
    pop_df <- pop_df %>% dplyr::filter(animal_subtype == type)
  }

  # --- 4️⃣ Unir datasets y calcular EF ---
  joined <- ch4_data %>%
    dplyr::left_join(vs_df, by = c("code", "animal_type", "animal_subtype")) %>%
    dplyr::left_join(coefficients_df, by = "animal_category") %>%
    dplyr::left_join(MCF_data, by = c("management_system", "system_climate")) %>%
    dplyr::mutate(
      awms = management_duration / 12,
      EF_CH4_kg_year = (vs * 365) * (B0 * 0.67 * mcf * awms)
    )

  # --- 5️⃣ Unir población y calcular emisiones totales ---
  final <- joined %>%
    dplyr::left_join(pop_df, by = c("code", "animal_type", "animal_subtype")) %>%
    dplyr::mutate(
      Emissions_CH4_Mg_year = EF_CH4_kg_year * population / 1e6
    ) %>%
    dplyr::select(code, animal_type, animal_subtype, zone, management_system, vs, B0, mcf, awms,
                  EF_CH4_kg_year, population, Emissions_CH4_Mg_year)

  # --- 6️⃣ Guardar CSV ---
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    write.csv(final, "output/CH4_manure.csv", row.names = FALSE)
    message("💾 Saved output to output/CH4_manure.csv")
  }

  final
}


