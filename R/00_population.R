#' Calcula la población animal (Versión Paquete)
#'
#' Carga los datos internos del paquete (categories, rate_parameters),
#' pide al usuario la población base (k18, k23, k24) de forma interactiva
#' y calcula la población restante.
#'
#' @details Esta función asume que los dataframes 'rate_parameters' y
#'   'categories' están disponibles en el entorno del paquete (cargados
#'   desde los archivos .rda en la carpeta 'data' del paquete).
#'
#' @param saveoutput Logical (opcional). Si es TRUE, guarda el resultado
#'   como CSV. Por defecto es TRUE.
#'
#' @return Un tibble con la población completa (base + calculada) incluyendo
#'   columnas descriptivas del dataframe de categorías.
#' @export
#'
calculate_population <- function(saveoutput = TRUE) {

  # --- 1. Pedir datos de población al usuario ---
  message("Por favor, introduce los datos de población base:")

  pop_k18_char <- readline("Población de 'k18' (Toros de carne): ")
  pop_k23_char <- readline("Población de 'k23' (Vacas de leche): ")
  pop_k24_char <- readline("Población de 'k24' (Vacas de carne): ")

  # Convertimos el texto a NÚMERO
  pop_k18 <- as.numeric(pop_k18_char)
  pop_k23 <- as.numeric(pop_k23_char)
  pop_k24 <- as.numeric(pop_k24_char)


  # --- 2. Validar inputs ---
  # Valida los NÚMEROS que el usuario acaba de escribir
  if (any(is.na(c(pop_k18, pop_k23, pop_k24)))) {
    stop("Error: Las poblaciones (k18, k23, k24) deben ser números válidos.")
  }

  # ¡NUEVO! Validar que los datos del paquete (.rda) existen.
  # (Esto da un error útil si 'data/rate_parameters.rda' no se cargó)
  if (!exists("rate_parameters") || !exists("categories")) {
    stop("Error: Faltan los datos 'rate_parameters' o 'categories'. ",
         "Asegúrate de que el paquete esté cargado correctamente.")
  }

  message("🟢 Inputs validados.")
  message("📊 Extrayendo tasas...")

  # --- 3. Extraer tasas ---

  # ¡MODIFICADO!
  # Esta función anidada ahora usa 'rate_parameters' (el objeto .rda
  # de tu paquete) directamente. Ya no necesita un argumento 'df'.
  get_rate <- function(param, subtype, sex_val = NA) {

    # 'rate_parameters' se refiere al objeto .rda cargado con el paquete
    filtered_df <- rate_parameters %>%
      dplyr::filter(parameter == param, animal_subtype == subtype)

    if (!is.na(sex_val)) {
      filtered_df <- filtered_df %>% dplyr::filter(sex == sex_val)
    }

    value <- filtered_df %>% dplyr::pull(value)

    if (length(value) == 0) {
      warning(paste("Tasa no encontrada para:", param, subtype, sex_val))
      return(NA_real_)
    }
    return(value[1])
  }

  # ¡MODIFICADO!
  # Ya no pasamos 'rate_parameters_df' a la función get_rate.
  rate_dairy_calving <- get_rate("calving_rate", "dairy")
  rate_beef_calving <- get_rate("calving_rate", "beef")
  rate_beef_male_repl <- get_rate("replacement_rate", "beef", "male")
  rate_beef_female_repl <- get_rate("replacement_rate", "beef", "female")
  rate_dairy_female_repl <- get_rate("replacement_rate", "dairy", "female")

  all_rates <- c(rate_dairy_calving, rate_beef_calving, rate_beef_male_repl,
                 rate_beef_female_repl, rate_dairy_female_repl)
  if (any(is.na(all_rates))) {
    stop("Error: Faltan una o más tasas. Revisa 'rate_parameters' del paquete.")
  }

  message("🧮 Calculating new populations...")

  # --- 4. Calcular nuevas poblaciones ---
  # (Esta sección no cambia)
  pop_k7 <- pop_k18 * rate_beef_male_repl
  pop_k8 <- pop_k24 * rate_beef_female_repl
  pop_k9 <- pop_k23 * rate_dairy_female_repl
  total_dairy_births_per_sex <- pop_k23 * rate_dairy_calving / 2
  total_beef_births_per_sex <- pop_k24 * rate_beef_calving / 2
  pop_k1 <- total_dairy_births_per_sex
  pop_k2 <- total_dairy_births_per_sex - pop_k9
  pop_k3 <- total_beef_births_per_sex - pop_k7
  pop_k5 <- total_beef_births_per_sex - pop_k8

  message("🧩 Assembling final dataframe...")

  # --- 5. Ensamblar 'tibble' de resultados ---
  # (Esta sección no cambia)
  new_populations <- tibble::tribble(
    ~code, ~population,
    "k1",  pop_k1,
    "k2",  pop_k2,
    "k3",  pop_k3,
    "k5",  pop_k5,
    "k7",  pop_k7,
    "k8",  pop_k8,
    "k9",  pop_k9,
    "k10", pop_k7,
    "k14", pop_k8,
    "k16", pop_k9,
    "k22", pop_k8
  )
  base_populations <- tibble::tribble(
    ~code, ~population,
    "k18", pop_k18,
    "k23", pop_k23,
    "k24", pop_k24
  )
  all_populations <- dplyr::bind_rows(base_populations, new_populations)

  # --- 6. Unir con descripciones ---

  # ¡MODIFICADO!
  # Usamos 'categories' (el objeto .rda de tu paquete) directamente.
  result <- all_populations %>%
    dplyr::left_join(categories, by = "code") %>%
    dplyr::select(
      code,
      population,
      category,
      sex,
      destination,
      housing_type,
      animal_type,
      animal_subtype
    )

  # --- 7. Guardar y devolver ---
  # (Esta sección no cambia)
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    write.csv(result, "output/population_result.csv", row.names = FALSE)
    message("💾 Saved output to output/population_result.csv")
  }

  return(result)
}
