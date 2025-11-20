#' Calculate Weighted Nutritional Variables
#'
#' Computes weighted averages of nutritional variables based on animal diets,
#' using the 'diet_tag' architecture.
#'
#' @param saveoutput Logical. Save results to CSV? Default TRUE
#'
#' @return A data frame with weighted nutritional values by all key properties.
#' @export
calculate_weighted_variable <- function(saveoutput = TRUE) {

  message("🟢 Loading input data (diet, ingredients, characteristics, categories)...")

  # --- 1. Cargar datasets ---
  diet <- load_dataset("diet")
  ingredients <- load_dataset("ingredients")
  characteristics <- load_dataset("characteristics")
  categories <- load_dataset("categories") # El "Traductor"

  # --- 2. Validaciones ---
  # Comprobamos que existan las columnas clave.
  # Nota: No exigimos animal_type/subtype en diet/ingredients, el diet_tag es suficiente.

  if (!all(c("group", "zone", "diet_tag", "forage_share", "feed_share") %in% names(diet))) {
    stop("Error: 'diet.csv' columnas incorrectas.")
  }
  if (!all(c("group", "zone", "diet_tag", "ingredient", "ingredient_share") %in% names(ingredients))) {
    stop("Error: 'ingredients.csv' columnas incorrectas.")
  }
  # En categories sí necesitamos identification y diet_tag
  if (!all(c("identification", "diet_tag", "animal_type", "animal_subtype") %in% names(categories))) {
    stop("Error: 'categories.csv' columnas incorrectas.")
  }

  # --- 3. Joins y Cálculo ---

  # Claves de unión (SIMPLIFICADAS)
  # Solo usamos lo necesario para identificar la dieta
  join_keys_diet <- c("group", "zone", "diet_tag")
  join_keys_char <- c("ingredient", "ingredient_type")

  # --- PASO A: Calcular el valor nutricional de cada 'diet_tag' ---

  # 1. Unir 'diet' e 'ingredients'
  diet_full <- diet %>%
    dplyr::left_join(ingredients, by = join_keys_diet, na_matches = "na")

  # 2. Unir 'characteristics' (valores nutricionales)
  diet_with_char <- diet_full %>%
    dplyr::left_join(characteristics, by = join_keys_char, na_matches = "na")

  # 3. Variables a calcular
  vars_to_calc <- intersect(c("cp", "de", "ash", "ndf"), names(diet_with_char))

  # 4. Cálculo ponderado
  calculated_diets <- diet_with_char %>%
    dplyr::mutate(weight = dplyr::case_when(
      ingredient_type == "feed"        ~ feed_share,
      ingredient_type == "forage"      ~ forage_share,
      ingredient_type == "milk"        ~ milk_share,
      ingredient_type == "milk_replacer" ~ milk_replacer_share,
      TRUE ~ 0
    )) %>%
    # Reemplazar NAs por 0
    dplyr::mutate(
      dplyr::across(
        c(dplyr::all_of(vars_to_calc), "ingredient_share", "weight"),
        ~ tidyr::replace_na(., 0)
      )
    ) %>%
    # Aplicar la fórmula de ponderación
    dplyr::mutate(
      de_weighted  = de  * ingredient_share/100,
      cp_weighted  = cp  * ingredient_share/100,
      ndf_weighted = ndf * ingredient_share/100,
      ash_weighted = ash * ingredient_share/100,
    ) %>%

    # 5. Agrupar por 'diet_tag' (NO por animal_type, eso viene luego)
    dplyr::group_by(
      group, zone, diet_tag
    ) %>%
    dplyr::summarise(
      de  = sum(de_weighted * weight/100, na.rm = TRUE),
      cp  = sum(cp_weighted * weight/100, na.rm = TRUE),
      ndf = sum(ndf_weighted * weight/100, na.rm = TRUE),
      ash = sum(ash_weighted * weight/100, na.rm = TRUE),
      .groups = "drop"
    )

  # --- PASO B: Unir el "Traductor" (categories) ---
  # Aquí es donde asignamos la dieta a cada animal específico

  variables <- categories %>%
    # Seleccionar columnas necesarias de categories
    dplyr::select(identification, diet_tag, animal_type, animal_subtype) %>%

    # Unir los valores calculados SOLO por 'diet_tag'
    # (categories provee el animal_type, calculated_diets provee los nutrientes)
    dplyr::left_join(calculated_diets, by = "diet_tag", na_matches = "na") %>%

    # Seleccionar y reordenar
    dplyr::select(
      group, zone, identification, animal_type, animal_subtype, diet_tag,
      dplyr::all_of(vars_to_calc)
    ) %>%
    # Filtrar filas que no hayan cruzado bien (opcional, por limpieza)
    dplyr::filter(!is.na(group)) %>%
    dplyr::arrange(group, zone, identification)

  # --- 6. Guardar salida ---
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    readr::write_csv(variables, "output/variables.csv")
    message("💾 Saved output to output/variables.csv")
  }

  return(variables)
}
