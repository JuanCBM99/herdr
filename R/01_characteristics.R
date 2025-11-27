#' Calculate Weighted Nutritional Variables (Refactored)
#'
#' Computes weighted averages of nutritional variables (DE, CP, NDF, Ash)
#' by mapping ingredients to diets, and diets to animals.
#' @export
calculate_weighted_variable <- function(saveoutput = TRUE) {

  message("🟢 Calculating Weighted Nutritional Variables...")

  # --- 1. Carga de Datos ---
  diet            <- load_dataset("diet")
  ingredients     <- load_dataset("ingredients")
  characteristics <- load_dataset("characteristics")
  categories      <- load_dataset("categories") # El "Traductor"

  # Validaciones rápidas
  stopifnot(
    all(c("group", "zone", "diet_tag", "forage_share", "feed_share") %in% names(diet)),
    all(c("diet_tag", "ingredient", "ingredient_share") %in% names(ingredients)),
    all(c("identification", "diet_tag") %in% names(categories))
  )

  # --- 2. Fase A: Calcular Perfil Nutricional por Diet Tag ---
  # Objetivo: Obtener una fila por dieta con sus valores promedios (DE, CP, etc.)

  diet_profiles <- diet %>%
    # Unir ingredientes y sus características nutricionales
    dplyr::left_join(ingredients, by = c("group", "zone", "diet_tag")) %>%
    dplyr::left_join(characteristics, by = c("ingredient", "ingredient_type")) %>%

    dplyr::mutate(
      # Determinar qué porcentaje de la dieta total representa esta categoría
      category_share = dplyr::case_when(
        ingredient_type == "feed"          ~ feed_share,
        ingredient_type == "forage"        ~ forage_share,
        ingredient_type == "milk"          ~ milk_share,
        ingredient_type == "milk_replacer" ~ milk_replacer_share,
        TRUE                               ~ 0
      ),

      # Limpieza numérica (Convertir a numérico y NAs a 0)
      across(
        c(de, cp, ash, ndf, ingredient_share, category_share),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),

      # Factor de Ponderación Final
      # (Share del Ingrediente % * Share de la Categoría %) / 10000
      # Ejemplo: Maíz (50% del concentrado) en una dieta con 40% concentrado = 0.50 * 0.40 = 0.20 del total
      weight_factor = (ingredient_share * category_share) / 10000
    ) %>%

    # Agrupamos por DIETA (group, zone, diet_tag) para sumar los aportes
    dplyr::group_by(group, zone, diet_tag) %>%
    dplyr::summarise(
      de  = sum(de * weight_factor,  na.rm = TRUE),
      cp  = sum(cp * weight_factor,  na.rm = TRUE),
      ndf = sum(ndf * weight_factor, na.rm = TRUE),
      ash = sum(ash * weight_factor, na.rm = TRUE),
      .groups = "drop"
    )

  # --- 3. Fase B: Asignar Dietas a Animales ---
  # Objetivo: Expandir los perfiles calculados a cada animal específico

  results <- categories %>%
    dplyr::select(identification, diet_tag, animal_type, animal_subtype) %>%

    # Unimos los perfiles calculados arriba
    # Usamos inner_join o left_join dependiendo si queremos conservar animales sin dieta definida
    # (Left join es más seguro para no perder animales)
    dplyr::left_join(diet_profiles, by = "diet_tag") %>%

    # Filtramos filas basura (si el join falló porque no había grupo/zona)
    dplyr::filter(!is.na(group)) %>%

    # Selección Final
    dplyr::select(
      group, zone, identification, animal_type, animal_subtype, diet_tag,
      de, cp, ndf, ash
    ) %>%
    dplyr::arrange(group, zone, identification) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))

  # --- 4. Guardado ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/variables.csv")
    message("💾 Saved output to output/variables.csv")
  }

  return(results)
}
