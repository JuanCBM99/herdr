#' Calculate Net Energy for Maintenance (NEm)
#'
#' Computes the Net Energy for Maintenance (NEm) for livestock animals
#' based on average weight and CFI coefficients.
#'
#' @param animal Character string (optional). Filter by animal type ("Cattle", "Sheep", "Goat").
#' @param type Character string (optional). Only applies to "Cattle".
#' @param saveoutput Logical. Save CSV? Default TRUE
#' @return Tibble with code, animal_type, animal_subtype, average_weight, cfi_value, NEm
#' @export
calculate_NEm <- function(animal = NULL, type = NULL, saveoutput = TRUE) {

  message("🟢 Loading input data...")
  weights <- load_dataset("weights")
  categories <- load_dataset("categories")
  coefficients <- load_dataset("coefficients")

  # --- Preparar tabla CFI ---
  cfi_tbl <- coefficients %>%
    dplyr::filter(tolower(coefficient) == "cfi") %>%
    dplyr::select(description, value) %>%
    deframe()

  # --- Cálculo ---
  NEm_result <- weights %>%
    dplyr::left_join(categories, by = c("code", "animal_type", "animal_subtype")) %>%
    dplyr::mutate(
      cfi_value = cfi_tbl[cfi],
      NEm = cfi_value * (average_weight^0.75)
    ) %>%
    dplyr::select(code, animal_type, animal_subtype, average_weight, cfi_value, NEm) %>%
    dplyr::arrange(code)

  # --- Filtrado ---
  if (!is.null(animal)) {
    NEm_result <- NEm_result %>% dplyr::filter(animal_type == animal)
    if (!is.null(type) && animal == "cattle") {
      NEm_result <- NEm_result %>% dplyr::filter(animal_subtype == type)
    }
  }

  # --- Guardar ---
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    write.csv(NEm_result, "output/NEm_result.csv", row.names = FALSE)
    message("💾 Saved output to output/NEm_result.csv")
  }

  return(NEm_result)
}


#' Calculate Net Energy for Activity (NEa)
#'
#' Computes NEa for a given animal type using NEm and housing/activity coefficients.
#' Allows using user-provided datasets in 'user_data/' or defaults from the package.
#'
#' @param animal Character string (optional). Filter by animal type.
#' @param type Character string (optional). Only applies for Cattle.
#' @param saveoutput Logical. Save results to CSV? Default TRUE
#'
#' @return A tibble with columns: code, animal_type, animal_subtype, cfi_value, NEm, ca, NEa
#' @export
calculate_NEa <- function(animal = NULL, type = NULL, saveoutput = TRUE) {

  message("🟢 Loading input data...")

  # --- Cargar datasets ---
  categories <- load_dataset("categories")
  coefficients <- load_dataset("coefficients")

  # --- Calcular NEm usando la función adaptada ---
  NEm <- calculate_NEm(animal = animal, type = type, saveoutput = FALSE)

  # --- Preparar categorías ---
  categories_sub <- categories %>%
    dplyr::select(code, animal_type, animal_subtype, ca, duration)

  if (!is.null(animal)) {
    categories_sub <- categories_sub %>% dplyr::filter(animal_type == animal)
    if (!is.null(type) && animal == "cattle") {
      categories_sub <- categories_sub %>% dplyr::filter(animal_subtype == type)
    }
  }

  categories_sub <- categories_sub %>%
    dplyr::mutate(duration = ifelse(is.na(duration), 0, duration))

  # --- Preparar coeficientes ca ---
  ca_coeff <- coefficients %>%
    dplyr::filter(tolower(coefficient) == "ca") %>%
    dplyr::select(description, value) %>%
    deframe()

  # --- Cálculo de NEa ---
  NEa_result <- NEm %>%
    dplyr::inner_join(categories_sub, by = c("code", "animal_type", "animal_subtype")) %>%
    dplyr::mutate(
      ca_val = ca_coeff[ca],
      ca_val = ifelse(is.na(ca_val), 0, ca_val),
      second_ca_val = ca_coeff[ifelse(ca == "stall", "pasture",
                                      ifelse(ca == "pasture", "stall", ca))],
      second_ca_val = ifelse(is.na(second_ca_val), 0, second_ca_val),
      ca = dplyr::case_when(
        animal_type == "Cattle" & ca %in% c("stall", "pasture") ~ (duration/12)*ca_val + ((12-duration)/12)*second_ca_val,
        animal_type == "Cattle" & ca == "grazing large areas" ~ ca_val,
        TRUE ~ ca_val
      ),
      NEa = ca * NEm
    ) %>%
    dplyr::select(code, animal_type, animal_subtype, cfi_value, NEm, ca, NEa)

  # --- Guardar salida ---
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    write.csv(NEa_result, "output/NEa_result.csv", row.names = FALSE)
    message("💾 Saved output to output/NEa_result.csv")
  }

  return(NEa_result)
}



#' Calculate Net Energy for Growth (NEg)
#'
#' Computes NEg for Cattle, Sheep, or Goat.
#' Allows using user-provided datasets in 'user_data/' or defaults from the package.
#'
#' @param animal Character string (optional). Animal type.
#' @param type Character string (optional). Only for Cattle subtype.
#' @param saveoutput Logical (optional). If TRUE, saves the result as CSV. Default TRUE.
#'
#' @return A tibble with columns: code, animal_type, animal_subtype, c, a, b, average_weight, adult_weight, weight_gain, NEg
#' @export
calculate_NEg <- function(animal = NULL, type = NULL, saveoutput = TRUE) {

  message("🟢 Loading input data...")

  # --- Cargar datasets ---
  weights <- load_dataset("weights")
  categories <- load_dataset("categories")
  coefficients <- load_dataset("coefficients")

  # --- Filtrar por animal y tipo ---
  weights_sub <- weights
  categories_sub <- categories
  if (!is.null(animal)) {
    weights_sub <- dplyr::filter(weights_sub, animal_type == animal)
    categories_sub <- dplyr::filter(categories_sub, animal_type == animal)
    if (!is.null(type) && animal == "cattle") {
      weights_sub <- dplyr::filter(weights_sub, animal_subtype == type)
      categories_sub <- dplyr::filter(categories_sub, animal_subtype == type)
    }
  }

  # --- Preparar df para cálculo ---
  df <- weights_sub %>%
    dplyr::select(code, animal_type, animal_subtype, weight_gain, average_weight, adult_weight) %>%
    dplyr::left_join(categories_sub %>% dplyr::select(code, animal_type, animal_subtype, c, a, b),
                     by = c("code","animal_type","animal_subtype"))

  coeff_vector <- coefficients %>% dplyr::select(animal_type, description, value)

  # --- Cálculo NEg ---
  df <- df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      NEg = dplyr::case_when(
        animal_type == "cattle" ~ {
          # Extraer el valor del coeficiente C (solo 1)
          C_val <- coeff_vector$value[
            coeff_vector$animal_type == "cattle" &
              coeff_vector$description == c
          ]
          C_val <- if (length(C_val) == 0 || all(is.na(C_val))) 0 else C_val[1]

          # Calcular NEg para ganado bovino
          ifelse(
            is.na(C_val) | is.na(average_weight) | is.na(adult_weight) | is.na(weight_gain),
            0,
            22.02 * ((average_weight / (C_val * adult_weight))^0.75) * (weight_gain^1.097)
          )
        },

        animal_type %in% c("Sheep", "Goat") ~ {
          # Extraer los valores a y b (solo 1 cada uno)
          a_val <- coeff_vector$value[
            coeff_vector$animal_type == animal_type &
              coeff_vector$description == a
          ]
          b_val <- coeff_vector$value[
            coeff_vector$animal_type == animal_type &
              coeff_vector$description == b
          ]
          a_val <- if (length(a_val) == 0 || all(is.na(a_val))) 0 else a_val[1]
          b_val <- if (length(b_val) == 0 || all(is.na(b_val))) 0 else b_val[1]

          # Calcular NEg para ovinos/caprinos
          ifelse(
            is.na(a_val) | is.na(b_val) | is.na(weight_gain),
            0,
            (a_val + 0.5 * b_val) * weight_gain
          )
        },

        TRUE ~ 0
      )
    ) %>%
    dplyr::ungroup()


  # --- Guardar salida ---
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    write.csv(df, "output/NEg_result.csv", row.names = FALSE)
    message("💾 Saved output to output/NEg_result.csv")
  }

  df %>%
    dplyr::select(code, animal_type, animal_subtype, c, a, b, average_weight, adult_weight, weight_gain, NEg)
}





#' Calculate Net Energy for Lactation (NEl)
#'
#' Computes NEl based on milk yield and fat content.
#' Allows using user-provided datasets in 'user_data/' or defaults from the package.
#'
#' @param animal Character string (optional). Animal type.
#' @param type Character string (optional). Only for Cattle subtype.
#' @param saveoutput Logical (optional). If TRUE, saves the result as CSV. Default TRUE.
#'
#' @return A tibble with columns: code, animal_type, animal_subtype, Milk_yield_kg_day_head, fat_content, NEl
#' @export
calculate_NEl <- function(animal = NULL, type = NULL, saveoutput = TRUE) {

  message("🟢 Loading input data...")

  # --- Cargar dataset categories ---
  categories <- load_dataset("categories")

  # --- Filtrado por datos válidos ---
  result <- categories %>%
    dplyr::filter(!is.na(milk_yield), !is.na(fat_content), !is.na(code)) %>%
    dplyr::mutate(
      Milk_yield_kg_day_head = milk_yield / 365,
      NEl = dplyr::case_when(
        animal_type == "cattle" ~ Milk_yield_kg_day_head * (1.47 + 0.4 * fat_content),
        animal_type %in% c("sheep", "goat") ~ Milk_yield_kg_day_head * 4.6,
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::select(code, animal_type, animal_subtype, Milk_yield_kg_day_head, fat_content, NEl)

  # --- Filtrado por animal / tipo ---
  if (!is.null(animal)) {
    result <- result %>% dplyr::filter(animal_type == animal)
    if (!is.null(type) && animal == "cattle") {
      result <- result %>% dplyr::filter(animal_subtype == type)
    }
  }

  # --- Guardar salida ---
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    write.csv(result, "output/NEl_result.csv", row.names = FALSE)
    message("💾 Saved output to output/NEl_result.csv")
  }

  result
}




#' Calculate Net Working Energy (NE_work)
#'
#' NE_work = NEm * hours of activity.
#' Allows using user-provided datasets in 'user_data/' or defaults from the package.
#'
#' @param animal Character string (optional)
#' @param type Character string (optional, only for Cattle)
#' @param saveoutput Logical (optional). If TRUE, saves the result as CSV. Default TRUE.
#' @return A tibble with code, animal_type, animal_subtype, hours, NEm, NE_work
#' @export
calculate_NE_work <- function(animal = NULL, type = NULL, saveoutput = TRUE) {

  message("🟢 Loading input data...")

  # --- Cargar dataset categories ---
  categories <- load_dataset("categories")

  # --- Calcular NEm usando función adaptada ---
  NEm <- calculate_NEm(animal = animal, type = type, saveoutput = FALSE)

  # --- Filtrado por animal / tipo ---
  categories_sub <- categories
  if (!is.null(animal)) categories_sub <- dplyr::filter(categories_sub, animal_type == animal)
  if (!is.null(type) && animal == "cattle") categories_sub <- dplyr::filter(categories_sub, animal_subtype == type)

  # --- Cálculo NE_work ---
  result <- categories_sub %>%
    dplyr::inner_join(NEm, by = c("code","animal_type","animal_subtype")) %>%
    dplyr::mutate(NE_work = hours * NEm) %>%
    dplyr::select(code, animal_type, animal_subtype, hours, NEm, NE_work)

  # --- Guardar salida ---
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    write.csv(result, "output/NE_work_result.csv", row.names = FALSE)
    message("💾 Saved output to output/NE_work_result.csv")
  }

  result
}




#' Calculate Net Energy for Wool Production (NE_wool)
#'
#' NE_wool = (wool_yield * 24) / 365
#' Allows using user-provided datasets in 'user_data/' or defaults from the package.
#'
#' @param animal Character string (optional)
#' @param type Character string (optional, only for Cattle)
#' @param saveoutput Logical (optional). If TRUE, saves the result as CSV. Default TRUE.
#' @return Tibble with code, animal_type, animal_subtype, wool_yield, NE_wool
#' @export
calculate_NE_wool <- function(animal = NULL, type = NULL, saveoutput = TRUE) {

  message("🟢 Loading input data...")

  # --- Cargar dataset categories ---
  categories <- load_dataset("categories")

  # --- Filtrado y cálculo NE_wool ---
  result <- categories %>%
    dplyr::filter(!is.na(wool_yield)) %>%
    dplyr::mutate(NE_wool = (wool_yield * 24) / 365) %>%
    dplyr::select(code, animal_type, animal_subtype, wool_yield, NE_wool)

  # --- Filtrado por animal / tipo ---
  if (!is.null(animal)) {
    result <- result %>% dplyr::filter(animal_type == animal)
    if (!is.null(type) && animal == "cattle") {
      result <- result %>% dplyr::filter(animal_subtype == type)
    }
  }

  # --- Guardar salida ---
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    write.csv(result, "output/NE_wool_result.csv", row.names = FALSE)
    message("💾 Saved output to output/NE_wool_result.csv")
  }

  result
}




#' Calculate Net Energy for Pregnancy (NE_pregnancy)
#'
#' Computes NE_pregnancy with species-specific rules.
#' Allows using user-provided datasets in 'user_data/' or defaults from the package.
#'
#' @param animal Character (optional)
#' @param type Character (optional, only for Cattle)
#' @param saveoutput Logical (optional). If TRUE, saves the result as CSV. Default TRUE.
#' @return Tibble with code, animal_type, animal_subtype, c_pregnancy, NEm, NE_pregnancy
#' @export
calculate_NE_pregnancy <- function(animal = NULL, type = NULL, saveoutput = TRUE) {

  message("🟢 Loading input data...")

  # --- Cargar datasets ---
  categories <- load_dataset("categories")
  coefficients <- load_dataset("coefficients")

  # --- Calcular NEm ---
  NEm <- calculate_NEm(animal = animal, type = type, saveoutput = FALSE)

  # --- Preparar dataframe categories ---
  cat_df <- categories %>%
    dplyr::select(code, animal_type, animal_subtype, c_pregnancy, pr)

  # Filtrado por animal / tipo
  if (!is.null(animal)) {
    cat_df <- cat_df %>% dplyr::filter(animal_type == animal)
    if (!is.null(type) && animal == "cattle") {
      cat_df <- cat_df %>% dplyr::filter(animal_subtype == type)
    }
  }

  # --- Tabla de coeficientes c_pregnancy ---
  c_preg_tbl <- coefficients %>%
    dplyr::filter(tolower(coefficient) == "c_pregnancy") %>%
    dplyr::select(description, value) %>%
    # MODIFICACIÓN: Normalizar los nombres para que la búsqueda no falle
    dplyr::mutate(description = tolower(trimws(description))) %>%
    deframe()

  # --- Cálculo NE_pregnancy ---
  result <- NEm %>%
    dplyr::left_join(cat_df, by = c("code","animal_type","animal_subtype")) %>%
    dplyr::mutate(
      c_pregnancy_value = dplyr::case_when(
        animal_type=="cattle" ~ {
          # MODIFICACIÓN: Normalizar la clave de búsqueda (el texto de c_pregnancy)
          lookup_key <- tolower(trimws(as.character(c_pregnancy)))
          # MODIFICACIÓN: Buscar usando la clave normalizada.
          # La lógica anterior de 'ifelse(length(val)==0...)' se elimina
          # porque la conversión de NA a 0 ya se hace más abajo.
          c_preg_tbl[lookup_key]
        },
        animal_type %in% c("sheep","goat") & pr==0 ~ 0,
        animal_type %in% c("sheep","goat") & pr>0 ~ {
          double_birth <- pmax(pr-1,0)
          single_birth <- 1-double_birth
          0.126*double_birth + 0.077*single_birth
        },
        TRUE ~ 0
      )
      ,
      # Esta línea es la que convierte los NA (búsquedas fallidas) en 0.
      c_pregnancy_value = ifelse(is.na(c_pregnancy_value), 0, c_pregnancy_value),
      NE_pregnancy = c_pregnancy_value * NEm
    ) %>%
    dplyr::select(code, animal_type, animal_subtype, c_pregnancy = c_pregnancy_value, NEm, NE_pregnancy)

  # --- Guardar salida ---
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    write.csv(result, "output/NE_pregnancy_result.csv", row.names = FALSE)
    message("💾 Saved output to output/NE_pregnancy_result.csv")
  }

  result
}
