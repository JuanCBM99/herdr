#' Calculate Net Energy for Maintenance (NEm)
#'
#' Computes NEm for all livestock animals based on average weight and
#' CFI coefficients found in the input files.
#'
#' @param saveoutput Logical. Save CSV? Default TRUE
#' @return Tibble with NEm for all animal categories.
#' @export
calculate_NEm <- function(saveoutput = TRUE) {

  message("🟢 Loading input data (weights, categories, coefficients)...")
  weights <- load_dataset("weights")
  categories <- load_dataset("categories")
  coefficients <- load_dataset("coefficients")

  # --- Validaciones de Columnas Esenciales ---
  req_cols_weights <- c("identification", "animal_type", "animal_subtype", "average_weight")
  if (!all(req_cols_weights %in% names(weights))) {
    stop(paste("Error: 'weights.csv' debe contener:", paste(req_cols_weights, collapse = ", ")))
  }

  req_cols_cat <- c("identification", "animal_type", "animal_subtype", "cfi")
  if (!all(req_cols_cat %in% names(categories))) {
    stop(paste("Error: 'categories.csv' debe contener:", paste(req_cols_cat, collapse = ", ")))
  }

  # --- Preparar tabla CFI ---
  cfi_tbl <- coefficients %>%
    dplyr::filter(tolower(coefficient) == "cfi") %>%
    dplyr::select(description, value) %>%
    tibble::deframe()

  # --- Cálculo ---

  categories_cfi <- categories %>%
    dplyr::select(identification, animal_type, animal_subtype, cfi)

  nem_result <- weights %>%
    dplyr::left_join(categories_cfi, by = c("identification", "animal_type", "animal_subtype")) %>%
    dplyr::mutate(
      cfi_value = cfi_tbl[cfi],
      NEm = cfi_value * (average_weight^0.75)
    ) %>%
    dplyr::select(
      dplyr::any_of(c("group", "zone")), # <--- any_of() aquí en select() está BIEN
      identification, animal_type, animal_subtype,
      average_weight, cfi_value, NEm
    ) %>%

    # --- ¡LÍNEA CORREGIDA! ---
    # Envolvemos any_of() dentro de across() para arrange()
    dplyr::arrange(dplyr::across(dplyr::any_of(c("group", "zone"))), identification)

  # --- Guardar ---
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    readr::write_csv(nem_result, "output/NEm_result.csv")
    message("💾 Saved output to output/NEm_result.csv")
  }

  return(nem_result)
}


#' Calculate Net Energy for Activity (NEa)
#'
#' Computes NEa for all animals using NEm and housing/activity coefficients.
#'
#' @param saveoutput Logical. Save results to CSV? Default TRUE
#' @return A tibble with NEa for all animal categories.
#' @export
calculate_NEa <- function(saveoutput = TRUE) { # <-- Filtros eliminados

  message("🟢 Loading input data (categories, coefficients)...")

  # --- Cargar datasets ---
  categories <- load_dataset("categories")
  coefficients <- load_dataset("coefficients")

  # --- Calcular NEm (versión sin filtros) ---
  NEm <- calculate_NEm(saveoutput = FALSE)

  # --- Preparar categorías ---
  # (Ya no se filtra, solo se seleccionan columnas)
  categories_sub <- categories %>%
    dplyr::select(identification, animal_type, animal_subtype, ca, grazing_months) %>%
    dplyr::mutate(grazing_months = ifelse(is.na(grazing_months), 0, grazing_months))

  # --- Preparar coeficientes ca ---
  ca_coeff <- coefficients %>%
    dplyr::filter(tolower(coefficient) == "ca") %>%
    dplyr::select(description, value) %>%
    tibble::deframe()

  # --- Cálculo de NEa ---
  NEa_result <- NEm %>%
    dplyr::inner_join(categories_sub, by = c("identification", "animal_type", "animal_subtype")) %>%
    dplyr::mutate(
      ca_val = ca_coeff[ca],
      ca_val = ifelse(is.na(ca_val), 0, ca_val),
      second_ca_val = ca_coeff[ifelse(ca == "stall", "pasture",
                                      ifelse(ca == "pasture", "stall", ca))],
      second_ca_val = ifelse(is.na(second_ca_val), 0, second_ca_val),
      ca = dplyr::case_when(
        animal_type == "cattle" & ca %in% c("stall", "pasture") ~ (grazing_months/12)*ca_val + ((12-grazing_months)/12)*second_ca_val,
        animal_type == "cattle" & ca == "grazing large areas" ~ ca_val,
        TRUE ~ ca_val
      ),
      NEa = ca * NEm
    ) %>%
    dplyr::select(
      dplyr::any_of(c("group", "zone")), # Mantiene group/zone si existen
      identification, animal_type, animal_subtype, cfi_value, NEm, ca, NEa
    )

  # --- Guardar salida ---
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    readr::write_csv(NEa_result, "output/NEa_result.csv")
    message("💾 Saved output to output/NEa_result.csv")
  }

  return(NEa_result)
}


  #' Calculate Net Energy for Growth (NEg)
  #'
  #' Computes NEg for all animals.
  #'
  #' @param saveoutput Logical (optional). If TRUE, saves the result as CSV. Default TRUE.
  #' @return A tibble with NEg for all animal categories.
  #' @export

  calculate_NEg <- function(saveoutput = TRUE) { # <-- Filtros eliminados

    message("🟢 Loading input data (weights, categories, coefficients)...")

    # --- Cargar datasets ---
    weights <- load_dataset("weights")
    categories <- load_dataset("categories")
    coefficients <- load_dataset("coefficients")

    # --- Bloque de filtrado eliminado ---

    # --- Preparar df para cálculo ---
    # (Se usan los dataframes completos)
    df <- weights %>%
      dplyr::select(
        dplyr::any_of(c("group", "zone")),
        identification, animal_type, animal_subtype, weight_gain, average_weight, adult_weight
      ) %>%
      dplyr::left_join(
        categories %>% dplyr::select(identification, animal_type, animal_subtype, c, a, b),
        by = c("identification","animal_type","animal_subtype")
      )

    coeff_vector <- coefficients %>% dplyr::select(animal_type, description, value)

    # --- Cálculo NEg ---
    df <- df %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        NEg = dplyr::case_when(
          animal_type == "cattle" ~ {
            C_val <- coeff_vector$value[
              coeff_vector$animal_type == "cattle" &
                coeff_vector$description == c
            ]
            C_val <- if (length(C_val) == 0 || all(is.na(C_val))) 0 else C_val[1]

            ifelse(
              is.na(C_val) | is.na(average_weight) | is.na(adult_weight) | is.na(weight_gain),
              0,
              22.02 * ((average_weight / (C_val * adult_weight))^0.75) * (weight_gain^1.097)
            )
          },

          animal_type %in% c("Sheep", "Goat") ~ {
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
      readr::write_csv(df, "output/NEg_result.csv")
      message("💾 Saved output to output/NEg_result.csv")
    }

    # Seleccionamos las columnas finales
    df %>%
      dplyr::select(
        dplyr::any_of(c("group", "zone")),
        identification, animal_type, animal_subtype, c, a, b,
        average_weight, adult_weight, weight_gain, NEg
      )
  }


  #' Calculate Net Energy for Lactation (NEl)
  #'
  #' Computes NEl based on milk yield and fat content for all animals.
  #'
  #' @param saveoutput Logical (optional). If TRUE, saves the result as CSV. Default TRUE.
  #' @return A tibble with NEl for all relevant animal categories.
  #' @export
  calculate_NEl <- function(saveoutput = TRUE) { # <-- Filtros eliminados

    message("🟢 Loading input data (categories)...")

    # --- Cargar dataset categories ---
    categories <- load_dataset("categories")

    # --- Filtrado por datos válidos ---
    result <- categories %>%
      dplyr::filter(!is.na(milk_yield), !is.na(fat_content), !is.na(identification)) %>%
      dplyr::mutate(
        Milk_yield_kg_day_head = milk_yield / 365,
        NEl = dplyr::case_when(
          animal_type == "cattle" ~ Milk_yield_kg_day_head * (1.47 + 0.4 * fat_content),
          animal_type %in% c("sheep", "goat") ~ Milk_yield_kg_day_head * 4.6,
          TRUE ~ NA_real_
        )
      ) %>%
      dplyr::select(
        dplyr::any_of(c("group", "zone")),
        identification, animal_type, animal_subtype,
        Milk_yield_kg_day_head, fat_content, NEl
      )

    # --- Bloque de filtrado por animal / tipo eliminado ---

    # --- Guardar salida ---
    if (saveoutput) {
      dir.create("output", showWarnings = FALSE)
      readr::write_csv(result, "output/NEl_result.csv")
      message("💾 Saved output to output/NEl_result.csv")
    }

    result
  }


  #' Calculate Net Working Energy (NE_work)
  #'
  #' NE_work = NEm * hours of activity.
  #'
  #' @param saveoutput Logical (optional). If TRUE, saves the result as CSV. Default TRUE.
  #' @return A tibble with NE_work for all relevant animal categories.
  #' @export
  calculate_NE_work <- function(saveoutput = TRUE) { # <-- Filtros eliminados

    message("🟢 Loading input data (categories)...")

    # --- Cargar dataset categories ---
    categories <- load_dataset("categories")

    # --- Calcular NEm (versión sin filtros) ---
    NEm <- calculate_NEm(saveoutput = FALSE)

    # --- Bloque de filtrado por animal / tipo eliminado ---

    # --- Cálculo NE_work ---
    # (Se usa el dataframe 'categories' completo)
    result <- categories %>%
      dplyr::filter(!is.na(hours) & hours > 0) %>% # Filtramos solo los que trabajan
      dplyr::inner_join(NEm, by = c("identification","animal_type","animal_subtype")) %>%
      dplyr::mutate(NE_work = hours * NEm) %>%
      dplyr::select(
        dplyr::any_of(c("group", "zone")),
        identification, animal_type, animal_subtype, hours, NEm, NE_work
      )

    # --- Guardar salida ---
    if (saveoutput) {
      dir.create("output", showWarnings = FALSE)
      readr::write_csv(result, "output/NE_work_result.csv")
      message("💾 Saved output to output/NE_work_result.csv")
    }

    result
  }


  #' Calculate Net Energy for Wool Production (NE_wool)
  #'
  #' NE_wool = (wool_yield * 24) / 365
  #'
  #' @param saveoutput Logical (optional). If TRUE, saves the result as CSV. Default TRUE.
  #' @return Tibble with NE_wool for all relevant animal categories.
  #' @export
  calculate_NE_wool <- function(saveoutput = TRUE) { # <-- Filtros eliminados

    message("🟢 Loading input data (categories)...")

    # --- Cargar dataset categories ---
    categories <- load_dataset("categories")

    # --- Filtrado y cálculo NE_wool ---
    result <- categories %>%
      dplyr::filter(!is.na(wool_yield) & wool_yield > 0) %>% # Filtramos solo los que producen lana
      dplyr::mutate(NE_wool = (wool_yield * 24) / 365) %>%
      dplyr::select(
        dplyr::any_of(c("group", "zone")),
        identification, animal_type, animal_subtype, wool_yield, NE_wool
      )

    # --- Bloque de filtrado por animal / tipo eliminado ---

    # --- Guardar salida ---
    if (saveoutput) {
      dir.create("output", showWarnings = FALSE)
      readr::write_csv(result, "output/NE_wool_result.csv")
      message("💾 Saved output to output/NE_wool_result.csv")
    }

    result
  }


  #' Calculate Net Energy for Pregnancy (NE_pregnancy)
  #'
  #' Computes NE_pregnancy with species-specific rules for all animals.
  #'
  #' @param saveoutput Logical (optional). If TRUE, saves the result as CSV. Default TRUE.
  #' @return Tibble with NE_pregnancy for all relevant animal categories.
  #' @export
  calculate_NE_pregnancy <- function(saveoutput = TRUE) { # <-- Filtros eliminados

    message("🟢 Loading input data (categories, coefficients)...")

    # --- Cargar datasets ---
    categories <- load_dataset("categories")
    coefficients <- load_dataset("coefficients")

    # --- Calcular NEm (versión sin filtros) ---
    NEm <- calculate_NEm(saveoutput = FALSE) # <-- Esta llamada ahora funciona

    # --- Preparar dataframe categories ---
    cat_df <- categories %>%
      dplyr::select(
        dplyr::any_of(c("group", "zone")), # <-- any_of() en select() está bien
        identification, animal_type, animal_subtype, c_pregnancy, pr
      )

    # --- Tabla de coeficientes c_pregnancy ---
    c_preg_tbl <- coefficients %>%
      dplyr::filter(tolower(coefficient) == "c_pregnancy") %>%
      dplyr::select(description, value) %>%
      dplyr::mutate(description = tolower(trimws(description))) %>%
      tibble::deframe()

    # --- ¡BLOQUE CORREGIDO! ---
    # Construimos las claves de unión dinámicamente

    # Claves base
    join_keys <- c("identification", "animal_type", "animal_subtype")

    # Añadimos 'group' y 'zone' a las claves SI existen en AMBOS dataframes
    if ("group" %in% names(NEm) && "group" %in% names(cat_df)) {
      join_keys <- c("group", join_keys)
    }
    if ("zone" %in% names(NEm) && "zone" %in% names(cat_df)) {
      join_keys <- c("zone", join_keys)
    }

    # --- Cálculo NE_pregnancy ---
    result <- NEm %>%
      # Usamos las 'join_keys' seguras
      dplyr::left_join(cat_df, by = join_keys) %>%
      dplyr::mutate(
        c_pregnancy_value = dplyr::case_when(
          animal_type=="cattle" ~ {
            lookup_key <- tolower(trimws(as.character(c_pregnancy)))
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
        , # Esta coma SÍ es correcta (separa 'c_pregnancy_value' de la siguiente línea)
        c_pregnancy_value = ifelse(is.na(c_pregnancy_value), 0, c_pregnancy_value),
        NE_pregnancy = c_pregnancy_value * NEm
      ) %>%
      # Filtramos para quedarnos solo con los que tienen valor
      dplyr::filter(NE_pregnancy > 0) %>%
      dplyr::select(
        dplyr::any_of(c("group", "zone")), # <-- any_of() en select() está bien
        identification, animal_type, animal_subtype, c_pregnancy = c_pregnancy_value, NEm, NE_pregnancy
      )

    # --- Guardar salida ---
    if (saveoutput) {
      dir.create("output", showWarnings = FALSE)
      readr::write_csv(result, "output/NE_pregnancy_result.csv")
      message("💾 Saved output to output/NE_pregnancy_result.csv")
    }

    return(result)
  }
