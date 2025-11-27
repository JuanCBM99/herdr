#' Calculate Net Energy for Maintenance (NEm) (Refactored)
#'
#' Computes NEm based on average weight and CFI coefficients using a relational join approach.
#' @export
calculate_NEm <- function(saveoutput = TRUE) {

  message("🟢 Calculating Net Energy for Maintenance (NEm)...")

  # --- 1. Carga de Datos ---
  weights      <- load_dataset("weights")
  categories   <- load_dataset("categories")
  coefficients <- load_dataset("coefficients")

  # Validaciones (concisas y efectivas)
  stopifnot(
    all(c("identification", "average_weight") %in% names(weights)),
    all(c("identification", "cfi") %in% names(categories))
  )

  # --- 2. Pipeline de Cálculo ---
  results <- weights %>%
    # 2.1 Unir con Categorías para obtener la etiqueta 'cfi' (ej. "lactating_cow")
    dplyr::left_join(
      categories %>%
        dplyr::select(identification, animal_type, animal_subtype, cfi_tag = cfi),
      by = c("identification", "animal_type", "animal_subtype")
    ) %>%

    # 2.2 Unir con Coeficientes para obtener el valor numérico
    # Esto reemplaza al vector de búsqueda manual y es más robusto
    dplyr::left_join(
      coefficients %>%
        dplyr::filter(tolower(coefficient) == "cfi") %>%
        dplyr::select(cfi_tag = description, cfi_value = value),
      by = "cfi_tag"
    ) %>%

    # 2.3 Cálculo y Limpieza
    dplyr::mutate(
      # Seguridad numérica
      across(c(average_weight, cfi_value), ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)),

      # Fórmula: Coeficiente * (Peso ^ 0.75)
      NEm = cfi_value * (average_weight ^ 0.75)
    ) %>%

    # 2.4 Selección flexible (mantiene group/zone solo si existen)
    dplyr::select(
      dplyr::any_of(c("group", "zone")),
      identification, animal_type, animal_subtype,
      average_weight, cfi_value, NEm
    ) %>%

    # Ordenar priorizando group/zone si existen, luego identificación
    dplyr::arrange(dplyr::across(dplyr::any_of(c("group", "zone"))), identification)

  # --- 3. Guardado ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/NEm_result.csv")
    message("💾 Saved output to output/NEm_result.csv")
  }

  return(results)
}

#' Calculate Net Energy for Activity (NEa) (Refactored)
#'
#' Computes NEa based on NEm and housing/activity coefficients (Ca),
#' applying weighted averages for mixed systems (grazing/stall) in cattle.
#' @export
calculate_NEa <- function(saveoutput = TRUE) {

  message("🟢 Calculating Net Energy for Activity (NEa)...")

  # --- 1. Carga de Datos ---
  categories   <- load_dataset("categories")
  coefficients <- load_dataset("coefficients")

  # Traemos NEm (aseguramos tipos numéricos aquí por si acaso)
  nem_df <- calculate_NEm(saveoutput = FALSE)

  # --- 2. Preparación de Constantes y Factores ---

  # Extraemos la tabla de coeficientes 'ca'
  ca_table <- coefficients %>%
    dplyr::filter(tolower(coefficient) == "ca") %>%
    dplyr::select(ca_tag = description, ca_value = value)

  # Extraemos valores específicos para la fórmula de vacuno (Pasture vs Stall)
  # Esto evita la lógica compleja de "si es A, dame B" dentro del mutate
  val_stall   <- ca_table$ca_value[ca_table$ca_tag == "stall"][1]
  val_pasture <- ca_table$ca_value[ca_table$ca_tag == "pasture"][1]

  # Validamos que existan (por seguridad)
  val_stall   <- if (is.na(val_stall)) 0 else val_stall
  val_pasture <- if (is.na(val_pasture)) 0 else val_pasture

  # --- 3. Pipeline de Cálculo ---
  results <- nem_df %>%
    # 3.1 Unir datos de categorización (Tag de actividad y Meses de pastoreo)
    dplyr::left_join(
      categories %>%
        dplyr::select(identification, animal_type, animal_subtype, ca_tag = ca, grazing_months),
      by = c("identification", "animal_type", "animal_subtype")
    ) %>%

    # 3.2 Unir el valor base del coeficiente (Lookup simple)
    dplyr::left_join(ca_table, by = "ca_tag") %>%

    # 3.3 Cálculos
    dplyr::mutate(
      # Seguridad numérica
      across(c(grazing_months, ca_value, NEm), ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)),

      # Lógica del Coeficiente de Actividad (Ca)
      Ca_final = dplyr::case_when(
        # CASO 1: Vacuno en sistema mixto (Stall/Pasture)
        # Se calcula el promedio ponderado basado en los meses de pastoreo
        animal_type == "cattle" & ca_tag %in% c("stall", "pasture") ~
          (val_pasture * (grazing_months / 12)) + (val_stall * ((12 - grazing_months) / 12)),

        # CASO 2: Vacuno en grandes áreas (Grazing large areas)
        # Usa su propio coeficiente directo
        animal_type == "cattle" & ca_tag == "grazing large areas" ~ ca_value,

        # CASO 3: Resto de animales o sistemas
        TRUE ~ ca_value
      ),

      # Cálculo final de Energía Neta para Actividad
      NEa = Ca_final * NEm
    ) %>%

    # 3.4 Selección y Limpieza
    dplyr::select(
      dplyr::any_of(c("group", "zone")), # Mantiene group/zone si vienen de NEm
      identification, animal_type, animal_subtype,
      NEm, ca_tag, grazing_months, Ca_coefficient = Ca_final, NEa
    ) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))

  # --- 4. Guardado ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/NEa_result.csv")
    message("💾 Saved output to output/NEa_result.csv")
  }

  return(results)
}


#' Calculate Net Energy for Growth (NEg) (Refactored)
#'
#' Computes NEg for all animals using vectorized operations.
#' Replaces rowwise lookups with efficient joins.
#' @export
calculate_NEg <- function(saveoutput = TRUE) {

  message("🟢 Calculating Net Energy for Growth (NEg)...")

  # --- 1. Carga de Datos ---
  weights      <- load_dataset("weights")
  categories   <- load_dataset("categories")
  coefficients <- load_dataset("coefficients")

  # Preparamos una tabla de búsqueda limpia para los coeficientes
  # Solo necesitamos 'description' (la clave) y 'value' (el número)
  coeff_lookup <- coefficients %>%
    dplyr::select(description, value) %>%
    dplyr::distinct(description, .keep_all = TRUE) # Seguridad contra duplicados

  # --- 2. Pipeline de Construcción de Datos ---
  # En lugar de buscar coeficientes fila por fila, los unimos todos de golpe

  df_joined <- weights %>%
    # 2.1 Unir categorías para obtener los tags de los coeficientes (c, a, b)
    dplyr::left_join(
      categories %>% dplyr::select(identification, animal_type, animal_subtype, c, a, b),
      by = c("identification", "animal_type", "animal_subtype")
    ) %>%

    # 2.2 Traer valor C (usado para Cattle)
    dplyr::left_join(coeff_lookup, by = c("c" = "description")) %>%
    dplyr::rename(C_val = value) %>%

    # 2.3 Traer valor A (usado para Sheep/Goat)
    dplyr::left_join(coeff_lookup, by = c("a" = "description")) %>%
    dplyr::rename(A_val = value) %>%

    # 2.4 Traer valor B (usado para Sheep/Goat)
    dplyr::left_join(coeff_lookup, by = c("b" = "description")) %>%
    dplyr::rename(B_val = value)

  # --- 3. Cálculo Vectorizado ---
  results <- df_joined %>%
    dplyr::mutate(
      # Limpieza de tipos y NAs antes de calcular
      across(
        c(average_weight, adult_weight, weight_gain, C_val, A_val, B_val),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),

      # Lógica NEg
      NEg = dplyr::case_when(
        # Lógica Vacuno (Cattle)
        # Fórmula: 22.02 * (Peso / (C * PesoAdulto))^0.75 * Ganancia^1.097
        tolower(animal_type) == "cattle" ~ dplyr::if_else(
          adult_weight > 0 & C_val > 0,
          22.02 * ((average_weight / (C_val * adult_weight))^0.75) * (weight_gain^1.097),
          0
        ),

        # Lógica Ovinos/Caprinos (Sheep/Goat)
        # Fórmula: (A + 0.5 * B) * Ganancia
        tolower(animal_type) %in% c("sheep", "goat") ~ (A_val + 0.5 * B_val) * weight_gain,

        # Otros
        TRUE ~ 0
      )
    ) %>%

    # --- 4. Limpieza Final ---
    dplyr::select(
      dplyr::any_of(c("group", "zone")),
      identification, animal_type, animal_subtype,
      c_tag = c, a_tag = a, b_tag = b, # Mantenemos los nombres originales como tags
      average_weight, adult_weight, weight_gain,
      NEg
    ) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))

  # --- 5. Guardado ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/NEg_result.csv")
    message("💾 Saved output to output/NEg_result.csv")
  }

  return(results)
}


#' Calculate Net Energy for Lactation (NEl) (Refactored)
#'
#' Computes NEl based on milk yield and fat content.
#' Assigns 0 energy for non-lactating animals instead of dropping them.
#' @export
calculate_NEl <- function(saveoutput = TRUE) {

  message("🟢 Calculating Net Energy for Lactation (NEl)...")

  # --- 1. Carga de Datos ---
  categories <- load_dataset("categories")

  # --- 2. Pipeline de Cálculo ---
  results <- categories %>%
    dplyr::mutate(
      # Limpieza de tipos y manejo de NAs (NAs se convierten en 0)
      across(
        c(milk_yield, fat_content),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),

      # Cálculo de producción diaria (asumiendo milk_yield anual)
      Milk_yield_kg_day = milk_yield / 365,

      # Cálculo de Energía (NEl)
      NEl = dplyr::case_when(
        # Si no hay producción, la energía es 0 (ahorra cálculo)
        Milk_yield_kg_day <= 0 ~ 0,

        # Fórmula Vacuno: Producción * (1.47 + 0.4 * Grasa)
        tolower(animal_type) == "cattle" ~ Milk_yield_kg_day * (1.47 + 0.4 * fat_content),

        # Fórmula Pequeños Rumiantes: Producción * Factor Fijo (4.6 MJ/kg aprox)
        tolower(animal_type) %in% c("sheep", "goat") ~ Milk_yield_kg_day * 4.6,

        # Resto (ej. cerdos, aves no lactantes en este contexto)
        TRUE ~ 0
      )
    ) %>%

    # --- 3. Limpieza Final ---
    dplyr::select(
      dplyr::any_of(c("group", "zone")), # Mantiene columnas si existen
      identification, animal_type, animal_subtype,
      Milk_yield_kg_day, fat_content, NEl
    ) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))

  # --- 4. Guardado ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/NEl_result.csv")
    message("💾 Saved output to output/NEl_result.csv")
  }

  return(results)
}


#' Calculate Net Working Energy (NE_work) (Refactored)
#'
#' Computes NE_work based on NEm and hours of activity.
#' Keeps all animals in the dataset (assigns 0 to non-working animals).
#' @export
calculate_NE_work <- function(saveoutput = TRUE) {

  message("🟢 Calculating Net Energy for Work (NE_work)...")

  # --- 1. Carga de Datos ---
  categories <- load_dataset("categories")

  # Traemos NEm (base del cálculo)
  nem_df <- calculate_NEm(saveoutput = FALSE)

  # --- 2. Pipeline de Cálculo ---
  results <- nem_df %>%
    # 2.1 Unimos la variable 'hours' desde categories
    dplyr::left_join(
      categories %>% dplyr::select(identification, animal_type, animal_subtype, hours),
      by = c("identification", "animal_type", "animal_subtype")
    ) %>%

    # 2.2 Cálculos y Limpieza
    dplyr::mutate(
      # Aseguramos que 'hours' y 'NEm' sean numéricos.
      # Si hours es NA (no hay dato), se convierte en 0.
      across(
        c(hours, NEm),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),

      # Fórmula: NEm * Horas (Asignará 0 si hours es 0)
      NE_work = hours * NEm
    ) %>%

    # --- 3. Limpieza Final ---
    dplyr::select(
      dplyr::any_of(c("group", "zone")), # Mantiene group/zone si vienen de NEm
      identification, animal_type, animal_subtype,
      hours, NEm, NE_work
    ) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))

  # --- 4. Guardado ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/NE_work_result.csv")
    message("💾 Saved output to output/NE_work_result.csv")
  }

  return(results)
}


#' Calculate Net Energy for Wool Production (NE_wool) (Refactored)
#'
#' Computes NE_wool based on wool yield.
#' Assigns 0 energy for non-wool producing animals instead of dropping them.
#' @export
calculate_NE_wool <- function(saveoutput = TRUE) {

  message("🟢 Calculating Net Energy for Wool (NE_wool)...")

  # --- 1. Carga de Datos ---
  categories <- load_dataset("categories")

  # --- 2. Pipeline de Cálculo ---
  results <- categories %>%
    dplyr::mutate(
      # Seguridad numérica: Convertimos texto a número y NA a 0
      wool_yield = tidyr::replace_na(suppressWarnings(as.numeric(wool_yield)), 0),

      # Cálculo directo (Vectorizado)
      # La fórmula aplica a todos. Si wool_yield es 0 (ej. vacas), NE_wool será 0.
      NE_wool = (wool_yield * 24) / 365
    ) %>%

    # --- 3. Selección y Limpieza ---
    dplyr::select(
      dplyr::any_of(c("group", "zone")), # Mantiene columnas si existen
      identification, animal_type, animal_subtype,
      wool_yield, NE_wool
    ) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))

  # --- 4. Guardado ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/NE_wool_result.csv")
    message("💾 Saved output to output/NE_wool_result.csv")
  }

  return(results)
}


#' Calculate Net Energy for Pregnancy (NE_pregnancy) (Refactored)
#'
#' Computes NE_pregnancy based on species-specific coefficients (C_pregnancy or PR).
#' Keeps all animals (assigns 0 to non-pregnant), ensuring data consistency.
#' @export
calculate_NE_pregnancy <- function(saveoutput = TRUE) {

  message("🟢 Calculating Net Energy for Pregnancy (NE_pregnancy)...")

  # --- 1. Carga de Datos ---
  categories   <- load_dataset("categories")
  coefficients <- load_dataset("coefficients")

  # Traemos NEm (Define la población base: group, zone, id...)
  nem_df <- calculate_NEm(saveoutput = FALSE)

  # Preparar tabla de coeficientes para Cattle (C_pregnancy)
  # Esto sustituye la creación manual del diccionario/vector
  coeff_lookup <- coefficients %>%
    dplyr::filter(tolower(coefficient) == "c_pregnancy") %>%
    dplyr::select(c_pregnancy_tag = description, c_value = value) %>%
    dplyr::distinct(c_pregnancy_tag, .keep_all = TRUE)

  # --- 2. Pipeline de Cálculo ---
  results <- nem_df %>%
    # 2.1 Unir datos de categorías (tags y tasas de preñez)
    # Usamos las claves universales. NEm ya trae group/zone, categories aporta los parámetros.
    dplyr::left_join(
      categories %>%
        dplyr::select(identification, animal_type, animal_subtype, c_pregnancy, pr),
      by = c("identification", "animal_type", "animal_subtype")
    ) %>%

    # 2.2 Unir el valor del coeficiente para Cattle
    dplyr::left_join(coeff_lookup, by = c("c_pregnancy" = "c_pregnancy_tag")) %>%

    # 2.3 Cálculos
    dplyr::mutate(
      # Seguridad numérica
      across(c(pr, c_value, NEm), ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)),

      # Cálculo del Factor de Gestación (C_preg)
      C_preg_factor = dplyr::case_when(
        # CASO 1: Vacuno (Usa coeficiente tabular Cb)
        tolower(animal_type) == "cattle" ~ c_value,

        # CASO 2: Ovinos y Caprinos (Usa tasa PR)
        # Lógica: Desglose en partos simples vs dobles basado en si PR > 1
        tolower(animal_type) %in% c("sheep", "goat") & pr > 0 ~ {
          double_birth <- pmax(pr - 1, 0) # Si PR es 1.2, double es 0.2
          single_birth <- 1 - double_birth # y single es 0.8
          (0.126 * double_birth) + (0.077 * single_birth)
        },

        # CASO 3: Resto
        TRUE ~ 0
      ),

      # Cálculo final: Factor * NEm
      NE_pregnancy = C_preg_factor * NEm
    ) %>%

    # --- 3. Limpieza Final ---
    dplyr::select(
      dplyr::any_of(c("group", "zone")), # Mantiene columnas si existen en NEm
      identification, animal_type, animal_subtype,
      c_pregnancy_tag = c_pregnancy, C_preg_factor, NE_pregnancy
    ) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))

  # --- 4. Guardado ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/NE_pregnancy_result.csv")
    message("💾 Saved output to output/NE_pregnancy_result.csv")
  }

  return(results)
}
