#' Calculate Net Energy for Maintenance (NEm)
#'
#' Computes the Net Energy for Maintenance (NEm) for livestock animals
#' based on their average weight and CFI coefficients.
#'
#' @param animal Character string (optional). Filter by animal type ("Cattle", "Sheep", "Goat").
#'        If NULL, all animals are returned.
#' @param type Character string (optional). Only applies to "Cattle" (e.g., "Dairy", "Beef").
#'
#' @return A tibble with columns: code, animal_type, animal_subtype, average_weight, cfi_value, NEm
#' @export
calculate_NEm <- function(animal = NULL, type = NULL, saveoutput = TRUE) {
  cfi_tbl <- coefficients %>%
    filter(tolower(coefficient) == "cfi") %>%
    select(description, value) %>%
    deframe()

  NEm_result <- weights %>%
    left_join(categories, by = c("code", "animal_type", "animal_subtype")) %>%
    mutate(
      cfi_value = cfi_tbl[cfi],
      NEm = cfi_value * (average_weight^0.75)
    ) %>%
    select(code, animal_type, animal_subtype, average_weight, cfi_value, NEm) %>%
    arrange(code)

  if (!is.null(animal)) {
    NEm_result <- NEm_result %>% filter(animal_type == animal)
    if (!is.null(type) && animal == "Cattle") {
      NEm_result <- NEm_result %>% filter(animal_subtype == type)
    }
  }
  # --- Guardar salida ---
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    write.csv(NEm_result, "output/NEm_result.csv", row.names = FALSE)
  }
  NEm_result
}


#' Calculate Net Energy for Activity (NEa)
#'
#' Computes NEa for a given animal type using NEm, housing/activity coefficients.
#'
#' @param animal Character string (optional). Filter by animal type.
#' @param type Character string (optional). Only applies for Cattle.
#'
#' @return A tibble with columns: code, animal_type, animal_subtype, cfi_value, NEm, ca, NEa
#' @export
calculate_NEa <- function(animal = NULL, type = NULL, saveoutput = TRUE) {
  NEm <- calculate_NEm(animal, type)

  categories_sub <- categories %>% select(code, animal_type, animal_subtype, ca, duration)

  if (!is.null(animal)) {
    categories_sub <- categories_sub %>% filter(animal_type == animal)
    if (!is.null(type) && animal == "Cattle") {
      categories_sub <- categories_sub %>% filter(animal_subtype == type)
    }
  }

  ca_coeff <- coefficients %>%
    filter(tolower(coefficient) == "ca") %>%
    select(description, value) %>%
    deframe()

  categories_sub <- categories_sub %>%
    mutate(duration = ifelse(is.na(duration), 0, duration))

  NEa_result <- NEm %>%
    inner_join(categories_sub, by = c("code", "animal_type", "animal_subtype")) %>%
    mutate(
      ca_val = ca_coeff[ca],
      ca_val = ifelse(is.na(ca_val), 0, ca_val),
      second_ca_val = ca_coeff[ifelse(ca == "Stall", "Pasture", ifelse(ca == "Pasture", "Stall", ca))],
      second_ca_val = ifelse(is.na(second_ca_val), 0, second_ca_val),
      ca = case_when(
        animal_type == "Cattle" & ca %in% c("Stall", "Pasture") ~ (duration/12)*ca_val + ((12-duration)/12)*second_ca_val,
        animal_type == "Cattle" & ca == "Grazing large areas" ~ ca_val,
        TRUE ~ ca_val
      ),
      NEa = ca * NEm
    ) %>%
    select(code, animal_type, animal_subtype, cfi_value, NEm, ca, NEa)

  # --- Guardar salida ---
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    write.csv(NEa_result, "output/NEa_result.csv", row.names = FALSE)
  }
  NEa_result
}


#' Calculate Net Energy for Growth (NEg)
#'
#' Computes NEg for Cattle, Sheep, or Goat.
#'
#' @param animal Character string (required). Animal type.
#' @param type Character string (optional). Only for Cattle subtype.
#' @param saveoutput Logical (optional). If TRUE, saves the result as CSV. Default FALSE.
#'
#' @return A tibble with columns: code, animal_type, animal_subtype, c, a, b, average_weight, adult_weight, weight_gain, NEg
#' @export
calculate_NEg <- function(animal = NULL, type = NULL, saveoutput = TRUE) {

  weights_sub <- weights
  categories_sub <- categories

  if (!is.null(animal)) {
    weights_sub <- weights_sub %>% filter(animal_type == animal)
    categories_sub <- categories_sub %>% filter(animal_type == animal)
    if (!is.null(type) && animal == "Cattle") {
      weights_sub <- weights_sub %>% filter(animal_subtype == type)
      categories_sub <- categories_sub %>% filter(animal_subtype == type)
    }
  }

  df <- weights_sub %>%
    select(code, animal_type, animal_subtype, weight_gain, average_weight, adult_weight) %>%
    left_join(categories_sub %>% select(code, animal_type, animal_subtype, c, a, b),
              by = c("code","animal_type","animal_subtype"))

  coeff_vector <- coefficients %>% select(animal_type, description, value)

  df <- df %>%
    rowwise() %>%
    mutate(
      NEg = case_when(
        animal_type == "Cattle" ~ {
          C_val <- coeff_vector$value[coeff_vector$animal_type=="Cattle" & coeff_vector$description==c]
          C_val <- if(length(C_val)==0) 0 else C_val
          ifelse(is.na(C_val)|is.na(average_weight)|is.na(adult_weight)|is.na(weight_gain),
                 0, 22.02*((average_weight/(C_val*adult_weight))^0.75)*(weight_gain^1.097))
        },
        animal_type %in% c("Sheep","Goat") ~ {
          a_val <- coeff_vector$value[coeff_vector$animal_type==animal_type & coeff_vector$description==a]
          b_val <- coeff_vector$value[coeff_vector$animal_type==animal_type & coeff_vector$description==b]
          a_val <- if(length(a_val)==0) 0 else a_val
          b_val <- if(length(b_val)==0) 0 else b_val
          ifelse(is.na(a_val)|is.na(b_val)|is.na(weight_gain),0,(a_val+0.5*b_val)*weight_gain)
        },
        TRUE ~ 0
      )
    ) %>%
    ungroup()

  # Guardar salida si saveoutput = TRUE
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    write.csv(df, "output/NEg_result.csv", row.names = FALSE)
  }

  df %>% select(code, animal_type, animal_subtype, c, a, b, average_weight, adult_weight, weight_gain, NEg)
}



#' Calculate Net Energy for Lactation (NEl)
#'
#' Computes NEl based on milk yield and fat content.
#'
#' @param animal Character string (optional). Animal type.
#' @param type Character string (optional). Only for Cattle subtype.
#' @param saveoutput Logical (optional). If TRUE, saves the result as CSV. Default FALSE.
#'
#' @return A tibble with code, animal_type, animal_subtype, Milk_yield_kg_day_head, fat_content, NEl
#' @export
calculate_NEl <- function(animal = NULL, type = NULL, saveoutput = TRUE) {

  result <- categories %>%
    filter(!is.na(milk_yield), !is.na(fat_content), !is.na(code)) %>%
    mutate(
      Milk_yield_kg_day_head = milk_yield / 365,
      NEl = case_when(
        animal_type == "Cattle" ~ Milk_yield_kg_day_head*(1.47 + 0.4*fat_content),
        animal_type %in% c("Sheep","Goat") ~ Milk_yield_kg_day_head*4.6,
        TRUE ~ NA_real_
      )
    ) %>%
    select(code, animal_type, animal_subtype, Milk_yield_kg_day_head, fat_content, NEl)

  # Filtrar por animal/type si se especifica
  if (!is.null(animal)) {
    result <- result %>% filter(animal_type == animal)
    if (!is.null(type) && animal == "Cattle") {
      result <- result %>% filter(animal_subtype == type)
    }
  }

  # Guardar salida si saveoutput = TRUE
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    write.csv(result, "output/NEl_result.csv", row.names = FALSE)
  }

  result
}


#' Calculate Net Working Energy (NE_work)
#'
#' NE_work = NEm * hours of activity.
#'
#' @param animal Character string (optional)
#' @param type Character string (optional, only for Cattle)
#' @param saveoutput Logical (optional). If TRUE, saves the result as CSV. Default FALSE.
#' @return A tibble with code, animal_type, animal_subtype, hours, NEm, NE_work
#' @export
calculate_NE_work <- function(animal = NULL, type = NULL, saveoutput = TRUE) {

  NEm <- calculate_NEm(animal, type)

  categories_sub <- categories
  if (!is.null(animal)) categories_sub <- categories_sub %>% filter(animal_type == animal)
  if (!is.null(type) && animal == "Cattle") categories_sub <- categories_sub %>% filter(animal_subtype == type)

  result <- categories_sub %>%
    inner_join(NEm, by = c("code","animal_type","animal_subtype")) %>%
    mutate(NE_work = hours * NEm) %>%
    select(code, animal_type, animal_subtype, hours, NEm, NE_work)

  # Guardar salida si saveoutput = TRUE
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    write.csv(result, "output/NE_work_result.csv", row.names = FALSE)
  }

  result
}


#' Calculate Net Energy for Wool Production (NE_wool)
#'
#' NE_wool = (wool_yield * 24) / 365
#'
#' @param animal Character string (optional)
#' @param type Character string (optional, only for Cattle)
#' @param saveoutput Logical (optional). If TRUE, saves the result as CSV. Default FALSE.
#' @return Tibble with code, animal_type, animal_subtype, wool_yield, NE_wool
#' @export
calculate_NE_wool <- function(animal = NULL, type = NULL, saveoutput = TRUE) {

  result <- categories %>%
    filter(!is.na(wool_yield)) %>%
    mutate(NE_wool = (wool_yield * 24) / 365) %>%
    select(code, animal_type, animal_subtype, wool_yield, NE_wool)

  # Filtrar por animal/type si se especifica
  if (!is.null(animal)) {
    result <- result %>% filter(animal_type == animal)
    if (!is.null(type) && animal == "Cattle") result <- result %>% filter(animal_subtype == type)
  }

  # Guardar salida si saveoutput = TRUE
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    write.csv(result, "output/NE_wool_result.csv", row.names = FALSE)
  }

  result
}


#' Calculate Net Energy for Pregnancy (NE_pregnancy)
#'
#' Computes NE_pregnancy with species-specific rules.
#'
#' @param animal Character (optional)
#' @param type Character (optional, only for Cattle)
#' @param saveoutput Logical (optional). If TRUE, saves the result as CSV. Default FALSE.
#' @return Tibble with code, animal_type, animal_subtype, c_pregnancy, NEm, NE_pregnancy
#' @export
calculate_NE_pregnancy <- function(animal = NULL, type = NULL, saveoutput = TRUE) {

  NEm <- calculate_NEm(animal, type)

  cat_df <- categories %>%
    select(code, animal_type, animal_subtype, c_pregnancy_cat = c_pregnancy, pr)

  # Filtrar por animal/type si se especifica
  if (!is.null(animal)) {
    cat_df <- cat_df %>% filter(animal_type == animal)
    if (!is.null(type) && animal == "Cattle") {
      cat_df <- cat_df %>% filter(animal_subtype == type)
    }
  }

  c_preg_tbl <- coefficients %>%
    filter(tolower(coefficient) == "c_pregnancy") %>%
    select(description, value) %>%
    deframe()

  result <- NEm %>%
    left_join(cat_df, by = c("code","animal_type","animal_subtype")) %>%
    mutate(
      c_pregnancy_value = case_when(
        animal_type == "Cattle" ~ c_preg_tbl[c_pregnancy_cat],
        animal_type %in% c("Sheep","Goat") & pr == 0 ~ 0,
        animal_type %in% c("Sheep","Goat") & pr > 0 ~ {
          double_birth_fraction <- pmax(pr-1,0)
          single_birth_fraction <- 1 - double_birth_fraction
          0.126*double_birth_fraction + 0.077*single_birth_fraction
        },
        TRUE ~ 0
      ),
      c_pregnancy_value = ifelse(is.na(c_pregnancy_value), 0, c_pregnancy_value),
      NE_pregnancy = c_pregnancy_value * NEm
    ) %>%
    select(code, animal_type, animal_subtype, c_pregnancy = c_pregnancy_value, NEm, NE_pregnancy)

  # Guardar salida si saveoutput = TRUE
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    write.csv(result, "output/NE_pregnancy_result.csv", row.names = FALSE)
  }

  result
}

