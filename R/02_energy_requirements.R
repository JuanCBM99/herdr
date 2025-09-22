#' Calculate Net Energy for Maintenance (NEm)
#'
#' Computes the Net Energy for Maintenance (NEm) for livestock animals
#' based on their average weight and CFI coefficients.
#' The function merges animal weights with category information and applies
#' the corresponding CFI coefficient to estimate maintenance energy.
#'
#' @param animal Character string (optional). If provided, filters the results
#'        to include only the specified animal type (e.g., "Cattle", "Sheep", "Goat").
#'        If \code{NULL}, all animals are returned.
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{code}{Unique code identifying the animal category.}
#'   \item{animal_type}{Type of animal (e.g., Cattle, Sheep, Goat).}
#'   \item{average_weight}{Average body weight of the animal (kg).}
#'   \item{cfi_value}{CFI coefficient value matched from the coefficients table.}
#'   \item{NEm}{Net Energy for Maintenance (MJ/day).}
#' }
#'
#' @details
#' The calculation follows the formula:
#' \deqn{NEm = CFI \times (average\_weight^{0.75})}
#' where \eqn{CFI} is obtained from the coefficients dataset.
#'
#' @examples
#' \dontrun{
#' # Calculate for all animals
#' calculate_NEm()
#'
#' # Calculate only for cattle
#' calculate_NEm(animal = "Cattle")
#' }
#'
#' @export

calculate_NEm <- function(animal = NULL) {

  # Obtener tabla de coeficientes CFI
  cfi_tbl <- coefficients %>%
    dplyr::filter(tolower(coefficient) == "cfi") %>%
    dplyr::select(description, value)

  result <- weights %>%
    dplyr::left_join(categories, by = c("code", "animal_type", "animal_subtype")) %>%
    dplyr::mutate(
      # convertir el cfi de categories (descripcion) a su valor
      cfi_value = cfi_tbl$value[match(cfi, cfi_tbl$description)],
      NEm = cfi_value * (average_weight^0.75)
    ) %>%
    dplyr::select(code, animal_type, average_weight, cfi_value, NEm) %>%
    dplyr::arrange(code)

  # Filtrar si se pasa un animal
  if (!is.null(animal)) {
    result <- result %>% dplyr::filter(animal_type == animal)
  }

  return(result)
}

#' Calculate Net Energy for Activity (NEa)
#'
#' Computes NEa for a given animal type (Cattle, Sheep, Goat),
#' using NEm, housing type, and coefficients.
#'
#' @param animal Character string. Must be one of: "Cattle", "Sheep", "Goat".
#'
#' @return A tibble with columns:
#'   \itemize{
#'     \item code
#'     \item animal_type
#'     \item cfi
#'     \item NEm
#'     \item housing_type
#'     \item duration
#'     \item ca
#'     \item NEa
#'   }
#'
#' @export
calculate_NEa <- function(animal) {

  # Validación dinámica según lo que hay en categories
  valid_animals <- unique(categories$animal_type)
  assertthat::assert_that(animal %in% valid_animals,
                          msg = paste0("The selected animal must be: ", paste(valid_animals, collapse = ", ")))

  # NEm
  NEm <- calculate_NEm()

  # Filtrar categories por animal
  categories_sub <- categories %>%
    dplyr::filter(animal_type == animal) %>%
    dplyr::select(code, housing_type, duration)

  # Coeficientes Ca
  ca_coeff <- coefficients %>%
    dplyr::filter(tolower(coefficient) == "ca", animal_type == animal) %>%
    dplyr::select(description, value) %>%
    tibble::deframe()

  # Calcular NEa
  result <- NEm %>%
    dplyr::filter(animal_type == animal) %>%
    dplyr::left_join(categories_sub, by = "code") %>%
    dplyr::mutate(
      ca = dplyr::case_when(
        animal == "Cattle" & housing_type %in% c("Stall", "Pasture") ~
          (duration / 12) * ca_coeff[housing_type] +
          ((12 - duration) / 12) * ca_coeff[ifelse(housing_type == "Stall", "Pasture", "Stall")],
        animal == "Cattle" & housing_type == "Grazing large areas" ~
          ca_coeff["Grazing large areas"],
        housing_type %in% names(ca_coeff) ~ ca_coeff[housing_type],
        TRUE ~ NA_real_
      ),
      NEa = ca * NEm
    ) %>%
    dplyr::select(code, animal_type, cfi_value, NEm, housing_type, duration, ca, NEa)

  return(result)
}

#' Calculate Net Energy for Growth (NEg) by animal
#'
#' Computes net energy for growth for Cattle, Sheep, or Goat using the appropriate formula.
#'
#' @param animal Character string. Must match an existing animal_type in categories.
#'
#' @return A tibble including calculated NEg per category for the specified animal.
#' @export
calculate_NEg <- function(animal) {

  # Validación dinámica
  valid_animals <- unique(categories$animal_type)
  assertthat::assert_that(animal %in% valid_animals,
                          msg = paste0("animal must be one of: ", paste(valid_animals, collapse = ", ")))

  # Filtrar weights y categories por animal
  weights_sub <- weights %>%
    dplyr::filter(animal_type == animal) %>%
    dplyr::select(code, weight_gain, average_weight, adult_weight)

  categories_sub <- categories %>%
    dplyr::filter(animal_type == animal) %>%
    dplyr::select(code, c, a, b)  # c solo para vacas, a/b para otros animales

  # Filtrar coeficientes por animal
  coefficients_sub <- coefficients %>%
    dplyr::filter(animal_type == animal) %>%
    dplyr::select(description, value)
  coeff_vector <- tibble::deframe(coefficients_sub)

  # Calcular NEg
  result <- weights_sub %>%
    dplyr::left_join(categories_sub, by = "code") %>%
    dplyr::mutate(
      NEg = dplyr::case_when(
        # Cattle formula
        animal == "Cattle" ~ {
          C <- coeff_vector[c]
          ifelse(is.na(C) | is.na(average_weight) | is.na(adult_weight) | is.na(weight_gain),
                 0,
                 22.02 * ((average_weight / (C * adult_weight))^0.75) * (weight_gain^1.097))
        },
        # Sheep/Goat formula
        animal %in% c("Sheep", "Goat") ~ {
          a_val <- coeff_vector[a]
          b_val <- coeff_vector[b]
          ifelse(is.na(a_val) | is.na(b_val) | is.na(weight_gain),
                 0,
                 (a_val + 0.5 * b_val) * weight_gain)
        },
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::select(code, c, a, b, average_weight, adult_weight, weight_gain, NEg)

  return(result)
}



#' Calculate Net Energy for Lactation (NEl)
#'
#' Calculates the Net Energy for Lactation (NEl) for livestock animals,
#' based on daily milk yield and fat content. The formula differs by species:
#' \itemize{
#'   \item Cattle: \eqn{NEl = Milk\_yield\_kg\_day\_head \times (1.47 + 0.4 \times fat\_content)}
#'   \item Sheep/Goat: \eqn{NEl = Milk\_yield\_kg\_day\_head \times 4.6}
#' }
#'
#' @param animal Character string (optional). If provided, filters the results
#'        to include only the specified animal type (e.g., "Cattle", "Sheep", "Goat").
#'        If \code{NULL}, all animals with valid milk yield and fat content are returned.
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{code}{Unique code identifying the animal category.}
#'   \item{animal_type}{Type of animal (Cattle, Sheep, Goat).}
#'   \item{Milk_yield_kg_day_head}{Average daily milk yield per head (kg/day).}
#'   \item{fat_content}{Milk fat content (%).}
#'   \item{NEl}{Net Energy for Lactation (MJ/day).}
#' }
#'
#' @examples
#' \dontrun{
#' # All animals
#' calculate_NEl()
#'
#' # Only cattle
#' calculate_NEl(animal = "Cattle")
#' }
#'
#' @export
calculate_NEl <- function(animal = NULL) {
  result <-categories %>%
    dplyr::filter(
      !is.na(milk_yield),
      !is.na(fat_content),
      !is.na(code)
    ) %>%
    dplyr::mutate(
      Milk_yield_kg_day_head = milk_yield / 365,
      NEl = dplyr::case_when(
        animal_type == "Cattle" ~ Milk_yield_kg_day_head * (1.47 + 0.4 * fat_content),
        animal_type %in% c("Sheep", "Goat") ~ Milk_yield_kg_day_head * 4.6,
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::select(code, animal_type, Milk_yield_kg_day_head, fat_content, NEl)

  # Filtrar si se pasa un animal
  if (!is.null(animal)) {
    result <- result %>% dplyr::filter(animal_type == animal)
  }

  return(result)
}


#' Calculate Net Working Energy (NE_work)
#'
#' Calculates the Net Working Energy (NE_work) for livestock animals by
#' multiplying Net Energy for Maintenance (NEm) by hours of activity.
#' Although the formula is the same across animals, an optional filter
#' allows restricting the calculation to a specific animal type.
#'
#' @param animal Character string (optional). If provided, filters the results
#'        to include only the specified animal type (e.g., "Cattle", "Sheep", "Goat").
#'        If \code{NULL}, all animals are returned.
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{code}{Unique code identifying the animal category.}
#'   \item{animal_type}{Type of animal (Cattle, Sheep, Goat).}
#'   \item{hours}{Daily activity hours for the category.}
#'   \item{NEm}{Net Energy for Maintenance (MJ/day).}
#'   \item{NE_work}{Net Energy for work (MJ/day).}
#' }
#'
#' @examples
#' \dontrun{
#' # All animals
#' calculate_NE_work()
#'
#' # Only sheep
#' calculate_NE_work(animal = "Sheep")
#' }
#'
#' @export
calculate_NE_work <- function(animal = NULL) {
  datos <- load_all_data()
  NEm <- calculate_NEm()

  result <- datos$categories %>%
    dplyr::inner_join(NEm, by = "code") %>%
    dplyr::mutate(NE_work = hours * NEm) %>%
    dplyr::select(code, animal_type, hours, NEm, NE_work)

  # Filtrar si se pasa un animal
  if (!is.null(animal)) {
    result <- result %>% dplyr::filter(animal_type == animal)
  }

  return(result)
}

#' Calculate Net Energy for Wool Production (NE_wool)
#'
#' Calculates the Net Energy required for wool production, based on the
#' annual wool yield and a fixed energy coefficient.
#'
#' The calculation follows the formula:
#' \deqn{NE\_wool = (wool\_yield \times 24) / 365}
#' where 24 MJ/kg is the default energy value of each kg of wool produced.
#'
#' @param animal Character string (optional). If provided, filters the results
#'        to include only the specified animal type (e.g., "Sheep", "Goat").
#'        If \code{NULL}, all animals with non-missing wool yield are returned.
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{code}{Unique code identifying the animal category.}
#'   \item{animal_type}{Type of animal (e.g., Sheep, Goat).}
#'   \item{wool_yield}{Annual wool yield per head (kg/year).}
#'   \item{NE_wool}{Net Energy for wool production (MJ/day).}
#' }
#'
#' @examples
#' \dontrun{
#' # All animals with wool yield
#' calculate_NE_wool()
#'
#' # Only sheep
#' calculate_NE_wool(animal = "Sheep")
#' }
#'
#' @export
calculate_NE_wool <- function(animal = NULL) {

  result <-categories %>%
    dplyr::filter(!is.na(wool_yield)) %>%
    dplyr::mutate(
      NE_wool = (wool_yield * 24) / 365
    ) %>%
    dplyr::select(code, animal_type, wool_yield, NE_wool)

  # Filtrar si se pasa un animal
  if (!is.null(animal)) {
    result <- result %>% dplyr::filter(animal_type == animal)
  }

  return(result)
}


#' Calculate Net Energy for Pregnancy (NE_pregnancy)
#'
#' Calculates the Net Energy required for pregnancy, based on species-specific
#' formulas for the pregnancy coefficient (\eqn{c_pregnancy}).
#'
#' \itemize{
#'   \item Cattle: \eqn{c_pregnancy} is matched from \code{coefficients} using
#'         the \code{c_pregnancy} column in \code{categories}.
#'   \item Sheep/Goat: \eqn{c_pregnancy = (0.126 \times double\_birth\_fraction) +
#'         (0.077 \times single\_birth\_fraction)}, where:
#'         \deqn{double\_birth\_fraction = \max(pr - 1, 0)}
#'         \deqn{single\_birth\_fraction = 1 - double\_birth\_fraction}
#' }
#'
#' @param animal Character string (optional). If provided, filters the results
#'        to include only the specified animal type (e.g., "Cattle", "Sheep", "Goat").
#'        If \code{NULL}, all animals are returned.
#'
#' @return A tibble with the following columns:
#' \describe{
#'   \item{code}{Unique code identifying the animal category.}
#'   \item{animal_type}{Type of animal (Cattle, Sheep, Goat).}
#'   \item{c_pregnancy}{Pregnancy coefficient (species-specific).}
#'   \item{NEm}{Net Energy for Maintenance (MJ/day).}
#'   \item{NE_pregnancy}{Net Energy for Pregnancy (MJ/day).}
#' }
#'
#' @examples
#' \dontrun{
#' # All animals
#' calculate_NE_pregnancy()
#'
#' # Only goats
#' calculate_NE_pregnancy(animal = "Goat")
#' }
#'
#' @export
calculate_NE_pregnancy <- function(animal = NULL) {
  NEm <- calculate_NEm()  # Debe devolver code, animal_type, NEm

  c_preg_tbl <- coefficients %>%
    dplyr::filter(tolower(coefficient) == "c_pregnancy") %>%
    dplyr::select(description, value)

  # Tomamos solo columnas necesarias de categories
  cat_df <- categories %>%
    dplyr::select(code, c_pregnancy, pr)

  # Join con NEm
  df <- NEm %>%
    dplyr::left_join(cat_df, by = "code") %>%
    dplyr::mutate(
      c_pregnancy_value = dplyr::case_when(
        animal_type == "Cattle" ~ c_preg_tbl$value[match(c_pregnancy, c_preg_tbl$description)],
        animal_type %in% c("Sheep", "Goat") ~ {
          double_birth_fraction <- ifelse(pr == 0, 0, pmax(pr - 1, 0))
          single_birth_fraction <- 1 - double_birth_fraction
          (0.126 * double_birth_fraction) + (0.077 * single_birth_fraction)
        },
        TRUE ~ NA_real_
      ),
      NE_pregnancy = c_pregnancy_value * NEm
    ) %>%
    dplyr::select(code, animal_type, c_pregnancy = c_pregnancy_value, NEm, NE_pregnancy)

  if (!is.null(animal)) {
    df <- df %>% dplyr::filter(animal_type == animal)
  }

  return(df)
}





