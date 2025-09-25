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

calculate_NEm <- function(animal = NULL,type= NULL) {

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
    dplyr::select(code, animal_type, animal_subtype, average_weight, cfi_value, NEm) %>%
    dplyr::arrange(code)

  # Filtrar si se pasa un animal
  if (!is.null(animal)) {
    result <- result %>% dplyr::filter(animal_type == animal, animal_subtype == type)
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

  # Validación dinámica
  valid_animals <- unique(categories$animal_type)
  assertthat::assert_that(animal %in% valid_animals,
                          msg = paste0("The selected animal must be: ", paste(valid_animals, collapse = ", ")))

  # NEm
  NEm <- calculate_NEm(animal=animal)

  # Filtrar categories por animal
  categories_sub <- categories %>%
    dplyr::filter(animal_type == animal) %>%
    dplyr::select(code, ca, duration)  # ca es ahora la “clave” para el coeficiente

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
        # Cattle con Stall/Pasture: mezcla ponderada según duration
        animal == "Cattle" & ca %in% c("Stall", "Pasture") ~
          (duration / 12) * ca_coeff[ca] +
          ((12 - duration) / 12) * ca_coeff[ifelse(ca == "Stall", "Pasture", "Stall")],

        # Cattle con Grazing large areas: coeficiente directo
        animal == "Cattle" & ca == "Grazing large areas" ~ ca_coeff["Grazing large areas"],

        # Otros animales: coeficiente directo
        ca %in% names(ca_coeff) ~ ca_coeff[ca],

        TRUE ~ NA_real_
      ),
      NEa = ca * NEm
    ) %>%
    dplyr::select(code, animal_type, cfi_value, NEm, ca, NEa)

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
  # Get NEm calculations - excluir animal_type si existe en NEm
  NEm <- calculate_NEm(animal = animal) %>%
    dplyr::select(-dplyr::any_of("animal_type"))

  # Join con categories manteniendo solo animal_type de categories
  result <- categories %>%
    dplyr::inner_join(NEm, by = "code") %>%
    dplyr::mutate(NE_work = hours * NEm) %>%
    dplyr::select(code, animal_type, hours, NEm, NE_work)

  # Filtrar si se especifica un animal
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
#' Computes the net energy required for pregnancy based on species-specific
#' coefficients and pregnancy rate. The calculation method depends on the
#' animal type:
#'
#' \itemize{
#'   \item \strong{Cattle}: Uses species-specific coefficients (\code{c_pregnancy})
#'         from the \code{coefficients} table, matched by category.
#'   \item \strong{Sheep and Goat}:
#'     \itemize{
#'       \item If pregnancy rate (\code{pr}) = 0 → energy requirement is set to 0.
#'       \item If \code{pr} > 0 → a weighted coefficient is calculated based on
#'             single and double birth fractions:
#'             \deqn{c_pregnancy = 0.077 \times (1 - (pr - 1)) + 0.126 \times (pr - 1)}
#'     }
#' }
#'
#' Final NE for pregnancy is computed as:
#' \deqn{NE_{pregnancy} = NEm \times c_{pregnancy}}
#'
#' @param animal Character (optional). Filter results by animal type (e.g.
#'   `"Cattle"`, `"Sheep"`, `"Goat"`). If \code{NULL}, returns all available
#'   animals.
#'
#' @details
#' This function requires:
#' \itemize{
#'   \item \code{calculate_NEm()} to provide maintenance net energy (\code{NEm}).
#'   \item A \code{coefficients} data frame with at least columns:
#'         \code{coefficient}, \code{description}, \code{value}.
#'   \item A \code{categories} data frame with at least columns:
#'         \code{code}, \code{c_pregnancy}, \code{pr}.
#' }
#'
#' @return A data frame with columns:
#' \itemize{
#'   \item \code{code}: Unique category code.
#'   \item \code{animal_type}: Species type (Cattle, Sheep, Goat).
#'   \item \code{c_pregnancy}: Pregnancy coefficient used in calculation.
#'   \item \code{NEm}: Net energy for maintenance.
#'   \item \code{NE_pregnancy}: Net energy required for pregnancy.
#' }
#'
#' @examples
#' \donttest{
#'   # Calculate for all animals
#'   calculate_NE_pregnancy()
#'
#'   # Filter for cattle only
#'   calculate_NE_pregnancy(animal = "Cattle")
#' }
#' @export

calculate_NE_pregnancy <- function(animal = NULL) {
  # Cargar datos
  NEm <- calculate_NEm()  # Debe devolver code, animal_type, NEm

  # Tabla de coeficientes para cattle
  c_preg_tbl <- coefficients %>%
    dplyr::filter(tolower(coefficient) == "c_pregnancy") %>%
    dplyr::select(description, value)

  # Solo columnas necesarias de categories y renombrar para evitar conflictos
  cat_df <- categories %>%
    dplyr::select(code, c_pregnancy_cat = c_pregnancy, pr)

  # Hacer join desde NEm
  df <- NEm %>%
    dplyr::left_join(cat_df, by = "code") %>%
    dplyr::mutate(
      c_pregnancy_value = dplyr::case_when(
        animal_type == "Cattle" ~ c_preg_tbl$value[match(c_pregnancy_cat, c_preg_tbl$description)],
        animal_type %in% c("Sheep", "Goat") & pr == 0 ~ 0,
        animal_type %in% c("Sheep", "Goat") & pr > 0 ~ {
          double_birth_fraction <- pmax(pr - 1, 0)
          single_birth_fraction <- 1 - double_birth_fraction
          (0.126 * double_birth_fraction) + (0.077 * single_birth_fraction)
        },
        TRUE ~ NA_real_
      ),
      NE_pregnancy = c_pregnancy_value * NEm
    ) %>%
    dplyr::select(code, animal_type, c_pregnancy = c_pregnancy_value, NEm, NE_pregnancy)

  # Filtrar por animal si se indica
  if (!is.null(animal)) {
    df <- df %>% dplyr::filter(animal_type == animal)
  }

  return(df)
}






