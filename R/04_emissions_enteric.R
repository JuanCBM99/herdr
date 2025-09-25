#' calculate methane emissions from enteric fermentation
#'
#' estimates methane emissions using ipcc tier 2 methodology based on
#' digestible energy (de), neutral detergent fiber (ndf), gross energy (ge),
#' and animal population. category-specific methane conversion factors (ym)
#' are applied depending on animal type and diet quality.
#'
#' @param animal character. type of animal ("cattle", "sheep", "goat").
#' @return a data frame with columns:
#' \itemize{
#'   \item code: category code
#'   \item de: digestible energy (% of ge)
#'   \item ndf: neutral detergent fiber fraction
#'   \item ge: gross energy (mj/day)
#'   \item ym: methane conversion factor (%)
#'   \item ef_kg_animal_year: emission factor (kg ch4/animal/year)
#'   \item n_population: animal population
#'   \item emissions_total: total emissions (kt ch4/year)
#' }
#' @export
#' @examples
#' \donttest{
#'   calculate_emissions_enteric(animal = "Cattle")
#'   calculate_emissions_enteric(animal = "Sheep")
#'   calculate_emissions_enteric(animal = "Goat")
#' }
calculate_emissions_enteric <- function(animal) {

  # dieta: de y ndf
  diet_vars <- calculate_weighted_variable(animal) %>%
    dplyr::select(code, de, ndf)

  # energia bruta
  ge_df <- calculate_ge(animal) %>%
    dplyr::select(code, ge)

  # poblacion
  categories <- categories %>%
    dplyr::select(code, n_population)

  # unir tablas y calcular emisiones
  resultado <- diet_vars %>%
    dplyr::inner_join(ge_df, by = "code") %>%
    dplyr::inner_join(categories, by = "code") %>%
    dplyr::mutate(
      ym = dplyr::case_when(
        animal == "Sheep" ~ 6.7,
        animal == "Goat" ~ 5.5,
        animal == "Cattle" & code == "k23" & de >= 70 & ndf <= 35 ~ 5.7,
        animal == "Cattle" & code == "k23" & de >= 70 & ndf > 35 ~ 6.0,
        animal == "Cattle" & code == "k23" & de >= 63 & de < 70 & ndf > 37 ~ 6.3,
        animal == "Cattle" & code == "k23" & de <= 62 & ndf > 38 ~ 6.5,
        animal == "Cattle" & code != "k23" & de >= 75 ~ 3.0,
        animal == "Cattle" & code != "k23" & de >= 72 ~ 4.0,
        animal == "Cattle" & code != "k23" & de >= 62 & de <= 71 ~ 6.3,
        animal == "Cattle" & code != "k23" & de < 62 ~ 7.0,
        TRUE ~ NA_real_
      ),
      ef_kg_animal_year = (ge * (ym / 100) * 365) / 55.65,
      emissions_total = ef_kg_animal_year * (n_population / 1e6)
    ) %>%
    dplyr::select(code, de, ndf, ge, ym, ef_kg_animal_year,
                  n_population, emissions_total)

  return(resultado)
}




