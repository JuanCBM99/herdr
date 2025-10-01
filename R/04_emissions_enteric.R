#' Calculate methane emissions from enteric fermentation
#'
#' Estimates methane emissions using IPCC Tier 2 methodology based on
#' digestible energy (DE), neutral detergent fiber (NDF), gross energy (GE),
#' and animal population. Category-specific methane conversion factors (Ym)
#' are applied depending on animal type and diet quality.
#'
#' @param animal character. Type of animal ("Cattle", "Sheep", "Goat").
#' @param type Optional character. Only for "Cattle" (e.g., "Dairy", "Beef").
#' @param zone Optional character vector. Only for "Cattle" when type is specified.
#' @param saveoutput Logical (optional). If TRUE, saves the result as CSV. Default FALSE.
#'
#' @return A tibble with columns:
#' \itemize{
#'   \item code: category code
#'   \item de: digestible energy (% of GE)
#'   \item ndf: neutral detergent fiber fraction
#'   \item ge: gross energy (MJ/day)
#'   \item ym: methane conversion factor (%)
#'   \item ef_kg_animal_year: emission factor (kg CH4/animal/year)
#'   \item n_population: animal population
#'   \item emissions_total: total emissions (kt CH4/year)
#' }
#'
#' @export
#' @examples
#' \donttest{
#' calculate_emissions_enteric(animal = "Cattle", type = "Dairy", zone = "A")
#' calculate_emissions_enteric(animal = "Sheep")
#' calculate_emissions_enteric(animal = "Goat")
#' }
calculate_emissions_enteric <- function(animal = NULL, type = NULL, zone = NULL, saveoutput = TRUE) {

  # 1️⃣ Detectar automáticamente animales y tipos si no se especifica
  animals_available <- unique(categories$animal_type)
  if (is.null(animal)) animal <- animals_available

  resultados_list <- list()

  for (animal in animal) {

    # Detectar tipos disponibles para ese animal
    types_available <- unique(categories$animal_subtype[categories$animal_type == animal])
    types_to_use <- if (is.null(type)) types_available else type[type %in% types_available]

    for (typ in types_to_use) {

      # DE y NDF
      diet_vars <- calculate_weighted_variable(animal = animal, type = typ, zone = zone) %>%
        select(code, animal_type, animal_subtype, de, ndf, zone)

      if (nrow(diet_vars) == 0) next  # saltar si no hay datos

      # GE
      ge_df <- calculate_ge(animal = animal, type = typ, zone = zone) %>%
        select(code, ge, animal_type, animal_subtype, zone)

      # Población
      pop_df <- categories %>%
        filter(animal_type == animal, animal_subtype == typ) %>%
        select(code, animal_type, animal_subtype, n_population)

      # Join de diet, ge y población
      df <- diet_vars %>%
        inner_join(ge_df, by = c("code", "animal_type", "animal_subtype", "zone")) %>%
        inner_join(pop_df, by = c("code", "animal_type", "animal_subtype"))

      # Calcular emisiones
      df <- df %>%
        mutate(
          ym = case_when(
            animal == "Sheep" ~ 6.7,
            animal == "Goat"  ~ 5.5,
            animal == "Cattle" & code == "k23" & de >= 70 & ndf <= 35 ~ 5.7,
            animal == "Cattle" & code == "k23" & de >= 70 & ndf > 35  ~ 6.0,
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
        select(code, animal_type, animal_subtype, zone, de, ndf, ge, ym,
               ef_kg_animal_year, n_population, emissions_total)

      resultados_list[[paste0(animal, "_", typ)]] <- df
    }
  }

  resultado_final <- bind_rows(resultados_list)

  # Guardar CSV si saveoutput = TRUE
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    write.csv(resultado_final, "output/enteric_emissions.csv", row.names = FALSE)
  }

  resultado_final
}





