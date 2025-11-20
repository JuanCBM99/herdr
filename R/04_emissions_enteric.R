#' Calculate methane emissions from enteric fermentation
#'
#' Computes enteric methane emissions for all animal categories, groups,
#' and zones by loading the census data internally.
#'
#' @param saveoutput Logical (optional). If TRUE, saves the result as CSV. Default TRUE.
#'
#' @return A tibble with enteric fermentation emissions.
#' @export
calculate_emissions_enteric <- function(saveoutput = TRUE) {

  message("🟢 Calculating nutritional data (for Ym)...")
  diet_vars <- calculate_weighted_variable(saveoutput = FALSE) %>%
    dplyr::select(group, zone, identification, animal_type, animal_subtype, de, ndf)

  if (nrow(diet_vars) == 0) {
    message("No diet data found. Returning empty result.")
    return(dplyr::tibble(
      group = character(), zone = character(), identification = character(),
      animal_type = character(), animal_subtype = character(),
      de = double(), ndf = double(), ge = double(), ym = double(),
      ef_kg_animal_year = double(), population = double(),
      emissions_total = double()
    ))
  }

  message("🟢 Calculating Gross Energy (GE)...")
  ge_df <- calculate_ge(saveoutput = FALSE) %>%
    dplyr::select(group, zone, identification, animal_type, animal_subtype, ge)

  message("🟢 Calculating population data...")
  full_pop_df <- calculate_population(saveoutput = FALSE) %>%
    dplyr::select(group, zone, identification, animal_type, animal_subtype, population)

  # 🔧 FIX: Unificar zonas vacías → NA y emparejar manualmente después
  diet_vars <- diet_vars %>%
    dplyr::mutate(zone = ifelse(zone == "" | is.na(zone), NA, zone))
  ge_df <- ge_df %>%
    dplyr::mutate(zone = ifelse(zone == "" | is.na(zone), NA, zone))
  full_pop_df <- full_pop_df %>%
    dplyr::mutate(zone = ifelse(zone == "" | is.na(zone), NA, zone))

  # --- 3. Unir y calcular ---
  join_keys <- c("group", "zone", "identification", "animal_type", "animal_subtype")

  # 🔧 JOIN compatible con NA (combina matches normales y NA)
  join_pairwise <- function(x, y) {
    normal <- dplyr::left_join(x %>% dplyr::filter(!is.na(zone)),
                               y %>% dplyr::filter(!is.na(zone)),
                               by = join_keys)
    na_join <- dplyr::left_join(x %>% dplyr::filter(is.na(zone)),
                                y %>% dplyr::filter(is.na(zone)),
                                by = join_keys)
    dplyr::bind_rows(normal, na_join)
  }

  final <- diet_vars %>%
    join_pairwise(ge_df) %>%
    join_pairwise(full_pop_df) %>%
    dplyr::mutate(
      ym = dplyr::case_when(
        animal_type == "sheep" ~ 6.7,
        animal_type == "goat" ~ 5.5,
        animal_type == "cattle" & identification == "mature_dairy_cattle" & de >= 70 & ndf <= 35 ~ 5.7,
        animal_type == "cattle" & identification == "mature_dairy_cattle" & de >= 70 & ndf > 35  ~ 6.0,
        animal_type == "cattle" & identification == "mature_dairy_cattle" & de >= 63 & de < 70 & ndf > 37 ~ 6.3,
        animal_type == "cattle" & identification == "mature_dairy_cattle" & de <= 62 & ndf > 38 ~ 6.5,
        animal_type == "cattle" & identification != "mature_dairy_cattle" & de >= 75 ~ 3.0,
        animal_type == "cattle" & identification != "mature_dairy_cattle" & de >= 72 ~ 4.0,
        animal_type == "cattle" & identification != "mature_dairy_cattle" & de >= 62 & de <= 71 ~ 6.3,
        animal_type == "cattle" & identification != "mature_dairy_cattle" & de < 62 ~ 7.0,
        TRUE ~ NA_real_
      ),
      ef_kg_animal_year = (ge * (ym / 100) * 365) / 55.65,
      emissions_total = ef_kg_animal_year * (population / 1e6)
    ) %>%
    dplyr::select(
      group, zone, identification, animal_type, animal_subtype,
      de, ndf, ge, ym, ef_kg_animal_year, population, emissions_total
    )

  # --- 4. Guardar salida ---
  if (saveoutput && nrow(final) > 0) {
    dir.create("output", showWarnings = FALSE)
    readr::write_csv(final, "output/enteric_emissions.csv")
    message("💾 Saved output to output/enteric_emissions.csv")
  }

  return(final)
}
