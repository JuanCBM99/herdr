#' Calculate methane emissions from enteric fermentation (Refactored)
#'
#' Computes enteric methane emissions based on Gross Energy (GE),
#' Digestible Energy (DE), NDF, and Ym factor.
#' @export
calculate_emissions_enteric <- function(saveoutput = TRUE) {

  message("🟢 Calculating enteric fermentation emissions...")

  # --- 1. Obtener datos base ---
  # Empezamos con las variables de dieta (DE, NDF)
  diet_vars <- calculate_weighted_variable(saveoutput = FALSE) %>%
    dplyr::select(group, zone, identification, animal_type, animal_subtype, de, ndf)

  # Salida temprana si no hay datos
  if (nrow(diet_vars) == 0) {
    message("⚠️ No diet data found. Returning empty structure.")
    return(dplyr::tibble(
      group = character(), zone = character(), identification = character(),
      animal_type = character(), animal_subtype = character(),
      de = double(), ndf = double(), ge = double(), ym = double(),
      ef_kg_animal_year = double(), population = double(), emissions_total = double()
    ))
  }

  join_keys <- c("group", "zone", "identification", "animal_type", "animal_subtype")

  # --- 2. Pipeline de Procesamiento ---
  results <- diet_vars %>%
    # Estandarización de zonas (vacío -> NA) para asegurar el join correcto
    dplyr::mutate(zone = dplyr::na_if(zone, "")) %>%

    # 2.1 Unir Energía Bruta (GE)
    dplyr::left_join(
      calculate_ge(saveoutput = FALSE) %>%
        dplyr::select(all_of(join_keys), ge) %>%
        dplyr::mutate(zone = dplyr::na_if(zone, "")),
      by = join_keys,
      na_matches = "na" # <-- Reemplaza a tu función 'join_pairwise'
    ) %>%

    # 2.2 Unir Población
    dplyr::left_join(
      calculate_population(saveoutput = FALSE) %>%
        dplyr::select(all_of(join_keys), population) %>%
        dplyr::mutate(zone = dplyr::na_if(zone, "")),
      by = join_keys,
      na_matches = "na"
    ) %>%

    # 2.3 Cálculos
    dplyr::mutate(
      # Seguridad de tipos numéricos
      across(c(de, ndf, ge, population), ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)),

      # Cálculo del Factor de Conversión de Metano (Ym)
      ym = dplyr::case_when(
        # Pequeños rumiantes
        animal_type == "sheep" ~ 6.7,
        animal_type == "goat"  ~ 5.5,

        # Ganado Lechero Maduro (Mature Dairy Cattle)
        animal_type == "cattle" & identification == "mature_dairy_cattle" ~ dplyr::case_when(
          de >= 70 & ndf <= 35 ~ 5.7,
          de >= 70 & ndf > 35  ~ 6.0,
          de >= 63 & de < 70 & ndf > 37 ~ 6.3,
          de <= 62 & ndf > 38  ~ 6.5,
          TRUE ~ 6.5 # Valor por defecto defensivo si cae en huecos lógicos, ajustar según IPCC si es necesario
        ),

        # Otros Bovinos (Cattle general)
        animal_type == "cattle" & identification != "mature_dairy_cattle" ~ dplyr::case_when(
          de >= 75 ~ 3.0,
          de >= 72 ~ 4.0,
          de >= 62 & de <= 71 ~ 6.3,
          de < 62  ~ 7.0,
          TRUE ~ 6.3 # Valor por defecto común
        ),

        TRUE ~ NA_real_
      ),

      # Factor de Emisión (kg CH4/cabeza/año)
      # Fórmula: (GE * (Ym/100) * 365 días) / 55.65 MJ/kg CH4
      ef_kg_animal_year = (ge * (ym / 100) * 365) / 55.65,

      # Emisiones Totales (Mg o Toneladas, asumiendo population / 1e6)
      emissions_total = ef_kg_animal_year * (population / 1e6)
    ) %>%

    # Selección y redondeo
    dplyr::select(
      group, zone, identification, animal_type, animal_subtype,
      de, ndf, ge, ym, ef_kg_animal_year, population, emissions_total
    ) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))

  # --- 3. Guardado ---
  if (isTRUE(saveoutput) && nrow(results) > 0) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/enteric_emissions.csv")
    message("💾 Saved output to output/enteric_emissions.csv")
  }

  return(results)
}
