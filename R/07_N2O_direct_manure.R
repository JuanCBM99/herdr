#' Calculate direct N₂O emissions from manure
#'
#' Computes direct N₂O emissions based on nitrogen excretion, emission factors,
#' management system, and climate for all animals.
#'
#' @param saveoutput Logical. If TRUE, saves output as CSV. Default TRUE.
#' @return A tibble with N₂O emissions per category
#' @export
calculate_N2O_direct_manure <- function(saveoutput = TRUE) {

  message("🟢 Calculating direct N₂O emissions from manure...")

  # --- 1️⃣ Cargar datasets de configuración (universales) ---
  categories_df <- load_dataset("categories") %>%
    dplyr::select(identification, animal_type, animal_subtype, milk_yield, fat_content)

  weights_df <- load_dataset("weights") %>%
    dplyr::select(identification, animal_type, animal_subtype, weight_gain)

  direct_df <- load_dataset("n2o_direct") # Tiene management_system, climate, etc.

  ef_tab <- load_dataset("emission_factors_direct") %>%
    dplyr::select(management_system, climate, value)

  # --- 2️⃣ Cargar datos calculados (específicos de group/zone) ---
  message("  -> Calculando GE, CP, NEg y Población...")

  ge <- calculate_ge(saveoutput = FALSE) %>%
    dplyr::select(group, zone, identification, animal_type, animal_subtype, ge)

  cp <- calculate_weighted_variable(saveoutput = FALSE) %>%
    dplyr::select(group, zone, identification, animal_type, animal_subtype, cp)

  NEg <- calculate_NEg(saveoutput = FALSE) %>%
    dplyr::select(identification, animal_type, animal_subtype, NEg) # NEg es universal

  pop_df <- calculate_population(saveoutput = FALSE) %>%
    dplyr::select(group, zone, identification, animal_type, animal_subtype, population)

  # --- 3️⃣ Unir datasets y calcular emisiones ---

  # Claves de unión
  join_keys_specific <- c("group", "zone", "identification", "animal_type", "animal_subtype")
  join_keys_universal <- c("identification", "animal_type", "animal_subtype")

  df <- ge %>%
    # Unir CP (específico)
    dplyr::left_join(cp, by = join_keys_specific, na_matches = "na") %>%
    # Unir Población (específico)
    dplyr::left_join(pop_df, by = join_keys_specific, na_matches = "na") %>%
    # Unir Categorías (universal)
    dplyr::left_join(categories_df, by = join_keys_universal) %>%
    # Unir Pesos (universal)
    dplyr::left_join(weights_df, by = join_keys_universal) %>%
    # Unir NEg (universal)
    dplyr::left_join(NEg, by = join_keys_universal) %>%
    # Unir sistemas de manejo (universal)
    dplyr::left_join(direct_df, by = join_keys_universal) %>%
    # Unir factores de emisión
    dplyr::left_join(ef_tab, by = c("management_system", "climate")) %>%
    dplyr::mutate(
      milk_protein = 1.9 + 0.4 * fat_content,
      N_retention = dplyr::case_when(
        animal_type %in% c("Sheep", "Goat") ~ 0.1,
        !is.na(milk_yield) & !is.na(milk_protein) &
          !is.na(weight_gain) & !is.na(NEg) & weight_gain != 0 ~
          ((milk_yield * milk_protein) / 6.38) +
          ((weight_gain * (268 - (7.03 * NEg / weight_gain)) / 1000) / 6.25),
        TRUE ~ 0
      ),
      N_intake = (ge / 18.45) * (cp / 100 / 6.25),
      N_excreted = ifelse(animal_type %in% c("Sheep", "Goat"),
                          (N_intake * (1 - N_retention)) * 365,
                          (N_intake - N_retention) * 365),
      awms = management_duration / 12,
      population = tidyr::replace_na(population, 0),
      Emisiones_N2O = population * N_excreted * awms * value * (44 / 28)
    )

  # --- 4️⃣ Guardar salida ---
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    readr::write_csv(df, "output/N2O_direct_manure.csv")
    message("💾 Saved output to output/N2O_direct_manure.csv")
  }

  df
}
