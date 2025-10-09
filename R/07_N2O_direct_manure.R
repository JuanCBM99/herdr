#' Calculate direct N₂O emissions from manure
#'
#' Computes direct N₂O emissions based on nitrogen excretion, emission factors,
#' management system, and climate.
#'
#' @param animal Character string (optional). Animal type ("Cattle", "Sheep", "Goat"). Default NULL.
#' @param type Character string (optional). Only for "Cattle" subtype. Default NULL.
#' @param zone Character vector (optional). Filter by zone. Default NULL.
#' @param saveoutput Logical. If TRUE, saves output as CSV. Default TRUE.
#' @return A tibble with N₂O emissions per category
#' @export
calculate_N2O_direct_manure <- function(animal = NULL, type = NULL, zone = NULL, saveoutput = TRUE) {

  message("🟢 Calculating direct N₂O emissions from manure...")

  # --- 1️⃣ Cargar datasets base ---
  categories_df <- load_dataset("categories") %>%
    dplyr::select(code, animal_type, animal_subtype, milk_yield, fat_content, n_population)

  weights_df <- load_dataset("weights") %>%
    dplyr::select(code, animal_type, animal_subtype, weight_gain)

  direct_df <- load_dataset("n2o_direct")

  ef_tab <- load_dataset("emission_factors_direct") %>%
    dplyr::select(management_system, climate, value)

  # --- 2️⃣ Filtrar por animal/type ---
  if (!is.null(animal)) categories_df <- categories_df %>% dplyr::filter(animal_type == animal)
  if (!is.null(type)) categories_df <- categories_df %>% dplyr::filter(animal_subtype == type)

  codes_validos <- categories_df$code

  # --- 3️⃣ Filtrar otras tablas por códigos válidos ---
  ge <- calculate_ge(animal = animal, type = type, zone = zone, saveoutput = FALSE) %>%
    dplyr::filter(code %in% codes_validos) %>%
    dplyr::select(code, animal_type, animal_subtype, zone, ge)

  cp <- calculate_weighted_variable(animal = animal, type = type, zones = zone, saveoutput = FALSE) %>%
    dplyr::filter(code %in% codes_validos) %>%
    dplyr::select(code, animal_type, animal_subtype, zone, cp)

  NEg <- calculate_NEg(animal = animal, type = type, saveoutput = FALSE) %>%
    dplyr::filter(code %in% codes_validos) %>%
    dplyr::select(code, animal_type, animal_subtype, NEg)

  direct_df <- direct_df %>% dplyr::filter(code %in% codes_validos)

  # --- 4️⃣ Calcular emisiones ---
  df <- ge %>%
    dplyr::inner_join(cp, by = c("code", "animal_type", "animal_subtype", "zone")) %>%
    dplyr::left_join(categories_df, by = c("code", "animal_type", "animal_subtype")) %>%
    dplyr::left_join(weights_df, by = c("code", "animal_type", "animal_subtype")) %>%
    dplyr::left_join(NEg, by = c("code", "animal_type", "animal_subtype")) %>%
    dplyr::inner_join(direct_df, by = c("code", "animal_type", "animal_subtype")) %>%
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
      Emisiones_N2O = n_population * N_excreted * awms * value * (44 / 28)
    )

  # --- 5️⃣ Guardar salida ---
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    readr::write_csv(df, "output/N2O_direct_manure.csv")
    message("💾 Saved output to output/N2O_direct_manure.csv")
  }

  df
}

