#' Calculate direct N₂O emissions from manure
#'
#' Computes direct N₂O emissions based on nitrogen excretion, emission factors,
#' management system, and climate.
#'
#' @param animal Character string (optional). Animal type ("Cattle", "Sheep", "Goat"). Default NULL.
#' @param type Character string (optional). Only for "Cattle" subtype. Default NULL.
#' @param zone Character vector (optional). Filter by zone. Default NULL.
#' @param saveoutput Logical. If TRUE, saves output as CSV in "output/N2O_direct_manure.csv". Default FALSE.
#' @return A tibble with N₂O emissions per category
#' @export
#' @examples
#' \donttest{
#' calculate_N2O_direct_manure(animal = "Cattle", type = "Dairy", zone = c("A"), saveoutput = TRUE)
#' }
calculate_N2O_direct_manure <- function(animal = NULL, type = NULL, zone = NULL, saveoutput = TRUE) {

  # --- Preparar tablas base ---
  categories_df <- categories %>%
    select(code, animal_type, animal_subtype, milk_yield, fat_content, n_population)

  weights_df <- weights %>% select(code, animal_type, animal_subtype, weight_gain)
  direct_df <- n2o_direct

  ef_tab <- emission_factors_direct %>%
    select(management_system, climate, value)

  # --- Filtrar por animal y type ---
  if (!is.null(animal)) categories_df <- categories_df %>% filter(animal_type == animal)
  if (!is.null(type)) categories_df <- categories_df %>% filter(animal_subtype == type)

  codes_validos <- categories_df$code

  # --- Filtrar otras tablas por códigos válidos ---
  ge <- calculate_ge(animal = animal, type = type, zone = zone) %>%
    filter(code %in% codes_validos) %>%
    select(code, animal_type, animal_subtype, zone, ge)

  cp <- calculate_weighted_variable(animal = animal, type = type, zone = zone) %>%
    filter(code %in% codes_validos) %>%
    select(code, animal_type, animal_subtype, zone, cp)

  NEg <- calculate_NEg(animal = animal, type = type) %>%
    filter(code %in% codes_validos) %>%
    select(code, animal_type, animal_subtype, NEg)

  direct_df <- direct_df %>% filter(code %in% codes_validos)

  # --- Calcular emisiones ---
  df <- ge %>%
    inner_join(cp, by = c("code", "animal_type", "animal_subtype", "zone")) %>%
    left_join(categories_df, by = c("code", "animal_type", "animal_subtype")) %>%
    left_join(weights_df, by = c("code", "animal_type", "animal_subtype")) %>%
    left_join(NEg, by = c("code", "animal_type", "animal_subtype")) %>%
    inner_join(direct_df, by = c("code", "animal_type", "animal_subtype")) %>%
    # ⬇️ unir EF por management_system + climate
    left_join(ef_tab, by = c("management_system", "climate")) %>%
    mutate(
      milk_protein = 1.9 + 0.4 * fat_content,
      N_retention = case_when(
        animal_type %in% c("Sheep", "Goat") ~ 0.1,  # Para Sheep y Goat
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

  # --- Guardar salida si saveoutput = TRUE ---
  if (saveoutput) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(df, "output/N2O_direct_manure.csv")
  }

  return(df)
}



