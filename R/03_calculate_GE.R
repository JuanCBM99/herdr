#' Calculate Gross Energy (GE)
#'
#' Computes gross energy requirements by summing net energy components
#' for maintenance, activity, growth, work, pregnancy, lactation, and wool production,
#' then adjusting by digestible energy (DE). Coefficients for maintenance (rem) and growth (reg) follow NRC methodology.
#'
#' @param animal Character. Optional. Type of animal ("Cattle", "Sheep", "Goat", etc.). If NULL, all animals are returned.
#' @param type Character. Optional. Only applies if animal = "Cattle" (e.g., "Dairy", "Beef").
#' @param zone Character vector (optional). Filter by zone.
#' @param saveoutput Logical (optional). If TRUE, saves the result as CSV. Default TRUE.
#'
#' @return A tibble with gross energy (GE), its components, animal type/subtype, and zone.
#' @export
calculate_ge <- function(animal = NULL, type = NULL, zone = NULL, saveoutput = TRUE) {

  message("🟢 Calculating gross energy (GE)...")

  # --- Calcular NE (usando funciones adaptadas) ---
  NEm <- calculate_NEm(animal = animal, type = type, saveoutput = FALSE)
  NEa <- calculate_NEa(animal = animal, type = type, saveoutput = FALSE)
  NEg <- calculate_NEg(animal = animal, type = type, saveoutput = FALSE)
  NE_work <- calculate_NE_work(animal = animal, type = type, saveoutput = FALSE)
  NE_pregnancy <- calculate_NE_pregnancy(animal = animal, type = type, saveoutput = FALSE)
  NEl <- calculate_NEl(animal = animal, type = type, saveoutput = FALSE)
  NE_wool <- calculate_NE_wool(animal = animal, type = type, saveoutput = FALSE)

  # --- Combinar NE ---
  NE_all <- NEm %>%
    dplyr::select(code, animal_type, animal_subtype, NEm) %>%
    dplyr::left_join(NEa %>% dplyr::select(code, animal_type, animal_subtype, NEa),
                     by = c("code", "animal_type", "animal_subtype")) %>%
    dplyr::left_join(NEg %>% dplyr::select(code, animal_type, animal_subtype, NEg),
                     by = c("code", "animal_type", "animal_subtype")) %>%
    dplyr::left_join(NE_work %>% dplyr::select(code, animal_type, animal_subtype, NE_work),
                     by = c("code", "animal_type", "animal_subtype")) %>%
    dplyr::left_join(NE_pregnancy %>% dplyr::select(code, animal_type, animal_subtype, NE_pregnancy),
                     by = c("code", "animal_type", "animal_subtype")) %>%
    dplyr::left_join(NEl %>% dplyr::select(code, animal_type, animal_subtype, NEl),
                     by = c("code", "animal_type", "animal_subtype")) %>%
    dplyr::left_join(NE_wool %>% dplyr::select(code, animal_type, animal_subtype, NE_wool),
                     by = c("code", "animal_type", "animal_subtype"))

  # --- Calcular DE por zone ---
  de_df <- calculate_weighted_variable(animal = animal, type = type, zones = zone, saveoutput = FALSE)

  # Filtrar por zona(s) si se especifica
  if (!is.null(zone)) {
    de_df <- de_df %>% dplyr::filter(zone %in% zone)
  }

  # --- Combinar NE con DE y calcular GE ---
  final <- NE_all %>%
    dplyr::inner_join(de_df, by = c("code", "animal_type", "animal_subtype")) %>%
    dplyr::mutate(
      dplyr::across(c(NEm, NEa, NEg, NE_work, NE_pregnancy, NEl, NE_wool, de),
                    ~ tidyr::replace_na(., 0))
    ) %>%
    dplyr::mutate(
      de_percent = ifelse(de == 0, 0.60, de / 100),
      reg = 1.164 - (5.16e-3 * de) + (1.308e-5 * de^2) - (37.4 / ifelse(de == 0, 60, de)),
      rem = 1.123 - (4.092e-3 * de) + (1.126e-5 * de^2) - (25.4 / ifelse(de == 0, 60, de)),
      ge = ((NEm + NEa + NEl + NE_work + NE_pregnancy) / ifelse(rem == 0, 1, rem) +
              (NEg + NE_wool) / ifelse(reg == 0, 1, reg)) / de_percent
    ) %>%
    dplyr::select(code, animal_type, animal_subtype, zone,
                  NEm, NEa, NEg, NE_work, NE_pregnancy, NEl, NE_wool,
                  de, rem, reg, ge)

  # --- Guardar salida ---
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    write.csv(final, "output/ge_result.csv", row.names = FALSE)
    message("💾 Saved output to output/ge_result.csv")
  }

  return(final)
}
