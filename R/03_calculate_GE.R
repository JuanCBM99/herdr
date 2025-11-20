#' Calculate Gross Energy (GE)
#'
#' Computes gross energy requirements for all animals by summing all
#' net energy components and adjusting by digestible energy (DE).
#'
#' @param saveoutput Logical (optional). If TRUE, saves the result as CSV. Default TRUE.
#'
#' @return A tibble with gross energy (GE) and its components for all
#' animal categories, groups, and zones.
#' @export
calculate_ge <- function(saveoutput = TRUE) {

  message("🟢 Calculating Gross Energy (GE)...")

  # --- 1. Calcular todos los componentes de NE (sin filtros) ---
  message("  -> 1/3: Calculating all Net Energy components...")
  # (Estas funciones devuelven 'identification', 'animal_type', 'animal_subtype'
  # y el valor NE. NO tienen 'group' ni 'zone' para categorías
  # universales como k1)
  NEm <- calculate_NEm(saveoutput = FALSE)
  NEa <- calculate_NEa(saveoutput = FALSE)
  NEg <- calculate_NEg(saveoutput = FALSE)
  NE_work <- calculate_NE_work(saveoutput = FALSE)
  NE_pregnancy <- calculate_NE_pregnancy(saveoutput = FALSE)
  NEl <- calculate_NEl(saveoutput = FALSE)
  NE_wool <- calculate_NE_wool(saveoutput = FALSE)

  # --- 2. Combinar todos los componentes de NE ---
  message("  -> 2/3: Combining Net Energy components...")

  # Claves de unión universales
  join_keys_universal <- c("identification", "animal_type", "animal_subtype")

  ne_list <- list(
    NEm,
    NEa %>% dplyr::select(all_of(join_keys_universal), NEa),
    NEg %>% dplyr::select(all_of(join_keys_universal), NEg),
    NE_work %>% dplyr::select(all_of(join_keys_universal), NE_work),
    NE_pregnancy %>% dplyr::select(all_of(join_keys_universal), NE_pregnancy),
    NEl %>% dplyr::select(all_of(join_keys_universal), NEl),
    NE_wool %>% dplyr::select(all_of(join_keys_universal), NE_wool)
  )

  # NE_all contiene las propiedades "universales"
  NE_all <- ne_list %>%
    purrr::reduce(dplyr::left_join, by = join_keys_universal)

  # --- 3. Calcular DE (sin filtros) ---
  message("  -> 3/3: Calculating weighted diet variables (DE)...")
  # de_df contiene las propiedades "específicas" (por group y zone)
  de_df <- calculate_weighted_variable(saveoutput = FALSE)

  # --- 4. Combinar NE con DE y calcular GE ---
  message("  -> 4/4: Calculating final Gross Energy (GE)...")

  # --- ¡ESTE ES EL CAMBIO CLAVE! ---
  # Unimos las propiedades 'universales' (NE_all) a las 'específicas' (de_df)
  # usando *solo* las claves universales.
  # Esto "difunde" el NEm de k1 (universal) a las filas de k1 en Zona A y Zona B.

  final <- de_df %>% # <-- Empezamos con 'de_df' (que tiene group/zone)
    dplyr::inner_join(NE_all, by = join_keys_universal) %>%

    dplyr::mutate(
      dplyr::across(
        c(NEm, NEa, NEg, NE_work, NE_pregnancy, NEl, NE_wool, de),
        ~ tidyr::replace_na(., 0)
      )
    ) %>%
    dplyr::mutate(
      de_percent = ifelse(de == 0, 0.60, de / 100),
      reg = 1.164 - (5.16e-3 * de) + (1.308e-5 * de^2) - (37.4 / ifelse(de == 0, 60, de)),
      rem = 1.123 - (4.092e-3 * de) + (1.126e-5 * de^2) - (25.4 / ifelse(de == 0, 60, de)),
      ge = ((NEm + NEa + NEl + NE_work + NE_pregnancy) / ifelse(rem == 0, 1, rem) +
              (NEg + NE_wool) / ifelse(reg == 0, 1, reg)) / de_percent
    ) %>%
    # Reordenamos las columnas para que se vea bien
    dplyr::select(
      group, zone, identification, animal_type, animal_subtype,
      NEm, NEa, NEg, NE_work, NE_pregnancy, NEl, NE_wool,
      de, rem, reg, ge
    )

  # --- Guardar salida ---
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    readr::write_csv(final, "output/ge_result.csv")
    message("💾 Saved output to output/ge_result.csv")
  }

  return(final)
}
