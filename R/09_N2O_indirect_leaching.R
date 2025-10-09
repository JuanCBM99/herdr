#' Calcula las emisiones indirectas de N₂O por lixiviación
#'
#' Details: Calcula las emisiones indirectas de N₂O generadas por lixiviación de nitrógeno excretado.
#'
#' @param animal Character string (optional). Tipo de ganado ("Cattle", "Sheep", "Goat"). Default NULL.
#' @param type Character string (optional). Solo para "Cattle" subtype. Default NULL.
#' @param zone Character vector (optional). Filtrar por zona. Default NULL.
#' @param saveoutput Logical. Si TRUE, guarda el resultado en "output/N2O_indirect_leaching.csv". Default FALSE.
#' @return Tibble con las emisiones de N₂O por lixiviación.
#' @export
#' @examples
#' \donttest{
#' calculate_N2O_indirect_leaching(animal = "Cattle", type = "Dairy", zone = c("A"), saveoutput = TRUE)
#' }
calculate_N2O_indirect_leaching <- function(animal = NULL, type = NULL, zone = NULL, saveoutput = TRUE) {

  # --- Calcular N_excreted usando la función directa adaptada ---
  df_n <- calculate_N2O_direct_manure(animal = animal, type = type, zone = zone) %>%
    dplyr::select(code, animal_type, animal_subtype, zone, N_excreted) %>%
    dplyr::distinct()

  # --- Población ---
  df_pop <- categories %>%
    dplyr::select(code, animal_type, animal_subtype, n_population) %>%
    dplyr::distinct()

  # --- Filtrar zonas si se especifica ---
  df_indirect <- n2o_indirect
  if (!is.null(zone)) {
    df_indirect <- df_indirect %>% filter(zone %in% zone)
  }

  # --- Calcular emisiones ---
  df <- df_indirect %>%
    dplyr::mutate(awms = duration / 12) %>%
    dplyr::left_join(df_n, by = c("code","animal_type","animal_subtype")) %>%
    dplyr::left_join(df_pop, by = c("code","animal_type","animal_subtype")) %>%
    dplyr::left_join(fractions, by = "management_system") %>%
    dplyr::mutate(
      N_leaching = n_population * N_excreted * awms * frac_leach_ms,
      EF5 = 0.011,
      N2O_L = EF5 * N_leaching * (44 / 28)
    )

  # --- FILTRO OPCIONAL ---
  if (!is.null(animal)) df <- df %>% dplyr::filter(animal_type %in% animal)
  if (!is.null(type)) df <- df %>% dplyr::filter(animal_subtype %in% type)
  if (!is.null(zone)) df <- df %>% dplyr::filter(zone %in% zone)

  # --- Guardar salida ---
  if (saveoutput) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(df, "output/N2O_indirect_leaching.csv")
  }

  return(df)
}



