#' Calcula las emisiones indirectas de N₂O por volatilización
#'
#' Details: Calcula emisiones indirectas de N₂O derivadas de la volatilización de nitrógeno excretado.
#'
#' @param animal Character string (opcional). Tipo de ganado ("cattle", "sheep", "goat"). Default NULL.
#' @param type Character string (opcional). Subtipo solo para cattle. Default NULL.
#' @param zone Character vector (opcional). Filtrar por zona. Default NULL.
#' @param saveoutput Logical. Si TRUE, guarda el resultado en "output/N2O_indirect_volatilization.csv". Default FALSE.
#' @return Tibble con emisiones indirectas por categoría
#' @export
#' @examples
#' \donttest{
#'   calculate_N2O_indirect_volatilization(animal = "cattle", type = "dairy", zone = c("a"), saveoutput = TRUE)
#' }
calculate_N2O_indirect_volatilization <- function(animal = NULL, type = NULL, zone = NULL, saveoutput = TRUE) {

  # --- Calcular N_excreted usando la función directa ---
  df_n <- calculate_N2O_direct_manure(animal = animal, type = type, zone = zone) %>%
    dplyr::select(code, animal_type, animal_subtype, zone, N_excreted) %>%
    dplyr::distinct()

  # --- Población ---
  df_pop <- categories %>%
    dplyr::select(code, animal_type, animal_subtype, n_population) %>%
    dplyr::distinct()

  # --- Calcular emisiones indirectas ---
  df <- n2o_indirect %>%
    dplyr::mutate(awms = duration / 12) %>%
    dplyr::left_join(df_n, by = c("code", "animal_type", "animal_subtype")) %>%
    dplyr::left_join(df_pop, by = c("code", "animal_type", "animal_subtype")) %>%
    dplyr::left_join(fractions, by = "management_system") %>%
    dplyr::mutate(
      n_volatilization = n_population * N_excreted * awms * frac_gas_ms
    ) %>%
    dplyr::left_join(
      emission_factors_volatilization %>% dplyr::select(climate, ef4 = value),
      by = "climate"
    ) %>%
    dplyr::mutate(
      n2o_g = ef4 * n_volatilization * (44 / 28)
    )

  # --- FILTRO OPCIONAL ---
  if (!is.null(animal)) df <- df %>% dplyr::filter(animal_type %in% animal)
  if (!is.null(type)) df <- df %>% dplyr::filter(animal_subtype %in% type)
  if (!is.null(zone)) df <- df %>% dplyr::filter(zone %in% zone)

  # --- Guardar CSV si saveoutput = TRUE ---
  if (saveoutput) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(df, "output/N2O_indirect_volatilization.csv")
  }

  return(df)
}








