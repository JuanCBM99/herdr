#' Calculate indirect N₂O emissions from leaching
#'
#' Computes indirect N₂O emissions from nitrogen leaching.
#'
#' @param animal Character string (optional). Animal type ("Cattle", "Sheep", "Goat"). Default NULL.
#' @param type Character string (optional). Subtype only for Cattle. Default NULL.
#' @param zone Character vector (optional). Filter by zone. Default NULL.
#' @param saveoutput Logical. If TRUE, saves output to "output/N2O_indirect_leaching.csv". Default TRUE.
#' @return Tibble with N₂O emissions per category from leaching
#' @export
calculate_N2O_indirect_leaching <- function(animal = NULL, type = NULL, zone = NULL, saveoutput = TRUE) {

  message("🟢 Calculating indirect N₂O emissions (leaching)...")

  # --- 1️⃣ Cargar datasets base ---
  n2o_indirect_df <- load_dataset("n2o_indirect")
  fractions_df <- load_dataset("fractions") %>% dplyr::select(management_system, frac_leach_ms)
  categories_df <- load_dataset("categories") %>% dplyr::select(code, animal_type, animal_subtype, n_population)

  # --- 2️⃣ Calcular N_excreted usando la función directa ---
  df_n <- calculate_N2O_direct_manure(animal = animal, type = type, zone = zone, saveoutput = FALSE) %>%
    dplyr::select(code, animal_type, animal_subtype, zone, N_excreted) %>%
    dplyr::distinct()

  # --- 3️⃣ Filtrar zonas si se especifica ---
  if (!is.null(zone)) n2o_indirect_df <- n2o_indirect_df %>% dplyr::filter(zone %in% zone)

  # --- 4️⃣ Calcular emisiones ---
  df <- n2o_indirect_df %>%
    dplyr::mutate(awms = duration / 12) %>%
    dplyr::left_join(df_n, by = c("code","animal_type","animal_subtype")) %>%
    dplyr::left_join(categories_df, by = c("code","animal_type","animal_subtype")) %>%
    dplyr::left_join(fractions_df, by = "management_system") %>%
    dplyr::mutate(
      N_leaching = n_population * N_excreted * awms * frac_leach_ms,
      EF5 = 0.011,
      N2O_L = EF5 * N_leaching * (44 / 28)
    )

  # --- 5️⃣ Filtrado opcional ---
  if (!is.null(animal)) df <- df %>% dplyr::filter(animal_type %in% animal)
  if (!is.null(type)) df <- df %>% dplyr::filter(animal_subtype %in% type)
  if (!is.null(zone)) df <- df %>% dplyr::filter(zone %in% zone)

  # --- 6️⃣ Guardar CSV ---
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    readr::write_csv(df, "output/N2O_indirect_leaching.csv")
    message("💾 Saved output to output/N2O_indirect_leaching.csv")
  }

  df
}




