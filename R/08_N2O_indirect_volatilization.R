#' Calculate indirect N₂O emissions from volatilization
#'
#' Computes indirect N₂O emissions derived from volatilization of excreted nitrogen.
#'
#' @param animal Character string (optional). Animal type ("Cattle", "Sheep", "Goat"). Default NULL.
#' @param type Character string (optional). Subtype only for Cattle. Default NULL.
#' @param zone Character vector (optional). Filter by zone. Default NULL.
#' @param saveoutput Logical. If TRUE, saves output to "output/N2O_indirect_volatilization.csv". Default TRUE.
#' @return Tibble with indirect N₂O emissions per category
#' @export
calculate_N2O_indirect_volatilization <- function(animal = NULL, type = NULL, zone = NULL, saveoutput = TRUE) {

  message("🟢 Calculating indirect N₂O emissions (volatilization)...")

  # --- 1️⃣ Cargar datasets base ---
  n2o_indirect_df <- load_dataset("n2o_indirect")
  fractions_df <- load_dataset("fractions") %>% dplyr::select(management_system, frac_gas_ms)
  ef4_df <- load_dataset("emission_factors_volatilization") %>% dplyr::select(climate, ef4 = value)
  categories_df <- load_dataset("categories") %>% dplyr::select(code, animal_type, animal_subtype, n_population)

  # --- 2️⃣ Calcular N_excreted usando la función directa ---
  df_n <- calculate_N2O_direct_manure(animal = animal, type = type, zone = zone, saveoutput = FALSE) %>%
    dplyr::select(code, animal_type, animal_subtype, zone, N_excreted) %>%
    dplyr::distinct()

  # --- 3️⃣ Calcular emisiones indirectas ---
  df <- n2o_indirect_df %>%
    dplyr::mutate(awms = duration / 12) %>%
    dplyr::left_join(df_n, by = c("code", "animal_type", "animal_subtype")) %>%
    dplyr::left_join(categories_df, by = c("code", "animal_type", "animal_subtype")) %>%
    dplyr::left_join(fractions_df, by = "management_system") %>%
    dplyr::left_join(ef4_df, by = "climate") %>%
    dplyr::mutate(
      n_volatilization = n_population * N_excreted * awms * frac_gas_ms,
      n2o_g = ef4 * n_volatilization * (44 / 28)
    )

  # --- 4️⃣ Filtrado opcional ---
  if (!is.null(animal)) df <- df %>% dplyr::filter(animal_type %in% animal)
  if (!is.null(type)) df <- df %>% dplyr::filter(animal_subtype %in% type)
  if (!is.null(zone)) df <- df %>% dplyr::filter(zone %in% zone)

  # --- 5️⃣ Guardar CSV ---
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    readr::write_csv(df, "output/N2O_indirect_volatilization.csv")
    message("💾 Saved output to output/N2O_indirect_volatilization.csv")
  }

  df
}



