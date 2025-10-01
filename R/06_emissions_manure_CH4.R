#' Calculate CH₄ Emissions from Manure Management
#'
#' Computes the CH₄ emission factor (EF) in kg CH₄/year/animal
#' and total CH₄ emissions per animal category in Mg (million kg CH₄/year),
#' based on Volatile Solids (VS), B₀, Methane Conversion Factor (MCF),
#' and Average annual fraction of manure handled with each system (AWMS).
#'
#' @param animal Character string (optional). Filter by animal type ("Cattle", "Sheep", "Goat"). Default NULL.
#' @param type Character string (optional). Only applies if `animal = "Cattle"` (e.g., "Dairy", "Beef"). Default NULL.
#' @param zone Character vector (optional). Filter by zone. Default NULL.
#' @param saveoutput Logical (optional). If TRUE, saves the result as CSV. Default FALSE.
#'
#' @examples
#' \dontrun{
#' calculate_CH4_manure(animal = "Cattle", saveoutput = TRUE)
#' calculate_CH4_manure(animal = "Cattle", type = "Dairy", zone = c("A", "B"))
#' }
#'
#' @export
calculate_CH4_manure <- function(animal = NULL, type = NULL, zone = NULL, saveoutput = TRUE) {

  # 1️⃣ Calcular VS con filtros (VS sí tiene zone)
  vs_df <- calculate_vs(animal = animal, type = type, zone = zone) %>%
    select(code, animal_type, animal_subtype, zone, vs)

  # 2️⃣ Info manejo filtrada por animal/type (no por zone)
  ch4_data <- ch4_mm %>%
    select(code, animal_category, animal_type, animal_subtype,
           management_system, system_climate, management_duration)

  if (!is.null(animal)) ch4_data <- ch4_data %>% filter(animal_type == animal)
  if (!is.null(type)) ch4_data <- ch4_data %>% filter(animal_subtype == type)

  # 3️⃣ B0
  B0_data <- coefficients %>%
    filter(coefficient == "B_0") %>%
    select(animal_category = description, B0 = value)

  # 4️⃣ MCF
  MCF_data <- mcf %>%
    select(management_system, system_climate, mcf)

  # 5️⃣ Unir y calcular EF
  joined <- ch4_data %>%
    left_join(vs_df, by = c("code", "animal_type", "animal_subtype")) %>%
    left_join(B0_data, by = "animal_category") %>%
    left_join(MCF_data, by = c("management_system", "system_climate")) %>%
    mutate(
      awms = management_duration / 12,
      EF_CH4_kg_year = (vs * 365) * (B0 * 0.67 * mcf * awms)
    )

  # 6️⃣ Población filtrada por animal/type (sin zone)
  cat_df <- categories %>%
    select(code, animal_type, animal_subtype, n_population)

  if (!is.null(animal)) cat_df <- cat_df %>% filter(animal_type == animal)
  if (!is.null(type)) cat_df <- cat_df %>% filter(animal_subtype == type)

  # 7️⃣ Unir población y calcular emisiones totales
  final <- joined %>%
    left_join(cat_df,
              by = c("code", "animal_type", "animal_subtype")) %>%
    mutate(
      Emissions_CH4_Mg_year = EF_CH4_kg_year * n_population / 1e6
    ) %>%
    select(code, animal_type, animal_subtype, zone, management_system, vs, B0, mcf, awms,
           EF_CH4_kg_year, n_population, Emissions_CH4_Mg_year)

  # --- Guardar CSV si saveoutput = TRUE ---
  if (saveoutput) {
    dir.create("output", showWarnings = FALSE)
    write.csv(final, "output/CH4_manure.csv", row.names = FALSE)
  }

  return(final)
}








