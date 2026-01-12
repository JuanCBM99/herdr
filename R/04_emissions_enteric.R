#' Calculate methane emissions from enteric fermentation (Refactored)
#'
#' Computes enteric methane emissions based on Gross Energy (GE),
#' Digestible Energy (DE), NDF, and Ym factor.
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @export
calculate_emissions_enteric <- function(saveoutput = TRUE) {

  message("\U0001f7e2 Calculating enteric fermentation emissions...")

  # --- 1. Data Preparation and Loading ---

  # Get Base Data: Start with weighted diet variables (DE, NDF)
  diet_vars <- calculate_weighted_variable(saveoutput = FALSE) %>%
    dplyr::select(group, zone, identification, animal_type, animal_subtype, de, ndf)

  # Early exit if no diet data is found
  if (nrow(diet_vars) == 0) {
    message("\u26a0 No diet data found. Returning empty structure.")
    return(dplyr::tibble(
      group = character(), zone = character(), identification = character(),
      animal_type = character(), animal_subtype = character(),
      de = double(), ndf = double(), ge = double(), ym = double(),
      ef_kg_animal_year = double(), population = double(), emissions_total = double()
    ))
  }

  join_keys <- c("group", "zone", "identification", "animal_type", "animal_subtype")

  # --- 2. Processing Pipeline: Merge Data ---
  results <- diet_vars %>%
    # Standardize zones (empty -> NA) for robust merging
    dplyr::mutate(zone = dplyr::na_if(zone, "")) %>%

    # 2.1 Join Gross Energy (GE) from the energy module
    dplyr::left_join(
      calculate_ge(saveoutput = FALSE) %>%
        dplyr::select(all_of(join_keys), ge) %>%
        dplyr::mutate(zone = dplyr::na_if(zone, "")),
      by = join_keys,
      na_matches = "na" # Handles joins where Zone is NA
    ) %>%

    # 2.2 Join Population from the demographic module
    dplyr::left_join(
      calculate_population(saveoutput = FALSE) %>%
        dplyr::select(all_of(join_keys), population) %>%
        dplyr::mutate(zone = dplyr::na_if(zone, "")),
      by = join_keys,
      na_matches = "na"
    ) %>%

    # --- 3. Calculations (Ym and Emission Factor) ---
    dplyr::mutate(
      # Numeric type safety and NA replacement
      across(c(de, ndf, ge, population), ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)),

      # Calculate Methane Conversion Factor (Ym) based on IPCC Tier 2 Feed Quality
      ym = dplyr::case_when(
        # Small Ruminants (Fixed Ym based on species)
        animal_type == "sheep" ~ 6.7,
        animal_type == "goat"  ~ 5.5,

        # Mature Dairy Cattle (Ym based on DE and NDF thresholds)
        animal_type == "cattle" & identification == "mature_dairy_cattle" ~ dplyr::case_when(
          de >= 70 & ndf <= 35 ~ 5.7, # High quality/low NDF
          de >= 70 & ndf > 35  ~ 6.0,
          de >= 63 & de < 70 & ndf > 37 ~ 6.3, # Medium quality
          de <= 62 & ndf > 38  ~ 6.5, # Low quality
          TRUE ~ 6.5
        ),

        # Other Bovine Categories (Ym based on DE thresholds, simplified feed regimes)
        animal_type == "cattle" & identification != "mature_dairy_cattle" ~ dplyr::case_when(
          de >= 75 ~ 3.0, # Feedlot (highest concentration)
          de >= 72 ~ 4.0, # Feedlot (other grains)
          de >= 62 & de <= 71 ~ 6.3, # Mixed/High quality forage
          de < 62  ~ 7.0, # Low quality forage
          TRUE ~ 6.3
        ),

        TRUE ~ NA_real_ # Unidentified animal type
      ),

      # Emission Factor (EF) Calculation (IPCC Eq 10.21)
      # EF = (GE * (Ym/100) * 365) / 55.65
      ef_kg_animal_year = (ge * (ym / 100) * 365) / 55.65,

      # Total Emissions (Gg CH4/year)
      # Total Emissions = EF * Population / 1e6 (kg to Gg conversion)
      emissions_total = ef_kg_animal_year * (population / 1e6)
    ) %>%

    # --- 4. Final Cleanup and Output ---

    # Select final columns and round results
    dplyr::select(
      group, zone, identification, animal_type, animal_subtype,
      de, ndf, ge, ym, ef_kg_animal_year, population, emissions_total
    ) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))

  # Save Output to CSV
  if (isTRUE(saveoutput) && nrow(results) > 0) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/enteric_emissions.csv")
    message("\U0001f4be Saved output to output/enteric_emissions.csv")
  }

  return(results)
}
