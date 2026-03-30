#' Calculate land use
#'
#' Computes total land use (m2) per animal based on validated DMI.
#' @param automatic_cycle Logical. If TRUE, uses the built-in model for automatic farm cycle calculation. Default is FALSE.
#' @param crop_yield_country Character. FAO Area to use for crop yields.
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @export
calculate_land_use <- function(automatic_cycle = FALSE,
                               crop_yield_country,
                               saveoutput = TRUE) {

  message("\U0001f4be Calculating land use based on validated DMI and Combined Yields...")

  # --- 1. Load Reference Data ---
  fao_raw      <- readr::read_csv("user_data/fao_crop_yields.csv", show_col_types = FALSE)
  forage_raw   <- readr::read_csv("user_data/forage_yields.csv", show_col_types = FALSE)
  name_mapping <- readr::read_csv("user_data/mapping.csv", show_col_types = FALSE)

  # Validation for country selection
  if (missing(crop_yield_country) || !crop_yield_country %in% unique(fao_raw$Area)) {
    stop("U+274C 'crop_yield_country' must be a valid area in the dataset.")
  }

  # --- 2. Yield Processing ---
  yields_combined <- dplyr::bind_rows(
    fao_raw %>% dplyr::select(Area, Item, Year, Value),
    forage_raw %>% dplyr::select(Area, Item = ingredient, Year, Value)
  )

  fao_yields <- name_mapping %>%
    dplyr::full_join(yields_combined, by = c("yield_name" = "Item")) %>%
    tidyr::complete(
      tidyr::nesting(ingredient, yield_name, agribalyse_name, allocation),
      Area, Year
    ) %>%
    dplyr::filter(!is.na(ingredient) | !is.na(Value)) %>%
    dplyr::group_by(Year, yield_name) %>%
    dplyr::mutate(
      avg = mean(Value, na.rm = TRUE),
      dm_yield = dplyr::coalesce(Value, ifelse(is.nan(avg), NA, avg))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(Area == crop_yield_country) %>%
    dplyr::group_by(ingredient) %>%
    dplyr::filter(Year == max(Year, na.rm = TRUE)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      ingredient,
      dm_yield,
      ha_per_kg = dplyr::if_else(dm_yield > 0, 1 / dm_yield, 0),
      economic_allocation = as.numeric(allocation),
      yield_name,
      agribalyse_name
    )

  # --- 3. Operational Data Loading (DMI, Diets, Population) ---
  DMI_df <- calculate_DMI(saveoutput = FALSE) %>%
    dplyr::distinct(region, subregion, animal_tag, class_flex, .keep_all = TRUE)

  diet_profiles <- readr::read_csv("user_data/diet_profiles.csv", show_col_types = FALSE) %>%
    dplyr::distinct(diet_tag, region, subregion, class_flex, .keep_all = TRUE)

  diet_ingredients <- readr::read_csv("user_data/diet_ingredients.csv", show_col_types = FALSE) %>%
    dplyr::distinct(diet_tag, region, subregion, class_flex, ingredient, .keep_all = TRUE)

  population_df <- calculate_population(automatic_cycle = FALSE, saveoutput = FALSE) %>%
    dplyr::distinct(animal_tag, region, subregion, class_flex, .keep_all = TRUE)

  # --- 4. Impact Calculation ---
  results <- DMI_df %>%
    dplyr::left_join(diet_profiles, by = c("region", "subregion", "class_flex")) %>%
    dplyr::inner_join(diet_ingredients, by = c("diet_tag", "region", "subregion", "class_flex")) %>%
    dplyr::left_join(fao_yields, by = "ingredient") %>%
    dplyr::mutate(

      ha_kg_allocated = ha_per_kg * dplyr::coalesce(economic_allocation, 1),

      share_factor = dplyr::case_when(
        ingredient_type == "forage"        ~ forage_share / 100,
        ingredient_type == "concentrate"   ~ concentrate_share / 100,
        ingredient_type == "milk"          ~ milk_share / 100,
        ingredient_type == "milk_replacer" ~ milk_replacer_share / 100,
        TRUE                               ~ 0
      ),

      annual_cons_kg = (DMI_kgday * 365) * share_factor * (as.numeric(ingredient_share) / 100),
      land_use_m2 = (ha_kg_allocated * annual_cons_kg) * 10000,
      land_use_m2 = dplyr::coalesce(land_use_m2, 0)
    ) %>%
    dplyr::left_join(
      population_df %>% dplyr::select(region, subregion, animal_tag, class_flex, population),
      by = c("region", "subregion", "animal_tag", "class_flex")
    ) %>%
    tidyr::drop_na(animal_tag) %>%
    dplyr::group_by(region, subregion, animal_tag, class_flex, animal_type, animal_subtype) %>%
    dplyr::summarise(
      population             = dplyr::first(population),
      validated_DMI_kg       = dplyr::first(DMI_kgday),
      total_consumption_kg   = sum(annual_cons_kg, na.rm = TRUE),
      land_use_per_animal_m2 = sum(land_use_m2, na.rm = TRUE),
      total_land_use_m2      = sum(land_use_m2 * population, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3)))

  # --- 5. Save Results ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/land_use_optimized.csv")
    message("\U0001f4be Land use report saved to output/land_use_optimized.csv")
  }

  return(results)
}
