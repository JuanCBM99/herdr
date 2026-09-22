#' Calculate land use
#'
#' Computes total land use (m2) per animal based on validated DMI and specific origin countries.
#' @param automatic_cycle Logical. If TRUE, uses the built-in model for automatic farm cycle calculation. Default is FALSE.
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @param farm_country Character. The country of the farm/study (e.g., "Spain"). Default is "Spain".
#' @param year Numeric. The reference year for FAO trade data calculation if origins are missing. Default is 2022.
#' @param max_trace_hops Numeric. When an ingredient's country of origin has to be inferred from
#'   trade data, this caps how many countries the algorithm will follow through re-export hubs. Default is 4.
#' @param ssr_threshold Numeric. Self-Sufficiency Ratio (Production / Apparent Consumption)
#'   above which a country is considered a genuine producer. Default is 0.70.
#' @param data_dir Path to the directory containing input CSV/data files. Defaults to `"user_data"`.
#' @export
calculate_land_use <- function(automatic_cycle = FALSE,
                               saveoutput = TRUE,
                               farm_country = "Spain",
                               year = 2024,
                               max_trace_hops = 4,
                               ssr_threshold = 0.70,
                               data_dir = "user_data") {

  message("\U0001f7e2 Calculating land use...")

  year_col <- paste0("Y", year)

  # FAOSTAT uses Area Codes >= 5000 for regional and other aggregate areas.
  country_area_code_max <- 5000

  # --- 1. Load reference data ---
  fao_ds <- arrow::open_dataset(file.path(data_dir, "fao_crops.parquet"))
  fao_filtered <- dplyr::filter(fao_ds, Element == "Yield")
  if ("Area Code" %in% names(fao_ds)) {
    fao_filtered <- dplyr::filter(fao_filtered, `Area Code` < country_area_code_max)
  }
  fao_raw <- fao_filtered %>%
    dplyr::select(Area, Item, dplyr::all_of(year_col)) %>%
    dplyr::collect() %>%
    dplyr::rename(Value = dplyr::all_of(year_col)) %>%
    dplyr::mutate(Year = as.numeric(year))

  # Assuming forages might use the same code structure; if not, you can remove the filter here.
  forage_raw <- arrow::read_parquet(file.path(data_dir, "fao_forages.parquet")) %>%
    dplyr::rename(Value = Yield) %>%
    dplyr::mutate(Year = as.numeric(year))

  name_mapping <- readr::read_csv(file.path(data_dir, "mapping.csv"), show_col_types = FALSE)

  feed_chars_raw <- readr::read_csv(file.path(data_dir, "feed_characteristics.csv"), show_col_types = FALSE)

  if (!"land_type" %in% colnames(feed_chars_raw)) {
    warning("\u26A0 Column 'land_type' not found in feed_characteristics.csv. Setting as NA.")
    feed_chars_raw$land_type <- NA_character_
  }

  if (!"DM_pct" %in% colnames(feed_chars_raw)) {
    warning("\u26A0 Column 'DM_pct' not found in feed_characteristics.csv. Assuming 100%.")
    feed_chars_raw$DM_pct <- 100
  }

  feed_chars <- feed_chars_raw %>%
    dplyr::filter(!is.na(ingredient)) %>%
    dplyr::mutate(DM_pct = suppressWarnings(as.numeric(DM_pct))) %>%
    dplyr::select(ingredient, land_type, DM_pct) %>%
    dplyr::distinct(ingredient, .keep_all = TRUE)

  # --- 2. Process yields by country ---
  yields_combined <- dplyr::bind_rows(
    fao_raw %>% dplyr::select(Area, Item, Value),
    forage_raw %>% dplyr::select(Area, Item, Value)
  ) %>%
    dplyr::filter(!is.na(Value), !is.na(Area)) %>%
    dplyr::distinct(Area, Item, .keep_all = TRUE)

  fao_yields <- name_mapping %>%
    dplyr::filter(!is.na(yield_name)) %>%
    dplyr::inner_join(yields_combined, by = c("yield_name" = "Item"), relationship = "many-to-many") %>%
    dplyr::transmute(
      ingredient,
      country_of_origin = Area,
      dm_yield = Value,
      ha_per_kg = dplyr::if_else(dm_yield > 0, 1 / dm_yield, 0),
      economic_allocation = as.numeric(economic_allocation)
    )

  # Spain national forage yields lookup as fallback proxy for other countries
  spain_forage_yields <- name_mapping %>%
    dplyr::filter(!is.na(yield_name)) %>%
    dplyr::inner_join(
      forage_raw %>% dplyr::filter(Area == "Spain") %>% dplyr::select(Item, Value),
      by = c("yield_name" = "Item"),
      relationship = "many-to-many"
    ) %>%
    dplyr::transmute(
      ingredient,
      spain_forage_yield = Value
    ) %>%
    dplyr::distinct(ingredient, .keep_all = TRUE)

  # --- 3. Load operational data & Handle Hybrid Country Origins ---
  DMI_df <- suppressMessages(calculate_DMI(saveoutput = FALSE, data_dir = data_dir)) %>%
    dplyr::distinct(region, diet_tag, subregion, animal_tag, class_flex, .keep_all = TRUE)

  diet_profiles <- readr::read_csv(file.path(data_dir, "diet_profiles.csv"), show_col_types = FALSE)
  diet_ingredients_raw <- readr::read_csv(
    file.path(data_dir, "diet_ingredients.csv"),
    col_types = readr::cols(
      custom_yield_kg_ha = readr::col_character(),
      .default = readr::col_guess()
    ),
    show_col_types = FALSE
  )

  legacy_diet_mode <- any(c("region", "subregion", "class_flex") %in% names(diet_profiles)) &&
                      any(duplicated(diet_profiles$diet_tag))

  if (!legacy_diet_mode) {
    diet_profiles <- diet_profiles %>%
      dplyr::select(-dplyr::any_of(c("region", "subregion", "class_flex"))) %>%
      dplyr::distinct(diet_tag, .keep_all = TRUE)
    diet_ingredients_raw <- diet_ingredients_raw %>%
      dplyr::select(-dplyr::any_of(c("region", "subregion", "class_flex")))
    diet_join_keys <- "diet_tag"
  } else {
    diet_profiles <- diet_profiles %>%
      dplyr::distinct(diet_tag, region, subregion, class_flex, .keep_all = TRUE)
    diet_join_keys <- intersect(c("region", "subregion", "class_flex", "diet_tag"), names(diet_profiles))
  }

  diet_ingredients_raw <- diet_ingredients_raw %>%
    dplyr::mutate(
      custom_yield_kg_ha = as.numeric(gsub(",", ".", custom_yield_kg_ha)),
      country_of_origin = dplyr::if_else(!is.na(custom_yield_kg_ha), "Custom Data", country_of_origin)
    )

  if (any(is.na(diet_ingredients_raw$country_of_origin))) {

    path_parquet_trade <- file.path(data_dir, "fao_trade_matrix.parquet")

    # nocov start
    if (!file.exists(path_parquet_trade)) {
      message("\u23f3 FAO trade matrix not found locally.")
      message("Downloading background database (187 MB)... This will only happen once.")
      url_release <- "https://github.com/JuanCBM99/herdr/releases/latest/download/fao_trade_matrix.parquet"

      tryCatch({
        download.file(url_release, destfile = path_parquet_trade, mode = "wb")
        message("\u2705 Download completed successfully.")
      }, error = function(e) {
        stop("Error downloading the trade matrix. Please check your internet connection: ", e$message)
      })
    }
    # nocov end

    message(paste0("\u23f3 Missing countries of origin found. Tracing real origin (beyond re-export hubs) for ", farm_country, " (", year, ")..."))

    fao_items <- unique(stats::na.omit(name_mapping$yield_name))

    # --- 3a. Load global production & trade tables ONCE ---
    global_prod_ds <- arrow::open_dataset(file.path(data_dir, "fao_crops.parquet"))
    global_prod_filtered <- dplyr::filter(
      global_prod_ds,
      Item %in% fao_items,
      Element == "Production"
    )
    if ("Area Code" %in% names(global_prod_ds)) {
      global_prod_filtered <- dplyr::filter(global_prod_filtered, `Area Code` < country_area_code_max)
    }
    global_prod <- global_prod_filtered %>%
      dplyr::select(Area, Item, dplyr::all_of(year_col)) %>%
      dplyr::collect() %>%
      dplyr::rename(Production = dplyr::all_of(year_col)) %>%
      dplyr::group_by(Area, Item) %>%
      dplyr::summarise(Production = sum(Production, na.rm = TRUE), .groups = "drop")

    global_trade <- arrow::open_dataset(path_parquet_trade) %>%
      dplyr::select(`Reporter Countries`, `Partner Countries`, Item, Element, dplyr::all_of(year_col)) %>%
      dplyr::filter(Item %in% fao_items) %>%
      dplyr::collect() %>%
      dplyr::rename(Value = dplyr::all_of(year_col)) %>%
      dplyr::filter(!is.na(Value))

    global_exp <- global_trade %>%
      dplyr::filter(Element == "Export quantity") %>%
      dplyr::group_by(`Reporter Countries`, Item) %>%
      dplyr::summarise(Total_Export = sum(Value, na.rm = TRUE), .groups = "drop")

    global_imp <- global_trade %>%
      dplyr::filter(Element == "Import quantity") %>%
      dplyr::group_by(`Reporter Countries`, Item) %>%
      dplyr::mutate(Total_Import = sum(Value, na.rm = TRUE)) %>%
      dplyr::arrange(dplyr::desc(Value)) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::select(`Reporter Countries`, Item, Top_Partner = `Partner Countries`, Total_Import)

    chain_table <- global_prod %>%
      dplyr::rename(`Reporter Countries` = Area) %>%
      dplyr::full_join(global_imp, by = c("Reporter Countries", "Item")) %>%
      dplyr::full_join(global_exp, by = c("Reporter Countries", "Item")) %>%
      dplyr::mutate(dplyr::across(c(Production, Total_Import, Total_Export), ~ tidyr::replace_na(., 0))) %>%
      dplyr::mutate(
        Apparent_Consumption   = Production + Total_Import - Total_Export,
        Apparent_Consumption   = ifelse(Apparent_Consumption <= 0, 1, Apparent_Consumption),
        Self_Sufficiency_Ratio = Production / Apparent_Consumption
      )

    # --- 3b. Bounded recursive traceback ---
    trace_origin <- function(item, start_country, chain_table,
                             max_hops = max_trace_hops, threshold = ssr_threshold) {

      current <- start_country
      visited <- character(0)

      for (hop in seq_len(max_hops)) {
        visited <- c(visited, current)

        row <- chain_table[chain_table$`Reporter Countries` == current & chain_table$Item == item, ]

        if (nrow(row) == 0 || is.na(row$Self_Sufficiency_Ratio[1])) {
          return(NA_character_)
        }
        if (row$Self_Sufficiency_Ratio[1] >= threshold) {
          return(current)
        }

        next_country <- row$Top_Partner[1]

        if (is.na(next_country) || next_country %in% visited) {
          return(NA_character_)
        }

        current <- next_country
      }

      NA_character_
    }

    resolved_origin <- purrr::map_chr(fao_items, ~ trace_origin(.x, farm_country, chain_table))
    names(resolved_origin) <- fao_items

    # --- 3c. Fallback for unresolved items ---
    fallback_yields <- purrr::map_dfr(fao_items, function(it) {
      top_producers <- global_prod %>%
        dplyr::filter(Item == it, Production > 0) %>%
        dplyr::arrange(dplyr::desc(Production)) %>%
        dplyr::slice_head(n = 3)

      if (nrow(top_producers) == 0) {
        return(tibble::tibble(Item = it, fallback_yield = NA_real_))
      }

      weighted_yield <- top_producers %>%
        dplyr::inner_join(yields_combined, by = c("Area", "Item")) %>%
        dplyr::summarise(fallback_yield = stats::weighted.mean(Value, w = Production, na.rm = TRUE)) %>%
        dplyr::pull(fallback_yield)

      if (length(weighted_yield) == 0 || is.nan(weighted_yield)) weighted_yield <- NA_real_

      tibble::tibble(Item = it, fallback_yield = weighted_yield)
    })

    fao_dictionary <- tibble::tibble(
      Item            = fao_items,
      resolved_origin = unname(resolved_origin),
      used_fallback   = is.na(resolved_origin)
    ) %>%
      dplyr::left_join(fallback_yields, by = "Item") %>%
      dplyr::mutate(
        Calculated_Origin = dplyr::case_when(
          !used_fallback                        ~ resolved_origin,
          used_fallback & !is.na(fallback_yield) ~ "Global Mix (Top 3 Producers)",
          TRUE                                   ~ farm_country
        ),
        fallback_yield = dplyr::if_else(used_fallback, fallback_yield, NA_real_)
      ) %>%
      dplyr::select(Item, Calculated_Origin, fallback_yield)

    final_dictionary <- name_mapping %>%
      dplyr::filter(!is.na(yield_name)) %>%
      dplyr::left_join(fao_dictionary, by = c("yield_name" = "Item")) %>%
      dplyr::mutate(Calculated_Origin = tidyr::replace_na(Calculated_Origin, farm_country)) %>%
      dplyr::select(ingredient, Calculated_Origin, fallback_yield)

    diet_ingredients <- diet_ingredients_raw %>%
      dplyr::left_join(final_dictionary, by = "ingredient") %>%
      dplyr::mutate(
        country_of_origin = dplyr::coalesce(country_of_origin, Calculated_Origin)
      ) %>%
      dplyr::select(-Calculated_Origin)
  } else {
    diet_ingredients <- diet_ingredients_raw %>%
      dplyr::mutate(fallback_yield = NA_real_)
  }

  population_df <- suppressMessages(calculate_population(automatic_cycle = automatic_cycle, saveoutput = FALSE, data_dir = data_dir)) %>%
    dplyr::distinct(animal_tag, region, subregion, class_flex, population)

  # --- 4. Validate ingredient shares ---
  invalid_share <- diet_ingredients %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(diet_join_keys, "ingredient_type")))) %>%
    dplyr::summarise(total = sum(as.numeric(ingredient_share), na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(abs(total - 100) > 0.1)

  if (nrow(invalid_share) > 0) {
    warning("\u26A0 Ingredient shares do not sum to 100% in: ", paste(unique(invalid_share$diet_tag), collapse = ", "))
  }

  # --- 5. Merge data and calculate impact ---
  results <- DMI_df %>%
    dplyr::inner_join(diet_profiles, by = diet_join_keys) %>%
    dplyr::inner_join(diet_ingredients, by = diet_join_keys, relationship = "many-to-many") %>%
    dplyr::left_join(fao_yields, by = c("ingredient", "country_of_origin")) %>%
    dplyr::left_join(spain_forage_yields, by = "ingredient") %>%
    dplyr::left_join(name_mapping %>% dplyr::select(ingredient, alloc_ref = economic_allocation) %>% dplyr::distinct(), by = "ingredient") %>%
    dplyr::left_join(feed_chars, by = "ingredient")

  # Detect forages where country_of_origin != "Spain" (or missing yield for country) and Spain's yield is used as fallback proxy
  forages_with_proxy <- results %>%
    dplyr::filter(
      ingredient_type == "forage",
      is.na(custom_yield_kg_ha),
      is.na(dm_yield),
      !is.na(spain_forage_yield),
      as.numeric(ingredient_share) > 0
    ) %>%
    dplyr::select(ingredient, country_of_origin) %>%
    dplyr::distinct()

  if (nrow(forages_with_proxy) > 0) {
    items_txt <- paste0(forages_with_proxy$ingredient, " (", forages_with_proxy$country_of_origin, ")", collapse = ", ")
    warning(paste0(
      "\u26A0 [herdr] Forage yields disclaimer: Currently herdr only includes established national forage yield data for Spain (MAPA). ",
      "Using Spanish yield as proxy for: ", items_txt, ". ",
      "To provide local yield data, specify 'custom_yield_kg_ha' in 'diet_ingredients.csv'."
    ), call. = FALSE)
  }

  results <- results %>%
    dplyr::mutate(
      raw_yield = dplyr::case_when(
        land_type %in% c("none", "no_land")                         ~ 0,
        !is.na(custom_yield_kg_ha)                                   ~ custom_yield_kg_ha,
        !is.na(dm_yield)                                             ~ dm_yield,
        ingredient_type == "forage" & !is.na(spain_forage_yield)     ~ spain_forage_yield,
        !is.na(fallback_yield)                                       ~ fallback_yield,
        TRUE                                                         ~ NA_real_
      ),
      dm_yield = dplyr::case_when(
        !is.na(custom_yield_kg_ha)                                   ~ raw_yield,
        ingredient_type == "forage"                                  ~ raw_yield,
        !is.na(raw_yield) & raw_yield > 0                           ~ raw_yield * (suppressWarnings(as.numeric(DM_pct)) / 100),
        TRUE                                                         ~ raw_yield
      ),
      ha_per_kg = dplyr::if_else(!is.na(dm_yield) & dm_yield > 0, 1 / dm_yield, 0)
    )

  # --- Validate that all consumed ingredients requiring land have yields ---
  missing_yields <- results %>%
    dplyr::mutate(economic_allocation = dplyr::coalesce(economic_allocation, as.numeric(alloc_ref), 1)) %>%
    dplyr::filter(
      is.na(dm_yield) | dm_yield <= 0,
      !land_type %in% c("none", "no_land"),
      as.numeric(ingredient_share) > 0,
      dplyr::coalesce(economic_allocation, 1) > 0
    ) %>%
    dplyr::select(ingredient, country_of_origin, ingredient_type) %>%
    dplyr::distinct()

  if (nrow(missing_yields) > 0) {
    forage_miss <- missing_yields %>% dplyr::filter(ingredient_type == "forage")
    other_miss  <- missing_yields %>% dplyr::filter(ingredient_type != "forage")
    warn_msgs <- c()
    if (nrow(forage_miss) > 0) {
      warn_msgs <- c(warn_msgs, paste0(
        "Forage yields are currently only available for Spain (MAPA). No yield found for: ",
        paste0(forage_miss$ingredient, " (", forage_miss$country_of_origin, ")", collapse = ", "),
        ". Land use set to 0 m2 for these forages. Please specify 'custom_yield_kg_ha' in 'diet_ingredients.csv'."
      ))
    }
    if (nrow(other_miss) > 0) {
      warn_msgs <- c(warn_msgs, paste0(
        "Missing yield for: ", paste0(other_miss$ingredient, " (", other_miss$country_of_origin, ")", collapse = ", "),
        ". Land use set to 0 m2. Please specify 'custom_yield_kg_ha' in 'diet_ingredients.csv'."
      ))
    }
    warning(paste0("\u26A0 [herdr] Land use disclaimer: ", paste(warn_msgs, collapse = " | ")), call. = FALSE)
  }

  results <- results %>%
    dplyr::mutate(
      ha_kg_allocated = dplyr::if_else(
        land_type %in% c("none", "no_land"),
        0,
        dplyr::coalesce(ha_per_kg, 0) * dplyr::coalesce(economic_allocation, 1)
      ),
      share_factor = dplyr::case_when(
        ingredient_type == "forage"        ~ forage_share / 100,
        ingredient_type == "concentrate"   ~ concentrate_share / 100,
        ingredient_type == "milk"          ~ milk_share / 100,
        ingredient_type == "milk_replacer" ~ milk_replacer_share / 100,
        TRUE ~ 0
      ),
      annual_cons_kg = (DMI_kgday * 365) * share_factor * (as.numeric(ingredient_share) / 100),
      land_use_m2 = (ha_kg_allocated * annual_cons_kg) * 10000
    ) %>%
    dplyr::left_join(population_df, by = c("region", "subregion", "animal_tag", "class_flex")) %>%
    tidyr::drop_na(animal_tag) %>%
    dplyr::group_by(
      region, subregion, animal_tag, class_flex,
      ingredient, land_type, country_of_origin,
      animal_type, animal_subtype
    ) %>%
    dplyr::summarise(
      population = dplyr::first(population),
      DM_pct = dplyr::first(DM_pct),
      dm_yield = dplyr::first(dm_yield),
      land_use_per_animal_m2 = sum(land_use_m2, na.rm = TRUE),
      total_land_use_m2 = sum(land_use_m2 * population, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 3)))

  # --- 6. Save output ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/land_use.csv")
    message("\U0001f4be Land use report saved to output/land_use.csv")
  }

  return(results)
}
