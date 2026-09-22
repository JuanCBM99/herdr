#' Calculate Total Livestock Production
#'
#' Computes total production of milk, meat, eggs, and fibre in physical units
#' (fresh milk, FPCM, live weight, carcass weight) and nutritional units
#' (kg of edible protein and fibre) based on FAO/GLEAM (Equations 9.1 to 9.5)
#' and IDF standards.
#'
#' @param automatic_cycle Logical. If TRUE, uses the built-in model for automatic farm cycle calculation. Default is FALSE.
#' @param saveoutput Logical. If TRUE (default), results are saved to output folder.
#' @param data_dir Path to the directory containing input CSV/data files. Defaults to `"user_data"`.
#' @return Tibble with animal production summary.
#' @export
calculate_production <- function(automatic_cycle = FALSE, saveoutput = TRUE, data_dir = "user_data") {

  message("\U0001f9c3 Calculating total livestock production (physical products & protein)...")

  join_keys <- c("region", "subregion", "animal_tag", "class_flex")

  # --- 1. Load Data Assets ---

  # FIX: Select only necessary columns from population to prevent .x and .y suffixes later
  pop_df   <- suppressMessages(calculate_population(automatic_cycle = automatic_cycle, saveoutput = FALSE, data_dir = data_dir)) %>%
    dplyr::select(dplyr::all_of(join_keys), population) %>%
    dplyr::distinct()

  weights  <- readr::read_csv(file.path(data_dir, "livestock_weights.csv"), show_col_types = FALSE)
  repro    <- readr::read_csv(file.path(data_dir, "reproduction_parameters.csv"), show_col_types = FALSE)
  ruminants <- readr::read_csv(file.path(data_dir, "ruminant_definitions.csv"), show_col_types = FALSE)
  monogastrics <- readr::read_csv(file.path(data_dir, "monogastric_definitions.csv"), show_col_types = FALSE)

  # Consolidate Definitions
  if (!"production_role" %in% names(ruminants)) {
    ruminants$production_role <- dplyr::case_when(
      grepl("mature", ruminants$animal_tag, ignore.case = TRUE) ~ "mature",
      grepl("replacement", ruminants$animal_tag, ignore.case = TRUE) ~ "replacement",
      TRUE ~ "slaughter"
    )
  }

  ruminants_clean <- ruminants %>%
    dplyr::select(dplyr::all_of(join_keys), animal_type, animal_subtype, milk_yield_kg_year, fat_content_pct, wool_yield_kg_year, production_role) %>%
    dplyr::mutate(
      dplyr::across(c(milk_yield_kg_year, fat_content_pct, wool_yield_kg_year), ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)),
      egg_mass_g_day = 0
    )

  if (nrow(monogastrics) > 0) {
    if (!"production_role" %in% names(monogastrics)) {
      monogastrics$production_role <- dplyr::case_when(
        grepl("mature|layer|sow", monogastrics$animal_tag, ignore.case = TRUE) ~ "mature",
        grepl("replacement", monogastrics$animal_tag, ignore.case = TRUE) ~ "replacement",
        TRUE ~ "slaughter"
      )
    }

    # Harmonize egg production inputs: eggs_per_year (intuitive) -> egg_mass_g_day (bioenergetics)
    if (!"egg_mass_g_day" %in% names(monogastrics)) {
      monogastrics$egg_mass_g_day <- 0
    }
    if ("eggs_per_year" %in% names(monogastrics)) {
      egg_wt <- if ("egg_weight_g" %in% names(monogastrics)) suppressWarnings(as.numeric(monogastrics$egg_weight_g)) else 60
      egg_wt <- dplyr::coalesce(egg_wt, 60)
      eggs_yr <- suppressWarnings(as.numeric(monogastrics$eggs_per_year))
      monogastrics$egg_mass_g_day <- dplyr::if_else(
        !is.na(eggs_yr) & eggs_yr > 0,
        (eggs_yr / 365) * egg_wt,
        suppressWarnings(as.numeric(monogastrics$egg_mass_g_day))
      )
    }

    monogastrics_clean <- monogastrics %>%
      dplyr::select(dplyr::all_of(join_keys), animal_type, animal_subtype, egg_mass_g_day, production_role) %>%
      dplyr::mutate(
        egg_mass_g_day = tidyr::replace_na(suppressWarnings(as.numeric(egg_mass_g_day)), 0),
        milk_yield_kg_year = 0,
        fat_content_pct = 0,
        wool_yield_kg_year = 0
      )
  } else {
    monogastrics_clean <- tibble::tibble()
  }

  # FIX: Simplified distinct syntax to avoid across() bugs in newer dplyr versions
  animal_defs <- dplyr::bind_rows(ruminants_clean, monogastrics_clean) %>%
    dplyr::distinct(region, subregion, animal_tag, class_flex, .keep_all = TRUE)

  # Replacement rate lookup
  repro_repl <- repro %>%
    dplyr::filter(parameter == "replacement_rate") %>%
    dplyr::select(animal_tag, replacement_rate = value) %>%
    dplyr::mutate(replacement_rate = suppressWarnings(as.numeric(replacement_rate))) %>%
    dplyr::distinct(animal_tag, .keep_all = TRUE)

  # --- 2. FAO/GLEAM Technical Coefficients (Table 9.1) ---
  production_constants <- tibble::tribble(
    ~animal_type, ~BFM,  ~MEAT_prot, ~DP_pct, ~MILK_prot_def,
    "cattle",     0.75,  0.2113,     54.0,    0.033,
    "sheep",      0.70,  0.2013,     47.0,    0.058,
    "goat",       0.70,  0.1920,     47.0,    0.034,
    "swine",      0.65,  0.2020,     73.0,    0.000,
    "poultry",    0.75,  0.1900,     70.0,    0.000
  )

  egg_prot_fraction <- 0.1240

  # --- 3. Pipeline Calculation ---
  results <- pop_df %>%
    dplyr::left_join(animal_defs, by = join_keys) %>%
    dplyr::left_join(weights, by = join_keys) %>%
    dplyr::left_join(repro_repl, by = "animal_tag") %>%
    dplyr::left_join(production_constants, by = "animal_type") %>%
    dplyr::mutate(
      dplyr::across(
        c(population, milk_yield_kg_year, fat_content_pct, wool_yield_kg_year,
          egg_mass_g_day, replacement_rate, productive_period_days, adult_weight_kg,
          final_weight_kg, BFM, MEAT_prot, DP_pct, MILK_prot_def),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),

      # Peso vivo al sacrificio (LW)
      slaughter_weight_kg = dplyr::case_when(
        final_weight_kg > 0 ~ final_weight_kg,
        adult_weight_kg > 0 ~ adult_weight_kg,
        TRUE ~ 0
      ),

      # Número anual de animales al sacrificio (N_exit)
      N_exit = dplyr::case_when(
        # 1. Animales de recría/reposición: NO van a matadero comercial
        production_role == "replacement" ~ 0,

        # 2. Adultos con tasa de reposición en CSV (vacuno, ovino, cerdas)
        production_role == "mature" & replacement_rate > 0 ~ population * replacement_rate,

        # 3. Adultos sin tasa en CSV (gallinas ponedoras/reproductoras): desvieje por ciclo de puesta
        production_role == "mature" & productive_period_days > 0 ~ population * (365 / productive_period_days),

        # 4. Animales de cebo/engorde/sacrificio comercial (broilers, cerdos de cebo, terneros, corderos)
        production_role == "slaughter" ~ population * (365 / dplyr::if_else(productive_period_days > 0, productive_period_days, 365)),

        TRUE ~ 0
      ),

      # --- A) PRODUCCIÓN DE LECHE ---
      milk_fresh_kg = population * milk_yield_kg_year,
      # Proteína real calculada según IPCC (o default de GLEAM si no hay grasa)
      milk_prot_pct = dplyr::if_else(fat_content_pct > 0, (1.9 + 0.4 * fat_content_pct) / 100, MILK_prot_def),
      milk_protein_kg = milk_fresh_kg * milk_prot_pct,
      # FPCM (Fat and Protein Corrected Milk - IDF)
      milk_FPCM_kg = dplyr::if_else(
        milk_fresh_kg > 0 & fat_content_pct > 0,
        milk_fresh_kg * (0.1226 * fat_content_pct + 0.0776 * (milk_prot_pct * 100) + 0.2534),
        milk_fresh_kg
      ),

      # --- B) PRODUCCIÓN DE CARNE ---
      meat_live_weight_kg = N_exit * slaughter_weight_kg,
      meat_carcass_weight_kg = meat_live_weight_kg * (DP_pct / 100),
      meat_boneless_kg = meat_carcass_weight_kg * BFM,
      meat_protein_kg = meat_boneless_kg * MEAT_prot,

      # --- C) PRODUCCIÓN DE HUEVOS ---
      egg_fresh_kg = dplyr::if_else(egg_mass_g_day > 0, (egg_mass_g_day * 365 / 1000) * population, 0),
      egg_protein_kg = egg_fresh_kg * egg_prot_fraction,

      # --- D) FIBRA / LANA ---
      wool_kg = population * wool_yield_kg_year,

      # --- E) PROTEÍNA TOTAL COMESTIBLE ---
      total_protein_kg = milk_protein_kg + meat_protein_kg + egg_protein_kg
    ) %>%
    dplyr::select(
      dplyr::all_of(join_keys), animal_type, animal_subtype, population, N_exit,
      # Productos comerciales
      milk_fresh_kg, milk_FPCM_kg, meat_live_weight_kg, meat_carcass_weight_kg, egg_fresh_kg, wool_kg,
      # Proteína comestible (GLEAM)
      milk_protein_kg, meat_protein_kg, egg_protein_kg, total_protein_kg
    ) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2)))

  # --- 4. Guardar archivo ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/production.csv")
    message("\U0001f4be Production report saved to output/production.csv")
  }

  return(results)
}
