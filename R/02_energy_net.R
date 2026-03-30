#' Calculate Net Energy for Maintenance (NEm)
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @export
calculate_NEm <- function(saveoutput = TRUE) {
  message("\U0001f7e2 Calculating Net Energy for Maintenance (NEm)...")

  # --- 1. Data Loading from user_data ---
  weights <- readr::read_csv("user_data/livestock_weights.csv", show_col_types = FALSE)
  categories <- readr::read_csv("user_data/livestock_definitions.csv", show_col_types = FALSE)
  coefficients <- readr::read_csv("user_data/ipcc_coefficients.csv", show_col_types = FALSE)

  # --- 2. Calculation Pipeline ---
  results <- weights %>%
    dplyr::left_join(
      categories %>%
        dplyr::select(animal_tag, region, subregion, class_flex, animal_type, animal_subtype, cfi_tag = cfi),
      by = c("animal_tag", "region", "subregion", "class_flex")
    ) %>%
    dplyr::left_join(
      coefficients %>%
        dplyr::filter(tolower(coefficient) == "cfi") %>%
        dplyr::select(cfi_tag = description, cfi_value = value),
      by = "cfi_tag"
    ) %>%
    dplyr::mutate(
      across(c(initial_weight_kg, final_weight_kg, cfi_value), ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)),
      NEm_MJday = cfi_value * (((initial_weight_kg + final_weight_kg)/2) ^ 0.75)
    ) %>%
    dplyr::select(region, subregion, animal_tag, class_flex, animal_type, animal_subtype, NEm_MJday) %>%
    dplyr::mutate(NEm_MJday = round(NEm_MJday, 3))

  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/NEm_result.csv")
  }
  return(results)
}

#' Calculate Net Energy for Activity (NEa)
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @export
calculate_NEa <- function(saveoutput = TRUE) {
  message("\U0001f7e2 Calculating Net Energy for Activity (NEa)...")

  # --- 1. Data Loading from user_data ---
  categories <- readr::read_csv("user_data/livestock_definitions.csv", show_col_types = FALSE)
  coefficients <- readr::read_csv("user_data/ipcc_coefficients.csv", show_col_types = FALSE)

  # Fetch NEm as base (contains geography and base energy)
  nem_df <- calculate_NEm(saveoutput = FALSE)

  # Prepare Ca coefficients table
  ca_table <- coefficients %>%
    dplyr::filter(tolower(coefficient) == "ca") %>%
    dplyr::select(ca_tag = description, ca_value = value)

  # --- 2. Calculation Pipeline ---
  results <- nem_df %>%
    # Join with categories to get the activity tag (ca)
    dplyr::left_join(
      categories %>%
        dplyr::select(animal_tag, region, subregion, class_flex, animal_type, animal_subtype, ca_tag = ca),
      by = c("animal_tag", "region", "subregion", "class_flex", "animal_type", "animal_subtype")
    ) %>%
    # Join with IPCC coefficient value
    dplyr::left_join(ca_table, by = "ca_tag") %>%
    # Calculate NEa: Factor * NEm
    dplyr::mutate(
      across(c(ca_value, NEm_MJday), ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)),
      NEa_MJday = ca_value * NEm_MJday
    ) %>%
    # Final selection of columns
    dplyr::select(region, subregion, animal_tag, class_flex, animal_type, animal_subtype, NEa_MJday) %>%
    dplyr::mutate(NEa_MJday = round(NEa_MJday, 3))

  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/NEa_result.csv")
  }
  return(results)
}

#' Calculate Net Energy for Growth (NEg)
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @export
calculate_NEg <- function(saveoutput = TRUE) {
  message("\U0001f7e2 Calculating Net Energy for Growth (NEg)...")

  # --- 1. Data Loading (Robust reading) ---
  # Forzamos que las columnas de unión sean texto ('c') para evitar errores de tipo logical
  weights <- readr::read_csv("user_data/livestock_weights.csv",
                             col_types = readr::cols(subregion = "c", class_flex = "c"),
                             show_col_types = FALSE)

  categories <- readr::read_csv("user_data/livestock_definitions.csv",
                                col_types = readr::cols(subregion = "c", class_flex = "c", c = "c", a = "c", b = "c"),
                                show_col_types = FALSE)

  coefficients <- readr::read_csv("user_data/ipcc_coefficients.csv",
                                  col_types = readr::cols(description = "c"),
                                  show_col_types = FALSE)

  coeff_lookup <- coefficients %>% dplyr::select(description, value) %>% dplyr::distinct()

  # --- 2. Join Pipeline ---
  results <- weights %>%
    dplyr::left_join(
      categories %>% dplyr::select(animal_tag, region, subregion, class_flex, animal_type, animal_subtype, c, a, b),
      by = c("animal_tag", "region", "subregion", "class_flex")
    ) %>%
    dplyr::left_join(coeff_lookup, by = c("c" = "description")) %>% dplyr::rename(C_val = value) %>%
    dplyr::left_join(coeff_lookup, by = c("a" = "description")) %>% dplyr::rename(A_val = value) %>%
    dplyr::left_join(coeff_lookup, by = c("b" = "description")) %>% dplyr::rename(B_val = value) %>%

    # --- 3. Calculations ---
    dplyr::mutate(
      dplyr::across(c(initial_weight_kg, final_weight_kg, adult_weight_kg, productive_period_days, C_val, A_val, B_val),
                    ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)),

      # Añadimos comprobación de denominadores > 0 para evitar NaN
      NEg_MJday = dplyr::case_when(
        tolower(animal_type) == "cattle" & (C_val * adult_weight_kg) > 0 & productive_period_days > 0 ~
          22.02 * ((((initial_weight_kg + final_weight_kg)/2) / (C_val * adult_weight_kg))^0.75) * (((final_weight_kg - initial_weight_kg)/productive_period_days)^1.097),

        tolower(animal_type) %in% c("sheep", "goat") & productive_period_days > 0 ~
          ((final_weight_kg - initial_weight_kg)/productive_period_days) * (A_val + 0.5 * B_val * (initial_weight_kg + final_weight_kg)),

        TRUE ~ 0 # Si falta algún dato crítico, la energía es 0, no NA
      )
    ) %>%

    # --- 4. Final Selection ---
    dplyr::select(region, subregion, animal_tag, class_flex, animal_type, animal_subtype, NEg_MJday) %>%
    dplyr::mutate(NEg_MJday = round(NEg_MJday, 3))

  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/NEg_result.csv")
    message("\U0001f4be Results saved to output/NEg_result.csv")
  }

  return(results)
}

#' Calculate Net Energy for Lactation (NEl)
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @export
calculate_NEl <- function(saveoutput = TRUE) {
  message("\U0001f7e2 Calculating Net Energy for Lactation (NEl)...")

  # --- 1. Data Loading from user_data ---
  categories <- readr::read_csv("user_data/livestock_definitions.csv", show_col_types = FALSE)
  weights <- readr::read_csv("user_data/livestock_weights.csv", show_col_types = FALSE) %>%
    dplyr::select(region, subregion, animal_tag, class_flex) %>% dplyr::distinct()

  # --- 2. Calculation Pipeline ---
  results <- categories %>%
    dplyr::inner_join(weights, by = c("region", "subregion", "animal_tag", "class_flex")) %>%
    dplyr::mutate(
      across(c(milk_yield_kg_year, fat_content_pct), ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)),
      Milk_yield_kg_day = milk_yield_kg_year / 365,
      NEl_MJday = dplyr::case_when(
        tolower(animal_type) == "cattle" ~ Milk_yield_kg_day * (1.47 + 0.4 * fat_content_pct),
        tolower(animal_type) == "sheep" ~ Milk_yield_kg_day * 4.6,
        tolower(animal_type) == "goat" ~ Milk_yield_kg_day * 3,
        TRUE ~ 0
      )
    ) %>%

    # --- 3. Final Selection ---
    dplyr::select(region, subregion, animal_tag, class_flex, animal_type, animal_subtype, NEl_MJday) %>%
    dplyr::mutate(NEl = round(NEl_MJday, 3))

  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/NEl_result.csv")
  }
  return(results)
}

#' Calculate Net Energy for Work (NE_work)
#'
#' Computes NE_work based on NEm and hours of activity.
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @export
calculate_NE_work <- function(saveoutput = TRUE) {

  message("\U0001f7e2 Calculating Net Energy for Work (NE_work)...")

  # --- 1. Data Loading from user_data ---
  categories <- readr::read_csv("user_data/livestock_definitions.csv", show_col_types = FALSE)

  nem_df <- calculate_NEm(saveoutput = FALSE)

  # --- 2. Calculation Pipeline ---
  results <- nem_df %>%

    dplyr::left_join(
      categories %>%
        dplyr::select(animal_tag, region, subregion, class_flex, work_hours),
      by = c("animal_tag", "region", "subregion", "class_flex")
    ) %>%

    dplyr::mutate(

      across(c(work_hours, NEm_MJday), ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)),

      NEwork_MJday = work_hours * NEm_MJday
    ) %>%

    # --- 3. Final Selection and Cleanup ---
    dplyr::select(
      region, subregion, animal_tag, class_flex,
      animal_type, animal_subtype, work_hours, NEm_MJday, NEwork_MJday
    ) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))

  # --- 4. Save Output ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/NE_work_result.csv")
    message("\U1F4BE Saved output to output/NE_work_result.csv")
  }

  return(results)
}

#' Calculate Net Energy for Wool (NE_wool)
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @export
calculate_NE_wool <- function(saveoutput = TRUE) {
  message("\U0001f7e2 Calculating Net Energy for Wool (NE_wool)...")

  # --- 1. Data Loading from user_data ---
  categories <- readr::read_csv("user_data/livestock_definitions.csv", show_col_types = FALSE)
  weights <- readr::read_csv("user_data/livestock_weights.csv", show_col_types = FALSE) %>%
    dplyr::select(region, subregion, animal_tag, class_flex) %>% dplyr::distinct()

  # --- 2. Calculation Pipeline ---
  results <- categories %>%
    dplyr::inner_join(weights, by = c("region", "subregion", "animal_tag", "class_flex")) %>%
    dplyr::mutate(
      wool_yield_kg_year = tidyr::replace_na(suppressWarnings(as.numeric(wool_yield_kg_year)), 0),
      NEwool_MJday = (wool_yield_kg_year * 24) / 365
    ) %>%

    # --- 3. Final Selection ---
    dplyr::select(region, subregion, animal_tag, class_flex, animal_type, animal_subtype, NEwool_MJday) %>%
    dplyr::mutate(NEwool_MJday = round(NEwool_MJday, 3))

  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/NE_wool_result.csv")
  }
  return(results)
}

#' Calculate Net Energy for Pregnancy (NE_pregnancy)
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @export
calculate_NE_pregnancy <- function(saveoutput = TRUE) {
  message("\U0001f7e2 Calculating Net Energy for Pregnancy (NE_pregnancy)...")

  # --- 1. Data Loading from user_data ---
  categories <- readr::read_csv("user_data/livestock_definitions.csv", show_col_types = FALSE)
  coefficients <- readr::read_csv("user_data/ipcc_coefficients.csv", show_col_types = FALSE)
  nem_df <- calculate_NEm(saveoutput = FALSE)

  coeff_lookup <- coefficients %>%
    dplyr::filter(tolower(coefficient) == "c_pregnancy") %>%
    dplyr::select(c_pregnancy_tag = description, c_value = value)

  # --- 2. Calculation Pipeline ---
  results <- nem_df %>%
    dplyr::left_join(
      categories %>% dplyr::select(animal_tag, region, subregion, class_flex, animal_type, animal_subtype, c_pregnancy, pr),
      by = c("animal_tag", "region", "subregion", "class_flex", "animal_type", "animal_subtype")
    ) %>%
    dplyr::left_join(coeff_lookup, by = c("c_pregnancy" = "c_pregnancy_tag")) %>%
    dplyr::mutate(
      across(c(pr, c_value, NEm_MJday), ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)),
      C_preg_factor = dplyr::case_when(
        tolower(animal_type) == "cattle" ~ c_value,
        tolower(animal_type) %in% c("sheep", "goat") ~ (0.126 * pmax(pr - 1, 0)) + (0.077 * (1 - pmax(pr - 1, 0))),
        TRUE ~ 0
      ),
      NEpregnancy_MJday = C_preg_factor * NEm_MJday
    ) %>%

    # --- 3. Final Selection ---
    dplyr::select(region, subregion, animal_tag, class_flex, animal_type, animal_subtype, NEpregnancy_MJday) %>%
    dplyr::mutate(NEpregnancy_MJday = round(NEpregnancy_MJday, 3))

  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/NE_pregnancy_result.csv")
  }
  return(results)
}
