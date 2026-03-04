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
    # Join with categories using the 4-key identity structure
    dplyr::left_join(
      categories %>%
        dplyr::select(animal_tag, region, subregion, class_flex, animal_type, animal_subtype, cfi_tag = cfi),
      by = c("animal_tag", "region", "subregion", "class_flex")
    ) %>%
    # Join with IPCC coefficients for CFI
    dplyr::left_join(
      coefficients %>%
        dplyr::filter(tolower(coefficient) == "cfi") %>%
        dplyr::select(cfi_tag = description, cfi_value = value),
      by = "cfi_tag"
    ) %>%
    # Calculate NEm using IPCC formula
    dplyr::mutate(
      across(c(average_weight, cfi_value), ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)),
      NEm = cfi_value * (average_weight ^ 0.75)
    ) %>%
    # Final selection of columns
    dplyr::select(region, subregion, animal_tag, class_flex, animal_type, animal_subtype, NEm) %>%
    dplyr::mutate(NEm = round(NEm, 3))

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
      across(c(ca_value, NEm), ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)),
      NEa = ca_value * NEm
    ) %>%
    # Final selection of columns
    dplyr::select(region, subregion, animal_tag, class_flex, animal_type, animal_subtype, NEa) %>%
    dplyr::mutate(NEa = round(NEa, 3))

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

  # --- 1. Data Loading from user_data ---
  weights <- readr::read_csv("user_data/livestock_weights.csv", show_col_types = FALSE)
  categories <- readr::read_csv("user_data/livestock_definitions.csv", show_col_types = FALSE)
  coefficients <- readr::read_csv("user_data/ipcc_coefficients.csv", show_col_types = FALSE)
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
      across(c(average_weight, adult_weight, weight_gain, C_val, A_val, B_val), ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)),
      NEg = dplyr::case_when(
        tolower(animal_type) == "cattle" ~ 22.02 * ((average_weight / (C_val * adult_weight + 0.0001))^0.75) * (weight_gain^1.097),
        tolower(animal_type) %in% c("sheep", "goat") ~ (A_val + 0.5 * B_val) * weight_gain,
        TRUE ~ 0
      )
    ) %>%

    # --- 4. Final Selection ---
    dplyr::select(region, subregion, animal_tag, class_flex, animal_type, animal_subtype, NEg) %>%
    dplyr::mutate(NEg = round(NEg, 3))

  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/NEg_result.csv")
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
      across(c(milk_yield, fat_content), ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)),
      Milk_yield_kg_day = milk_yield / 365,
      NEl = dplyr::case_when(
        tolower(animal_type) == "cattle" ~ Milk_yield_kg_day * (1.47 + 0.4 * fat_content),
        tolower(animal_type) %in% c("sheep", "goat") ~ Milk_yield_kg_day * 4.6,
        TRUE ~ 0
      )
    ) %>%

    # --- 3. Final Selection ---
    dplyr::select(region, subregion, animal_tag, class_flex, animal_type, animal_subtype, NEl) %>%
    dplyr::mutate(NEl = round(NEl, 3))

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
  # Load categories to get work hours assigned [cite: 24, 28]
  categories <- readr::read_csv("user_data/livestock_definitions.csv", show_col_types = FALSE)

  # Fetch NEm as the base (provides geography and baseline energy)
  nem_df <- calculate_NEm(saveoutput = FALSE)

  # --- 2. Calculation Pipeline ---
  results <- nem_df %>%
    # 2.1 Join with categories to get work hours [cite: 26, 27]
    # We use the 4-key identity structure for a perfect match [cite: 16, 32]
    dplyr::left_join(
      categories %>%
        dplyr::select(animal_tag, region, subregion, class_flex, work_hours),
      by = c("animal_tag", "region", "subregion", "class_flex")
    ) %>%

    # 2.2 Calculations
    dplyr::mutate(
      # Ensure hours and NEm are numeric and handle NAs
      across(c(work_hours, NEm), ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)),

      # Formula: NE_work = hours * NEm (or specific factor based on intensity)
      NE_work = work_hours * NEm
    ) %>%

    # --- 3. Final Selection and Cleanup ---
    dplyr::select(
      region, subregion, animal_tag, class_flex,
      animal_type, animal_subtype, work_hours, NEm, NE_work
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
      wool_yield = tidyr::replace_na(suppressWarnings(as.numeric(wool_yield)), 0),
      NE_wool = (wool_yield * 24) / 365
    ) %>%

    # --- 3. Final Selection ---
    dplyr::select(region, subregion, animal_tag, class_flex, animal_type, animal_subtype, NE_wool) %>%
    dplyr::mutate(NE_wool = round(NE_wool, 3))

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
      across(c(pr, c_value, NEm), ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)),
      C_preg_factor = dplyr::case_when(
        tolower(animal_type) == "cattle" ~ c_value,
        tolower(animal_type) %in% c("sheep", "goat") ~ (0.126 * pmax(pr - 1, 0)) + (0.077 * (1 - pmax(pr - 1, 0))),
        TRUE ~ 0
      ),
      NE_pregnancy = C_preg_factor * NEm
    ) %>%

    # --- 3. Final Selection ---
    dplyr::select(region, subregion, animal_tag, class_flex, animal_type, animal_subtype, NE_pregnancy) %>%
    dplyr::mutate(NE_pregnancy = round(NE_pregnancy, 3))

  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/NE_pregnancy_result.csv")
  }
  return(results)
}
