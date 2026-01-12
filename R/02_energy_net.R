#' Calculate Net Energy for Maintenance (NEm)
#' Computes NEm based on average weight and CFI coefficients using a relational join approach.
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @export
calculate_NEm <- function(saveoutput = TRUE) {

  message("\U0001f7e2 Calculating Net Energy for Maintenance (NEm)...")

  # --- 1. Data Loading ---
  weights       <- load_dataset("weights")
  categories    <- load_dataset("categories")
  coefficients  <- load_dataset("coefficients")

  # Concisely effective validations
  stopifnot(
    all(c("identification", "average_weight") %in% names(weights)),
    all(c("identification", "cfi") %in% names(categories))
  )

  # --- 2. Calculation Pipeline ---
  results <- weights %>%
    # 2.1 Join Categories to get the 'cfi' tag (e.g., "lactating_cow")
    dplyr::left_join(
      categories %>%
        dplyr::select(identification, animal_type, animal_subtype, cfi_tag = cfi),
      by = c("identification", "animal_type", "animal_subtype")
    ) %>%

    # 2.2 Join Coefficients to get the numeric value
    # This uses a robust relational lookup
    dplyr::left_join(
      coefficients %>%
        dplyr::filter(tolower(coefficient) == "cfi") %>%
        dplyr::select(cfi_tag = description, cfi_value = value),
      by = "cfi_tag"
    ) %>%

    # 2.3 Calculation and Cleanup
    dplyr::mutate(
      # Numeric safety
      across(c(average_weight, cfi_value), ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)),

      # Formula: Coefficient * (Weight ^ 0.75)
      NEm = cfi_value * (average_weight ^ 0.75)
    ) %>%

    # 2.4 Flexible selection (retains group/zone only if they exist)
    dplyr::select(
      dplyr::any_of(c("group", "zone")),
      identification, animal_type, animal_subtype,
      average_weight, cfi_value, NEm
    ) %>%

    # Arrange prioritizing group/zone if they exist, then identification
    dplyr::arrange(dplyr::across(dplyr::any_of(c("group", "zone"))), identification)

  # --- 3. Save Output ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/NEm_result.csv")
    message("\U0001f4be Saved output to output/NEm_result.csv")
  }

  return(results)
}


#' Calculate Net Energy for Activity (NEa) (Refactored)
#'
#' Computes NEa based on NEm and activity coefficients (Ca).
#' Uses a simplified direct lookup based on the assigned Ca tag.
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @export
calculate_NEa <- function(saveoutput = TRUE) {

  message("\U0001f7e2 Calculating Net Energy for Activity (NEa) via direct C_a lookup (Simplified)...")

  # --- 1. Data Loading ---
  categories   <- load_dataset("categories")
  coefficients <- load_dataset("coefficients")

  # Fetch NEm (NEm is the base for NEa calculation)
  nem_df <- calculate_NEm(saveoutput = FALSE)

  # --- 2. Coefficient Preparation ---

  # Extract the 'ca' coefficients table (Need only tag and value)
  ca_table <- coefficients %>%
    dplyr::filter(tolower(coefficient) == "ca") %>%
    dplyr::select(ca_tag = description, ca_value = value)

  # --- 3. Calculation Pipeline ---
  results <- nem_df %>%
    # 3.1 Join categorization data using identification, type, and subtype (for robustness)
    # Extract only the 'ca_tag'
    dplyr::left_join(
      categories %>%
        dplyr::select(identification, animal_type, animal_subtype, ca_tag = ca),
      by = c("identification", "animal_type", "animal_subtype")
    ) %>%

    # 3.2 Join the base coefficient value (Simple Lookup)
    dplyr::left_join(ca_table, by = "ca_tag") %>%

    # 3.3 Calculations
    dplyr::mutate(
      # Numeric safety
      across(c(ca_value, NEm), ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)),

      # Simplified Logic: NEa = C_a * NEm (C_a is the direct lookup value)
      NEa = ca_value * NEm
    ) %>%

    # 3.4 Selection and Cleanup (Categorization keys are maintained)
    dplyr::select(
      dplyr::any_of(c("group", "zone")),
      identification,
      animal_type,
      animal_subtype,
      NEm,
      Ca_coefficient = ca_value,
      NEa
    ) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))

  # --- 4. Save Output ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/NEa_result.csv")
    message("\U0001f4be Saved output to output/NEa_result.csv")
  }

  return(results)
}


#' Calculate Net Energy for Growth (NEg) (Refactored)
#'
#' Computes NEg for all animals using vectorized operations.
#' Replaces rowwise lookups with efficient joins.
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @export
calculate_NEg <- function(saveoutput = TRUE) {

  message("\U0001f7e2 Calculating Net Energy for Growth (NEg)...")

  # --- 1. Data Loading ---
  weights      <- load_dataset("weights")
  categories   <- load_dataset("categories")
  coefficients <- load_dataset("coefficients")

  # Prepare a clean lookup table for the coefficients
  # We only need 'description' (the key) and 'value' (the number)
  coeff_lookup <- coefficients %>%
    dplyr::select(description, value) %>%
    dplyr::distinct(description, .keep_all = TRUE) # Safety against duplicates

  # --- 2. Data Construction Pipeline ---
  # Instead of looking up coefficients row by row, we join them all at once

  df_joined <- weights %>%
    # 2.1 Join categories to get the coefficient tags (c, a, b)
    dplyr::left_join(
      categories %>% dplyr::select(identification, animal_type, animal_subtype, c, a, b),
      by = c("identification", "animal_type", "animal_subtype")
    ) %>%

    # 2.2 Fetch C value (used for Cattle)
    dplyr::left_join(coeff_lookup, by = c("c" = "description")) %>%
    dplyr::rename(C_val = value) %>%

    # 2.3 Fetch A value (used for Sheep/Goat)
    dplyr::left_join(coeff_lookup, by = c("a" = "description")) %>%
    dplyr::rename(A_val = value) %>%

    # 2.4 Fetch B value (used for Sheep/Goat)
    dplyr::left_join(coeff_lookup, by = c("b" = "description")) %>%
    dplyr::rename(B_val = value)

  # --- 3. Vectorized Calculation ---
  results <- df_joined %>%
    dplyr::mutate(
      # Type cleaning and NA handling before calculation
      across(
        c(average_weight, adult_weight, weight_gain, C_val, A_val, B_val),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),

      # NEg Logic
      NEg = dplyr::case_when(
        # Cattle Logic
        # Formula: 22.02 * (Weight / (C * AdultWeight))^0.75 * Gain^1.097
        tolower(animal_type) == "cattle" ~ dplyr::if_else(
          adult_weight > 0 & C_val > 0,
          22.02 * ((average_weight / (C_val * adult_weight))^0.75) * (weight_gain^1.097),
          0
        ),

        # Sheep/Goat Logic
        # Formula: (A + 0.5 * B) * Gain
        tolower(animal_type) %in% c("sheep", "goat") ~ (A_val + 0.5 * B_val) * weight_gain,

        # Other
        TRUE ~ 0
      )
    ) %>%

    # --- 4. Final Cleanup ---
    dplyr::select(
      dplyr::any_of(c("group", "zone")),
      identification, animal_type, animal_subtype,
      c_tag = c, a_tag = a, b_tag = b, # Retain original names as tags
      average_weight, adult_weight, weight_gain,
      NEg
    ) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))

  # --- 5. Save Output ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/NEg_result.csv")
    message("\U0001f4be Saved output to output/NEg_result.csv")
  }

  return(results)
}


#' Calculate Net Energy for Lactation (NEl) (Refactored)
#'
#' Computes NEl based on milk yield and fat content.
#' Assigns 0 energy for non-lactating animals instead of dropping them.
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @export
calculate_NEl <- function(saveoutput = TRUE) {

  message("\U0001f7e2 Calculating Net Energy for Lactation (NEl)...")

  # --- 1. Data Loading ---
  categories <- load_dataset("categories")

  # --- 2. Calculation Pipeline ---
  results <- categories %>%
    dplyr::mutate(
      # Type cleaning and NA handling (NAs become 0)
      across(
        c(milk_yield, fat_content),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),

      # Calculate daily production (assuming annual milk_yield)
      Milk_yield_kg_day = milk_yield / 365,

      # Net Energy (NEl) Calculation
      NEl = dplyr::case_when(
        # If no production, energy is 0 (saves calculation)
        Milk_yield_kg_day <= 0 ~ 0,

        # Cattle Formula: Production * (1.47 + 0.4 * Fat)
        tolower(animal_type) == "cattle" ~ Milk_yield_kg_day * (1.47 + 0.4 * fat_content),

        # Small Ruminants Formula: Production * Fixed Factor (approx 4.6 MJ/kg)
        tolower(animal_type) %in% c("sheep", "goat") ~ Milk_yield_kg_day * 4.6,

        # Other (e.g., non-lactating animals in this context)
        TRUE ~ 0
      )
    ) %>%

    # --- 3. Final Cleanup ---
    dplyr::select(
      dplyr::any_of(c("group", "zone")), # Retain columns if they exist
      identification, animal_type, animal_subtype,
      Milk_yield_kg_day, fat_content, NEl
    ) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))

  # --- 4. Save Output ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/NEl_result.csv")
    message("\U0001f4be Saved output to output/NEl_result.csv")
  }

  return(results)
}


#' Calculate Net Working Energy (NE_work) (Refactored)
#'
#' Computes NE_work based on NEm and hours of activity.
#' Keeps all animals in the dataset (assigns 0 to non-working animals).
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @export
calculate_NE_work <- function(saveoutput = TRUE) {

  message("\U0001f7e2 Calculating Net Energy for Work (NE_work)...")

  # --- 1. Data Loading ---
  categories <- load_dataset("categories")

  # Fetch NEm (calculation base)
  nem_df <- calculate_NEm(saveoutput = FALSE)

  # --- 2. Calculation Pipeline ---
  results <- nem_df %>%
    # 2.1 Join the 'hours' variable from categories
    dplyr::left_join(
      categories %>% dplyr::select(identification, animal_type, animal_subtype, hours),
      by = c("identification", "animal_type", "animal_subtype")
    ) %>%

    # 2.2 Calculations and Cleanup
    dplyr::mutate(
      # Ensure 'hours' and 'NEm' are numeric.
      # If hours is NA (no data), it converts to 0.
      across(
        c(hours, NEm),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),

      # Formula: NEm * Hours (Will assign 0 if hours is 0)
      NE_work = hours * NEm
    ) %>%

    # --- 3. Final Cleanup ---
    dplyr::select(
      dplyr::any_of(c("group", "zone")), # Retain group/zone if they come from NEm
      identification, animal_type, animal_subtype,
      hours, NEm, NE_work
    ) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))

  # --- 4. Save Output ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/NE_work_result.csv")
    message("\U0001f4be Saved output to output/NE_work_result.csv")
  }

  return(results)
}


#' Calculate Net Energy for Wool Production (NE_wool) (Refactored)
#'
#' Computes NE_wool based on wool yield.
#' Assigns 0 energy for non-wool producing animals instead of dropping them.
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @export
calculate_NE_wool <- function(saveoutput = TRUE) {

  message("\U0001f7e2 Calculating Net Energy for Wool (NE_wool)...")

  # --- 1. Data Loading ---
  categories <- load_dataset("categories")

  # --- 2. Calculation Pipeline ---
  results <- categories %>%
    dplyr::mutate(
      # Numeric safety: Convert text to number and NA to 0
      wool_yield = tidyr::replace_na(suppressWarnings(as.numeric(wool_yield)), 0),

      # Direct Calculation (Vectorized)
      # Formula: (Annual Wool Yield * 24 MJ/kg) / 365 days
      NE_wool = (wool_yield * 24) / 365
    ) %>%

    # --- 3. Selection and Cleanup ---
    dplyr::select(
      dplyr::any_of(c("group", "zone")), # Retain columns if they exist
      identification, animal_type, animal_subtype,
      wool_yield, NE_wool
    ) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))

  # --- 4. Save Output ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/NE_wool_result.csv")
    message("\U0001f4be Saved output to output/NE_wool_result.csv")
  }

  return(results)
}


#' Calculate Net Energy for Pregnancy (NE_pregnancy) (Refactored)
#'
#' Computes NE_pregnancy based on species-specific coefficients (C_pregnancy or PR).
#' Keeps all animals (assigns 0 to non-pregnant), ensuring data consistency.
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @export
calculate_NE_pregnancy <- function(saveoutput = TRUE) {

  message("\U0001f7e2 Calculating Net Energy for Pregnancy (NE_pregnancy)...")

  # --- 1. Data Loading ---
  categories   <- load_dataset("categories")
  coefficients <- load_dataset("coefficients")

  # Fetch NEm (Defines the base population: group, zone, id...)
  nem_df <- calculate_NEm(saveoutput = FALSE)

  # Prepare coefficient table for Cattle (C_pregnancy)
  # This replaces manual dictionary/vector creation
  coeff_lookup <- coefficients %>%
    dplyr::filter(tolower(coefficient) == "c_pregnancy") %>%
    dplyr::select(c_pregnancy_tag = description, c_value = value) %>%
    dplyr::distinct(c_pregnancy_tag, .keep_all = TRUE)

  # --- 2. Calculation Pipeline ---
  results <- nem_df %>%
    # 2.1 Join categories data (tags and pregnancy rates)
    # Use universal keys. NEm brings group/zone, categories provides parameters.
    dplyr::left_join(
      categories %>%
        dplyr::select(identification, animal_type, animal_subtype, c_pregnancy, pr),
      by = c("identification", "animal_type", "animal_subtype")
    ) %>%

    # 2.2 Join the coefficient value for Cattle
    dplyr::left_join(coeff_lookup, by = c("c_pregnancy" = "c_pregnancy_tag")) %>%

    # 2.3 Calculations
    dplyr::mutate(
      # Numeric safety
      across(c(pr, c_value, NEm), ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)),

      # Pregnancy Factor (C_preg) Calculation
      C_preg_factor = dplyr::case_when(
        # CASE 1: Cattle (Uses tabular coefficient Cb)
        tolower(animal_type) == "cattle" ~ c_value,

        # CASE 2: Sheep and Goats (Uses PR rate)
        # Logic: Breakdown into single vs double births based on if PR > 1
        tolower(animal_type) %in% c("sheep", "goat") & pr > 0 ~ {
          double_birth <- pmax(pr - 1, 0) # If PR is 1.2, double is 0.2
          single_birth <- 1 - double_birth # and single is 0.8
          (0.126 * double_birth) + (0.077 * single_birth)
        },

        # CASE 3: Rest
        TRUE ~ 0
      ),

      # Final Calculation: Factor * NEm
      NE_pregnancy = C_preg_factor * NEm
    ) %>%

    # --- 3. Final Cleanup ---
    dplyr::select(
      dplyr::any_of(c("group", "zone")), # Retain columns if they exist in NEm
      identification, animal_type, animal_subtype,
      c_pregnancy_tag = c_pregnancy, C_preg_factor, NE_pregnancy
    ) %>%
    dplyr::mutate(across(where(is.numeric), ~ round(.x, 3)))

  # --- 4. Save Output ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/NE_pregnancy_result.csv")
    message("\U0001f4be Saved output to output/NE_pregnancy_result.csv")
  }

  return(results)
}
