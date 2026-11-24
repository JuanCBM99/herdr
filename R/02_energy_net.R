#' Calculate Net Energy for Maintenance (NEm)
#'
#' Computes net energy for maintenance based on animal weight and CFI coefficient.
#'
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @param data_dir Character. Path to the folder containing input CSV files. Default is \code{"user_data"}.
#' @param weights Optional preloaded \code{livestock_weights} dataframe.
#' @param categories Optional preloaded \code{ruminant_definitions} dataframe.
#' @param coefficients Optional preloaded \code{ipcc_coefficients} dataframe.
#' @param validate Logical. Whether to run coefficient validation. Default is TRUE.
#' @return A tibble with columns \code{farm_id}, \code{region}, \code{subregion},
#'   \code{animal_tag}, \code{animal_type}, \code{animal_subtype}, and \code{NEm_MJday} (MJ/day).
#' @examples
#' \dontrun{
#' calculate_NEm(saveoutput = FALSE, data_dir = "user_data")
#' }
#' @export
calculate_NEm <- function(saveoutput = TRUE, data_dir = "user_data",
                          weights = NULL, categories = NULL, coefficients = NULL, validate = TRUE) {
  message("Calculating Net Energy for Maintenance (NEm)...")

  # --- 1. Data Loading from user_data ---
  if (is.null(weights)) weights <- herdr_read_input_csv(file.path(data_dir, "livestock_weights.csv"))
  if (is.null(categories)) categories <- herdr_read_input_csv(file.path(data_dir, "ruminant_definitions.csv"), optional = TRUE)
  if (is.null(coefficients)) coefficients <- readr::read_csv(file.path(data_dir, "ipcc_coefficients.csv"), show_col_types = FALSE)

  if (isTRUE(validate)) herdr_validate_ruminant_coefficients(categories, coefficients)

  if (nrow(categories) == 0) {
    return(tibble::tibble(
      farm_id = character(), region = character(), subregion = character(),
      animal_tag = character(), animal_type = character(), animal_subtype = character(),
      NEm_MJday = numeric()
    ))
  }

  # --- 2. Calculation Pipeline ---
  results <- weights %>%
    dplyr::left_join(
      categories %>%
        dplyr::select(farm_id, animal_tag, region, subregion, animal_type, animal_subtype, cfi_tag = cfi),
      by = c("farm_id", "animal_tag", "region", "subregion")
    ) %>%
    dplyr::left_join(
      coefficients %>%
        dplyr::filter(tolower(coefficient) == "cfi") %>%
        dplyr::select(cfi_tag = description, cfi_value = value),
      by = "cfi_tag"
    ) %>%
    dplyr::mutate(
      dplyr::across(c(initial_weight_kg, final_weight_kg, cfi_value), ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)),
      NEm_MJday = cfi_value * (((initial_weight_kg + final_weight_kg) / 2)^0.75)
    ) %>%
    dplyr::select(farm_id, region, subregion, animal_tag, animal_type, animal_subtype, NEm_MJday)

  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/NEm_result.csv")
  }
  return(results)
}

#' Calculate Net Energy for Activity (NEa)
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @param data_dir Character. Path to the folder containing input CSV files. Default is \code{"user_data"}.
#' @param nem_df Optional precalculated NEm dataframe to avoid recomputation.
#' @param weights Optional preloaded \code{livestock_weights} dataframe.
#' @param categories Optional preloaded \code{ruminant_definitions} dataframe.
#' @param coefficients Optional preloaded \code{ipcc_coefficients} dataframe.
#' @return A tibble with columns \code{farm_id}, \code{region}, \code{subregion},
#'   \code{animal_tag}, \code{animal_type}, \code{animal_subtype}, and \code{NEa_MJday} (MJ/day).
#' @examples
#' \dontrun{
#' calculate_NEa(saveoutput = FALSE, data_dir = "user_data")
#' }
#' @export
calculate_NEa <- function(saveoutput = TRUE, data_dir = "user_data", nem_df = NULL,
                          weights = NULL, categories = NULL, coefficients = NULL) {
  message("Calculating Net Energy for Activity (NEa)...")

  # --- 1. Data Loading ---
  if (is.null(weights)) weights <- herdr_read_input_csv(file.path(data_dir, "livestock_weights.csv"))
  if (is.null(categories)) categories <- herdr_read_input_csv(file.path(data_dir, "ruminant_definitions.csv"), optional = TRUE)
  if (is.null(coefficients)) coefficients <- readr::read_csv(file.path(data_dir, "ipcc_coefficients.csv"), show_col_types = FALSE)
  if (nrow(categories) == 0) {
    return(tibble::tibble(
      farm_id = character(), region = character(), subregion = character(),
      animal_tag = character(), animal_type = character(), animal_subtype = character(),
      NEa_MJday = numeric()
    ))
  }
  if (is.null(nem_df)) {
    nem_df <- calculate_NEm(
      saveoutput = FALSE, data_dir = data_dir,
      weights = weights, categories = categories, coefficients = coefficients
    )
  }

  ca_table <- coefficients %>%
    dplyr::filter(tolower(coefficient) == "ca") %>%
    dplyr::select(ca_tag = description, ca_value = value)

  # --- 2. Calculation Pipeline ---
  results <- weights %>%
    dplyr::left_join(
      categories %>%
        dplyr::select(farm_id, animal_tag, region, subregion, animal_type, animal_subtype, ca_tag = ca),
      by = c("farm_id", "animal_tag", "region", "subregion")
    ) %>%
    dplyr::left_join(ca_table, by = "ca_tag") %>%
    dplyr::left_join(
      nem_df %>% dplyr::select(farm_id, animal_tag, region, subregion, animal_type, animal_subtype, NEm_MJday),
      by = c("farm_id", "animal_tag", "region", "subregion", "animal_type", "animal_subtype")
    ) %>%
    dplyr::mutate(
      dplyr::across(c(initial_weight_kg, final_weight_kg, ca_value, NEm_MJday), ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)),
      avg_weight = (initial_weight_kg + final_weight_kg) / 2,
      NEa_MJday = dplyr::case_when(
        tolower(animal_type) == "cattle" ~ ca_value * NEm_MJday,
        tolower(animal_type) %in% c("sheep", "goat") ~ ca_value * avg_weight,
        TRUE ~ 0
      )
    ) %>%
    # --- 3. Final Selection ---
    dplyr::select(farm_id, region, subregion, animal_tag, animal_type, animal_subtype, NEa_MJday)

  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/NEa_result.csv")
  }
  return(results)
}

#' Calculate Net Energy for Growth (NEg)
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @param data_dir Character. Path to the folder containing input CSV files. Default is \code{"user_data"}.
#' @param weights Optional preloaded \code{livestock_weights} dataframe.
#' @param categories Optional preloaded \code{ruminant_definitions} dataframe.
#' @param coefficients Optional preloaded \code{ipcc_coefficients} dataframe.
#' @param validate Logical. Whether to run coefficient validation. Default is TRUE.
#' @return A tibble with columns \code{farm_id}, \code{region}, \code{subregion},
#'   \code{animal_tag}, \code{animal_type}, \code{animal_subtype}, and \code{NEg_MJday} (MJ/day).
#' @examples
#' \dontrun{
#' calculate_NEg(saveoutput = FALSE, data_dir = "user_data")
#' }
#' @export
calculate_NEg <- function(saveoutput = TRUE, data_dir = "user_data",
                          weights = NULL, categories = NULL, coefficients = NULL, validate = TRUE) {
  message("Calculating Net Energy for Growth (NEg)...")

  # --- 1. Data Loading (Robust reading) ---
  if (is.null(weights)) {
    weights <- herdr_read_input_csv(file.path(data_dir, "livestock_weights.csv"))
  } else {
    weights <- dplyr::mutate(weights, dplyr::across(dplyr::any_of(c("farm_id", "region", "subregion", "animal_tag")), as.character))
  }

  if (is.null(categories)) {
    categories <- herdr_read_input_csv(
      file.path(data_dir, "ruminant_definitions.csv"),
      optional = TRUE,
      extra_char_cols = c("c", "a", "b")
    )
  } else {
    categories <- dplyr::mutate(categories, dplyr::across(dplyr::any_of(c("farm_id", "region", "subregion", "animal_tag", "c", "a", "b")), as.character))
  }

  if (is.null(coefficients)) {
    coefficients <- readr::read_csv(file.path(data_dir, "ipcc_coefficients.csv"),
      col_types = readr::cols(description = "c"),
      show_col_types = FALSE
    )
  } else {
    coefficients <- dplyr::mutate(coefficients, dplyr::across(dplyr::any_of("description"), as.character))
  }

  if (nrow(categories) == 0) {
    return(tibble::tibble(
      farm_id = character(), region = character(), subregion = character(),
      animal_tag = character(), animal_type = character(), animal_subtype = character(),
      NEg_MJday = numeric()
    ))
  }

  if (isTRUE(validate)) herdr_validate_ruminant_coefficients(categories, coefficients)

  coeff_lookup <- coefficients %>%
    dplyr::filter(tolower(coefficient) %in% c("c", "a", "b")) %>%
    dplyr::select(description, value) %>%
    dplyr::distinct()

  # --- 2. Join Pipeline ---
  results <- weights %>%
    dplyr::left_join(
      categories %>% dplyr::select(farm_id, animal_tag, region, subregion, animal_type, animal_subtype, c, a, b),
      by = c("farm_id", "animal_tag", "region", "subregion")
    ) %>%
    dplyr::left_join(coeff_lookup, by = c("c" = "description")) %>%
    dplyr::rename(C_val = value) %>%
    dplyr::left_join(coeff_lookup, by = c("a" = "description")) %>%
    dplyr::rename(A_val = value) %>%
    dplyr::left_join(coeff_lookup, by = c("b" = "description")) %>%
    dplyr::rename(B_val = value) %>%
    # --- 3. Calculations ---
    dplyr::mutate(
      dplyr::across(
        c(initial_weight_kg, final_weight_kg, adult_weight_kg, productive_period_days, C_val, A_val, B_val),
        ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)
      ),
      NEg_MJday = dplyr::case_when(
        tolower(animal_type) == "cattle" & (C_val * adult_weight_kg) > 0 & productive_period_days > 0 & (final_weight_kg > initial_weight_kg) ~
          22.02 * ((((initial_weight_kg + final_weight_kg) / 2) / (C_val * adult_weight_kg))^0.75) * (((final_weight_kg - initial_weight_kg) / productive_period_days)^1.097),
        tolower(animal_type) %in% c("sheep", "goat") & productive_period_days > 0 & (final_weight_kg > initial_weight_kg) ~
          ((final_weight_kg - initial_weight_kg) / productive_period_days) * (A_val + 0.5 * B_val * (initial_weight_kg + final_weight_kg)),
        TRUE ~ 0
      )
    ) %>%
    # --- 4. Final Selection ---
    dplyr::select(farm_id, region, subregion, animal_tag, animal_type, animal_subtype, NEg_MJday)

  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/NEg_result.csv")
    message("[herdr] Results saved to output/NEg_result.csv")
  }

  return(results)
}

#' Calculate Net Energy for Lactation (NEl)
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @param data_dir Character. Path to the folder containing input CSV files. Default is \code{"user_data"}.
#' @param categories Optional preloaded \code{ruminant_definitions} dataframe.
#' @param weights Optional preloaded \code{livestock_weights} dataframe.
#' @return A tibble with columns \code{farm_id}, \code{region}, \code{subregion},
#'   \code{animal_tag}, \code{animal_type}, \code{animal_subtype}, and \code{NEl_MJday} (MJ/day).
#' @examples
#' \dontrun{
#' calculate_NEl(saveoutput = FALSE, data_dir = "user_data")
#' }
#' @export
calculate_NEl <- function(saveoutput = TRUE, data_dir = "user_data", categories = NULL, weights = NULL) {
  message("Calculating Net Energy for Lactation (NEl)...")

  # --- 1. Data Loading from user_data ---
  if (is.null(categories)) categories <- herdr_read_input_csv(file.path(data_dir, "ruminant_definitions.csv"), optional = TRUE)
  if (is.null(weights)) weights <- herdr_read_input_csv(file.path(data_dir, "livestock_weights.csv"))
  if (nrow(categories) == 0) {
    return(tibble::tibble(
      farm_id = character(), region = character(), subregion = character(),
      animal_tag = character(), animal_type = character(), animal_subtype = character(),
      NEl_MJday = numeric()
    ))
  }
  weights_keys <- weights %>%
    dplyr::select(farm_id, region, subregion, animal_tag) %>%
    dplyr::distinct()

  # --- 2. Calculation Pipeline ---
  results <- categories %>%
    dplyr::inner_join(weights_keys, by = c("farm_id", "region", "subregion", "animal_tag")) %>%
    dplyr::mutate(
      dplyr::across(c(milk_yield_kg_year, fat_content_pct), ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)),
      Milk_yield_kg_day = milk_yield_kg_year / 365,
      NEl_MJday = dplyr::case_when(
        tolower(animal_type) == "cattle" ~ Milk_yield_kg_day * (1.47 + 0.4 * fat_content_pct),
        tolower(animal_type) == "sheep" ~ Milk_yield_kg_day * 4.6,
        tolower(animal_type) == "goat" ~ Milk_yield_kg_day * 3,
        TRUE ~ 0
      )
    ) %>%
    # --- 3. Final Selection ---
    dplyr::select(farm_id, region, subregion, animal_tag, animal_type, animal_subtype, NEl_MJday)

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
#' @param data_dir Character. Path to the folder containing input CSV files. Default is \code{"user_data"}.
#' @param nem_df Optional precalculated NEm dataframe to avoid recomputation.
#' @param categories Optional preloaded \code{ruminant_definitions} dataframe.
#' @return A tibble with columns \code{farm_id}, \code{region}, \code{subregion},
#'   \code{animal_tag}, \code{animal_type}, \code{animal_subtype}, and \code{NEwork_MJday} (MJ/day).
#' @examples
#' \dontrun{
#' calculate_NE_work(saveoutput = FALSE, data_dir = "user_data")
#' }
#' @export
calculate_NE_work <- function(saveoutput = TRUE, data_dir = "user_data", nem_df = NULL, categories = NULL) {
  message("Calculating Net Energy for Work (NE_work)...")

  # --- 1. Data Loading from user_data ---
  if (is.null(categories)) categories <- herdr_read_input_csv(file.path(data_dir, "ruminant_definitions.csv"), optional = TRUE)
  if (nrow(categories) == 0) {
    return(tibble::tibble(
      farm_id = character(), region = character(), subregion = character(),
      animal_tag = character(), animal_type = character(), animal_subtype = character(),
      NEwork_MJday = numeric()
    ))
  }

  if (is.null(nem_df)) nem_df <- calculate_NEm(saveoutput = FALSE, data_dir = data_dir, categories = categories)

  # --- 2. Calculation Pipeline ---
  results <- nem_df %>%
    dplyr::left_join(
      categories %>%
        dplyr::select(farm_id, animal_tag, region, subregion, work_hours),
      by = c("farm_id", "animal_tag", "region", "subregion")
    ) %>%
    dplyr::mutate(
      dplyr::across(c(work_hours, NEm_MJday), ~ tidyr::replace_na(suppressWarnings(as.numeric(.)), 0)),
      NEwork_MJday = 0.10 * work_hours * NEm_MJday
    ) %>%
    # --- 3. Final Selection and Cleanup ---
    dplyr::select(
      farm_id, region, subregion, animal_tag,
      animal_type, animal_subtype, NEwork_MJday
    )

  # --- 4. Save Output ---
  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/NE_work_result.csv")
    message("[herdr] Saved output to output/NE_work_result.csv")
  }

  return(results)
}

#' Calculate Net Energy for Wool (NE_wool)
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @param data_dir Character. Path to the folder containing input CSV files. Default is \code{"user_data"}.
#' @param categories Optional preloaded \code{ruminant_definitions} dataframe.
#' @param weights Optional preloaded \code{livestock_weights} dataframe.
#' @return A tibble with columns \code{farm_id}, \code{region}, \code{subregion},
#'   \code{animal_tag}, \code{animal_type}, \code{animal_subtype}, and \code{NEwool_MJday} (MJ/day).
#' @examples
#' \dontrun{
#' calculate_NE_wool(saveoutput = FALSE, data_dir = "user_data")
#' }
#' @export
calculate_NE_wool <- function(saveoutput = TRUE, data_dir = "user_data", categories = NULL, weights = NULL) {
  message("Calculating Net Energy for Wool (NE_wool)...")

  # --- 1. Data Loading from user_data ---
  if (is.null(categories)) categories <- herdr_read_input_csv(file.path(data_dir, "ruminant_definitions.csv"), optional = TRUE)
  if (is.null(weights)) weights <- herdr_read_input_csv(file.path(data_dir, "livestock_weights.csv"))
  if (nrow(categories) == 0) {
    return(tibble::tibble(
      farm_id = character(), region = character(), subregion = character(),
      animal_tag = character(), animal_type = character(), animal_subtype = character(),
      NEwool_MJday = numeric()
    ))
  }
  weights_keys <- weights %>%
    dplyr::select(farm_id, region, subregion, animal_tag) %>%
    dplyr::distinct()

  # --- 2. Calculation Pipeline ---
  results <- categories %>%
    dplyr::inner_join(weights_keys, by = c("farm_id", "region", "subregion", "animal_tag")) %>%
    dplyr::mutate(
      wool_yield_kg_year = tidyr::replace_na(suppressWarnings(as.numeric(wool_yield_kg_year)), 0),
      NEwool_MJday = (wool_yield_kg_year * 24) / 365
    ) %>%
    # --- 3. Final Selection ---
    dplyr::select(farm_id, region, subregion, animal_tag, animal_type, animal_subtype, NEwool_MJday)

  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/NE_wool_result.csv")
  }
  return(results)
}

#' Calculate Net Energy for Pregnancy (NE_pregnancy)
#' @param saveoutput If TRUE (default) the results are saved in the output folder.
#' @param data_dir Character. Path to the folder containing input CSV files. Default is \code{"user_data"}.
#' @param nem_df Optional precalculated NEm dataframe to avoid recomputation.
#' @param categories Optional preloaded \code{ruminant_definitions} dataframe.
#' @param coefficients Optional preloaded \code{ipcc_coefficients} dataframe.
#' @param validate Logical. Whether to run coefficient validation. Default is TRUE.
#' @return A tibble with columns \code{farm_id}, \code{region}, \code{subregion},
#'   \code{animal_tag}, \code{animal_type}, \code{animal_subtype}, and \code{NEpregnancy_MJday} (MJ/day).
#' @examples
#' \dontrun{
#' calculate_NE_pregnancy(saveoutput = FALSE, data_dir = "user_data")
#' }
#' @export
calculate_NE_pregnancy <- function(saveoutput = TRUE, data_dir = "user_data", nem_df = NULL,
                                   categories = NULL, coefficients = NULL, validate = TRUE) {
  message("Calculating Net Energy for Pregnancy (NE_pregnancy)...")

  # --- 1. Data Loading ---
  if (is.null(categories)) categories <- herdr_read_input_csv(file.path(data_dir, "ruminant_definitions.csv"), optional = TRUE)
  if (is.null(coefficients)) coefficients <- readr::read_csv(file.path(data_dir, "ipcc_coefficients.csv"), show_col_types = FALSE)

  if (nrow(categories) == 0) {
    return(tibble::tibble(
      farm_id = character(), region = character(), subregion = character(),
      animal_tag = character(), animal_type = character(), animal_subtype = character(),
      NEpregnancy_MJday = numeric()
    ))
  }

  if (isTRUE(validate)) herdr_validate_ruminant_coefficients(categories, coefficients)

  if (is.null(nem_df)) {
    nem_df <- calculate_NEm(
      saveoutput = FALSE, data_dir = data_dir,
      categories = categories, coefficients = coefficients, validate = FALSE
    )
  }

  c_preg_col <- intersect(c("c_pregnancy_cattle", "c_pregnancy"), names(categories))
  if (length(c_preg_col) == 0) {
    stop(paste0("Error: Missing pregnancy column ('c_pregnancy_cattle' or 'c_pregnancy') in '", file.path(data_dir, "ruminant_definitions.csv"), "'"))
  }

  required_cols <- c("pr_sheep_goat", "pregnancy_rate")
  missing_cols <- setdiff(required_cols, names(categories))
  if (length(missing_cols) > 0) {
    stop(paste0("Error: Missing column(s) in '", file.path(data_dir, "ruminant_definitions.csv"), "': ", paste(missing_cols, collapse = ", ")))
  }

  coeff_lookup <- coefficients %>%
    dplyr::filter(tolower(coefficient) == "c_pregnancy") %>%
    dplyr::select(c_pregnancy_tag = description, c_value = value)

  cat_sub <- categories %>%
    dplyr::select(
      farm_id, animal_tag, region, subregion, animal_type, animal_subtype,
      c_pregnancy_tag = dplyr::all_of(c_preg_col[1]), pr_sheep_goat, pregnancy_rate
    )

  # --- 2. Calculation Pipeline ---
  results <- nem_df %>%
    dplyr::left_join(
      cat_sub,
      by = c("farm_id", "animal_tag", "region", "subregion", "animal_type", "animal_subtype")
    ) %>%
    dplyr::left_join(coeff_lookup, by = "c_pregnancy_tag") %>%
    dplyr::mutate(
      raw_preg_rate = suppressWarnings(as.numeric(pregnancy_rate)),
      pr_sheep_goat = tidyr::replace_na(suppressWarnings(as.numeric(pr_sheep_goat)), 0),
      pregnancy_rate = dplyr::case_when(
        !is.na(raw_preg_rate) ~ raw_preg_rate,
        tolower(animal_type) %in% c("sheep", "goat", "goats") & pr_sheep_goat > 0 ~ 1.0,
        TRUE ~ 0
      ),
      c_value = tidyr::replace_na(suppressWarnings(as.numeric(c_value)), 0),
      NEm_MJday = tidyr::replace_na(suppressWarnings(as.numeric(NEm_MJday)), 0),
      # IPCC 2019, Vol 4, Ch 10, Table 10.7 (cattle = c_value; sheep/goat: 1 -> 0.077, 2 -> 0.126, >=3 -> 0.150)
      C_preg_factor = dplyr::case_when(
        tolower(animal_type) == "cattle" ~ c_value,
        tolower(animal_type) %in% c("sheep", "goat", "goats") & pr_sheep_goat <= 0 ~ 0,
        tolower(animal_type) %in% c("sheep", "goat", "goats") & pr_sheep_goat <= 1.0 ~ 0.077,
        tolower(animal_type) %in% c("sheep", "goat", "goats") & pr_sheep_goat <= 2.0 ~
          0.077 + (pr_sheep_goat - 1.0) * (0.126 - 0.077),
        tolower(animal_type) %in% c("sheep", "goat", "goats") & pr_sheep_goat < 3.0 ~
          0.126 + (pr_sheep_goat - 2.0) * (0.150 - 0.126),
        tolower(animal_type) %in% c("sheep", "goat", "goats") ~ 0.150,
        TRUE ~ 0
      ),
      NEpregnancy_MJday = C_preg_factor * NEm_MJday * pregnancy_rate
    ) %>%
    # --- 3. Final Selection ---
    dplyr::select(farm_id, region, subregion, animal_tag, animal_type, animal_subtype, NEpregnancy_MJday)

  if (isTRUE(saveoutput)) {
    if (!dir.exists("output")) dir.create("output")
    readr::write_csv(results, "output/NE_pregnancy_result.csv")
  }
  return(results)
}
