library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)

test_that("calculate_land_use produces correct output structure using provided CSVs", {

  # 1. Set test directory to use existing CSV files
  withr::local_dir(test_path("test_data"))

  # 2. Normalize existing files (trim whitespace, force character cols)
  files_to_fix <- c(
    "livestock_weights.csv",
    "livestock_definitions.csv",
    "ipcc_coefficients.csv",
    "diet_profiles.csv",
    "diet_ingredients.csv",
    "feed_characteristics.csv",
    "mapping.csv",
    "fao_crop_yields.csv",
    "forage_yields.csv",
    "fao_production.csv"
  )

  for (f in files_to_fix) {
    path <- file.path("user_data", f)
    if (file.exists(path)) {
      read_csv(path, col_types = cols(.default = "c"), show_col_types = FALSE) %>%
        mutate(across(everything(), trimws)) %>%
        write_csv(path)
    }
  }

  # 3. Guard: skip if diet_ingredients.csv has missing origins and no local
  #    FAO trade inputs (production file / parquet) are available
  #    (avoids a 187MB download + missing-file crash in tests)
  diet_ing_path <- file.path("user_data", "diet_ingredients.csv")
  parquet_path  <- file.path("user_data", "fao_trade_matrix.parquet")
  prod_path     <- file.path("user_data", "fao_production.csv")

  if (file.exists(diet_ing_path)) {
    diet_ing <- read_csv(diet_ing_path, show_col_types = FALSE)
    needs_fao_engine   <- any(is.na(diet_ing$country_of_origin))
    missing_fao_inputs <- needs_fao_engine && (!file.exists(parquet_path) || !file.exists(prod_path))
    skip_if(missing_fao_inputs, "Missing country_of_origin values and no local FAO trade inputs (fao_production.csv / parquet); skipping to avoid network download in tests.")
  }

  # 4. Execute land use calculation
  results <- suppressWarnings(suppressMessages(
    calculate_land_use(automatic_cycle = FALSE, saveoutput = FALSE)
  ))

  # 5. Assertions
  expect_s3_class(results, "data.frame")

  expected_cols <- c(
    "region", "subregion", "animal_tag", "class_flex",
    "ingredient", "country_of_origin", "animal_type", "animal_subtype",
    "population", "dm_yield",
    "land_use_per_animal_m2", "total_land_use_m2"
  )
  expect_true(all(expected_cols %in% colnames(results)))

  # Basic consistency checks
  if (nrow(results) > 0) {
    expect_true(all(results$land_use_per_animal_m2 >= 0, na.rm = TRUE))
    expect_true(all(results$total_land_use_m2 >= 0, na.rm = TRUE))
    expect_false(any(is.na(results$land_use_per_animal_m2)))
  }
})
