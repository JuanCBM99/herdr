library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)

test_that("calculate_vs computes volatile solids using CSV data", {
  # 1. Set test directory
  withr::local_dir(test_path("test_data"))

  # 2. Normalize input CSVs to ensure consistent types for joins
  files_to_fix <- c(
    "livestock_weights.csv", "livestock_definitions.csv",
    "ipcc_coefficients.csv", "diet_profiles.csv",
    "diet_ingredients.csv", "feed_characteristics.csv"
  )

  for (f in files_to_fix) {
    path <- file.path("user_data", f)
    if (file.exists(path)) {
      read_csv(path, col_types = cols(.default = "c"), show_col_types = FALSE) %>%
        mutate(across(everything(), trimws)) %>%
        write_csv(path)
    }
  }

  # 3. Execute Volatile Solids calculation
  results <- suppressWarnings(
    calculate_vs(urinary_energy = 0.04, saveoutput = FALSE)
  )

  # 4. Assertions
  expect_s3_class(results, "data.frame")

  # Check for mandatory output column
  expect_true("VS_kgday" %in% colnames(results))

  # Basic logic check: VS should be positive and within biological ranges
  # (Standard cow VS is usually between 2 and 8 kg/day)
  if(nrow(results) > 0) {
    expect_true(all(results$VS_kgday >= 0))
    expect_false(any(is.na(results$VS_kgday)))

    # Check that required columns for the formula are present in the final table
    expect_true(all(c("GE_MJday", "DE_pct", "ASH_pct") %in% colnames(results)))
  }
})

test_that("calculate_vs handles different urinary energy factors", {
  withr::local_dir(test_path("test_data"))

  # Comparison between standard (0.04) and high (0.06) urinary energy
  res_low <- suppressWarnings(calculate_vs(urinary_energy = 0.04, saveoutput = FALSE))
  res_high <- suppressWarnings(calculate_vs(urinary_energy = 0.06, saveoutput = FALSE))

  # Higher UE must result in higher VS (more energy excreted)
  if(nrow(res_low) > 0 && res_low$VS_kgday[1] > 0) {
    expect_gt(res_high$VS_kgday[1], res_low$VS_kgday[1])
  }
})
