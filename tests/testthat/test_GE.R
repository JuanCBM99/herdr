library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)

test_that("calculate_ge computes Gross Energy using CSV test data", {
  # Set test directory
  withr::local_dir(test_path("test_data"))

  # Normalize all required CSVs
  files_to_fix <- c(
    "livestock_weights.csv",
    "livestock_definitions.csv",
    "ipcc_coefficients.csv",
    "diet_profiles.csv",
    "diet_ingredients.csv",
    "feed_characteristics.csv"
  )

  for (f in files_to_fix) {
    path <- file.path("user_data", f)
    if (file.exists(path)) {
      read_csv(path, col_types = cols(.default = "c"), show_col_types = FALSE) %>%
        mutate(across(everything(), trimws)) %>%
        write_csv(path)
    }
  }

  # EXECUTE: We use suppressWarnings to keep the test output clean
  # Individual NE components are already tested elsewhere
  results <- suppressWarnings(calculate_ge(saveoutput = FALSE))

  # Assertions
  expect_s3_class(results, "data.frame")
  expect_true("GE_MJday" %in% colnames(results))
  expect_false(any(is.na(results$GE_MJday)))

  if(nrow(results) > 0) {
    expect_true(all(results$GE_MJday >= 0))

    # Biological logic: Gross Energy must be higher than Net Maintenance Energy
    sample_row <- results %>% filter(NEm_MJday > 0) %>% head(1)
    if(nrow(sample_row) > 0) {
      expect_true(sample_row$GE_MJday > sample_row$NEm_MJday)
    }
  }
})

test_that("calculate_ge handles default digestibility for missing data", {
  withr::local_dir(test_path("test_data"))

  # Execute silently
  results <- suppressWarnings(calculate_ge(saveoutput = FALSE))

  expect_true(all(results$REM > 0))
  expect_true(all(results$REG > 0))
  expect_true(all(results$DE_pct >= 0))
})
