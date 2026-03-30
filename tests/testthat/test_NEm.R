library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)

test_that("calculate_NEm computes metabolic energy correctly with test data", {
  # Set test directory
  withr::local_dir(test_path("test_data"))

  # Fix column types to avoid logical vs character errors
  read_csv("user_data/livestock_weights.csv", col_types = cols(.default = "c"), show_col_types = FALSE) %>%
    write_csv("user_data/livestock_weights.csv")
  read_csv("user_data/livestock_definitions.csv", col_types = cols(.default = "c"), show_col_types = FALSE) %>%
    write_csv("user_data/livestock_definitions.csv")
  read_csv("user_data/ipcc_coefficients.csv", col_types = cols(.default = "c"), show_col_types = FALSE) %>%
    write_csv("user_data/ipcc_coefficients.csv")

  # Block: Run calculation
  # This function uses weights and IPCC coefficients to calculate MJ/day
  results <- calculate_NEm(saveoutput = FALSE)

  # Block: Assertions
  expect_s3_class(results, "data.frame")
  expect_true("NEm_MJday" %in% colnames(results))

  # Verify that energy values are positive and numeric
  expect_true(all(results$NEm_MJday >= 0))
  expect_type(results$NEm_MJday, "double")
})

test_that("calculate_NEm handles missing data safely", {
  withr::local_dir(test_path("test_data"))

  # Block: Run function with existing test data
  results <- calculate_NEm(saveoutput = FALSE)

  # Check that rows with missing weights don't crash the function
  # and result in 0 or NA depending on your internal logic
  expect_false(any(is.nan(results$NEm_MJday)))
})
