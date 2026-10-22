library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)

test_that("calculate_NEg computes growth energy without NAs using test data", {
  # Set test directory
  withr::local_dir(test_path("test_data"))

  # Execute calculation
  results <- calculate_NEg(saveoutput = FALSE)

  # Assertions
  expect_s3_class(results, "data.frame")

  # Diagnostic: Print rows with NA if test fails
  if(any(is.na(results$NEg_MJday))) {
    print(results %>% filter(is.na(NEg_MJday)))
  }

  expect_false(any(is.na(results$NEg_MJday)), info = "Check for join mismatches or division by zero")
  expect_true(all(results$NEg_MJday >= 0))
})
