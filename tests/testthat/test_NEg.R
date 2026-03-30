library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)

test_that("calculate_NEg computes growth energy without NAs using test data", {
  # Set test directory
  withr::local_dir(test_path("test_data"))

  # Normalize CSVs to ensure consistent joins and numeric types
  read_csv("user_data/livestock_weights.csv", col_types = cols(.default = "c"), show_col_types = FALSE) %>%
    mutate(across(everything(), trimws)) %>%
    write_csv("user_data/livestock_weights.csv")

  read_csv("user_data/livestock_definitions.csv", col_types = cols(.default = "c"), show_col_types = FALSE) %>%
    mutate(across(everything(), trimws)) %>%
    write_csv("user_data/livestock_definitions.csv")

  read_csv("user_data/ipcc_coefficients.csv", col_types = cols(.default = "c"), show_col_types = FALSE) %>%
    mutate(across(everything(), trimws)) %>%
    write_csv("user_data/ipcc_coefficients.csv")

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
