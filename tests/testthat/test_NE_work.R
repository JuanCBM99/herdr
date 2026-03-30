library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)

test_that("calculate_NE_work computes work energy correctly", {
  # Set test directory
  withr::local_dir(test_path("test_data"))

  # Normalize CSVs: ensure work_hours and join keys are correctly typed
  read_csv("user_data/livestock_weights.csv", col_types = cols(.default = "c"), show_col_types = FALSE) %>%
    write_csv("user_data/livestock_weights.csv")

  read_csv("user_data/livestock_definitions.csv", col_types = cols(.default = "c"), show_col_types = FALSE) %>%
    write_csv("user_data/livestock_definitions.csv")

  read_csv("user_data/ipcc_coefficients.csv", col_types = cols(.default = "c"), show_col_types = FALSE) %>%
    write_csv("user_data/ipcc_coefficients.csv")

  # Execute calculation
  results <- calculate_NE_work(saveoutput = FALSE)

  # Assertions
  expect_s3_class(results, "data.frame")
  expect_true("NEwork_MJday" %in% colnames(results))

  # Logic check: result should be numeric and >= 0
  expect_false(any(is.na(results$NEwork_MJday)))
  expect_true(all(results$NEwork_MJday >= 0))
})

test_that("calculate_NE_work handles missing work hours with zero", {
  withr::local_dir(test_path("test_data"))

  # Inject NA in work_hours for testing
  defs <- read_csv("user_data/livestock_definitions.csv", show_col_types = FALSE) %>%
    mutate(work_hours = NA)
  write_csv(defs, "user_data/livestock_definitions.csv")

  results <- calculate_NE_work(saveoutput = FALSE)

  # If work_hours is NA/0, energy must be 0
  expect_true(all(results$NEwork_MJday == 0))
})
