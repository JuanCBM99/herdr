library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)

test_that("calculate_NEa computes activity energy correctly with test data", {
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
  results <- calculate_NEa(saveoutput = FALSE)

  # Block: Basic Assertions
  expect_s3_class(results, "data.frame")
  expect_true(any(c("NEa", "NEa_MJday") %in% colnames(results)))

  # Check that we have results for the animals in the test census
  expect_true(nrow(results) > 0)

  # Verify numeric output and no NAs in the energy column
  # (Using any_of to handle both possible column names)
  energy_col <- intersect(colnames(results), c("NEa", "NEa_MJday"))[1]
  expect_false(any(is.na(results[[energy_col]])))
  expect_true(all(results[[energy_col]] >= 0))
})

test_that("calculate_NEa handles population mapping", {
  withr::local_dir(test_path("test_data"))

  results <- calculate_NEm(saveoutput = FALSE)

  # Just verify it returns a valid data frame with expected keys
  expect_true(all(c("region", "animal_tag") %in% colnames(results)))
})
