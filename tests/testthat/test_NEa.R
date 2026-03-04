library(testthat)
library(herdr)
library(readr)
library(dplyr)

setup_nea_env <- function() {
  if (!dir.exists("user_data")) dir.create("user_data")

  # 1. Weights (Required by NEm)
  write_csv(data.frame(
    region = "spain", subregion = "north", animal_tag = "mature_dairy_cattle",
    class_flex = "lactation", average_weight = 600
  ), "user_data/livestock_weights.csv")

  # 2. Definitions (Required by both)
  # Includes cfi for NEm and ca for NEa
  write_csv(data.frame(
    region = "spain", subregion = "north", animal_tag = "mature_dairy_cattle",
    class_flex = "lactation", animal_type = "cattle", animal_subtype = "dairy",
    cfi = "dairy_cow", ca = "stall_fed"
  ), "user_data/livestock_definitions.csv")

  # 3. IPCC Coefficients (Required by both)
  write_csv(data.frame(
    coefficient = c("cfi", "ca"),
    description = c("dairy_cow", "stall_fed"),
    value = c(0.335, 0.17)
  ), "user_data/ipcc_coefficients.csv")
}

cleanup_nea_env <- function() {
  if (dir.exists("user_data")) unlink("user_data", recursive = TRUE)
  if (dir.exists("output")) unlink("output", recursive = TRUE)
}

# --- TESTS ---

test_that("calculate_NEa computes activity energy correctly", {
  setup_nea_env()

  # Execute
  results <- calculate_NEa(saveoutput = FALSE)

  # Math check:
  # NEm was approx 40.612 (from previous test)
  # NEa = Ca * NEm = 0.17 * 40.612 = 6.90404
  # Expected result rounded to 3 decimals: 6.904

  expect_s3_class(results, "data.frame")
  # Using tolerance to handle the floating point chain (NEm calculation + NEa calculation)
  expect_equal(results$NEa[1], 6.904, tolerance = 0.002)

  cleanup_nea_env()
})

test_that("calculate_NEa handles missing activity factors with zero", {
  setup_nea_env()

  # Set an unknown activity tag in definitions to force an NA in join
  write_csv(data.frame(
    region = "spain", subregion = "north", animal_tag = "mature_dairy_cattle",
    class_flex = "lactation", animal_type = "cattle", animal_subtype = "dairy",
    cfi = "dairy_cow", ca = "unknown_activity"
  ), "user_data/livestock_definitions.csv")

  results <- calculate_NEa(saveoutput = FALSE)

  # NEa should be 0 instead of NA due to your replace_na logic
  expect_equal(results$NEa[1], 0)

  cleanup_nea_env()
})
