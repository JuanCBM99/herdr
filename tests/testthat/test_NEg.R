library(testthat)
library(herdr)
library(readr)
library(dplyr)

setup_neg_env <- function() {
  if (!dir.exists("user_data")) dir.create("user_data")

  # 1. Weights: Must contain the keys for the first join
  write_csv(data.frame(
    region = "spain",
    subregion = "test",
    animal_tag = c("growing_beef_cattle", "lamb_meat"),
    class_flex = "none",
    average_weight = c(300, 30),
    adult_weight = c(600, 70),
    weight_gain = c(1.0, 0.2)
  ), "user_data/livestock_weights.csv")

  # 2. Definitions: Must contain columns c, a, and b for the join to work
  write_csv(data.frame(
    region = "spain",
    subregion = "test",
    animal_tag = c("growing_beef_cattle", "lamb_meat"),
    class_flex = "none",
    animal_type = c("cattle", "sheep"),
    animal_subtype = c("beef", "meat"),
    c = c("steer", NA),
    a = c(NA, "lamb_a"),
    b = c(NA, "lamb_b")
  ), "user_data/livestock_definitions.csv")

  # 3. IPCC Coefficients: The lookup table for A, B, and C values
  write_csv(data.frame(
    description = c("steer", "lamb_a", "lamb_b"),
    value = c(1.0, 2.5, 12.0)
  ), "user_data/ipcc_coefficients.csv")
}

cleanup_neg_env <- function() {
  if (dir.exists("user_data")) unlink("user_data", recursive = TRUE)
  if (dir.exists("output")) unlink("output", recursive = TRUE)
}

# --- TESTS ---

test_that("calculate_NEg computes cattle growth correctly", {
  setup_neg_env()

  results <- calculate_NEg(saveoutput = FALSE)

  # Calculation check:
  # Cattle: 22.02 * ((300 / (1.0 * 600))^0.75) * (1^1.097)
  # 22.02 * (0.5^0.75) * 1 = 13.093

  val <- results %>% filter(animal_type == "cattle") %>% pull(NEg)
  expect_equal(val, 13.093, tolerance = 0.005)

  cleanup_neg_env()
})



test_that("calculate_NEg computes sheep/goat growth correctly", {
  setup_neg_env()

  results <- calculate_NEg(saveoutput = FALSE)

  # Sheep Math: (A + 0.5 * B) * weight_gain
  # (2.5 + 0.5 * 12.0) * 0.2 = 1.7

  val <- results %>% filter(animal_type == "sheep") %>% pull(NEg)
  expect_equal(val, 1.700)

  cleanup_neg_env()
})
