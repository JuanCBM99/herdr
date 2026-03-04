library(testthat)
library(herdr)
library(readr)
library(dplyr)

setup_pregnancy_env <- function() {
  if (!dir.exists("user_data")) dir.create("user_data")

  # 1. Weights (Required by NEm)
  write_csv(data.frame(
    region = "spain", subregion = "north",
    animal_tag = c("pregnant_cow", "pregnant_ewe"),
    class_flex = "gestation",
    average_weight = c(600, 70)
  ), "user_data/livestock_weights.csv")

  # 2. Definitions: Mapping C_pregnancy for cattle and PR for sheep
  write_csv(data.frame(
    region = "spain", subregion = "north",
    animal_tag = c("pregnant_cow", "pregnant_ewe"),
    class_flex = "gestation",
    animal_type = c("cattle", "sheep"),
    animal_subtype = "dairy",
    cfi = c("dairy_cow", "dairy_sheep"),
    c_pregnancy = c("cattle_preg", NA), # Coefficient tag for cattle
    pr = c(NA, 1.5)                      # Pregnancy rate for sheep (150%)
  ), "user_data/livestock_definitions.csv")

  # 3. IPCC Coefficients
  write_csv(data.frame(
    coefficient = c("cfi", "cfi", "c_pregnancy"),
    description = c("dairy_cow", "dairy_sheep", "cattle_preg"),
    value = c(0.335, 0.335, 0.10) # 0.10 factor for cattle pregnancy
  ), "user_data/ipcc_coefficients.csv")
}

cleanup_pregnancy_env <- function() {
  if (dir.exists("user_data")) unlink("user_data", recursive = TRUE)
  if (dir.exists("output")) unlink("output", recursive = TRUE)
}

# --- TESTS ---

test_that("calculate_NE_pregnancy computes cattle energy correctly", {
  setup_pregnancy_env()
  results <- calculate_NE_pregnancy(saveoutput = FALSE)

  # Cattle Math:
  # 1. NEm = 0.335 * (600 ^ 0.75) ≈ 40.612
  # 2. NE_preg = c_value (0.10) * NEm ≈ 4.061

  cow_val <- results %>% filter(animal_type == "cattle") %>% pull(NE_pregnancy)
  expect_equal(cow_val, 4.061, tolerance = 0.005)

  cleanup_pregnancy_env()
})



test_that("calculate_NE_pregnancy computes sheep energy correctly", {
  setup_pregnancy_env()
  results <- calculate_NE_pregnancy(saveoutput = FALSE)

  # Sheep Math (pr = 1.5):
  # 1. NEm = 0.335 * (70 ^ 0.75) ≈ 8.106
  # 2. Factor = (0.126 * (1.5-1)) + (0.077 * (1 - (1.5-1)))
  #    Factor = (0.126 * 0.5) + (0.077 * 0.5) = 0.063 + 0.0385 = 0.1015
  # 3. NE_preg = 0.1015 * 8.106 ≈ 0.823

  sheep_val <- results %>% filter(animal_type == "sheep") %>% pull(NE_pregnancy)
  expect_equal(sheep_val, 0.823, tolerance = 0.005)

  cleanup_pregnancy_env()
})
