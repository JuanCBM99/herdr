library(testthat)
library(herdr)
library(readr)
library(dplyr)

setup_nel_env <- function() {
  if (!dir.exists("user_data")) dir.create("user_data")

  # 1. Definitions: Crucial for milk_yield and fat_content
  write_csv(data.frame(
    region = "spain", subregion = "north",
    animal_tag = c("mature_dairy_cow", "mature_dairy_sheep"),
    class_flex = "lactation",
    animal_type = c("cattle", "sheep"),
    animal_subtype = "dairy",
    milk_yield = c(8000, 500), # kg/year
    fat_content = c(3.5, 6.0)   # %
  ), "user_data/livestock_definitions.csv")

  # 2. Weights: Required for the inner_join (even if not used in math)
  write_csv(data.frame(
    region = "spain", subregion = "north",
    animal_tag = c("mature_dairy_cow", "mature_dairy_sheep"),
    class_flex = "lactation",
    average_weight = c(600, 70)
  ), "user_data/livestock_weights.csv")
}

cleanup_nel_env <- function() {
  if (dir.exists("user_data")) unlink("user_data", recursive = TRUE)
  if (dir.exists("output")) unlink("output", recursive = TRUE)
}

# --- TESTS ---

test_that("calculate_NEl computes cattle lactation energy correctly", {
  setup_nel_env()
  results <- calculate_NEl(saveoutput = FALSE)

  # Cattle Math:
  # Yield per day = 8000 / 365 = 21.9178
  # NEl = 21.9178 * (1.47 + 0.4 * 3.5)
  # NEl = 21.9178 * 2.87 = 62.9041

  cow_val <- results %>% filter(animal_type == "cattle") %>% pull(NEl)
  expect_equal(cow_val, 62.904, tolerance = 0.002)

  cleanup_nel_env()
})



test_that("calculate_NEl computes sheep/goat lactation energy correctly", {
  setup_nel_env()
  results <- calculate_NEl(saveoutput = FALSE)

  # Sheep Math:
  # Yield per day = 500 / 365 = 1.3698
  # NEl = 1.3698 * 4.6 = 6.3013

  sheep_val <- results %>% filter(animal_type == "sheep") %>% pull(NEl)
  expect_equal(sheep_val, 6.301, tolerance = 0.002)

  cleanup_nel_env()
})
