library(testthat)
library(herdr)
library(readr)
library(dplyr)

setup_wool_env <- function() {
  if (!dir.exists("user_data")) dir.create("user_data")

  # 1. Definitions: Including wool_yield (kg/year)
  write_csv(data.frame(
    region = "spain", subregion = "north",
    animal_tag = c("merino_sheep", "dairy_cow"),
    class_flex = "none",
    animal_type = c("sheep", "cattle"),
    animal_subtype = c("wool", "dairy"),
    wool_yield = c(4.5, NA) # 4.5 kg/year for sheep, NA for cow
  ), "user_data/livestock_definitions.csv")

  # 2. Weights: Required for the inner_join
  write_csv(data.frame(
    region = "spain", subregion = "north",
    animal_tag = c("merino_sheep", "dairy_cow"),
    class_flex = "none",
    average_weight = c(70, 600)
  ), "user_data/livestock_weights.csv")
}

cleanup_wool_env <- function() {
  if (dir.exists("user_data")) unlink("user_data", recursive = TRUE)
  if (dir.exists("output")) unlink("output", recursive = TRUE)
}

# --- TESTS ---

test_that("calculate_NE_wool computes energy for wool correctly", {
  setup_wool_env()

  results <- calculate_NE_wool(saveoutput = FALSE)

  # Math check for Sheep:
  # NE_wool = (4.5 * 24) / 365
  # 108 / 365 = 0.29589...
  # Rounded to 3 decimals: 0.296

  sheep_val <- results %>% filter(animal_tag == "merino_sheep") %>% pull(NE_wool)
  expect_equal(sheep_val, 0.296)

  cleanup_wool_env()
})

test_that("calculate_NE_wool handles missing or NA yield with zero", {
  setup_wool_env()

  results <- calculate_NE_wool(saveoutput = FALSE)

  # Cattle should have 0.000 since wool_yield was NA
  cow_val <- results %>% filter(animal_tag == "dairy_cow") %>% pull(NE_wool)
  expect_equal(cow_val, 0)

  cleanup_wool_env()
})
