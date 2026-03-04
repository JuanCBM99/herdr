library(testthat)
library(herdr)
library(readr)
library(dplyr)

setup_manure_ch4_env <- function() {
  if (!dir.exists("user_data")) dir.create("user_data")

  # Standard template for manure management
  mm_template <- data.frame(
    region = "spain", subregion = "test", animal_tag = "dairy_cow", class_flex = "none",
    animal_type = "cattle", animal_subtype = "dairy", allocation = 1.0,
    system_base = "slurry", management_months = 12, system_climate = "temperate",
    system_subclimate = "none", system_variant = "none", climate_zone = "none",
    climate_moisture = "none"
  )
  write_csv(mm_template, "user_data/manure_management.csv")

  # IPCC Master (MCF Lookups)
  write_csv(data.frame(
    system_base = "slurry", management_months = 12, system_climate = "temperate",
    system_subclimate = "none", system_variant = "none", climate_zone = "none",
    climate_moisture = "none", animal_type = "cattle", animal_subtype = "dairy",
    mcf = 0.17
  ), "user_data/ipcc_mm.csv")

  # IPCC Coefficients (B0 Lookups)
  write_csv(data.frame(
    coefficient = "b_0", animal_type = "cattle", animal_subtype = "dairy",
    value = 0.24
  ), "user_data/ipcc_coefficients.csv")

  # Basic census for population dependency
  write_csv(data.frame(
    region = "spain", subregion = "test", animal_tag = "dairy_cow", class_flex = "none",
    population = 1000, animal_type = "cattle", animal_subtype = "dairy"
  ), "user_data/livestock_census.csv")
}

cleanup_manure_ch4_env <- function() {
  if (dir.exists("user_data")) unlink("user_data", recursive = TRUE)
  if (dir.exists("output")) unlink("output", recursive = TRUE)
}

# --- TESTS ---

test_that("calculate_CH4_manure handles allocations exceeding 100%", {
  setup_manure_ch4_env()

  # Update while keeping all columns to avoid "object not found"
  bad_mm <- read_csv("user_data/manure_management.csv", show_col_types = FALSE) %>%
    mutate(allocation = 1.5)
  write_csv(bad_mm, "user_data/manure_management.csv")

  # This should now reach the "Allocation Assertion" in your code
  expect_error(calculate_CH4_manure(saveoutput = FALSE), "Manure allocation exceeds 1.0")

  cleanup_manure_ch4_env()
})



test_that("calculate_CH4_manure detects invalid system combinations", {
  setup_manure_ch4_env()

  # Change the system to something that doesn't exist in ipcc_mm.csv
  invalid_mm <- read_csv("user_data/manure_management.csv", show_col_types = FALSE) %>%
    mutate(system_base = "imaginary_system")
  write_csv(invalid_mm, "user_data/manure_management.csv")

  # This targets the "Combinations Integrity Check" (Step 2.1)
  expect_error(calculate_CH4_manure(saveoutput = FALSE), "Invalid system/climate combinations")

  cleanup_manure_ch4_env()
})
