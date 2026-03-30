library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)

test_that("calculate_land_use computes m2 requirements correctly using CSV data", {
  # 1. Set test directory
  withr::local_dir(test_path("test_data"))

  # 2. Normalize and prepare input CSVs
  # Land use depends on yields, mapping, and the entire nutrition chain
  files_to_fix <- c(
    "fao_crop_yields.csv", "forage_yields.csv", "mapping.csv",
    "diet_profiles.csv", "diet_ingredients.csv", "feed_characteristics.csv",
    "livestock_definitions.csv", "livestock_weights.csv",
    "livestock_census.csv", "ipcc_coefficients.csv", "reproduction_parameters.csv"
  )

  for (f in files_to_fix) {
    path <- file.path("user_data", f)
    if (file.exists(path)) {
      read_csv(path, col_types = cols(.default = "c"), show_col_types = FALSE) %>%
        mutate(across(everything(), trimws)) %>%
        write_csv(path)
    }
  }

  # 3. Execute land use calculation
  # We specify the country that exists in your fao_crop_yields.csv (e.g., "Spain")
  results <- suppressWarnings(
    calculate_land_use(crop_yield_country = "Spain", saveoutput = FALSE)
  )

  # 4. Assertions: Structure
  expect_s3_class(results, "data.frame")
  expect_true("land_use_per_animal_m2" %in% colnames(results))
  expect_true("total_land_use_m2" %in% colnames(results))

  # 5. Assertions: Biological and Mathematical Logic
  if(nrow(results) > 0) {
    # Land use must be positive (it's physically impossible to occupy negative space)
    expect_true(all(results$land_use_per_animal_m2 >= 0))

    # Check that total land use is consistent with population
    # Total = per_animal * population
    sample <- results[1, ]
    if(sample$population > 0) {
      calc_total <- sample$land_use_per_animal_m2 * sample$population
      expect_equal(sample$total_land_use_m2, calc_total, tolerance = 0.01)
    }
  }

  # 6. Assertion: Error Handling
  # The function should stop if the country is not in the FAO database
  expect_error(
    calculate_land_use(crop_yield_country = "Mars", saveoutput = FALSE),
    "must be a valid area"
  )
})
