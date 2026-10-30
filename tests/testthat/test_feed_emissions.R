library(testthat)
library(herdr)
library(dplyr)
library(withr)

test_that("calculate_feed_emissions calculates footprints accurately", {
  # 1. Isolated test environment
  temp_test_dir <- tempfile()
  dir.create(temp_test_dir)
  file.copy(from = test_path("test_data/user_data"), to = temp_test_dir, recursive = TRUE)
  withr::local_dir(temp_test_dir)

  res <- suppressWarnings(calculate_feed_emissions(saveoutput = FALSE))

  expect_s3_class(res, "data.frame")
  expect_true(nrow(res) > 0)

  # Check columns
  expected_cols <- c(
    "region", "subregion", "animal_tag", "animal_type", "animal_subtype", "population",
    "feed_dmi_kg_year", "feed_fresh_kg_year",
    "feed_CO2e_total_kg_year", "feed_CO2e_fossil_kg_year", "feed_CO2e_luc_kg_year",
    "feed_water_m3_year", "feed_CO2e_per_head_year", "feed_water_per_head_year"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(res), info = paste("Missing column:", col))
  }

  # Verify values are positive and reasonable
  expect_true(all(res$feed_CO2e_total_kg_year >= 0))
  expect_true(all(res$feed_water_m3_year >= 0))
  expect_true(all(res$feed_CO2e_per_head_year >= 0))

  # Test in-memory injection
  dmi_mem <- suppressWarnings(suppressMessages(calculate_DMI(saveoutput = FALSE)))
  pop_mem <- suppressMessages(calculate_population(saveoutput = FALSE))

  res_mem <- suppressWarnings(calculate_feed_emissions(saveoutput = FALSE, DMI_df = dmi_mem, population_df = pop_mem))
  expect_equal(nrow(res_mem), nrow(res))
  expect_equal(res_mem$feed_CO2e_total_kg_year, res$feed_CO2e_total_kg_year)
})
