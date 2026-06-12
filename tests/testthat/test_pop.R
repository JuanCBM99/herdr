library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)

test_that("calculate_population automatic cycle works with cleaned test data", {
  # Set temporary directory context for the execution of this test block
  withr::local_dir(test_path("test_data"))

  # Ensure your test_path("test_data/user_data/") directory contains:
  # - livestock_census.csv
  # - livestock_definitions.csv
  # - monogastric_definitions.csv
  # - reproduction_parameters.csv

  # Run orchestration
  results <- calculate_population(
    automatic_cycle = TRUE,
    saveoutput = FALSE
  )

  # Assertions
  expect_s3_class(results, "data.frame")
  expect_true("population" %in% colnames(results))
  expect_true(any(grepl("calves", results$animal_tag, ignore.case = TRUE)))
  expect_false(any(is.na(results$animal_type)))
  expect_false(any(is.na(results$animal_subtype)))

  # Ensure poultry carries over safely in automatic mode from the census asset
  if (any(results$animal_type == "poultry")) {
    poultry_results <- results %>% dplyr::filter(animal_type == "poultry")
    expect_true(all(poultry_results$population > 0))
  }
})
