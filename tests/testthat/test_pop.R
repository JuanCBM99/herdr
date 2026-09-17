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
  # - ruminant_definitions.csv
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

test_that("calculate_population populates animal_type and animal_subtype even when offspring are missing in definitions", {
  withr::local_dir(test_path("test_data"))

  # Run automatic cycle
  results <- calculate_population(automatic_cycle = TRUE, saveoutput = FALSE)

  # Check calves specifically
  calves <- results %>% dplyr::filter(grepl("calv|yearl", animal_tag))
  expect_true(nrow(calves) > 0)
  expect_true(all(calves$animal_type == "cattle"))
  expect_true(all(!is.na(calves$animal_subtype)))
  expect_true(all(calves$animal_subtype %in% c("dairy", "beef", "feedlot")))
})

test_that("calculate_population retrieves birth/calving rates directly from definitions", {
  temp_dir <- tempfile()
  dir.create(file.path(temp_dir, "user_data"), recursive = TRUE)
  file.copy(list.files(test_path("test_data/user_data"), full.names = TRUE), file.path(temp_dir, "user_data"))

  # Keep ONLY replacement_rate in reproduction_parameters.csv
  repro_path <- file.path(temp_dir, "user_data/reproduction_parameters.csv")
  repro <- read_csv(repro_path, show_col_types = FALSE) %>%
    filter(parameter == "replacement_rate")
  write_csv(repro, repro_path)

  withr::with_dir(temp_dir, {
    res <- calculate_population(automatic_cycle = TRUE, saveoutput = FALSE)
    expect_s3_class(res, "data.frame")
    expect_true("population" %in% names(res))
    expect_true(any(grepl("calves", res$animal_tag)))
    # Dairy calves female replacement should match replacement_rate (0.27 * 848686)
    repl <- res %>% filter(animal_tag == "dairy_calves_female_replacement") %>% pull(population)
    expect_equal(round(repl, 2), 229145.22)
  })
})


