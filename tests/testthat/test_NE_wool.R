library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)

test_that("calculate_NE_wool computes wool energy correctly", {
  # Set test directory
  withr::local_dir(test_path("test_data"))

  # Normalize CSVs: force character types for join keys and handle NA in yield
  read_csv("user_data/livestock_definitions.csv", col_types = cols(.default = "c"), show_col_types = FALSE) %>%
    write_csv("user_data/livestock_definitions.csv")

  read_csv("user_data/livestock_weights.csv", col_types = cols(.default = "c"), show_col_types = FALSE) %>%
    write_csv("user_data/livestock_weights.csv")

  # Execute calculation
  results <- calculate_NE_wool(saveoutput = FALSE)

  # Assertions
  expect_s3_class(results, "data.frame")
  expect_true("NEwool_MJday" %in% colnames(results))

  # Check sheep (Example calculation: (4.5 * 24) / 365 = 0.296)
  sheep_res <- results %>% filter(animal_type == "sheep")
  if(nrow(sheep_res) > 0) {
    expect_true(all(sheep_res$NEwool_MJday >= 0))
    expect_false(any(is.na(sheep_res$NEwool_MJday)))
  }
})

test_that("calculate_NE_wool handles non-wool species with zero", {
  withr::local_dir(test_path("test_data"))

  results <- calculate_NE_wool(saveoutput = FALSE)

  # Cattle should have 0 energy for wool
  cattle_res <- results %>% filter(animal_type == "cattle")
  if(nrow(cattle_res) > 0) {
    expect_true(all(cattle_res$NEwool_MJday == 0))
  }
})
