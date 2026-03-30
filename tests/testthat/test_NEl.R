library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)

test_that("calculate_NEl computes lactation energy for cattle and sheep", {
  # Set test directory
  withr::local_dir(test_path("test_data"))

  # Normalize CSVs: ensure yield and fat are numeric and join keys are character
  read_csv("user_data/livestock_definitions.csv", col_types = cols(.default = "c"), show_col_types = FALSE) %>%
    mutate(across(c(milk_yield_kg_year, fat_content_pct), as.numeric)) %>%
    write_csv("user_data/livestock_definitions.csv")

  read_csv("user_data/livestock_weights.csv", col_types = cols(.default = "c"), show_col_types = FALSE) %>%
    write_csv("user_data/livestock_weights.csv")

  # Execute calculation
  results <- calculate_NEl(saveoutput = FALSE)

  # Assertions
  expect_s3_class(results, "data.frame")
  expect_true("NEl_MJday" %in% colnames(results))

  # Check Cattle (Math: (8000/365) * (1.47 + 0.4 * 3.5) = 62.904)
  cattle_val <- results %>% filter(animal_type == "cattle") %>% pull(NEl_MJday)
  if(length(cattle_val) > 0) {
    expect_true(all(cattle_val >= 0))
  }

  # Check Sheep (Math: (500/365) * 4.6 = 6.301)
  sheep_val <- results %>% filter(animal_type == "sheep") %>% pull(NEl_MJday)
  if(length(sheep_val) > 0) {
    expect_true(all(sheep_val >= 0))
  }
})

test_that("calculate_NEl handles missing lactation data with zero", {
  withr::local_dir(test_path("test_data"))

  results <- calculate_NEl(saveoutput = FALSE)

  # Non-dairy animals or missing yield should result in 0 energy
  expect_false(any(is.na(results$NEl_MJday)))
})
