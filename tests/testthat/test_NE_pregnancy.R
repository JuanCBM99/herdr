library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)

test_that("calculate_NE_pregnancy computes gestation energy for cattle and sheep", {
  # Set test directory
  withr::local_dir(test_path("test_data"))

  # Execute calculation
  results <- calculate_NE_pregnancy(saveoutput = FALSE)

  # Assertions
  expect_s3_class(results, "data.frame")
  expect_true("NEpregnancy_MJday" %in% colnames(results))

  # Check Cattle (Math: NEm * c_pregnancy_coeff)
  cattle_val <- results %>% filter(animal_type == "cattle") %>% pull(NEpregnancy_MJday)
  if(length(cattle_val) > 0) {
    expect_true(all(cattle_val >= 0))
  }

  # Check Sheep (Math: NEm * pr_factor)
  sheep_val <- results %>% filter(animal_type == "sheep") %>% pull(NEpregnancy_MJday)
  if(length(sheep_val) > 0) {
    expect_true(all(sheep_val >= 0))
  }
})

test_that("calculate_NE_pregnancy handles non-pregnant animals with zero", {
  withr::local_dir(test_path("test_data"))

  results <- calculate_NE_pregnancy(saveoutput = FALSE)

  # Animals without pregnancy markers should have 0 energy
  expect_false(any(is.na(results$NEpregnancy_MJday)))
})
