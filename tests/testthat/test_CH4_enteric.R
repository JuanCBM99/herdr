library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)

test_that("calculate_emissions_enteric computes methane emissions using CSV data", {
  withr::local_dir(test_path("test_data"))

  results <- calculate_emissions_enteric(saveoutput = FALSE)

  expect_s3_class(results, "data.frame")

  # Filter out poultry because monogastric species do not produce enteric methane
  ruminant_results <- results %>% dplyr::filter(tolower(animal_type) != "poultry")

  # Changed 'msg' to 'info' to comply with testthat syntax
  expect_false(any(is.na(ruminant_results$Ym_pct)),
               info = "Error: Ym_pct is NA. Check if animal_type in CSV is 'cattle', 'sheep' or 'goat'.")

  expected_values <- c(6.5, 5.7, 4.5, 3.0)
  expect_true(all(ruminant_results$Ym_pct %in% expected_values),
              info = paste("Unexpected Ym values found:", paste(unique(ruminant_results$Ym_pct), collapse = ", ")))
})
