library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)

test_that("calculate_DMI produces correct output structure using provided CSVs", {
  # 1. Set test directory to use existing CSV files
  withr::local_dir(test_path("test_data"))

  # Execute DMI calculation
  results <- suppressWarnings(calculate_DMI(saveoutput = FALSE))

  # 4. Assertions
  expect_s3_class(results, "data.frame")

  # Check for columns as defined in your function (Note: DMI_kgday without extra underscore)
  expect_true("DMI_kgday" %in% colnames(results))
  expect_true("DMI_bw_pct" %in% colnames(results))

  # Basic consistency checks
  if(nrow(results) > 0) {
    expect_true(all(results$DMI_kgday >= 0))
    expect_false(any(is.na(results$DMI_kgday)))
  }
})
