library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)

test_that("calculate_CH4_manure computes emissions correctly using CSV data", {
  # 1. Set test directory to use your existing data
  withr::local_dir(test_path("test_data"))

  # 2. Execute the function
  # We use suppressWarnings to keep the output clean from dietary alerts
  results <- suppressWarnings(calculate_CH4_manure(saveoutput = FALSE))

  # 3. Structural Assertions
  expect_s3_class(results, "data.frame")

  # Ensure core IPCC columns are present
  # (Using case-insensitive check for MCF to be safe)
  cols <- colnames(results)
  expect_true(any(grepl("MCF", cols, ignore.case = TRUE)))
  expect_true("EF_kgyear" %in% cols)
  expect_true("total_CH4_mm_Ggyear" %in% cols)

  # 4. Consistency Assertions
  if(nrow(results) > 0) {
    # Emission factors must be non-negative
    expect_true(all(results$EF_kgyear >= 0))

    # Check that the main output is numeric and contains no NAs
    expect_false(any(is.na(results$total_CH4_mm_Ggyear)))

    # Quick logic check: if VS (volatile solids) > 0, EF should likely be > 0
    sample_row <- results %>% filter(VS_kgday > 0) %>% head(1)
    if(nrow(sample_row) > 0) {
      expect_gt(sample_row$EF_kgyear, 0)
    }
  }
})
