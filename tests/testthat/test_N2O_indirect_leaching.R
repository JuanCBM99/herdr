library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)

test_that("calculate_N2O_indirect_leaching integrates correctly using CSV data", {
  # 1. Set test directory to use your existing data
  withr::local_dir(test_path("test_data"))

  # Execute Indirect Leaching calculation
  # This triggers: Leaching -> Direct N2O -> Nitrogen Balance -> GE -> Population
  results <- suppressWarnings(
    calculate_N2O_indirect_leaching(automatic_cycle = FALSE, saveoutput = FALSE)
  )

  # 4. Assertions: Structure
  expect_s3_class(results, "data.frame")

  # Check for core columns as defined in your function
  required_cols <- c("frac_leach", "EF5", "N_leaching_kg_year", "N2O_leach_kgyear")
  expect_true(all(required_cols %in% colnames(results)))

  # 5. Assertions: Mathematical Logic
  if(nrow(results) > 0) {
    # The leaching fraction (frac_leach) should be correctly joined from ipcc_mm.csv
    expect_false(any(is.na(results$frac_leach)))
    expect_false(any(is.na(results$EF5)))

    # Verify the stoichiometry: N2O = EF5 * N_leaching * (44/28)
    # We test the first row to ensure the conversion factor is applied
    res1 <- results[1, ]
    if(res1$N_leaching_kg_year > 0) {
      # Stoichiometric check for Leaching specifically:
      expected_leach_n2o <- res1$EF5 * res1$N_leaching_kg_year * (44 / 28)
      expect_equal(res1$N2O_leach_kgyear, expected_leach_n2o, tolerance = 0.001)
    }

    # Ensure results are non-negative
    expect_true(all(results$N2O_leach_kgyear >= 0))
  }
})
