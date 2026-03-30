library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)

test_that("calculate_N2O_indirect_volatilization completes the N cycle using CSV data", {
  # 1. Set test directory to use your existing data
  withr::local_dir(test_path("test_data"))

  # 2. Normalize input CSVs
  # This ensures that keys for joining (animal_type, systems, etc.) are clean
  files_to_fix <- c(
    "livestock_definitions.csv", "livestock_weights.csv",
    "livestock_census.csv", "manure_management.csv",
    "ipcc_mm.csv", "diet_profiles.csv",
    "diet_ingredients.csv", "feed_characteristics.csv",
    "ipcc_coefficients.csv"
  )

  for (f in files_to_fix) {
    path <- file.path("user_data", f)
    if (file.exists(path)) {
      read_csv(path, col_types = cols(.default = "c"), show_col_types = FALSE) %>%
        mutate(across(everything(), trimws)) %>%
        write_csv(path)
    }
  }

  # 3. Execute Indirect Volatilization calculation
  # This internally triggers calculate_N2O_direct_manure -> calculate_ge -> etc.
  results <- suppressWarnings(
    calculate_N2O_indirect_volatilization(automatic_cycle = FALSE, saveoutput = FALSE)
  )

  # 4. Assertions: Structure
  expect_s3_class(results, "data.frame")

  # Check for mandatory IPCC columns for Indirect N2O
  required_cols <- c("frac_gas", "EF4", "N_volatilization_kg_year", "N2O_vol_kgyear")
  expect_true(all(required_cols %in% colnames(results)))

  # 5. Assertions: Logic
  if(nrow(results) > 0) {
    # Volatilization cannot be negative
    expect_true(all(results$N_volatilization_kg_year >= 0))

    # Check that factors were correctly joined from ipcc_mm.csv
    # frac_gas is typically between 0.1 and 0.48 depending on the system
    expect_false(any(is.na(results$frac_gas)))
    expect_false(any(is.na(results$EF4)))

    # If there is nitrogen excretion, there must be some volatilization
    sample_active <- results %>% filter(N_excreted_kgheadday > 0) %>% head(1)
    if(nrow(sample_active) > 0) {
      expect_gt(sample_active$N_volatilization_kg_year, 0)
      expect_gt(sample_active$N2O_vol_kgyear, 0)
    }

    # Mathematical consistency: N2O = EF4 * N_vol * (44/28)
    # Testing with the first row
    res1 <- results[1, ]
    expected_n2o <- res1$EF4 * res1$N_volatilization_kg_year * (44 / 28)
    expect_equal(res1$N2O_vol_kgyear, expected_n2o, tolerance = 0.001)
  }
})
