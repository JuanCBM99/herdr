library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)

test_that("calculate_DMI produces correct output structure using provided CSVs", {
  # 1. Set test directory to use existing CSV files
  withr::local_dir(test_path("test_data"))

  # 2. Normalize existing files
  files_to_fix <- c(
    "livestock_weights.csv",
    "livestock_definitions.csv",
    "ipcc_coefficients.csv",
    "diet_profiles.csv",
    "diet_ingredients.csv",
    "feed_characteristics.csv"
  )

  for (f in files_to_fix) {
    path <- file.path("user_data", f)
    if (file.exists(path)) {
      read_csv(path, col_types = cols(.default = "c"), show_col_types = FALSE) %>%
        mutate(across(everything(), trimws)) %>%
        write_csv(path)
    }
  }

  # 3. Execute DMI calculation
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
