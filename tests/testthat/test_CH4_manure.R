library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)

test_that("calculate_CH4_manure computes emissions correctly using CSV data", {
  withr::local_dir(test_path("test_data"))

  # Execute the function safely
  results <- suppressWarnings(calculate_CH4_manure(saveoutput = FALSE))

  expect_s3_class(results, "data.frame")

  # Ensure core columns are present in the final dataset output
  cols <- colnames(results)
  expect_true(any(grepl("MCF", cols, ignore.case = TRUE)))
  expect_true("EF_kgyear" %in% cols)
  expect_true("total_CH4_mm_kgyear" %in% cols)

  if (nrow(results) > 0) {
    expect_true(all(results$EF_kgyear >= 0, na.rm = TRUE))
    expect_false(any(is.na(results$total_CH4_mm_kgyear)))

    # Only run the positive check on rows that computed successfully
    sample_row <- results %>%
      dplyr::filter(VS_kgday > 0 & !is.na(EF_kgyear)) %>%
      head(1)

    if (nrow(sample_row) > 0) {
      expect_gt(sample_row$EF_kgyear, 0)
    }
  }
})

test_that("calculate_CH4_manure respects explicit data_dir parameter", {
  custom_dir <- test_path("test_data", "user_data")
  res <- suppressWarnings(calculate_CH4_manure(data_dir = custom_dir, saveoutput = FALSE))
  expect_s3_class(res, "data.frame")
  expect_true("total_CH4_mm_kgyear" %in% names(res))
  if (nrow(res) > 0) {
    expect_true(all(res$total_CH4_mm_kgyear >= 0, na.rm = TRUE))
  }
})
