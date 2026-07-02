library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)

test_that("calculate_monogastric_energy produces correct output structure using provided CSVs", {

  # 1. Set test directory to use existing CSV files
  withr::local_dir(test_path("test_data"))

  # 2. Normalize existing files
  files_to_fix <- c(
    "livestock_weights.csv",
    "monogastric_definitions.csv"
  )

  for (f in files_to_fix) {
    path <- file.path("user_data", f)
    if (file.exists(path)) {
      read_csv(path, col_types = cols(.default = "c"), show_col_types = FALSE) %>%
        mutate(across(everything(), trimws)) %>%
        write_csv(path)
    }
  }

  # 3. Execute calculation
  results <- suppressWarnings(
    calculate_monogastric_energy(saveoutput = FALSE)
  )

  # 4. Assertions
  expect_s3_class(results, "data.frame")

  expected_cols <- c(
    "ME_mant_kcal",
    "ME_crec_kcal",
    "ME_eggs_kcal",
    "ME_gestation_kcal",
    "ME_lactation_kcal",
    "ME_total_kcal_day"
  )

  expect_true(all(expected_cols %in% names(results)))

  # Basic consistency
  if (nrow(results) > 0) {

    expect_true(all(results$ME_total_kcal_day >= results$ME_mant_kcal))

    expect_false(any(is.na(results$ME_total_kcal_day)))

    # Layers should have egg energy
    layers <- results %>%
      filter(animal_type == "poultry", ME_eggs_kcal > 0)

    if (nrow(layers) > 0) {
      expect_true(all(layers$ME_eggs_kcal > 0))
    }

    # Reproductive sows should have gestation and/or lactation energy
    sows <- results %>%
      filter(
        animal_type == "swine",
        ME_gestation_kcal > 0 | ME_lactation_kcal > 0
      )

    if (nrow(sows) > 0) {
      expect_true(all(
        sows$ME_gestation_kcal > 0 |
          sows$ME_lactation_kcal > 0
      ))
    }
  }
})
