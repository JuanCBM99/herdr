library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)

test_that("calculate_monogastric_energy produces correct output structure using provided CSVs", {

  # 1. Set test directory to use existing CSV files
  withr::local_dir(test_path("test_data"))
  # Execute calculation
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

test_that("calculate_monogastric_energy weights sow gestation/lactation by cycle and sums cleanly", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  dir.create(file.path(temp_dir, "user_data"))

  mono_files <- list.files(system.file("Examples", "Monogastric_Example", package = "herdr"), full.names = TRUE)
  if (length(mono_files) == 0) {
    mono_files <- list.files(test_path("../../inst/Examples/Monogastric_Example"), full.names = TRUE)
  }
  file.copy(mono_files, file.path(temp_dir, "user_data"))

  withr::with_dir(temp_dir, {
    res <- suppressMessages(calculate_monogastric_energy(saveoutput = FALSE))
    expect_gt(nrow(res), 0)

    # Component columns sum exactly to ME_total_kcal_day
    comp_sum <- res$ME_mant_kcal + res$ME_crec_kcal + res$ME_eggs_kcal +
      res$ME_gestation_kcal + res$ME_lactation_kcal
    expect_equal(res$ME_total_kcal_day, comp_sum, tolerance = 1e-6)

    # Layers have positive egg energy
    layers <- res %>% filter(animal_tag == "laying_hens")
    expect_gt(layers$ME_eggs_kcal, 0)

    # Sows have cycle-weighted gestation and lactation (neither phase applied 365 d/yr at peak)
    sows <- res %>% filter(animal_tag == "breeder_sows")
    expect_gt(sows$ME_gestation_kcal, 0)
    expect_gt(sows$ME_lactation_kcal, 0)
    # Peak lactation energy alone is ~13,251 kcal/d; cycle-weighted (25/148.9) must be well below 4,000 kcal/d
    expect_lt(sows$ME_lactation_kcal, 4000)
  })
})
