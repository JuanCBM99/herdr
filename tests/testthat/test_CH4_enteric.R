library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)

test_that("calculate_emissions_enteric computes methane emissions using CSV data", {
  withr::local_dir(test_path("test_data"))

  results <- suppressWarnings(calculate_emissions_enteric(saveoutput = FALSE))

  expect_s3_class(results, "data.frame")

  # Filter out poultry because monogastric species do not produce enteric methane

  # Changed 'msg' to 'info' to comply with testthat syntax
  expect_false(any(is.na(results$Ym_pct)),
               info = "Error: Ym_pct is NA. Check if animal_type in CSV is 'cattle', 'sheep' or 'goat'.")

  expected_values <- c(6.5, 6,  5.7, 4.5, 3.0)
  expect_true(all(results$Ym_pct %in% expected_values),
              info = paste("Unexpected Ym values found:", paste(unique(results$Ym_pct), collapse = ", ")))
})

test_that("calculate_emissions_enteric calculates enteric methane for swine and zero for poultry", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  dir.create(file.path(temp_dir, "user_data"))
  mono_files <- list.files(system.file("Examples", "Monogastric_Example", package = "herdr"), full.names = TRUE)
  if (length(mono_files) == 0) {
    mono_files <- list.files("inst/Examples/Monogastric_Example", full.names = TRUE)
  }
  file.copy(mono_files, file.path(temp_dir, "user_data"))

  withr::with_dir(temp_dir, {
    res <- suppressWarnings(calculate_emissions_enteric(saveoutput = FALSE))

    expect_s3_class(res, "data.frame")

    # Poultry must have 0 enteric emissions
    poultry_res <- res %>% dplyr::filter(animal_type == "poultry")
    expect_true(all(poultry_res$EF_kgheadyear == 0))
    expect_true(all(poultry_res$total_CH4_enteric_Ggyear == 0))

    # Swine must have positive enteric emissions according to IPCC 2019 Table 10.10
    swine_res <- res %>% dplyr::filter(animal_type == "swine")
    expect_true(all(swine_res$EF_kgheadyear > 0))
    expect_true(all(swine_res$total_CH4_enteric_Ggyear > 0))

    # Fattening pigs (average liveweight (20 + 111.82)/2 = ~65.9 kg) EF should be ~1.40 kg CH4/year per IPCC Eq. 10.10
    fattening <- swine_res %>% dplyr::filter(animal_tag == "fattening_pigs")
    expect_equal(fattening$EF_kgheadyear, 1.40, tolerance = 0.05)
  })
})

