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

  expected_values <- c(7.0, 6.7, 6.5, 6.3, 6.0, 5.7, 5.5, 4.0, 3.0, 0)
  expect_true(all(results$Ym_pct %in% expected_values),
              info = paste("Unexpected Ym values found:", paste(unique(results$Ym_pct), collapse = ", ")))
})

test_that("calculate_emissions_enteric enforces IPCC 2019 Table 10.12 NDF<=35 boundary for lactating dairy cows", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  dir.create(file.path(temp_dir, "user_data"))
  dairy_files <- list.files(system.file("Examples", "Level1_Spain_Dairy_Cattle_2015", package = "herdr"), full.names = TRUE)
  if (length(dairy_files) == 0) {
    dairy_files <- list.files("inst/Examples/Level1_Spain_Dairy_Cattle_2015", full.names = TRUE)
  }
  file.copy(dairy_files, file.path(temp_dir, "user_data"))

  withr::with_dir(temp_dir, {
    diet_lo <- tibble::tibble(
      farm_id = "farm_1", region = "spain", subregion = NA_character_,
      animal_tag = "mature_dairy_cattle", animal_type = "cattle", animal_subtype = "dairy",
      DE_pct = 72, NDF_pct = 35.0
    )
    diet_hi <- dplyr::mutate(diet_lo, NDF_pct = 35.1)
    ge_stub <- tibble::tibble(
      farm_id = "farm_1", region = "spain", subregion = NA_character_,
      animal_tag = "mature_dairy_cattle", animal_type = "cattle", animal_subtype = "dairy",
      GE_MJday = 300
    )
    pop_stub <- tibble::tibble(
      farm_id = "farm_1", region = "spain", subregion = NA_character_,
      animal_tag = "mature_dairy_cattle", animal_type = "cattle", animal_subtype = "dairy",
      population = 100
    )

    res_lo <- suppressMessages(calculate_emissions_enteric(saveoutput = FALSE, diet_vars = diet_lo, ge_df = ge_stub, pop_df = pop_stub))
    res_hi <- suppressMessages(calculate_emissions_enteric(saveoutput = FALSE, diet_vars = diet_hi, ge_df = ge_stub, pop_df = pop_stub))
    expect_equal(res_lo$Ym_pct, 5.7)
    expect_equal(res_hi$Ym_pct, 6.0)
  })
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

    # IPCC 2019 Table 10.10 + Eq. 10.19: Tier 1 swine factor is fixed at 1.5 kg CH4/head/year
    # (high productivity systems), independent of liveweight
    expect_true(all(swine_res$EF_kgheadyear == 1.5))
    fattening <- swine_res %>% dplyr::filter(animal_tag == "fattening_pigs")
    expect_equal(fattening$EF_kgheadyear, 1.5)
  })
})

