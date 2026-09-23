library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)

test_that("calculate_production executes and returns expected schema and non-negative values", {
  # 1. Create isolated temporary test environment
  temp_test_dir <- tempfile()
  dir.create(temp_test_dir)

  # Copy test data or user_data into temp environment
  source_data <- if (dir.exists(test_path("test_data/user_data"))) {
    test_path("test_data/user_data")
  } else {
    "user_data"
  }
  file.copy(from = source_data, to = temp_test_dir, recursive = TRUE)

  withr::local_dir(temp_test_dir)

  # 2. Execute calculate_production without writing to disk
  res <- suppressMessages(calculate_production(automatic_cycle = FALSE, saveoutput = FALSE))

  # 3. Structure assertions
  expect_s3_class(res, "data.frame")
  expect_true(nrow(res) > 0)

  expected_cols <- c(
    "region", "subregion", "animal_tag", "class_flex",
    "animal_type", "animal_subtype", "population", "N_exit",
    "milk_fresh_kg", "milk_FPCM_kg", "meat_live_weight_kg",
    "meat_carcass_weight_kg", "egg_fresh_kg", "wool_kg",
    "milk_protein_kg", "meat_protein_kg", "egg_protein_kg",
    "total_protein_kg"
  )

  for (col in expected_cols) {
    expect_true(col %in% colnames(res), info = paste("Missing expected column:", col))
  }

  # 4. Mathematical assertions
  expect_true(all(res$population >= 0, na.rm = TRUE))
  expect_true(all(res$N_exit >= 0, na.rm = TRUE))
  expect_true(all(res$milk_fresh_kg >= 0, na.rm = TRUE))
  expect_true(all(res$meat_live_weight_kg >= 0, na.rm = TRUE))
  expect_true(all(res$meat_carcass_weight_kg >= 0, na.rm = TRUE))
  expect_true(all(res$egg_fresh_kg >= 0, na.rm = TRUE))
  expect_true(all(res$wool_kg >= 0, na.rm = TRUE))
  expect_true(all(res$total_protein_kg >= 0, na.rm = TRUE))

  # Carcass weight must be strictly less than or equal to live weight
  expect_true(all(res$meat_carcass_weight_kg <= res$meat_live_weight_kg + 1e-6, na.rm = TRUE))

  # Total edible protein must match sum of individual protein pools within rounding tolerance
  protein_sum <- round(res$milk_protein_kg + res$meat_protein_kg + res$egg_protein_kg, 2)
  expect_equal(res$total_protein_kg, protein_sum, tolerance = 0.05)
})

test_that("calculate_production writes output to output/production.csv when saveoutput = TRUE", {
  temp_test_dir <- tempfile()
  dir.create(temp_test_dir)

  source_data <- if (dir.exists(test_path("test_data/user_data"))) {
    test_path("test_data/user_data")
  } else {
    "user_data"
  }
  file.copy(from = source_data, to = temp_test_dir, recursive = TRUE)

  withr::local_dir(temp_test_dir)

  res <- suppressMessages(calculate_production(automatic_cycle = FALSE, saveoutput = TRUE))

  expect_true(file.exists("output/production.csv"))
  saved_df <- readr::read_csv("output/production.csv", show_col_types = FALSE)
  expect_equal(nrow(saved_df), nrow(res))
})

test_that("calculate_production works under automatic_cycle = TRUE", {
  temp_test_dir <- tempfile()
  dir.create(temp_test_dir)

  source_data <- if (dir.exists(test_path("test_data/user_data"))) {
    test_path("test_data/user_data")
  } else {
    "user_data"
  }
  file.copy(from = source_data, to = temp_test_dir, recursive = TRUE)

  withr::local_dir(temp_test_dir)

  res_auto <- suppressMessages(suppressWarnings(
    calculate_production(automatic_cycle = TRUE, saveoutput = FALSE)
  ))

  expect_s3_class(res_auto, "data.frame")
  expect_true("total_protein_kg" %in% names(res_auto))
})

test_that("calculate_production correctly produces N_exit = 0 when replacement_rate is 0", {
  temp_test_dir <- tempfile()
  dir.create(temp_test_dir)

  source_data <- if (dir.exists(test_path("test_data/user_data"))) {
    test_path("test_data/user_data")
  } else {
    "user_data"
  }
  file.copy(from = source_data, to = temp_test_dir, recursive = TRUE)

  # Explicitly set replacement_rate to 0 for mature_dairy_cattle
  repro_file <- file.path(temp_test_dir, "user_data", "reproduction_parameters.csv")
  repro_df <- readr::read_csv(repro_file, show_col_types = FALSE)
  repro_df <- repro_df %>%
    dplyr::mutate(value = dplyr::if_else(animal_tag == "mature_dairy_cattle" & parameter == "replacement_rate", 0, value))
  readr::write_csv(repro_df, repro_file)

  res_zero <- suppressMessages(calculate_production(
    automatic_cycle = FALSE,
    saveoutput = FALSE,
    data_dir = file.path(temp_test_dir, "user_data")
  ))

  dairy_row <- res_zero %>% dplyr::filter(animal_tag == "mature_dairy_cattle")
  expect_true(nrow(dairy_row) > 0)
  expect_equal(dairy_row$N_exit[1], 0)
  expect_equal(dairy_row$meat_live_weight_kg[1], 0)
  expect_equal(dairy_row$meat_carcass_weight_kg[1], 0)
  # Milk production should still be positive
  expect_gt(dairy_row$milk_fresh_kg[1], 0)
})

test_that("calculate_production succeeds gracefully when optional tables are missing", {
  temp_test_dir <- tempfile()
  dir.create(temp_test_dir)

  source_data <- if (dir.exists(test_path("test_data/user_data"))) {
    test_path("test_data/user_data")
  } else {
    "user_data"
  }
  file.copy(from = source_data, to = temp_test_dir, recursive = TRUE)

  # Remove optional reproduction_parameters and monogastric_definitions
  unlink(file.path(temp_test_dir, "user_data", "reproduction_parameters.csv"))
  unlink(file.path(temp_test_dir, "user_data", "monogastric_definitions.csv"))

  res_missing <- suppressMessages(calculate_production(
    automatic_cycle = FALSE,
    saveoutput = FALSE,
    data_dir = file.path(temp_test_dir, "user_data")
  ))

  expect_s3_class(res_missing, "data.frame")
  expect_true("milk_fresh_kg" %in% names(res_missing))
})
