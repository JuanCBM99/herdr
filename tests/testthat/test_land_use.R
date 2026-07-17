library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)

# ---------------------------------------------------------------------------
# 1. Happy path: structure and basic sanity of the output
# ---------------------------------------------------------------------------
test_that("calculate_land_use produces correct output structure using provided CSVs", {

  withr::local_dir(test_path("test_data"))

  files_to_fix <- c(
    "livestock_weights.csv",
    "livestock_definitions.csv",
    "ipcc_coefficients.csv",
    "diet_profiles.csv",
    "diet_ingredients.csv",
    "feed_characteristics.csv",
    "mapping.csv",
    "fao_crop_yields.csv",
    "forage_yields.csv",
    "fao_production.csv"
  )

  for (f in files_to_fix) {
    path <- file.path("user_data", f)
    if (file.exists(path)) {
      read_csv(path, col_types = cols(.default = "c"), show_col_types = FALSE) %>%
        mutate(across(everything(), trimws)) %>%
        write_csv(path)
    }
  }

  diet_ing_path <- file.path("user_data", "diet_ingredients.csv")
  parquet_path  <- file.path("user_data", "fao_trade_matrix.parquet")
  prod_path     <- file.path("user_data", "fao_production.csv")

  if (file.exists(diet_ing_path)) {
    diet_ing <- read_csv(diet_ing_path, show_col_types = FALSE)
    needs_fao_engine   <- any(is.na(diet_ing$country_of_origin))
    missing_fao_inputs <- needs_fao_engine && (!file.exists(parquet_path) || !file.exists(prod_path))
    skip_if(missing_fao_inputs, "Missing country_of_origin values and no local FAO trade inputs; skipping.")
  }

  results <- suppressWarnings(suppressMessages(
    calculate_land_use(automatic_cycle = FALSE, saveoutput = FALSE)
  ))

  expect_s3_class(results, "data.frame")

  expected_cols <- c(
    "region", "subregion", "animal_tag", "class_flex",
    "ingredient", "country_of_origin", "animal_type", "animal_subtype",
    "population", "dm_yield",
    "land_use_per_animal_m2", "total_land_use_m2"
  )
  expect_true(all(expected_cols %in% colnames(results)))

  if (nrow(results) > 0) {
    expect_true(all(results$land_use_per_animal_m2 >= 0, na.rm = TRUE))
    expect_true(all(results$total_land_use_m2 >= 0, na.rm = TRUE))
    expect_false(any(is.na(results$land_use_per_animal_m2)))
  }
})

# ---------------------------------------------------------------------------
# 2. FAO engine branch: triggered when country_of_origin has NA
# ---------------------------------------------------------------------------
test_that("calculate_land_use triggers FAO engine when country_of_origin has NA", {

  withr::local_dir(test_path("test_data"))

  diet_ing_path <- file.path("user_data", "diet_ingredients.csv")
  parquet_path  <- file.path("user_data", "fao_trade_matrix.parquet")
  prod_path     <- file.path("user_data", "fao_production.csv")

  skip_if_not(file.exists(diet_ing_path))
  skip_if(!file.exists(parquet_path) || !file.exists(prod_path),
          "Need local parquet + fao_production.csv to test FAO engine branch without downloading.")

  original <- read_csv(diet_ing_path, show_col_types = FALSE)
  withr::defer(write_csv(original, diet_ing_path))

  modified <- original
  modified$country_of_origin[1] <- NA
  write_csv(modified, diet_ing_path)

  results <- suppressWarnings(suppressMessages(
    calculate_land_use(saveoutput = FALSE, farm_country = "Spain", year = 2022)
  ))

  expect_s3_class(results, "data.frame")
  expect_true(nrow(results) > 0)
})


# ---------------------------------------------------------------------------
# 3. Warning: missing yield for an ingredient/origin combination
# ---------------------------------------------------------------------------
test_that("calculate_land_use warns when yield data is missing for an ingredient/origin combo", {

  withr::local_dir(test_path("test_data"))

  mapping_path <- file.path("user_data", "mapping.csv")
  skip_if_not(file.exists(mapping_path))

  original <- read_csv(mapping_path, show_col_types = FALSE)
  withr::defer(write_csv(original, mapping_path))

  broken <- original
  broken$yield_name[1] <- "NONEXISTENT_CROP_XYZ"
  write_csv(broken, mapping_path)

  expect_warning(
    suppressMessages(calculate_land_use(saveoutput = FALSE)),
    "Missing yield for"
  )
})


