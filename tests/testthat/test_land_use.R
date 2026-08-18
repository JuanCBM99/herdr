library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)
library(arrow)

# ==============================================================================
# TEST 1: WITHOUT NAs (Does not trigger the FAO engine)
# ==============================================================================
test_that("calculate_land_use computes m2 safely without triggering FAO engine", {
  temp_test_dir <- tempfile()
  dir.create(temp_test_dir)
  file.copy(from = test_path("test_data/user_data"), to = temp_test_dir, recursive = TRUE)
  withr::local_dir(temp_test_dir)

  dir.create("user_data", showWarnings = FALSE)

  dummy_crops <- data.frame(Area = "Spain", Item = "Maize", Element = "Yield", Y2022 = 10)
  arrow::write_parquet(dummy_crops, "user_data/fao_crops.parquet")

  dummy_forages <- data.frame(Area = "Spain", Item = "Alfalfa", Yield = 5)
  arrow::write_parquet(dummy_forages, "user_data/fao_forages.parquet")

  path_diet <- "user_data/diet_ingredients.csv"
  if (file.exists(path_diet)) {
    df <- read_csv(path_diet, col_types = cols(.default = "c"), show_col_types = FALSE)

    if (!"country_of_origin" %in% names(df)) {
      df <- df %>% mutate(country_of_origin = "Spain")
    } else {
      df <- df %>% mutate(country_of_origin = ifelse(is.na(country_of_origin), "Spain", country_of_origin))
    }

    if (!"custom_yield_kg_ha" %in% names(df)) {
      df <- df %>% mutate(custom_yield_kg_ha = NA_character_)
    }

    write_csv(df, path_diet)
  }

  results <- suppressWarnings(calculate_land_use(farm_country = "Spain", year = 2022, saveoutput = FALSE))
  expect_s3_class(results, "data.frame")
})

# ==============================================================================
# TEST 2: WITH NAs (Forces the FAO engine using a dynamic Parquet Mock)
# ==============================================================================
test_that("calculate_land_use handles NA origins via FAO engine safely (Parquet Mock)", {
  temp_test_dir <- tempfile()
  dir.create(temp_test_dir)
  file.copy(from = test_path("test_data/user_data"), to = temp_test_dir, recursive = TRUE)
  withr::local_dir(temp_test_dir)

  dir.create("user_data", showWarnings = FALSE)

  path_diet <- "user_data/diet_ingredients.csv"
  if (file.exists(path_diet)) {
    df <- read_csv(path_diet, col_types = cols(.default = "c"), show_col_types = FALSE)
    df$country_of_origin[1] <- NA

    if (!"custom_yield_kg_ha" %in% names(df)) {
      df <- df %>% mutate(custom_yield_kg_ha = NA_character_)
    }

    write_csv(df, path_diet)
  }

  map_path <- "user_data/mapping.csv"
  real_item <- "Maize"
  if (file.exists(map_path)) {
    map_df <- read_csv(map_path, col_types = cols(.default = "c"), show_col_types = FALSE)
    valid_items <- na.omit(map_df$yield_name)
    if (length(valid_items) > 0) real_item <- valid_items[1]
  }

  dummy_crops <- data.frame(
    Area = c("Spain", "Spain"),
    Item = c(real_item, real_item),
    Element = c("Yield", "Production"),
    Y2022 = c(10, 1000)
  )
  arrow::write_parquet(dummy_crops, "user_data/fao_crops.parquet")

  dummy_forages <- data.frame(Area = "Spain", Item = "Alfalfa", Yield = 5)
  arrow::write_parquet(dummy_forages, "user_data/fao_forages.parquet")

  dummy_trade <- data.frame(
    `Reporter Countries` = "Spain",
    `Partner Countries` = "France",
    Item = real_item,
    Element = c("Import quantity", "Export quantity"),
    Y2022 = c(500, 100),
    check.names = FALSE
  )
  arrow::write_parquet(dummy_trade, "user_data/fao_trade_matrix.parquet")

  results <- suppressWarnings(calculate_land_use(farm_country = "Spain", year = 2022, saveoutput = FALSE))

  expect_s3_class(results, "data.frame")
  expect_true("land_use_per_animal_m2" %in% colnames(results))
})

# ==============================================================================
# TEST 3: WARNINGS (Using a fake country to test missing yields safely)
# ==============================================================================
test_that("calculate_land_use throws correct warnings for missing yields", {
  temp_test_dir <- tempfile()
  dir.create(temp_test_dir)
  file.copy(from = test_path("test_data/user_data"), to = temp_test_dir, recursive = TRUE)
  withr::local_dir(temp_test_dir)

  dir.create("user_data", showWarnings = FALSE)

  dummy_crops <- data.frame(Area = "Spain", Item = "Maize", Element = "Yield", Y2022 = 10)
  arrow::write_parquet(dummy_crops, "user_data/fao_crops.parquet")

  dummy_forages <- data.frame(Area = "Spain", Item = "Alfalfa", Yield = 5)
  arrow::write_parquet(dummy_forages, "user_data/fao_forages.parquet")

  path_diet <- "user_data/diet_ingredients.csv"
  if (file.exists(path_diet)) {
    df <- read_csv(path_diet, col_types = cols(.default = "c"), show_col_types = FALSE) %>%
      mutate(country_of_origin = "Mars")

    if (!"custom_yield_kg_ha" %in% names(df)) {
      df <- df %>% mutate(custom_yield_kg_ha = NA_character_)
    }

    write_csv(df, path_diet)
  }

  warns <- capture_warnings(calculate_land_use(farm_country = "Spain", year = 2022, saveoutput = FALSE))

  expect_true(any(grepl("Missing yield", warns)))
})
