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

    df <- df %>% mutate(custom_yield_kg_ha = "5000")

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

    df <- df %>% mutate(custom_yield_kg_ha = "5000")
    df$custom_yield_kg_ha[1] <- NA

    write_csv(df, path_diet)
  }

  map_path <- "user_data/mapping.csv"
  real_item <- "Maize (corn)"
  if (file.exists(map_path)) {
    map_df <- read_csv(map_path, col_types = cols(.default = "c"), show_col_types = FALSE)
    match_item <- map_df$yield_name[map_df$ingredient == df$ingredient[1]]
    if (length(match_item) > 0 && !is.na(match_item[1])) real_item <- match_item[1]
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
# TEST 3: WARNING DISCLAIMER (Using missing country yields without halting)
# ==============================================================================
test_that("calculate_land_use emits warning disclaimer for missing yields without halting", {
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

  expect_warning(
    res <- calculate_land_use(farm_country = "Spain", year = 2022, saveoutput = FALSE),
    "Land use disclaimer"
  )
  expect_s3_class(res, "data.frame")
})

test_that("calculate_land_use uses Spanish forage yield as proxy fallback for other countries with warning", {
  temp_test_dir <- tempfile()
  dir.create(temp_test_dir)
  file.copy(from = test_path("test_data/user_data"), to = temp_test_dir, recursive = TRUE)
  withr::local_dir(temp_test_dir)

  dir.create("user_data", showWarnings = FALSE)

  dummy_crops <- data.frame(
    Area = rep(c("France", "Spain"), each = 4),
    Item = rep(c("Maize (corn)", "Wheat", "Soya beans", "Rape or colza seed"), 2),
    Element = "Yield",
    Y2022 = 10
  )
  arrow::write_parquet(dummy_crops, "user_data/fao_crops.parquet")

  dummy_forages <- data.frame(
    Area = c("Spain", "Spain", "Spain"),
    Item = c("Maize for forage and silage", "Winter cereals, for forage", "Other grasses, for forage"),
    Yield = c(9000, 3000, 4500)
  )
  arrow::write_parquet(dummy_forages, "user_data/fao_forages.parquet")

  path_diet <- "user_data/diet_ingredients.csv"
  if (file.exists(path_diet)) {
    df <- read_csv(path_diet, col_types = cols(.default = "c"), show_col_types = FALSE) %>%
      mutate(country_of_origin = "France", custom_yield_kg_ha = NA_character_)
    write_csv(df, path_diet)
  }

  expect_warning(
    calculate_land_use(farm_country = "France", year = 2022, saveoutput = FALSE),
    "Forage yields disclaimer"
  )
})

test_that("calculate_land_use works with normalized diets without region/subregion/class_flex", {
  temp_test_dir <- tempfile()
  dir.create(temp_test_dir)
  file.copy(from = test_path("test_data/user_data"), to = temp_test_dir, recursive = TRUE)
  withr::local_dir(temp_test_dir)

  dir.create("user_data", showWarnings = FALSE)

  dummy_crops <- data.frame(Area = "Spain", Item = "Maize", Element = "Yield", Y2022 = 10)
  arrow::write_parquet(dummy_crops, "user_data/fao_crops.parquet")

  dummy_forages <- data.frame(Area = "Spain", Item = "Alfalfa", Yield = 5)
  arrow::write_parquet(dummy_forages, "user_data/fao_forages.parquet")

  # Strip geographic columns from diet tables
  path_prof <- "user_data/diet_profiles.csv"
  path_diet <- "user_data/diet_ingredients.csv"
  if (file.exists(path_prof) && file.exists(path_diet)) {
    read_csv(path_prof, col_types = cols(.default = "c"), show_col_types = FALSE) %>%
      dplyr::select(-dplyr::any_of(c("region", "subregion", "class_flex"))) %>%
      write_csv(path_prof)

    read_csv(path_diet, col_types = cols(.default = "c"), show_col_types = FALSE) %>%
      dplyr::select(-dplyr::any_of(c("region", "subregion", "class_flex"))) %>%
      mutate(custom_yield_kg_ha = "5000", country_of_origin = "Spain") %>%
      write_csv(path_diet)
  }

  results <- suppressWarnings(calculate_land_use(farm_country = "Spain", year = 2022, saveoutput = FALSE))
  expect_s3_class(results, "data.frame")
  expect_true("land_use_per_animal_m2" %in% names(results))
  expect_true(nrow(results) > 0)
})

