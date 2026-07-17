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

  path_diet <- "user_data/diet_ingredients.csv"
  if (file.exists(path_diet)) {
    df <- read_csv(path_diet, col_types = cols(.default = "c"), show_col_types = FALSE)

    if (!"country_of_origin" %in% names(df)) {
      df <- df %>% mutate(country_of_origin = "Spain")
    } else {
      df <- df %>% mutate(country_of_origin = ifelse(is.na(country_of_origin), "Spain", country_of_origin))
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

  # 1. Force an NA in the ingredients to trigger the FAO code block
  path_diet <- "user_data/diet_ingredients.csv"
  if (file.exists(path_diet)) {
    df <- read_csv(path_diet, col_types = cols(.default = "c"), show_col_types = FALSE)
    df$country_of_origin[1] <- NA
    write_csv(df, path_diet)
  }

  # 2. Dynamically pick a valid item from the user's mapping file to avoid logical NA type mismatches
  map_path <- "user_data/mapping.csv"
  real_item <- "Maize" # Default fallback
  if (file.exists(map_path)) {
    map_df <- read_csv(map_path, col_types = cols(.default = "c"), show_col_types = FALSE)
    valid_items <- na.omit(map_df$yield_name)
    if (length(valid_items) > 0) real_item <- valid_items[1]
  }

  # 3. Create dummy production file with the REAL item
  path_prod <- "user_data/fao_production.csv"
  dummy_prod <- data.frame(Area = "Spain", Year = 2022, Item = real_item, Value = 1000)
  write_csv(dummy_prod, path_prod)

  # 4. TROJAN HORSE: Dummy Parquet file using the REAL item
  dummy_parquet <- data.frame(
    `Reporter Countries` = "Spain",
    `Partner Countries` = "France",
    Item = real_item,
    Element = c("Import quantity", "Export quantity"),
    Y2022 = c(500, 100),
    check.names = FALSE
  )
  arrow::write_parquet(dummy_parquet, "user_data/fao_trade_matrix.parquet")

  # Execute
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

  # Change the origin to "Mars". This passes calculate_DMI safely but
  # fails gracefully in calculate_land_use, triggering the warning we want.
  path_diet <- "user_data/diet_ingredients.csv"
  if (file.exists(path_diet)) {
    df <- read_csv(path_diet, col_types = cols(.default = "c"), show_col_types = FALSE) %>%
      mutate(country_of_origin = "Mars")
    write_csv(df, path_diet)
  }

  # Verify that the correct warning is triggered about missing yields
  expect_warning(
    calculate_land_use(farm_country = "Spain", year = 2022, saveoutput = FALSE),
    "Missing yield"
  )
})
