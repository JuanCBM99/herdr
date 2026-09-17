library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)

test_that("calculate_weighted_variable handles integrity errors and biological warnings", {
  # Set test directory
  withr::local_dir(test_path("test_data"))

  # Fix column types to avoid logical vs character errors
  read_csv("user_data/diet_profiles.csv", col_types = cols(.default = "c"), show_col_types = FALSE) %>%
    write_csv("user_data/diet_profiles.csv")
  read_csv("user_data/diet_ingredients.csv", col_types = cols(.default = "c"), show_col_types = FALSE) %>%
    write_csv("user_data/diet_ingredients.csv")
  read_csv("user_data/feed_characteristics.csv", col_types = cols(.default = "c"), show_col_types = FALSE) %>%
    write_csv("user_data/feed_characteristics.csv")
  read_csv("user_data/ruminant_definitions.csv", col_types = cols(.default = "c"), show_col_types = FALSE) %>%
    write_csv("user_data/ruminant_definitions.csv")

  # Load base data for manipulation
  diet_path <- "user_data/diet_profiles.csv"
  original_diets <- read_csv(diet_path, show_col_types = FALSE)

  # Block: Test Integrity Error (Shares < 100%)
  bad_integrity <- original_diets[1, ] %>%
    mutate(forage_share = 50, concentrate_share = 10, milk_share = 0, milk_replacer_share = 0)
  write_csv(bad_integrity, diet_path)

  expect_error(
    calculate_weighted_variable(saveoutput = FALSE),
    "Diet Profile Error"
  )

  # Block: Test Biological Warning (High concentrate)
  bad_biology <- original_diets[1, ] %>%
    mutate(forage_share = 10, concentrate_share = 90, milk_share = 0, milk_replacer_share = 0)
  write_csv(bad_biology, diet_path)

  expect_warning(
   expect_warning(
    calculate_weighted_variable(saveoutput = FALSE),
    "Warning \\(Forage\\)"
  ),
  "Warning \\(Protein\\)"
  )
})

test_that("calculate_weighted_variable works with normalized diets without region/subregion/class_flex", {
  temp_test_dir <- tempfile()
  dir.create(temp_test_dir)
  file.copy(from = test_path("test_data/user_data"), to = temp_test_dir, recursive = TRUE)
  withr::local_dir(temp_test_dir)

  diet_path <- "user_data/diet_profiles.csv"
  ingr_path <- "user_data/diet_ingredients.csv"
  original_diets <- read_csv(diet_path, show_col_types = FALSE)
  original_ingrs <- read_csv(ingr_path, show_col_types = FALSE)

  clean_diets <- original_diets %>% dplyr::select(-dplyr::any_of(c("region", "subregion", "class_flex")))
  clean_ingrs <- original_ingrs %>% dplyr::select(-dplyr::any_of(c("region", "subregion", "class_flex")))

  write_csv(clean_diets, diet_path)
  write_csv(clean_ingrs, ingr_path)

  res <- suppressWarnings(calculate_weighted_variable(saveoutput = FALSE))
  expect_s3_class(res, "data.frame")
  expect_true("diet_tag" %in% names(res))
  expect_true("GE_feed_kcal_kg" %in% names(res))
  expect_true(nrow(res) > 0)
})
