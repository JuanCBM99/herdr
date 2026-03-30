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
  read_csv("user_data/livestock_definitions.csv", col_types = cols(.default = "c"), show_col_types = FALSE) %>%
    write_csv("user_data/livestock_definitions.csv")

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
