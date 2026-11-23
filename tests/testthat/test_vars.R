library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)

test_that("calculate_weighted_variable handles integrity errors and biological warnings", {
  # Set test directory in an isolated temporary location
  temp_test_dir <- tempfile()
  dir.create(temp_test_dir)
  file.copy(from = test_path("test_data/user_data"), to = temp_test_dir, recursive = TRUE)
  withr::local_dir(temp_test_dir)

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

test_that("calculate_weighted_variable works with normalized diets without region/subregion", {
  temp_test_dir <- tempfile()
  dir.create(temp_test_dir)
  file.copy(from = test_path("test_data/user_data"), to = temp_test_dir, recursive = TRUE)
  withr::local_dir(temp_test_dir)

  diet_path <- "user_data/diet_profiles.csv"
  ingr_path <- "user_data/diet_ingredients.csv"
  original_diets <- read_csv(diet_path, show_col_types = FALSE)
  original_ingrs <- read_csv(ingr_path, show_col_types = FALSE)

  clean_diets <- original_diets %>% dplyr::select(-dplyr::any_of(c("region", "subregion")))
  clean_ingrs <- original_ingrs %>% dplyr::select(-dplyr::any_of(c("region", "subregion")))

  write_csv(clean_diets, diet_path)
  write_csv(clean_ingrs, ingr_path)

  res <- suppressWarnings(calculate_weighted_variable(saveoutput = FALSE))
  expect_s3_class(res, "data.frame")
  expect_true("diet_tag" %in% names(res))
  expect_true("GE_feed_kcal_kg" %in% names(res))
  expect_true(nrow(res) > 0)
})

test_that("calculate_weighted_variable computes human-edible CP from mapping.csv human_edible", {
  temp_test_dir <- tempfile()
  dir.create(temp_test_dir)
  file.copy(from = test_path("test_data/user_data"), to = temp_test_dir, recursive = TRUE)
  withr::local_dir(temp_test_dir)

  diets <- read_csv("user_data/diet_profiles.csv", show_col_types = FALSE)
  target_diet <- diets$diet_tag[1]
  diets_one <- diets[1, ] %>%
    mutate(forage_share = 0, concentrate_share = 100, milk_share = 0, milk_replacer_share = 0)
  write_csv(diets_one, "user_data/diet_profiles.csv")

  # Mix of: alfalfa_meal (human_edible=0), bakery_byproduct (human_edible=0),
  # linseed_meal (human_edible=1), and skimmed_milk (land_type=none, human_edible=1)
  custom_ing <- tibble::tibble(
    diet_tag = target_diet,
    ingredient_type = rep("concentrate", 4),
    ingredient = c("alfalfa_meal_17_5_cp", "bakery_byproduct_2_5_ash", "linseed_meal", "skimmed_milk"),
    ingredient_share = c(25, 25, 25, 25),
    country_of_origin = rep("Spain", 4),
    custom_yield_kg_ha = rep(NA_real_, 4)
  )
  write_csv(custom_ing, "user_data/diet_ingredients.csv")

  fc <- read_csv("user_data/feed_characteristics.csv", show_col_types = FALSE) %>%
    dplyr::filter(ingredient %in% custom_ing$ingredient)
  cp_by_ing <- setNames(as.numeric(fc$CP_pct), fc$ingredient)

  res_he <- suppressWarnings(calculate_weighted_variable(saveoutput = FALSE))
  row_he <- res_he %>% dplyr::filter(diet_tag == target_diet) %>% dplyr::slice(1)

  # Only linseed_meal (he=1) and skimmed_milk (he=1, despite land_type=none) should count,
  # weighted by each ingredient's own CP_pct (not the scalar diet average)
  expected_he_cp <- 0.25 * cp_by_ing[["linseed_meal"]] + 0.25 * cp_by_ing[["skimmed_milk"]]
  expect_equal(row_he$CP_cropland_pct, expected_he_cp, tolerance = 1e-6)
  expect_equal(row_he$CP_pct, mean(unname(cp_by_ing[custom_ing$ingredient])), tolerance = 1e-6)
})
