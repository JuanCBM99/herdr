library(testthat)
library(herdr)
library(dplyr)
library(withr)

test_that("calculate_feed_emissions calculates footprints accurately", {
  # 1. Isolated test environment
  temp_test_dir <- tempfile()
  dir.create(temp_test_dir)
  file.copy(from = test_path("test_data/user_data"), to = temp_test_dir, recursive = TRUE)
  withr::local_dir(temp_test_dir)

  res <- suppressWarnings(calculate_feed_emissions(saveoutput = FALSE))

  expect_s3_class(res, "data.frame")
  expect_true(nrow(res) > 0)

  # Check columns
  expected_cols <- c(
    "region", "subregion", "animal_tag", "animal_type", "animal_subtype", "population",
    "feed_dmi_kg_year", "feed_fresh_kg_year",
    "feed_CO2e_total_kg_year", "feed_CO2e_fossil_kg_year", "feed_CO2e_luc_kg_year",
    "feed_water_m3_year", "feed_CO2e_per_head_year", "feed_water_per_head_year"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(res), info = paste("Missing column:", col))
  }

  # Verify values are positive and reasonable
  expect_true(all(res$feed_CO2e_total_kg_year >= 0))
  expect_true(all(res$feed_water_m3_year >= 0))
  expect_true(all(res$feed_CO2e_per_head_year >= 0))

  # Test in-memory injection
  dmi_mem <- suppressWarnings(suppressMessages(calculate_DMI(saveoutput = FALSE)))
  pop_mem <- suppressMessages(calculate_population(saveoutput = FALSE))

  res_mem <- suppressWarnings(calculate_feed_emissions(saveoutput = FALSE, DMI_df = dmi_mem, population_df = pop_mem))
  expect_equal(nrow(res_mem), nrow(res))
  expect_equal(res_mem$feed_CO2e_total_kg_year, res$feed_CO2e_total_kg_year)
})


test_that("calculate_feed_emissions scales co-products mapped to raw parent crop LCIs by EA / mass_fraction", {
  temp_test_dir <- tempfile()
  dir.create(temp_test_dir)
  file.copy(from = test_path("test_data/user_data"), to = temp_test_dir, recursive = TRUE)
  withr::local_dir(temp_test_dir)

  map_path <- "user_data/mapping.csv"
  map_df <- readr::read_csv(map_path, show_col_types = FALSE)
  map_df$kg_coproduct_per_1ton_maincrop <- 1000

  # Find first consumed concentrate in diet_ingredients
  diet_df <- readr::read_csv("user_data/diet_ingredients.csv", show_col_types = FALSE)
  target_ing <- diet_df$ingredient[as.numeric(diet_df$ingredient_share) > 0][1]

  # Set it to a raw parent crop LCI in Cereal grains with EA = 1 vs EA = 0.25 and mass = 500 kg/t (factor = 0.5)
  map_df$agribalyse_name[map_df$ingredient == target_ing] <- "Soft wheat grain, conventional, national average, animal feed, at farm gate, production {FR} U"
  map_df$economic_allocation[map_df$ingredient == target_ing] <- 1
  map_df$kg_coproduct_per_1ton_maincrop[map_df$ingredient == target_ing] <- 1000
  readr::write_csv(map_df, map_path)
  res_full <- suppressWarnings(calculate_feed_emissions(saveoutput = TRUE))
  ing_full <- readr::read_csv("output/feed_emissions_by_ingredient.csv", show_col_types = FALSE)
  ef_full <- ing_full$ef_climate_total_kgCO2e_kg[ing_full$ingredient == target_ing][1]

  map_df$economic_allocation[map_df$ingredient == target_ing] <- 0.25
  map_df$kg_coproduct_per_1ton_maincrop[map_df$ingredient == target_ing] <- 500
  readr::write_csv(map_df, map_path)
  res_scaled <- suppressWarnings(calculate_feed_emissions(saveoutput = TRUE))
  ing_scaled <- readr::read_csv("output/feed_emissions_by_ingredient.csv", show_col_types = FALSE)
  ef_scaled <- ing_scaled$ef_climate_total_kgCO2e_kg[ing_scaled$ingredient == target_ing][1]

  expect_equal(ef_scaled, ef_full * (0.25 / (500 / 1000)), tolerance = 1e-6)
})

test_that("calculate_feed_emissions preserves multiple farms sharing region, subregion, animal_tag, and diet_tag", {
  temp_test_dir <- tempfile()
  dir.create(temp_test_dir)
  file.copy(from = test_path("test_data/user_data"), to = temp_test_dir, recursive = TRUE)
  withr::local_dir(temp_test_dir)

  dmi_two_farms <- tibble::tibble(
    farm_id = c("farm_1", "farm_2"),
    region = c("spain", "spain"),
    subregion = c(NA_character_, NA_character_),
    animal_tag = c("mature_dairy_cattle", "mature_dairy_cattle"),
    animal_type = c("cattle", "cattle"),
    animal_subtype = c("dairy", "dairy"),
    diet_tag = c("diet_dairy_mature", "diet_dairy_mature"),
    DMI_kgday = c(20, 22)
  )
  pop_two_farms <- tibble::tibble(
    farm_id = c("farm_1", "farm_2"),
    region = c("spain", "spain"),
    subregion = c(NA_character_, NA_character_),
    animal_tag = c("mature_dairy_cattle", "mature_dairy_cattle"),
    animal_type = c("cattle", "cattle"),
    animal_subtype = c("dairy", "dairy"),
    population = c(100, 150)
  )

  res <- suppressWarnings(
    calculate_feed_emissions(saveoutput = FALSE, DMI_df = dmi_two_farms, population_df = pop_two_farms)
  )
  expect_setequal(res$farm_id, c("farm_1", "farm_2"))
  expect_equal(nrow(res), 2)
  expect_true(all(res$feed_CO2e_total_kg_year > 0))
})
