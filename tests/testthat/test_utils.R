library(testthat)
library(herdr)
library(withr)

test_that("herdr_init initializes project structure and copies example templates", {
  withr::with_tempdir({
    expect_message(herdr_init(), "Initializing herdr project structure")

    expect_true(dir.exists("user_data"))
    expect_true(dir.exists("Examples"))

    # Verify that essential template files were copied into user_data
    ud_files <- list.files("user_data")
    expect_true(length(ud_files) > 0)
    expect_true(any(grepl("livestock_census\\.csv", ud_files)))
  })
})

test_that("reference data paths resolve bundled datasets correctly", {
  forage_path <- herdr_get_forages_path()
  expect_true(file.exists(forage_path))

  ecoalim_path <- herdr_get_ecoalim_path()
  expect_true(file.exists(ecoalim_path))

  df_eco <- arrow::read_parquet(ecoalim_path)
  expect_equal(nrow(df_eco), 224)
  expect_equal(ncol(df_eco), 38)
})

test_that("herdr_cache_dir returns a valid directory path", {
  cache <- herdr_cache_dir()
  expect_type(cache, "character")
  expect_true(dir.exists(cache))
})

test_that("reference data paths prioritize local data_dir when present", {
  withr::with_tempdir({
    dir.create("custom_data")
    mock_file <- file.path("custom_data", "forages.parquet")
    file.copy(herdr_get_forages_path(), mock_file)

    resolved <- herdr_get_forages_path(data_dir = "custom_data")
    expect_equal(normalizePath(resolved), normalizePath(mock_file))
  })
})

test_that("herdr_validate_manure_allocation verifies cohort allocation constraints", {
  # 1. Valid allocation exactly 1.0 (no error, no warning)
  valid_manure <- data.frame(
    region = "spain", subregion = NA_character_,
    animal_tag = "mature_dairy_cattle", class_flex = NA_character_,
    allocation = c(0.4, 0.6)
  )
  expect_silent(herdr_validate_manure_allocation(valid_manure))

  # 2. Over-allocation > 1.001 (must throw error)
  over_manure <- data.frame(
    region = "spain", subregion = NA_character_,
    animal_tag = "mature_dairy_cattle", class_flex = NA_character_,
    allocation = c(0.7, 0.5)
  )
  expect_error(
    herdr_validate_manure_allocation(over_manure),
    "Manure allocation exceeds 1.0"
  )

  # 3. Under-allocation < 0.999 (must throw error)
  under_manure <- data.frame(
    region = "spain", subregion = NA_character_,
    animal_tag = "mature_dairy_cattle", class_flex = NA_character_,
    allocation = c(0.3, 0.4)
  )
  expect_error(
    herdr_validate_manure_allocation(under_manure),
    "Manure allocation is less than 1.0"
  )

  # 4. Valid allocation within tolerance (silent)
  approx_manure <- data.frame(
    region = "spain", subregion = NA_character_,
    animal_tag = "mature_dairy_cattle", class_flex = NA_character_,
    allocation = c(0.3333, 0.3333, 0.3334)
  )
  expect_silent(herdr_validate_manure_allocation(approx_manure))

  # 5. Empty or NULL input (safe pass)
  expect_null(herdr_validate_manure_allocation(NULL))
  expect_null(herdr_validate_manure_allocation(data.frame()))
})

test_that("herdr_validate_diet_shares verifies diet profile and ingredient constraints", {
  # 1. Valid diet and ingredients (silent pass)
  valid_diets <- data.frame(
    diet_tag = "dairy_lactating",
    forage_share = 60, concentrate_share = 40, milk_share = 0, milk_replacer_share = 0
  )
  valid_ings <- data.frame(
    diet_tag = c("dairy_lactating", "dairy_lactating"),
    ingredient_type = c("forage", "concentrate"),
    ingredient_share = c(100, 100)
  )
  expect_silent(herdr_validate_diet_shares(valid_diets, valid_ings))

  # 2. Diet category shares do not sum to 100% (error)
  invalid_cat_diets <- data.frame(
    diet_tag = "dairy_lactating",
    forage_share = 60, concentrate_share = 30, milk_share = 0, milk_replacer_share = 0
  )
  expect_error(
    herdr_validate_diet_shares(invalid_cat_diets, valid_ings),
    "Diet Profile Error: Shares do not sum to 100%"
  )

  # 3. Ingredient shares within a category do not sum to 100% (error)
  invalid_ing_shares <- data.frame(
    diet_tag = c("dairy_lactating", "dairy_lactating"),
    ingredient_type = c("forage", "forage"),
    ingredient_share = c(50, 40)
  )
  expect_error(
    herdr_validate_diet_shares(valid_diets, invalid_ing_shares),
    "Ingredient Error: Shares do not sum to 100%"
  )

  # 4. Active category > 0% but missing ingredients (error)
  missing_cat_ings <- data.frame(
    diet_tag = "dairy_lactating",
    ingredient_type = "forage",
    ingredient_share = 100
  )
  expect_error(
    herdr_validate_diet_shares(valid_diets, missing_cat_ings),
    "Active diet categories missing ingredients"
  )

  # 5. Empty or NULL tables (safe pass)
  expect_true(herdr_validate_diet_shares(NULL, NULL))
  expect_true(herdr_validate_diet_shares(data.frame(), data.frame()))
})
