library(testthat)
library(herdr)
library(dplyr)

test_that("calculate_land_use computes m2 based on GE, EB and crop yields", {

  test_keys <- data.frame(
    region = "R1", subregion = "S1", animal_tag = "cow_land", class_flex = "grazing",
    animal_type = "cattle", animal_subtype = "dairy",
    stringsAsFactors = FALSE
  )

  mock_ge   <- mutate(test_keys, ge = 300)
  mock_char <- mutate(test_keys, eb = 18.45, diet_tag = "diet_A")
  mock_pop  <- mutate(test_keys, population = 100)

  # diet_profiles.csv: 100% forage
  mock_diet <- data.frame(
    diet_tag = "diet_A", region = "R1", subregion = "S1", class_flex = "grazing",
    forage_share = 100, concentrate_share = 0, milk_share = 0, milk_replacer_share = 0,
    stringsAsFactors = FALSE
  )

  # diet_ingredients.csv: forage is 100% grass
  mock_ingredients <- data.frame(
    diet_tag = "diet_A", region = "R1", subregion = "S1", class_flex = "grazing",
    ingredient = "grass", ingredient_type = "forage", ingredient_share = 100,
    stringsAsFactors = FALSE
  )

  # crop_yields.csv: grass yields 5,000 kg DM / ha
  mock_crops <- data.frame(
    ingredient = "grass", dry_matter_yield = 5000,
    stringsAsFactors = FALSE
  )

  res <- testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(
      calculate_land_use(automatic_cycle = FALSE, saveoutput = FALSE),

      read_csv = function(file, ...) {
        if (grepl("diet_profiles.csv", file)) return(mock_diet)
        if (grepl("diet_ingredients.csv", file)) return(mock_ingredients)
        if (grepl("crop_yields.csv", file)) return(mock_crops)
        return(data.frame())
      },
      .package = "readr"
    ),

    calculate_ge = function(...) mock_ge,
    calculate_weighted_variable = function(...) mock_char,
    calculate_population = function(...) mock_pop,
    .package = "herdr"
  )

  # DMI_day = 300 / 18.45 = 16.260 kg
  # Annual consumption = 16.260 * 365 * 1.0 * 1.0 = 5934.9 kg
  # Land Use (m2) = (5934.9 / 5000) * 10,000 = 11869.8 m2

  expect_equal(res$DMI_day_kg[1], 16.260, tolerance = 0.001)
  expect_equal(res$Land_use_per_animal[1], 11869.8, tolerance = 0.1)

  # Total for 100 animals: 1,186,980 m2
  expect_equal(res$Land_use_Total_m2[1], 1186980, tolerance = 1)

  expect_true(all(c("DMI_day_kg", "Land_use_Total_m2") %in% colnames(res)))
})
