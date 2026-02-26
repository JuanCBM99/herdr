library(testthat)
library(herdr)
library(dplyr)

test_that("calculate_land_use computes m2 based on GE, EB and crop yields", {

  # 1. Setup Identity
  test_keys <- data.frame(
    region = "R1", subregion = "S1", animal_tag = "cow_land", class_flex = "grazing",
    animal_type = "cattle", animal_subtype = "dairy",
    stringsAsFactors = FALSE
  )

  # 2. Prepare Internal Mocks (herdr)
  # GE = 300 MJ/day, EB = 18.45 MJ/kg (DMI resultante = 16.26 kg/day)
  mock_ge   <- mutate(test_keys, ge = 300)
  mock_char <- mutate(test_keys, eb = 18.45, diet_tag = "diet_A")
  mock_pop  <- mutate(test_keys, population = 100)

  # 3. Prepare CSV Mocks (readr)
  # diet.csv: Define que el 100% es forraje
  mock_diet <- data.frame(
    diet_tag = "diet_A", region = "R1", subregion = "S1", class_flex = "grazing",
    forage_share = 100, feed_share = 0, milk_share = 0, milk_replacer_share = 0,
    stringsAsFactors = FALSE
  )

  # ingredients.csv: El forraje es 100% pasto
  mock_ingredients <- data.frame(
    diet_tag = "diet_A", region = "R1", subregion = "S1", class_flex = "grazing",
    ingredient = "grass", ingredient_type = "forage", ingredient_share = 100,
    stringsAsFactors = FALSE
  )

  # crops.csv: El pasto rinde 5,000 kg DM / ha
  mock_crops <- data.frame(
    ingredient = "grass", dry_matter_yield = 5000,
    stringsAsFactors = FALSE
  )

  # 4. Execution with Nested Mocking
  res <- testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(
      calculate_land_use(automatic_cycle = FALSE, saveoutput = FALSE),

      read_csv = function(file, ...) {
        if (grepl("diet.csv", file)) return(mock_diet)
        if (grepl("ingredients.csv", file)) return(mock_ingredients)
        if (grepl("crops.csv", file)) return(mock_crops)
        return(data.frame())
      },
      .package = "readr"
    ),

    calculate_ge = function(...) mock_ge,
    calculate_weighted_variable = function(...) mock_char,
    calculate_population = function(...) mock_pop,
    .package = "herdr"
  )

  # 5. Assertions
  # --- Verificación de Lógica ---
  # DMI_day = 300 / 18.45 = 16.260 kg
  # Consumo anual = 16.260 * 365 * 1.0 (share) * 1.0 (ing_share) = 5934.9 kg
  # Land Use (m2) = (5934.9 / 5000) * 10,000 = 11869.8 m2

  expect_equal(res$DMI_day_kg[1], 16.260, tolerance = 0.001)
  expect_equal(res$Land_use_per_animal[1], 11869.8, tolerance = 0.1)

  # Total para 100 animales: 1,186,980 m2
  expect_equal(res$Land_use_Total_m2[1], 1186980, tolerance = 1)

  # Verificación de llaves
  expect_true(all(c("DMI_day_kg", "Land_use_Total_m2") %in% colnames(res)))
})
