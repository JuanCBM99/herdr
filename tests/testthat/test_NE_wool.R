library(testthat)
library(herdr)
library(dplyr)

test_that("calculate_NE_wool computes daily wool energy correctly", {

  # 1. Setup Identity and Mock Data
  test_keys <- data.frame(
    region = "Reg1", subregion = "Sub1",
    animal_tag = "sheep_wool_test", class_flex = "grazing",
    stringsAsFactors = FALSE
  )

  # Mock Categories: Definimos la producción anual de lana
  mock_cats <- mutate(test_keys,
                      animal_type = "sheep",
                      animal_subtype = "merino",
                      wool_yield = 4.5) # 4.5 kg de lana al año

  # Mock Weights: Necesario para el inner_join de identidad
  mock_weights <- select(test_keys, region, subregion, animal_tag, class_flex)

  # 2. Execution with Mocked readr
  res <- testthat::with_mocked_bindings(
    calculate_NE_wool(saveoutput = FALSE),

    # Interceptamos la lectura de los dos archivos necesarios
    read_csv = function(file, ...) {
      if (grepl("categories.csv", file)) return(mock_cats)
      if (grepl("weights.csv", file)) return(mock_weights)
      return(data.frame())
    },
    .package = "readr"
  )

  # 3. Assertions
  # Fórmula: (wool_yield * 24) / 365
  # (4.5 * 24) / 365 = 108 / 365 = 0.29589...
  expected_wool <- round((4.5 * 24) / 365, 3)

  val_calc <- res$NE_wool[1]

  expect_true("NE_wool" %in% colnames(res))
  expect_equal(val_calc, expected_wool)

  # Verificamos que la identidad se mantiene
  expect_equal(res$animal_tag[1], "sheep_wool_test")
})
