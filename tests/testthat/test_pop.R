library(testthat)
library(herdr)
library(dplyr)

test_that("calculate_population handles manual and automatic modes correctly", {

  # 1. Setup Mock Data
  mock_categories <- data.frame(
    animal_tag = c("mature_sheep_female_dairy", "lamb_dairy_slaughter"),
    region = "R1", subregion = "S1", class_flex = "grazing",
    animal_type = "sheep", animal_subtype = "dairy",
    stringsAsFactors = FALSE
  )

  mock_census <- data.frame(
    animal_tag = "mature_sheep_female_dairy",
    region = "R1", subregion = "S1", class_flex = "grazing",
    population = 100,
    stringsAsFactors = FALSE
  )

  # 2. Nested Mocks
  # Level 1: herdr helpers
  res_auto <- testthat::with_mocked_bindings(

    # Level 2: readr CSVs
    testthat::with_mocked_bindings(
      calculate_population(automatic_cycle = TRUE, saveoutput = FALSE),

      read_csv = function(file, ...) {
        if (grepl("census.csv", file)) return(mock_census)
        if (grepl("categories.csv", file)) return(mock_categories)
        if (grepl("rate_parameters.csv", file)) return(data.frame())
        return(data.frame())
      },
      .package = "readr"
    ),

    # Simulamos que el helper de ovejas devuelve 100 madres y 50 crías
    calculate_population_sheep = function(...) {
      data.frame(
        region = "R1", subregion = "S1",
        animal_tag = c("mature_sheep_female_dairy", "lamb_dairy_slaughter"),
        population = c(100, 50),
        class_flex = "grazing", # IMPORTANTE: debe coincidir con categories
        stringsAsFactors = FALSE
      )
    },
    # Mock de los otros para que no den error si no hay datos
    calculate_population_cattle = function(...) data.frame(),
    calculate_population_goat   = function(...) data.frame(),
    .package = "herdr"
  )

  # 3. Assertions
  expect_s3_class(res_auto, "data.frame")

  # Verificamos que se hayan unido los resultados del helper (100 + 50 = 2 filas)
  # Si esto da 0, es que el join final con categories falló
  expect_equal(nrow(res_auto), 2)

  # Verificamos valores
  expect_equal(sum(res_auto$population), 150)

  # Verificamos que las columnas de identidad están presentes
  expect_true(all(c("region", "class_flex", "animal_subtype") %in% colnames(res_auto)))
})
