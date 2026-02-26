library(testthat)
library(herdr)
library(dplyr)

test_that("calculate_NEa correctly computes activity energy based on NEm", {

  # 1. Setup Identity and Mock Data
  test_keys <- data.frame(
    region = "Reg1", subregion = "Sub1",
    animal_tag = "cow_activity_test", class_flex = "grazing",
    animal_type = "cattle", animal_subtype = "dairy",
    stringsAsFactors = FALSE
  )

  # Mock para NEm (lo que devolvería la función interna)
  mock_nem_res <- mutate(test_keys, NEm = 40.0)

  # Mock para categories (vincula el animal con su coeficiente de actividad 'ca')
  mock_cats <- mutate(test_keys, ca = "ca_grazing_hilly")

  # Mock para coeficientes IPCC
  # Ca para pastoreo en terreno ondulado suele ser 0.36 (IPCC)
  mock_coeffs <- data.frame(
    coefficient = "ca",
    description = "ca_grazing_hilly",
    value = 0.36,
    stringsAsFactors = FALSE
  )

  # 2. Execution with Nested Mocks
  res <- testthat::with_mocked_bindings(

    # Nivel 2: Interceptamos las lecturas de CSV
    testthat::with_mocked_bindings(
      calculate_NEa(saveoutput = FALSE),

      read_csv = function(file, ...) {
        if (grepl("categories.csv", file)) return(mock_cats)
        if (grepl("ipcc_coefficients.csv", file)) return(mock_coeffs)
        return(data.frame())
      },
      .package = "readr"
    ),

    # Nivel 1: Mockeamos la función interna NEm para controlar el valor base
    calculate_NEm = function(...) mock_nem_res,
    .package = "herdr"
  )

  # 3. Assertions
  # NEa = Ca * NEm -> 0.36 * 40.0 = 14.4
  expected_nea <- round(0.36 * 40.0, 3)

  val_calc <- res$NEa[1]

  expect_true("NEa" %in% colnames(res))
  expect_equal(val_calc, expected_nea)

  # Verificamos que se mantengan las llaves de identidad
  expect_equal(res$class_flex[1], "grazing")
})
