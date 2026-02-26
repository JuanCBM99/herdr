library(testthat)
library(herdr)
library(dplyr)

test_that("calculate_NE_work computes work energy correctly by joining NEm", {

  # 1. Setup Identity and Mock Data
  test_keys <- data.frame(
    region = "Reg1", subregion = "Sub1",
    animal_tag = "ox_work_test", class_flex = "stall",
    animal_type = "cattle", animal_subtype = "work",
    stringsAsFactors = FALSE
  )

  # Mock para NEm (valor base de mantenimiento)
  mock_nem_res <- mutate(test_keys, NEm = 50.0)

  # Mock para categories (donde se definen las horas de trabajo)
  mock_cats <- mutate(test_keys, work_hours = 4) # 4 horas de trabajo diario

  # 2. Execution with Nested Mocks
  res <- testthat::with_mocked_bindings(

    # Nivel 2: Interceptamos la lectura de categories.csv
    testthat::with_mocked_bindings(
      calculate_NE_work(saveoutput = FALSE),

      read_csv = function(file, ...) {
        if (grepl("categories.csv", file)) return(mock_cats)
        return(data.frame())
      },
      .package = "readr"
    ),

    # Nivel 1: Mockeamos la función interna NEm
    calculate_NEm = function(...) mock_nem_res,
    .package = "herdr"
  )

  # 3. Assertions
  # NE_work = work_hours * NEm -> 4 * 50.0 = 200.0
  expected_work <- round(4 * 50.0, 3)

  val_calc <- res$NE_work[1]

  expect_true("NE_work" %in% colnames(res))
  expect_equal(val_calc, expected_work)

  # Verificamos que se mantengan los metadatos y llaves
  expect_equal(res$animal_tag[1], "ox_work_test")
  expect_equal(res$work_hours[1], 4)
})
