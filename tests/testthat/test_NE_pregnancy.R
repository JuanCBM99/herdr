library(testthat)
library(herdr)
library(dplyr)

test_that("calculate_NE_pregnancy computes pregnancy energy for cattle and small ruminants", {

  # 1. Setup Identity and Mock Data
  test_keys <- data.frame(
    region = "Reg1", subregion = "Sub1", class_flex = "grazing",
    stringsAsFactors = FALSE
  )

  # Mock NEm (Base energy)
  mock_nem_res <- data.frame(
    test_keys,
    animal_tag = c("cow_preg", "sheep_preg"),
    animal_type = c("cattle", "sheep"),
    animal_subtype = c("dairy", "meat"),
    NEm = c(40.0, 4.0),
    stringsAsFactors = FALSE
  )

  # CORRECCIÓN: Añadimos animal_type y animal_subtype al mock de categories
  mock_cats <- data.frame(
    test_keys,
    animal_tag = c("cow_preg", "sheep_preg"),
    animal_type = c("cattle", "sheep"),      # <--- FALTA ESTO
    animal_subtype = c("dairy", "meat"),     # <--- Y ESTO
    c_pregnancy = c("cp_cattle", "cp_sheep"),
    pr = c(1, 1.5),
    stringsAsFactors = FALSE
  )

  # Mock Coefficients
  mock_coeffs <- data.frame(
    coefficient = "c_pregnancy",
    description = "cp_cattle",
    value = 0.10,
    stringsAsFactors = FALSE
  )

  # 2. Execution with Nested Mocks
  res <- testthat::with_mocked_bindings(

    # Level 2: readr CSVs
    testthat::with_mocked_bindings(
      calculate_NE_pregnancy(saveoutput = FALSE),

      read_csv = function(file, ...) {
        if (grepl("categories.csv", file)) return(mock_cats)
        if (grepl("ipcc_coefficients.csv", file)) return(mock_coeffs)
        return(data.frame())
      },
      .package = "readr"
    ),

    # Level 1: herdr NEm call
    calculate_NEm = function(...) mock_nem_res,
    .package = "herdr"
  )

  # 3. Assertions
  # Cattle: 0.10 * 40.0 = 4.0
  val_cattle <- res %>% filter(animal_type == "cattle") %>% pull(NE_pregnancy)
  expect_equal(val_cattle, 4.0)

  # Sheep: pr = 1.5 -> Factor 0.1015 * 4.0 = 0.406
  val_sheep <- res %>% filter(animal_type == "sheep") %>% pull(NE_pregnancy)
  expect_equal(val_sheep, 0.406)

  expect_true("NE_pregnancy" %in% colnames(res))
})
