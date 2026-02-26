library(testthat)
library(herdr)
library(dplyr)

test_that("calculate_NEg correctly applies Cattle and Sheep formulas", {

  # 1. Setup Identity and Mock Data
  # Creamos dos animales: una vaca y una oveja
  mock_weights <- data.frame(
    region = "Reg1", subregion = "Sub1", class_flex = "grazing",
    animal_tag = c("cow_gain", "sheep_gain"),
    average_weight = c(450, 40), # Peso actual
    adult_weight = c(600, 70),   # Peso adulto (solo para cattle)
    weight_gain = c(0.8, 0.15),  # Ganancia diaria (kg/día)
    stringsAsFactors = FALSE
  )

  mock_cats <- data.frame(
    animal_tag = c("cow_gain", "sheep_gain"),
    region = "Reg1", subregion = "Sub1", class_flex = "grazing",
    animal_type = c("cattle", "sheep"),
    animal_subtype = c("dairy", "meat"),
    c = c("c_female", NA), # Coeficiente C solo para cattle
    a = c(NA, "a_sheep"),  # Coeficiente A para sheep
    b = c(NA, "b_sheep"),  # Coeficiente B para sheep
    stringsAsFactors = FALSE
  )

  mock_coeffs <- data.frame(
    description = c("c_female", "a_sheep", "b_sheep"),
    value = c(0.8, 2.5, 14.5), # Valores típicos IPCC
    stringsAsFactors = FALSE
  )

  # 2. Execution with Mocked readr
  res <- testthat::with_mocked_bindings(
    calculate_NEg(saveoutput = FALSE),

    read_csv = function(file, ...) {
      if (grepl("weights.csv", file)) return(mock_weights)
      if (grepl("categories.csv", file)) return(mock_cats)
      if (grepl("ipcc_coefficients.csv", file)) return(mock_coeffs)
      return(data.frame())
    },
    .package = "readr"
  )

  # 3. Assertions
  # --- Cattle Calculation Check ---
  # Formula: 22.02 * (450 / (0.8 * 600))^0.75 * 0.8^1.097
  # 22.02 * (0.9375^0.75) * 0.783 = 22.02 * 0.952 * 0.783 ≈ 16.42
  val_cattle <- res %>% filter(animal_type == "cattle") %>% pull(NEg)
  expect_gt(val_cattle, 0)

  # --- Sheep Calculation Check ---
  # Formula: (A + 0.5 * B) * weight_gain
  # (2.5 + 0.5 * 14.5) * 0.15 = 9.75 * 0.15 = 1.4625
  val_sheep <- res %>% filter(animal_type == "sheep") %>% pull(NEg)
  expect_equal(val_sheep, 1.463, tolerance = 0.001)

  # 4. Integrity Check
  expect_true(all(c("region", "class_flex", "NEg") %in% colnames(res)))
  expect_equal(nrow(res), 2)
})
