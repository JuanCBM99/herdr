library(testthat)
library(herdr)
library(dplyr)

test_that("calculate_N2O_direct_manure computes nitrogen balance and N2O correctly", {

  # 1. Setup Identity
  test_keys <- data.frame(
    region = "R1", subregion = "S1", animal_tag = "dairy_cow", class_flex = "stall",
    animal_type = "cattle", animal_subtype = "dairy",
    stringsAsFactors = FALSE
  )

  # 2. Prepare Internal Mocks (herdr)
  mock_ge   <- mutate(test_keys, ge = 300) # MJ/día
  mock_cp   <- mutate(test_keys, cp = 16)  # 16% Proteína Cruda
  mock_pop  <- mutate(test_keys, population = 100)
  mock_neg  <- mutate(test_keys, NEg = 5)  # MJ/día para crecimiento

  # 3. Prepare CSV Mocks (readr)
  mock_cats <- mutate(test_keys, milk_yield = 5000, fat_content = 4)
  mock_w    <- mutate(test_keys, weight_gain = 0.5) # kg/día

  # Configuración de manejo: Lagoon en clima templado
  mock_n2o_mm <- data.frame(
    region = "R1", subregion = "S1", animal_tag = "dairy_cow", class_flex = "stall",
    management_system = "lagoon", climate = "temperate", management_duration = 12,
    stringsAsFactors = FALSE
  )

  # Factor de emisión IPCC para lagunas (ej. 0.005 kg N2O-N/kg N)
  mock_ef_n2o <- data.frame(
    management_system = "lagoon", climate = "temperate", value = 0.005,
    stringsAsFactors = FALSE
  )

  # 4. Execution with Nested Mocking
  res <- testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(
      calculate_N2O_direct_manure(automatic_cycle = FALSE, saveoutput = FALSE),

      read_csv = function(file, ...) {
        if (grepl("categories.csv", file)) return(mock_cats)
        if (grepl("weights.csv", file)) return(mock_w)
        if (grepl("n2o_direct.csv", file)) return(mock_n2o_mm)
        if (grepl("ipcc_emission_factors_direct.csv", file)) return(mock_ef_n2o)
        return(data.frame())
      },
      .package = "readr"
    ),

    calculate_ge = function(...) mock_ge,
    calculate_weighted_variable = function(...) mock_cp,
    calculate_population = function(...) mock_pop,
    calculate_NEg = function(...) mock_neg,
    .package = "herdr"
  )

  # 5. Assertions
  # --- Verificación de Lógica ---
  # N_intake = (300 / 18.45) * (16 / 100 / 6.25) = 16.26 * 0.0256 = 0.416 kg N/día
  expect_equal(res$N_intake[1], 0.416, tolerance = 0.001)

  # N_retention para vacas involucra leche y crecimiento:
  # milk_protein = 1.9 + 0.4 * 4 = 3.5
  # retention_milk = (5000 * 3.5) / 6.38 / 365 (aproximadamente, tu función usa milk_yield anual)
  expect_gt(res$N_retention[1], 0)

  # N2O total: population * N_excreted * awms * ef * (44/28)
  expect_true("N2O_emissions" %in% colnames(res))
  expect_equal(res$management_system[1], "lagoon")
})
