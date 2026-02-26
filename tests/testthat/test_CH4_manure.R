library(testthat)
library(herdr)
library(dplyr)

test_that("calculate_CH4_manure computes emissions based on VS and management factors", {

  # 1. Setup Identity
  test_keys <- data.frame(
    region = "Reg1", subregion = "Sub1",
    animal_tag = "dairy_cow", class_flex = "stall",
    animal_type = "cattle", animal_subtype = "dairy",
    stringsAsFactors = FALSE
  )

  # 2. Prepare Internal Mocks (herdr)
  mock_vs  <- mutate(test_keys, vs = 4.5)           # 4.5 kg VS/día
  mock_pop <- mutate(test_keys, population = 1000)  # 1,000 animales

  # 3. Prepare CSV Mocks (readr)
  # ch4_mm: Configuración del sistema de manejo
  mock_ch4_mm <- data.frame(
    region = "Reg1", subregion = "Sub1", animal_tag = "dairy_cow", class_flex = "stall",
    animal_category = "dairy_cow_category", management_system = "lagoon",
    system_climate = "warm", management_duration = 12, # 12 meses (AWMS = 1)
    stringsAsFactors = FALSE
  )

  # ipcc_coefficients: Para el valor B0
  mock_coefs <- data.frame(
    coefficient = "b_0",
    description = "dairy_cow_category",
    value = 0.24, # m3 CH4 / kg VS
    stringsAsFactors = FALSE
  )

  # ipcc_mcf: Para el factor de conversión de metano
  mock_mcf <- data.frame(
    management_system = "lagoon",
    system_climate = "warm",
    mcf = 77, # 77% (IPCC default para lagunas en clima cálido)
    stringsAsFactors = FALSE
  )

  # 4. Execution with Nested Mocking
  res <- testthat::with_mocked_bindings(

    testthat::with_mocked_bindings(
      calculate_CH4_manure(automatic_cycle = FALSE, saveoutput = FALSE),

      read_csv = function(file, ...) {
        if (grepl("ch4_mm.csv", file)) return(mock_ch4_mm)
        if (grepl("ipcc_coefficients.csv", file)) return(mock_coefs)
        if (grepl("ipcc_mcf.csv", file)) return(mock_mcf)
        return(data.frame())
      },
      .package = "readr"
    ),

    calculate_vs = function(...) mock_vs,
    calculate_population = function(...) mock_pop,
    .package = "herdr"
  )

  # 5. Assertions
  # --- Verificación del Cálculo (IPCC Eq 10.23) ---
  # AWMS = 12/12 = 1.0
  # EF = (4.5 * 365) * (0.24 * 0.67 * (77 / 100) * 1.0)
  # EF = 1642.5 * 0.123864 = 203.447 kg CH4 / animal / año
  # Total Mg = 203.447 * 1000 / 1000 = 203.447 Mg

  expect_s3_class(res, "data.frame")
  expect_equal(res$EF_CH4_kg_year[1], 203.447, tolerance = 0.001)
  expect_equal(res$Emissions_CH4_Mg_year[1], 203.447, tolerance = 0.001)

  # Verificamos que se unieron correctamente los factores
  expect_equal(res$management_system[1], "lagoon")
  expect_equal(res$B0[1], 0.24)
  expect_equal(res$mcf[1], 77)
})
