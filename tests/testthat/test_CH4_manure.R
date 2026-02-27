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
  mock_vs  <- mutate(test_keys, vs = 4.5)           # 4.5 kg VS/day
  mock_pop <- mutate(test_keys, population = 1000)  # 1,000 animals

  # 3. Prepare CSV Mocks (readr) — schemas match actual file structures

  # manure_management.csv: user management configuration
  mock_manure_mgmt <- data.frame(
    region = "Reg1", subregion = "Sub1", animal_tag = "dairy_cow", class_flex = "stall",
    animal_type = "cattle", animal_subtype = "dairy",
    system_base = "lagoon", management_months = 12,
    system_climate = "warm", system_subclimate = NA_character_,
    climate_zone = NA_character_, system_variant = NA_character_,
    climate_moisture = NA_character_, allocation = 1.0,
    stringsAsFactors = FALSE
  )

  # ipcc_mm.csv: IPCC master table with MCF factors
  mock_ipcc_mm <- data.frame(
    system_base = "lagoon", management_months = 12,
    system_climate = "warm", system_subclimate = NA_character_,
    climate_zone = NA_character_, system_variant = NA_character_,
    climate_moisture = NA_character_,
    animal_type = "cattle", animal_subtype = "dairy",
    mcf = 77,  # 77% (IPCC default for lagoons in warm climate)
    stringsAsFactors = FALSE
  )

  # ipcc_coefficients.csv: B0 coefficient per animal type/subtype
  mock_coefs <- data.frame(
    coefficient = "b_0",
    animal_type = "cattle",
    animal_subtype = "dairy",
    value = 0.24,  # m3 CH4 / kg VS
    stringsAsFactors = FALSE
  )

  # 4. Execution with Nested Mocking
  res <- testthat::with_mocked_bindings(

    testthat::with_mocked_bindings(
      calculate_CH4_manure(automatic_cycle = FALSE, saveoutput = FALSE),

      read_csv = function(file, ...) {
        if (grepl("manure_management.csv", file)) return(mock_manure_mgmt)
        if (grepl("ipcc_mm.csv", file)) return(mock_ipcc_mm)
        if (grepl("ipcc_coefficients.csv", file)) return(mock_coefs)
        return(data.frame())
      },
      .package = "readr"
    ),

    calculate_vs = function(...) mock_vs,
    calculate_population = function(...) mock_pop,
    .package = "herdr"
  )

  # 5. Assertions
  # EF = (4.5 * 365) * (0.24 * 0.67 * (77/100) * 1.0)
  # EF = 1642.5 * 0.123864 = 203.447 kg CH4 / animal / year
  # Emissions_CH4_Mg_year = 203.447 * 1000 / 1000 = 203.447 Mg

  expect_s3_class(res, "data.frame")
  expect_equal(res$EF_CH4_kg_year[1], 203.447, tolerance = 0.001)
  expect_equal(res$Emissions_CH4_Mg_year[1], 203.447, tolerance = 0.001)

  # Verify management system and factors
  expect_equal(res$system_base[1], "lagoon")
  expect_equal(res$B0[1], 0.24)
  expect_equal(res$mcf[1], 77)
})
