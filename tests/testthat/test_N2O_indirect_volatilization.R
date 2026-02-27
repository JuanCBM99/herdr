library(testthat)
library(herdr)
library(dplyr)

test_that("calculate_N2O_indirect_volatilization computes gas loss and indirect N2O", {

  # 1. Setup Identity Keys
  test_keys <- data.frame(
    region = "R1", subregion = "S1", animal_tag = "dairy_cow", class_flex = "stall",
    animal_type = "cattle", animal_subtype = "dairy",
    stringsAsFactors = FALSE
  )

  # 2. Prepare Internal Mocks (herdr)
  mock_direct_n2o <- mutate(test_keys, N_excreted = 150)
  mock_pop        <- mutate(test_keys, population = 100)

  # 3. Prepare CSV Mocks (readr) — schemas match actual file structures

  # manure_management.csv: management configuration
  mock_manure_mgmt <- data.frame(
    region = "R1", subregion = "S1", animal_tag = "dairy_cow", class_flex = "stall",
    animal_type = "cattle", animal_subtype = "dairy",
    system_base = "slurry", management_months = 12,
    system_climate = "cool", system_subclimate = NA_character_,
    climate_zone = NA_character_, system_variant = NA_character_,
    climate_moisture = NA_character_, allocation = 1.0,
    stringsAsFactors = FALSE
  )

  # ipcc_mm.csv: IPCC master table with frac_gas and EF4
  # IPCC suggests ~20% (0.20) gas fraction for slurry
  # EF4 = 0.01 kg N2O-N / kg N volatilised
  mock_ipcc_mm <- data.frame(
    system_base = "slurry", management_months = 12,
    system_climate = "cool", system_subclimate = NA_character_,
    climate_zone = NA_character_, system_variant = NA_character_,
    climate_moisture = NA_character_,
    animal_type = "cattle", animal_subtype = "dairy",
    frac_gas = 0.20,
    EF4 = 0.01,
    stringsAsFactors = FALSE
  )

  # 4. Execution with Nested Mocking
  res <- testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(
      calculate_N2O_indirect_volatilization(automatic_cycle = FALSE, saveoutput = FALSE),

      read_csv = function(file, ...) {
        if (grepl("manure_management.csv", file)) return(mock_manure_mgmt)
        if (grepl("ipcc_mm.csv", file)) return(mock_ipcc_mm)
        return(data.frame())
      },
      .package = "readr"
    ),

    calculate_N2O_direct_manure = function(...) mock_direct_n2o,
    calculate_population = function(...) mock_pop,
    .package = "herdr"
  )

  # 5. Assertions
  # N_excreted_total = 100 * 150 * 1.0 * 0.20 = 3,000 kg N volatilised
  # N2O_g = 3,000 * 0.01 * (44/28) = 30 * 1.5714 = 47.143 kg N2O

  expect_equal(res$n_volatilization_kg_year[1], 3000)
  expect_equal(res$n2o_g[1], 47.143, tolerance = 0.001)

  expect_true(all(c("frac_gas", "EF4", "n2o_g") %in% colnames(res)))
  expect_equal(res$system_base[1], "slurry")
})
