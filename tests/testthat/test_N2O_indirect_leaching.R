library(testthat)
library(herdr)
library(dplyr)

test_that("calculate_N2O_indirect_leaching computes nitrogen leaching and N2O-L correctly", {

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

  # ipcc_mm.csv: IPCC master table with frac_leach and EF5
  # IPCC suggests ~30% (0.30) leaching fraction
  # EF5 = 0.011 kg N2O-N / kg N leached (Eq 10.29)
  mock_ipcc_mm <- data.frame(
    system_base = "slurry", management_months = 12,
    system_climate = "cool", system_subclimate = NA_character_,
    climate_zone = NA_character_, system_variant = NA_character_,
    climate_moisture = NA_character_,
    animal_type = "cattle", animal_subtype = "dairy",
    frac_leach = 0.30,
    EF5 = 0.011,
    stringsAsFactors = FALSE
  )

  # 4. Execution with Nested Mocking
  res <- testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(
      calculate_N2O_indirect_leaching(automatic_cycle = FALSE, saveoutput = FALSE),

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
  # N_leaching = 100 * 150 * 1.0 * 0.30 = 4,500 kg N leached
  # N2O_l = 4,500 * 0.011 * (44/28) = 49.5 * 1.5714 = 77.786 kg N2O

  expect_equal(res$n_leaching_kg_year[1], 4500)
  expect_equal(res$n2o_l[1], 77.786, tolerance = 0.001)

  expect_equal(res$EF5[1], 0.011)
  expect_equal(res$frac_leach[1], 0.30)
  expect_true("n2o_l" %in% colnames(res))
})
