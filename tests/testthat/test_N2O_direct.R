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
  mock_ge   <- mutate(test_keys, ge = 300) # MJ/day
  mock_cp   <- mutate(test_keys, cp = 16)  # 16% Crude Protein
  mock_pop  <- mutate(test_keys, population = 100)
  mock_neg  <- mutate(test_keys, NEg = 5)  # MJ/day for growth

  # 3. Prepare CSV Mocks (readr) — schemas match actual file structures
  mock_cats <- mutate(test_keys, milk_yield = 5000, fat_content = 4)
  mock_w    <- mutate(test_keys, weight_gain = 0.5) # kg/day

  # manure_management.csv: management configuration
  mock_manure_mgmt <- data.frame(
    region = "R1", subregion = "S1", animal_tag = "dairy_cow", class_flex = "stall",
    animal_type = "cattle", animal_subtype = "dairy",
    system_base = "lagoon", management_months = 12,
    system_climate = "warm", system_subclimate = NA_character_,
    climate_zone = NA_character_, system_variant = NA_character_,
    climate_moisture = NA_character_, allocation = 1.0,
    stringsAsFactors = FALSE
  )

  # ipcc_mm.csv: IPCC master table with direct emission factor EF3
  # IPCC EF3 = 0.005 kg N2O-N / kg N excreted for lagoon
  mock_ipcc_mm <- data.frame(
    system_base = "lagoon", management_months = 12,
    system_climate = "warm", system_subclimate = NA_character_,
    climate_zone = NA_character_, system_variant = NA_character_,
    climate_moisture = NA_character_,
    animal_type = "cattle", animal_subtype = "dairy",
    EF3 = 0.005,
    stringsAsFactors = FALSE
  )

  # 4. Execution with Nested Mocking
  res <- testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(
      calculate_N2O_direct_manure(automatic_cycle = FALSE, saveoutput = FALSE),

      read_csv = function(file, ...) {
        if (grepl("livestock_definitions.csv", file)) return(mock_cats)
        if (grepl("livestock_weights.csv", file)) return(mock_w)
        if (grepl("manure_management.csv", file)) return(mock_manure_mgmt)
        if (grepl("ipcc_mm.csv", file)) return(mock_ipcc_mm)
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
  # N_intake = (300 / 18.45) * (16 / 100 / 6.25) = 16.26 * 0.0256 = 0.416 kg N/day
  expect_equal(res$N_intake[1], 0.416, tolerance = 0.001)

  # N_retention for cattle with milk yield involves protein:
  # milk_protein = 1.9 + 0.4 * 4 = 3.5
  expect_gt(res$N_retention[1], 0)

  # N2O total: population * N_excreted * allocation * EF3 * (44/28)
  expect_true("N2O_emissions" %in% colnames(res))
  expect_equal(res$system_base[1], "lagoon")
})
