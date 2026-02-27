library(testthat)
library(herdr)
library(dplyr)

test_that("generate_impact_assessment consolidates all modules and applies filters", {

  # 1. Setup Identity
  test_keys <- data.frame(
    region = "North", subregion = "Zone1",
    animal_tag = "cow_test", class_flex = "grazing",
    animal_type = "cattle", animal_subtype = "dairy",
    stringsAsFactors = FALSE
  )

  # 2. Prepare Mocks for all 6 modules
  mock_ch4_ent <- mutate(test_keys, emissions_total = 0.5)
  mock_ch4_man <- mutate(test_keys, Emissions_CH4_Mg_year = 0.2)

  mock_n2o_dir <- mutate(test_keys, N2O_emissions = 10)
  mock_n2o_vol <- mutate(test_keys, n2o_g = 2)
  mock_n2o_lea <- mutate(test_keys, n2o_l = 3)

  mock_land    <- mutate(test_keys, Land_use_Total_m2 = 5000)

  # 3. Execution with Mocked Internal herdr Functions
  res <- testthat::with_mocked_bindings(
    generate_impact_assessment(
      automatic_cycle = FALSE,
      saveoutput = FALSE,
      region = "North"
    ),

    calculate_emissions_enteric = function(...) mock_ch4_ent,
    calculate_CH4_manure = function(...) mock_ch4_man,
    calculate_N2O_direct_manure = function(...) mock_n2o_dir,
    calculate_N2O_indirect_volatilization = function(...) mock_n2o_vol,
    calculate_N2O_indirect_leaching = function(...) mock_n2o_lea,
    calculate_land_use = function(...) mock_land,

    .package = "herdr"
  )

  # 4. Assertions
  expect_s3_class(res, "data.frame")

  # Verify column names match expected output
  expect_true(all(c("CH4_enteric_Mg", "CH4_manure_Mg", "N2O_direct_kg", "Land_m2") %in% colnames(res)))

  # Verify values are correctly passed through
  expect_equal(res$CH4_enteric_Mg[1], 0.5)
  expect_equal(res$N2O_direct_kg[1], 10)
  expect_equal(res$Land_m2[1], 5000)

  # Verify filter worked
  expect_true(all(res$region == "North"))

  # Verify empty result when filtering for non-existent animal type
  res_empty <- testthat::with_mocked_bindings(
    generate_impact_assessment(saveoutput = FALSE, animal = "sheep"),
    calculate_emissions_enteric = function(...) mock_ch4_ent,
    calculate_CH4_manure = function(...) mock_ch4_man,
    calculate_N2O_direct_manure = function(...) mock_n2o_dir,
    calculate_N2O_indirect_volatilization = function(...) mock_n2o_vol,
    calculate_N2O_indirect_leaching = function(...) mock_n2o_lea,
    calculate_land_use = function(...) mock_land,
    .package = "herdr"
  )
  expect_equal(nrow(res_empty), 0)
})
