library(testthat)
library(herdr)
library(dplyr)

test_that("calculate_emissions_enteric assigns correct Ym and computes total Mg", {

  # 1. Setup Identity
  test_keys <- data.frame(
    region = "R1", subregion = "S1",
    animal_tag = "mature_dairy_cattle", class_flex = "grazing",
    animal_type = "cattle", animal_subtype = "dairy",
    stringsAsFactors = FALSE
  )

  # 2. Prepare Mocks
  mock_diet <- mutate(test_keys, de = 72, ndf = 30)
  mock_ge   <- mutate(test_keys, ge = 300)
  mock_pop  <- mutate(test_keys, population = 1000)

  # 3. Execution with Mocked Internal herdr Functions
  res <- testthat::with_mocked_bindings(
    calculate_emissions_enteric(automatic_cycle = FALSE, saveoutput = FALSE),

    calculate_weighted_variable = function(...) mock_diet,
    calculate_ge = function(...) mock_ge,
    calculate_population = function(...) mock_pop,

    .package = "herdr"
  )

  # 4. Assertions
  # For mature_dairy_cattle with DE >= 70 and NDF <= 35, Ym should be 5.7
  expect_equal(res$ym[1], 5.7)

  # EF = (300 * 0.057 * 365) / 55.65 = 112.156 kg/animal/year
  # emissions_total = 112.156 * (1000 / 1e6) = 0.112 Gg
  expect_equal(res$ef_kg_animal_year[1], 112.156, tolerance = 0.01)
  expect_equal(res$emissions_total[1], 0.112, tolerance = 0.01)

  expect_true(all(c("ge", "ym", "emissions_total") %in% colnames(res)))
  expect_equal(res$animal_tag[1], "mature_dairy_cattle")
})
