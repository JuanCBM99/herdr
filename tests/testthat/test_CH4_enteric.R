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
  # Simulamos una dieta de alta calidad (DE=72, NDF=30)
  mock_diet <- mutate(test_keys, de = 72, ndf = 30)
  # Simulamos una GE de 300 MJ/día
  mock_ge   <- mutate(test_keys, ge = 300)
  # Población de 1,000 animales
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
  # --- Verificación de Ym ---
  # Para mature_dairy_cattle con DE >= 70 y NDF <= 35, Ym debería ser 5.7
  expect_equal(res$ym[1], 5.7)

  # --- Verificación de Cálculo ---
  # EF = (300 * 0.057 * 365) / 55.65 = 112.156 kg/animal/año
  # Total Mg = 112.156 * (1000 / 1000) = 0.112 Mg (si population es 1000)
  # Ojo: Tu código usa (population / 1e6) para Mg. 112.156 * 1000 / 1,000,000 = 0.112

  expect_equal(res$ef_kg_animal_year[1], 112.156, tolerance = 0.01)
  expect_equal(res$emissions_total[1], 0.112, tolerance = 0.01)

  # Verificación de llaves
  expect_true(all(c("ge", "ym", "emissions_total") %in% colnames(res)))
  expect_equal(res$animal_tag[1], "mature_dairy_cattle")
})
