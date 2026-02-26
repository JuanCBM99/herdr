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
  # Simulamos 150 kg de N excretado anual por animal y 100 animales
  mock_direct_n2o <- mutate(test_keys, N_excreted = 150)
  mock_pop        <- mutate(test_keys, population = 100)

  # 3. Prepare CSV Mocks (readr)
  # n2o_indirect: Configuración del sistema
  mock_n2o_ind <- data.frame(
    region = "R1", subregion = "S1", animal_tag = "dairy_cow", class_flex = "stall",
    management_system = "slurry", duration = 12,
    stringsAsFactors = FALSE
  )

  # ipcc_fractions: Fracción que lixivia (Frac_Leach)
  # El IPCC sugiere ~30% (0.30) en sistemas donde hay escorrentía o lixiviación
  mock_fractions <- data.frame(
    management_system = "slurry",
    frac_leach_ms = 0.30,
    stringsAsFactors = FALSE
  )

  # ipcc_emission_factors_indirect: Factor EF5
  # El IPCC usa 0.011 kg N2O-N / kg N lixiviado (Eq 10.29)
  mock_ef_ind <- data.frame(
    description = "EF5",
    value = 0.011,
    stringsAsFactors = FALSE
  )

  # 4. Execution with Nested Mocking
  res <- testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(
      calculate_N2O_indirect_leaching(automatic_cycle = FALSE, saveoutput = FALSE),

      read_csv = function(file, ...) {
        if (grepl("n2o_indirect.csv", file)) return(mock_n2o_ind)
        if (grepl("ipcc_fractions.csv", file)) return(mock_fractions)
        if (grepl("ipcc_emission_factors_indirect.csv", file)) return(mock_ef_ind)
        return(data.frame())
      },
      .package = "readr"
    ),

    calculate_N2O_direct_manure = function(...) mock_direct_n2o,
    calculate_population = function(...) mock_pop,
    .package = "herdr"
  )

  # 5. Assertions
  # --- Verificación del Cálculo ---
  # N_excreted_total = 100 animales * 150 kg N = 15,000 kg N
  # N_leaching = 15,000 * (12/12) * 0.30 = 4,500 kg N
  # N2O_l = 4,500 * 0.011 * (44 / 28) = 49.5 * 1.5714 = 77.786 kg N2O

  expect_equal(res$n_leaching[1], 4500)
  expect_equal(res$n2o_l[1], 77.786, tolerance = 0.001)

  # Verificación de integridad
  expect_equal(res$ef5[1], 0.011)
  expect_equal(res$frac_leach_ms[1], 0.30)
  expect_true("n2o_l" %in% colnames(res))
})
