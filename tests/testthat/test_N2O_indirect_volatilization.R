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
  # Simulamos 150 kg de N excretado al año por animal y 100 animales
  mock_direct_n2o <- mutate(test_keys, N_excreted = 150)
  mock_pop        <- mutate(test_keys, population = 100)

  # 3. Prepare CSV Mocks (readr)
  # n2o_indirect: Configuración de manejo
  mock_n2o_ind <- data.frame(
    region = "R1", subregion = "S1", animal_tag = "dairy_cow", class_flex = "stall",
    management_system = "slurry", climate = "cool", duration = 12,
    stringsAsFactors = FALSE
  )

  # ipcc_fractions: Fracción que volatiliza (Frac_Gas)
  # El IPCC sugiere ~20% (0.20) para purines (slurry)
  mock_fractions <- data.frame(
    management_system = "slurry",
    frac_gas_ms = 0.20,
    stringsAsFactors = FALSE
  )

  # ipcc_emission_factors_indirect: Factor EF4
  # El IPCC usa 0.01 kg N2O-N / kg N volatilizado
  mock_ef_ind <- data.frame(
    climate = "cool",
    value = 0.01,
    stringsAsFactors = FALSE
  )

  # 4. Execution with Nested Mocking
  res <- testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(
      calculate_N2O_indirect_volatilization(automatic_cycle = FALSE, saveoutput = FALSE),

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
  # N_volatilization = 15,000 * (12/12) * 0.20 = 3,000 kg N
  # N2O_g = 3,000 * 0.01 * (44 / 28) = 30 * 1.5714 = 47.143 kg N2O

  expect_equal(res$n_volatilization[1], 3000)
  expect_equal(res$n2o_g[1], 47.143, tolerance = 0.001)

  # Verificación de integridad
  expect_true(all(c("frac_gas_ms", "ef4", "n2o_g") %in% colnames(res)))
  expect_equal(res$management_system[1], "slurry")
})
