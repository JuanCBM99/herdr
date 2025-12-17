library(testthat)
library(herdr)
library(dplyr)

test_that("calculate_N2O_indirect_leaching calcula correctamente la pérdida por lixiviación", {

  keys <- data.frame(
    group = "G1", zone = "Z1", identification = "cow_test",
    animal_type = "cattle", animal_subtype = "dairy",
    stringsAsFactors = FALSE
  )

  # 1. Mock de Direct Manure (N_excreted)
  mock_direct <- cbind(keys, N_excreted = 150)

  # 2. Mock de Población
  mock_pop <- cbind(keys, population = 50)

  # 3. Mocks de Datasets
  mock_n2o_indirect <- data.frame(
    identification = "cow_test", animal_type = "cattle", animal_subtype = "dairy",
    management_system = "pasture", duration = 12,
    stringsAsFactors = FALSE
  )

  mock_fractions <- data.frame(
    management_system = "pasture",
    frac_leach_ms = 0.30, # 30% se lixivia
    stringsAsFactors = FALSE
  )

  # OJO: Aquí simulamos la estructura que busca tu función (description == "EF5")
  mock_efs_indirect <- data.frame(
    description = c("EF4", "EF5"),
    value = c(0.01, 0.0075), # EF5 es 0.0075 según IPCC
    stringsAsFactors = FALSE
  )

  # --- Cálculo Esperado ---
  # N_leaching = 50 (pop) * 150 (N_ex) * 1 (awms) * 0.30 (frac) = 2250 kg N
  # N2O_L = 0.0075 (EF5) * 2250 * (44/28) = 26.518 kg N2O
  expected_n2o_l <- 0.0075 * 2250 * (44/28)

  with_mocked_bindings(
    {
      res <- calculate_N2O_indirect_leaching(saveoutput = FALSE)

      val_n2o_l <- res$N2O_L[res$identification == "cow_test"]

      # Verificaciones
      expect_s3_class(res, "data.frame")
      expect_true("N_leaching" %in% names(res))
      expect_equal(val_n2o_l, expected_n2o_l, tolerance = 1e-3)
      expect_gt(val_n2o_l, 0)
    },
    calculate_N2O_direct_manure = function(...) mock_direct,
    calculate_population = function(...) mock_pop,
    load_dataset = function(name) {
      switch(name,
             "n2o_indirect" = mock_n2o_indirect,
             "fractions" = mock_fractions,
             "emission_factors_indirect" = mock_efs_indirect)
    },
    .package = "herdr"
  )
})
