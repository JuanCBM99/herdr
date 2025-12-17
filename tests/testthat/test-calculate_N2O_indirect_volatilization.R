library(testthat)
library(herdr)
library(dplyr)

test_that("calculate_N2O_indirect_volatilization calcula correctamente la pérdida por gas", {

  keys <- data.frame(
    group = "G1", zone = "Z1", identification = "cow_test",
    animal_type = "cattle", animal_subtype = "dairy",
    stringsAsFactors = FALSE
  )

  # 1. Mock de la función madre (Direct) - Necesitamos N_excreted
  # Supongamos 200 kg de N excretado por animal al año
  mock_direct <- cbind(keys, N_excreted = 200)

  # 2. Mock de Población
  mock_pop <- cbind(keys, population = 100)

  # 3. Mocks de Datasets (load_dataset)
  mock_n2o_indirect <- data.frame(
    identification = "cow_test", animal_type = "cattle", animal_subtype = "dairy",
    management_system = "solid_storage", climate = "temperate", duration = 12,
    stringsAsFactors = FALSE
  )

  mock_fractions <- data.frame(
    management_system = "solid_storage",
    frac_gas_ms = 0.30, # 30% se volatiliza
    stringsAsFactors = FALSE
  )

  mock_efs_indirect <- data.frame(
    climate = "temperate",
    value = 0.01, # EF4 estándar
    stringsAsFactors = FALSE
  )

  # --- Cálculo Esperado ---
  # n_volatilization = 100 (pop) * 200 (N_ex) * 1 (awms) * 0.30 (frac) = 6000 kg N
  # n2o_g = 0.01 (ef4) * 6000 * (44/28) = 94.286 kg N2O
  expected_n2o_g <- 0.01 * 6000 * (44/28)

  with_mocked_bindings(
    {
      res <- calculate_N2O_indirect_volatilization(saveoutput = FALSE)

      val_n2o_g <- res$n2o_g[res$identification == "cow_test"]

      expect_s3_class(res, "data.frame")
      expect_true("n_volatilization" %in% names(res))
      expect_equal(val_n2o_g, expected_n2o_g, tolerance = 1e-3)
      expect_gt(val_n2o_g, 0)
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
