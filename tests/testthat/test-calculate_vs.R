library(testthat)
library(herdr)
library(dplyr)

test_that("calculate_vs calcula los sólidos volátiles correctamente", {

  # 1. Claves de identificación
  keys <- data.frame(
    group = "G1", zone = "Z1", identification = "cow_test",
    animal_type = "cattle", animal_subtype = "dairy",
    stringsAsFactors = FALSE
  )

  # 2. Mock de Energía Bruta y Digestibilidad (GE y DE)
  mock_ge_de <- cbind(keys, ge = 180, de = 65)

  # 3. Mock de Nutrición (Cenizas/Ash)
  # Usamos un valor estándar de 8% de cenizas
  mock_nutrition <- cbind(keys, ash = 8)

  # --- CÁLCULO MANUAL (IPCC Tier 2) ---
  # GE = 180 MJ/día
  # DE = 65%
  # Ash = 8%
  # UE (Energía urinaria) suele ser 0.04 * GE
  # VS = [GE * (1 - DE/100) + (0.04 * GE)] * [(1 - Ash/100) / 18.45]
  # VS = [180 * 0.35 + 7.2] * [0.92 / 18.45]
  # VS = [63 + 7.2] * 0.04986 = 3.50 MJ... aprox

  expected_vs_min <- 2.0
  expected_vs_max <- 5.0

  with_mocked_bindings(
    {
      res <- calculate_vs(saveoutput = FALSE)

      # Verificamos que el animal existe
      val_vs <- res$vs[res$identification == "cow_test"]

      # Verificaciones
      expect_s3_class(res, "data.frame")
      expect_true("vs" %in% names(res))

      # El valor debe estar en un rango lógico para bovinos (kg/día)
      expect_gt(val_vs, expected_vs_min)
      expect_lt(val_vs, expected_vs_max)
    },
    calculate_ge = function(...) mock_ge_de,
    calculate_weighted_variable = function(...) mock_nutrition
  )
})
