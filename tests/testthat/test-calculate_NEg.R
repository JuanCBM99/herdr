library(testthat)
library(herdr)

test_that("calculate_NEg calcula el crecimiento para diferentes especies", {

  # 1. Mock de WEIGHTS (Un ternero y un cordero)
  mock_weights <- data.frame(
    identification = c("calf_test", "lamb_test"),
    animal_type = c("cattle", "sheep"),
    animal_subtype = c("growing", "meat"),
    average_weight = c(200, 30),
    adult_weight = c(500, 70),
    weight_gain = c(0.8, 0.2),
    stringsAsFactors = FALSE
  )

  # 2. Mock de CATEGORIES (Asignamos tags c, a, b)
  mock_categories <- data.frame(
    identification = c("calf_test", "lamb_test"),
    animal_type = c("cattle", "sheep"),
    animal_subtype = c("growing", "meat"),
    c = c("coeff_c_cattle", NA),
    a = c(NA, "coeff_a_sheep"),
    b = c(NA, "coeff_b_sheep"),
    stringsAsFactors = FALSE
  )

  # 3. Mock de COEFFICIENTS
  mock_coeffs <- data.frame(
    description = c("coeff_c_cattle", "coeff_a_sheep", "coeff_b_sheep"),
    value = c(0.8, 15.1, 0.3), # Valores de ejemplo
    stringsAsFactors = FALSE
  )

  # --- CÁLCULOS MANUALES ESPERADOS ---
  # Cattle: 22.02 * (200 / (0.8 * 500))^0.75 * (0.8^1.097)
  # 22.02 * (0.5^0.75) * 0.783 = 22.02 * 0.5946 * 0.783 = 10.252...
  expected_cattle <- round(22.02 * ((200 / (0.8 * 500))^0.75) * (0.8^1.097), 3)

  # Sheep: (A + 0.5 * B) * Gain
  # (15.1 + 0.5 * 0.3) * 0.2 = 15.25 * 0.2 = 3.05
  expected_sheep <- round((15.1 + 0.5 * 0.3) * 0.2, 3)

  with_mocked_bindings(
    {
      res <- calculate_NEg(saveoutput = FALSE)

      # Verificación Cattle
      val_cattle <- res$NEg[res$identification == "calf_test"]
      expect_equal(val_cattle, expected_cattle, tolerance = 1e-3)

      # Verificación Sheep
      val_sheep <- res$NEg[res$identification == "lamb_test"]
      expect_equal(val_sheep, expected_sheep, tolerance = 1e-3)
    },
    load_dataset = function(name) {
      switch(name,
             "weights" = mock_weights,
             "categories" = mock_categories,
             "coefficients" = mock_coeffs)
    }
  )
})
