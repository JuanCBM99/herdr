library(testthat)
library(herdr)

test_that("calculate_NE_work calcula la energía de trabajo correctamente", {

  # 1. Mock de WEIGHTS (Para que NEm funcione internamente)
  mock_weights <- data.frame(
    identification = "ox_test", animal_type = "cattle", animal_subtype = "adult",
    average_weight = 600, stringsAsFactors = FALSE
  )

  # 2. Mock de CATEGORIES (Añadimos 4 horas de trabajo)
  mock_categories <- data.frame(
    identification = "ox_test", animal_type = "cattle", animal_subtype = "adult",
    cfi = "cfi_ox", hours = 4,
    stringsAsFactors = FALSE
  )

  # 3. Mock de COEFFICIENTS
  mock_coeffs <- data.frame(
    coefficient = "cfi", description = "cfi_ox", value = 0.335,
    stringsAsFactors = FALSE
  )

  # --- CÁLCULO MANUAL ESPERADO ---
  # NEm = 0.335 * (600 ^ 0.75) = 40.528...
  # NE_work = NEm * hours = 40.528 * 4 = 162.112...
  nem_esperado <- 0.335 * (600 ^ 0.75)
  expected_work <- round(nem_esperado * 4, 3)

  with_mocked_bindings(
    {
      res <- calculate_NE_work(saveoutput = FALSE)

      # Verificación de clase
      expect_s3_class(res, "data.frame")

      # Verificamos el valor final
      val_calc <- res$NE_work[res$identification == "ox_test"]
      expect_equal(val_calc, expected_work, tolerance = 1e-3)

      # Verificar que si 'hours' es 0, NE_work sea 0
      # (Podríamos añadir otro animal al mock para probar esto rápidamente)
    },
    load_dataset = function(name) {
      switch(name,
             "weights" = mock_weights,
             "categories" = mock_categories,
             "coefficients" = mock_coeffs)
    }
  )
})
