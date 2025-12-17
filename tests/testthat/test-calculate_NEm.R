library(testthat)
library(herdr)

test_that("calculate_NEm calcula la energía de mantenimiento correctamente", {

  # 1. Mock de WEIGHTS (Un animal de 500 kg)
  mock_weights <- data.frame(
    identification = "cow_test",
    animal_type = "cattle",
    animal_subtype = "dairy",
    average_weight = 500,
    stringsAsFactors = FALSE
  )

  # 2. Mock de CATEGORIES (Asignamos el tag 'cfi_dairy')
  mock_categories <- data.frame(
    identification = "cow_test",
    animal_type = "cattle",
    animal_subtype = "dairy",
    cfi = "cfi_dairy",
    stringsAsFactors = FALSE
  )

  # 3. Mock de COEFFICIENTS (Valor del cfi para vacas lecheras = 0.335)
  # Nota: Tu función busca donde coefficient == "cfi" y description == cfi_tag
  mock_coeffs <- data.frame(
    coefficient = "cfi",
    description = "cfi_dairy",
    value = 0.335,
    stringsAsFactors = FALSE
  )

  # --- CÁLCULO MANUAL ESPERADO ---
  # NEm = cfi_value * (weight ^ 0.75)
  # NEm = 0.335 * (500 ^ 0.75)
  # NEm = 0.335 * 105.737... = 35.4218...
  expected_val <- round(0.335 * (500 ^ 0.75), 3)

  # Ejecutamos con mocking de carga de datos
  with_mocked_bindings(
    {
      res <- calculate_NEm(saveoutput = FALSE)

      # Verificaciones
      expect_s3_class(res, "data.frame")

      # Extraemos el valor calculado
      val_calc <- res$NEm[res$identification == "cow_test"]

      # Opción recomendada: usar un margen de tolerancia (tolerance)
      # Esto es lo más profesional en tests científicos
      expect_equal(val_calc, expected_val, tolerance = 1e-3)

      # Verificar que se incluyan las columnas de metadatos
      expect_true(all(c("cfi_value", "average_weight") %in% names(res)))
    },
    load_dataset = function(name) {
      switch(name,
             "weights" = mock_weights,
             "categories" = mock_categories,
             "coefficients" = mock_coeffs)
    }
  )
})
