library(testthat)
library(herdr)

test_that("calculate_NEa calcula la energía de actividad correctamente", {

  # 1. Mocks necesarios para NEm (que es llamada internamente)
  mock_weights <- data.frame(
    identification = "sheep_test", animal_type = "sheep", animal_subtype = "meat",
    average_weight = 60, stringsAsFactors = FALSE
  )

  mock_categories <- data.frame(
    identification = "sheep_test", animal_type = "sheep", animal_subtype = "meat",
    cfi = "cfi_sheep", ca = "ca_pasture", # Añadimos el tag 'ca'
    stringsAsFactors = FALSE
  )

  # 2. Mock de COEFFICIENTS (CFI para mantenimiento y CA para actividad)
  mock_coeffs <- data.frame(
    coefficient = c("cfi", "ca"),
    description = c("cfi_sheep", "ca_pasture"),
    value = c(0.217, 0.017), # Valores típicos IPCC
    stringsAsFactors = FALSE
  )

  # --- CÁLCULO MANUAL ESPERADO ---
  # 1. NEm = 0.217 * (60 ^ 0.75) = 4.68
  # 2. NEa = Ca_value * NEm = 0.017 * 4.68 = 0.0795...
  nem_esperado <- 0.217 * (60 ^ 0.75)
  nea_esperado <- round(0.017 * nem_esperado, 3)

  # Ejecutamos con mocking
  with_mocked_bindings(
    {
      res <- calculate_NEa(saveoutput = FALSE)

      # Verificaciones
      expect_s3_class(res, "data.frame")

      # Verificamos el valor final de NEa
      val_calc <- res$NEa[res$identification == "sheep_test"]

      # Usamos tolerancia por los decimales
      expect_equal(val_calc, nea_esperado, tolerance = 1e-3)

      # Verificamos que la columna del coeficiente esté presente
      expect_true("Ca_coefficient" %in% names(res))
    },
    load_dataset = function(name) {
      switch(name,
             "weights" = mock_weights,
             "categories" = mock_categories,
             "coefficients" = mock_coeffs)
    }
  )
})
