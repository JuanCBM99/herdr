library(testthat)
library(herdr)

test_that("calculate_ge calcula la energía bruta total integrando todos los NE", {

  # Set completo de llaves de unión requeridas por el paquete
  keys <- data.frame(
    group = "G1",
    zone = "Z1",
    identification = "cow_test",
    animal_type = "cattle",
    animal_subtype = "dairy",
    stringsAsFactors = FALSE
  )

  # Mock de nutrición (Digestibilidad DE = 65%)
  # Aseguramos que tenga las 5 llaves + 'de'
  mock_nutrition <- cbind(keys, de = 65)

  with_mocked_bindings(
    {
      res <- calculate_ge(saveoutput = FALSE)

      # Verificaciones básicas
      expect_s3_class(res, "data.frame")
      expect_true("ge" %in% names(res))

      # Verificación lógica: La Energía Bruta siempre es mayor a la Neta
      # Total NE simulado = 35 + 5 + 20 = 60 MJ/día
      val_ge <- res$ge[res$identification == "cow_test"]
      expect_gt(val_ge, 60)

      # Verificar que no hay NAs en las columnas clave
      expect_false(any(is.na(res$ge)))
    },
    # Mockeamos cada función para que devuelva las llaves + su valor
    calculate_NEm = function(...) cbind(keys, NEm = 35),
    calculate_NEa = function(...) cbind(keys, NEa = 5),
    calculate_NEg = function(...) cbind(keys, NEg = 0),
    calculate_NEl = function(...) cbind(keys, NEl = 20),
    calculate_NE_work = function(...) cbind(keys, NE_work = 0),
    calculate_NE_wool = function(...) cbind(keys, NE_wool = 0),
    calculate_NE_pregnancy = function(...) cbind(keys, NE_pregnancy = 0),
    calculate_weighted_variable = function(...) mock_nutrition
  )
})
