library(testthat)
library(herdr)

test_that("calculate_NE_pregnancy calcula la energía de preñez correctamente", {

  # 1. Mock de WEIGHTS (Sincronizado con llaves de unión)
  mock_weights <- data.frame(
    group = "G1", zone = "Z1",
    identification = "pregnant_cow",
    animal_type = "cattle",
    animal_subtype = "dairy",
    average_weight = 500,
    stringsAsFactors = FALSE
  )

  # 2. Mock de CATEGORIES (Usando nombres exactos: c_pregnancy y pr)
  mock_categories <- data.frame(
    group = "G1", zone = "Z1",
    identification = "pregnant_cow",
    animal_type = "cattle",
    animal_subtype = "dairy",
    cfi = "cfi_dairy",
    c_pregnancy = "cp_dairy", # Cambiado de c_preg a c_pregnancy
    pr = 1,                   # Añadida columna pr
    stringsAsFactors = FALSE
  )

  # 3. Coeficientes
  mock_coeffs <- data.frame(
    coefficient = c("cfi", "c_pregnancy"),
    description = c("cfi_dairy", "cp_dairy"),
    value = c(0.335, 0.10),
    stringsAsFactors = FALSE
  )

  # --- CÁLCULO MANUAL ESPERADO ---
  nem_calc <- 0.335 * (500 ^ 0.75)
  expected_preg <- round(0.10 * nem_calc, 3)

  with_mocked_bindings(
    {
      res <- calculate_NE_pregnancy(saveoutput = FALSE)

      expect_s3_class(res, "data.frame")

      val_calc <- res$NE_pregnancy[res$identification == "pregnant_cow"]
      expect_equal(val_calc, expected_preg, tolerance = 1e-3)
    },
    load_dataset = function(name) {
      switch(name,
             "weights" = mock_weights,
             "categories" = mock_categories,
             "coefficients" = mock_coeffs)
    }
  )
})
