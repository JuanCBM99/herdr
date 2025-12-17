library(testthat)
library(herdr)
library(dplyr)
library(tidyr)

test_that("calculate_emissions_enteric calcula el metano entérico correctamente", {

  # --- 1. Claves de identificación de prueba ---
  keys <- data.frame(
    group = "G1", zone = "Z1", identification = "cow_test",
    animal_type = "cattle", animal_subtype = "dairy",
    stringsAsFactors = FALSE
  )

  # --- 2. Mocks de datos ---
  # GE (Gross Energy)
  mock_ge <- cbind(keys, ge = 180)

  # Dieta ponderada (DE, NDF)
  mock_nutrition <- cbind(keys, diet_tag = "D1", forage_share = 0, feed_share = 0, de = 65, ndf = 40)

  # Población con 'zone' NA para coincidir con la función
  mock_pop_res <- cbind(keys, population = 100) %>%
    dplyr::mutate(zone = dplyr::na_if(zone, ""))

  # Coeficientes IPCC (Ym)
  mock_coeffs <- data.frame(
    coefficient = "ym", description = "ym_dairy", value = 6.5,
    stringsAsFactors = FALSE
  )

  # Categorías de animales
  mock_cats <- cbind(keys, ym = "ym_dairy")

  # --- 3. Ejecutar la función usando mocks ---
  res <- with_mocked_bindings(
    calculate_emissions_enteric(saveoutput = FALSE),
    calculate_population = function(...) mock_pop_res,
    calculate_ge = function(...) mock_ge,
    calculate_weighted_variable = function(...) mock_nutrition,
    load_dataset = function(name) {
      switch(name,
             "coefficients" = mock_coeffs,
             "categories" = mock_cats)
    }
  )

  # --- 4. Extraer valores para verificaciones ---
  ef_col <- names(res)[grepl("^ef", names(res))][1]
  val_ef <- res[[ef_col]][res$identification == "cow_test"]
  val_total <- res$emissions_total[res$identification == "cow_test"]

  # --- 5. Verificaciones ---
  # EF debe ser positivo
  expect_gt(val_ef, 0)

  # Emisiones totales = EF * population / 1e6
  expect_equal(val_total, val_ef * 100 / 1e6, tolerance = 1e-2)

})
