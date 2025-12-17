library(testthat)
library(herdr)

test_that("calculate_weighted_variable realiza la ponderación nutricional correctamente", {

  # 1. Mock de DIET (Definimos que la dieta 'D1' tiene 60% forraje y 40% feed)
  mock_diet <- data.frame(
    group = "G1", zone = "Z1", diet_tag = "D1",
    forage_share = 60, feed_share = 40, milk_share = 0, milk_replacer_share = 0,
    stringsAsFactors = FALSE
  )

  # 2. Mock de INGREDIENTS (Un ingrediente por categoría al 100% de su grupo)
  mock_ingredients <- data.frame(
    group = "G1", zone = "Z1", diet_tag = "D1",
    ingredient = c("Alfalfa", "Maiz"),
    ingredient_type = c("forage", "feed"),
    ingredient_share = c(100, 100), # 100% del forraje es Alfalfa, 100% del feed es Maiz
    stringsAsFactors = FALSE
  )

  # 3. Mock de CHARACTERISTICS (Valores de DE para cada ingrediente)
  mock_chars <- data.frame(
    ingredient = c("Alfalfa", "Maiz"),
    ingredient_type = c("forage", "feed"),
    de = c(60, 80), # Alfalfa 60%, Maiz 80%
    cp = c(18, 9), ndf = c(40, 15), ash = c(8, 2),
    stringsAsFactors = FALSE
  )

  # 4. Mock de CATEGORIES (Mapeo de animal a dieta)
  mock_categories <- data.frame(
    identification = "vacas_test", diet_tag = "D1",
    animal_type = "cattle", animal_subtype = "dairy",
    stringsAsFactors = FALSE
  )

  # --- CÁLCULO MANUAL ESPERADO ---
  # DE total = (DE_Alfalfa * %Alfalfa_en_forraje * %Forraje_total) + (DE_Maiz * %Maiz_en_feed * %Feed_total)
  # DE total = (60 * 1.0 * 0.60) + (80 * 1.0 * 0.40)
  # DE total = 36 + 32 = 68

  # Ejecutamos la función (Nota: requiere que estas tablas se carguen vía load_dataset)
  # Para el test, podemos usar 'mockery' o simplemente confiar en la lógica si pasamos los datos.
  # Dado que tu función usa load_dataset(), lo más limpio es mockear la carga de datos.

  with_mocked_bindings(
    {
      res <- calculate_weighted_variable(saveoutput = FALSE)

      # Verificaciones
      expect_s3_class(res, "data.frame")

      # Verificamos el valor de DE calculado para nuestro animal de test
      val_de <- res$de[res$identification == "vacas_test"]
      expect_equal(val_de, 68)

      # Verificamos que se han redondeado los resultados (tu función usa round(x, 3))
      expect_equal(val_de, round(val_de, 3))
    },
    load_dataset = function(name) {
      switch(name,
             "diet" = mock_diet,
             "ingredients" = mock_ingredients,
             "characteristics" = mock_chars,
             "categories" = mock_categories)
    }
  )
})
