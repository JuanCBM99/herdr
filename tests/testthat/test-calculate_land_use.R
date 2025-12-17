library(testthat)
library(herdr)
library(dplyr)

test_that("calculate_land_use calcula correctamente la superficie necesaria", {

  # 1. Definimos las claves una sola vez para consistencia
  keys <- data.frame(
    group = "G1",
    zone = "Z1",
    identification = "cow_test",
    animal_type = "cattle",
    animal_subtype = "dairy",
    stringsAsFactors = FALSE
  )

  # 2. Mock de Categorías (DMI) - Incluye diet_tag para el join
  mock_categories <- keys %>%
    mutate(diet_tag = "diet_1", dm_ingested_total = 5000)

  # 3. Mock de Dieta - DEBE tener las columnas de keys
  mock_diet <- keys %>%
    mutate(diet_tag = "diet_1")

  # 4. Mock de Ingredientes - DEBE tener las columnas de keys y los shares
  mock_ingredients <- keys %>%
    mutate(
      diet_tag = "diet_1",
      ingredient = "corn",
      ingredient_type = "forage",
      ingredient_share = 100,
      forage_share = 100,
      feed_share = 0,
      milk_share = 0,
      milk_replacer_share = 0
    )

  # 5. Mock de Cultivos (Yield)
  mock_crops <- data.frame(
    ingredient = "corn",
    dry_matter_yield = 10000,
    stringsAsFactors = FALSE
  )

  # 6. Mock de Población
  mock_pop <- keys %>%
    mutate(population = 10)

  # --- Ejecución ---
  with_mocked_bindings(
    {
      res <- calculate_land_use(saveoutput = FALSE)

      # Verificaciones finales
      val_animal <- res$Land_use_per_animal[res$identification == "cow_test"]
      val_total <- res$Land_use_Total_m2[res$identification == "cow_test"]

      expect_s3_class(res, "data.frame")

      # Cálculo: 5000 kg consumo / 10000 yield * 10000 m2/ha = 5000 m2
      expect_equal(val_animal, 5000, tolerance = 1e-3)
      expect_equal(val_total, 50000, tolerance = 1e-3)
    },
    calculate_population = function(...) mock_pop,
    load_dataset = function(name) {
      switch(name,
             "categories" = mock_categories,
             "diet" = mock_diet,
             "ingredients" = mock_ingredients,
             "crops" = mock_crops)
    },
    .package = "herdr"
  )
})
