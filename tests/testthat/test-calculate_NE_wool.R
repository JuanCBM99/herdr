library(testthat)
library(herdr)

test_that("calculate_NE_wool calcula la energía para la lana correctamente", {

  # 1. Mock de CATEGORIES con TODAS las columnas que pide el select final
  mock_categories <- data.frame(
    group = "G1",
    zone = "South",
    identification = c("merino_sheep", "dairy_cow"),
    animal_type = c("sheep", "cattle"),
    animal_subtype = c("meat", "dairy"), # Columna que faltaba
    wool_yield = c(4.5, 0),
    stringsAsFactors = FALSE
  )

  # --- CÁLCULO MANUAL ESPERADO ---
  expected_sheep <- round((4.5 / 365) * 24, 3)

  with_mocked_bindings(
    {
      res <- calculate_NE_wool(saveoutput = FALSE)

      expect_s3_class(res, "data.frame")

      # Verificamos que el valor calculado sea correcto
      val_sheep <- res$NE_wool[res$identification == "merino_sheep"]
      expect_equal(val_sheep, expected_sheep, tolerance = 1e-3)

      # Verificamos que la columna 'animal_subtype' esté presente en el resultado
      expect_true("animal_subtype" %in% names(res))
    },
    load_dataset = function(name) {
      if(name == "categories") return(mock_categories)
    }
  )
})
