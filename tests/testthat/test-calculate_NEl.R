library(testthat)
library(herdr)

test_that("calculate_NEl calcula la energía de lactancia correctamente", {

  # 1. Mock de CATEGORIES (Una vaca, una oveja y un animal seco/macho)
  mock_categories <- data.frame(
    identification = c("dairy_cow", "dairy_sheep", "dry_animal"),
    animal_type = c("cattle", "sheep", "cattle"),
    animal_subtype = c("lactating", "lactating", "non-lactating"),
    milk_yield = c(5000, 300, 0),    # Producción anual en kg
    fat_content = c(4.0, 6.0, 0),   # % de grasa
    stringsAsFactors = FALSE
  )

  # --- CÁLCULOS MANUALES ESPERADOS ---

  # Cattle: (5000 / 365) * (1.47 + 0.4 * 4.0)
  # 13.6986 * 3.07 = 42.054...
  expected_cow <- round((5000 / 365) * (1.47 + 0.4 * 4.0), 3)

  # Sheep: (300 / 365) * 4.6
  # 0.8219 * 4.6 = 3.7807...
  expected_sheep <- round((300 / 365) * 4.6, 3)

  # Ejecutamos con mocking
  with_mocked_bindings(
    {
      res <- calculate_NEl(saveoutput = FALSE)

      # Verificación Vaca
      val_cow <- res$NEl[res$identification == "dairy_cow"]
      expect_equal(val_cow, expected_cow, tolerance = 1e-3)

      # Verificación Oveja
      val_sheep <- res$NEl[res$identification == "dairy_sheep"]
      expect_equal(val_sheep, expected_sheep, tolerance = 1e-3)

      # Verificación Animal seco (debe ser 0)
      val_dry <- res$NEl[res$identification == "dry_animal"]
      expect_equal(val_dry, 0)
    },
    load_dataset = function(name) {
      if(name == "categories") return(mock_categories)
    }
  )
})
