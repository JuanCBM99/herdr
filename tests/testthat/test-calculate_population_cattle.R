library(testthat)
library(herdr)

test_that("calculate_population_cattle calcula la demografía correctamente", {

  # 1. Crear Censo de prueba (3 vacas lecheras, 2 vacas de carne, 1 toro)
  mock_census <- data.frame(
    group = "G1",
    zone = "North",
    identification = c("mature_dairy_cattle", "mature_beef_cattle", "mature_beef_bull"),
    population = c(100, 100, 10),
    animal_type = "cattle",
    stringsAsFactors = FALSE
  )

  # 2. Crear Parámetros de prueba
  # Ponemos valores sencillos: Tasa parto 1.0 (100%), Tasa reemplazo 0.2 (20%)
  mock_rates <- data.frame(
    animal_type = "cattle",
    parameter = c("calving_rate", "calving_rate", "replacement_rate", "replacement_rate", "replacement_rate"),
    animal_subtype = c("dairy", "beef", "beef", "beef", "dairy"),
    sex = c(NA, NA, "male", "female", "female"),
    value = c(1.0, 1.0, 0.1, 0.2, 0.25),
    stringsAsFactors = FALSE
  )

  # 3. Ejecutar la función
  # Nota: No necesitamos 'categories' para el cálculo interno, pero la función lo acepta
  res <- calculate_population_cattle(mock_census, mock_rates)

  # --- VERIFICACIONES ---

  # A. Verificar que el resultado es un dataframe
  expect_s3_class(res, "data.frame")

  # B. Verificar cálculo de reemplazo de carne (hembras)
  # 100 vacas carne * 0.2 reemplazo = 20
  val_beef_repl <- res$population[res$identification == "beef_calves_female_replacement"]
  expect_equal(val_beef_repl, 20)

  # C. Verificar lógica de Feedlot (Zonas en NA)
  # Los animales de feedlot deben tener zone = NA
  feedlot_data <- res[res$identification %in% c("feedlot_calves_male", "feedlot_calves_female"), ]
  expect_true(all(is.na(feedlot_data$zone)))

  # D. Verificar que no hay poblaciones negativas o cero
  expect_true(all(res$population > 0))
})

test_that("calculate_population_cattle lanza error si faltan categorías base", {
  # Censo incompleto (falta el toro)
  incomplete_census <- data.frame(
    identification = c("mature_dairy_cattle", "mature_beef_cattle"),
    population = c(100, 100),
    animal_type = "cattle"
  )

  expect_error(calculate_population_cattle(incomplete_census, data.frame()))
})
