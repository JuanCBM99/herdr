test_that("calculate_population_sheep calcula reemplazos y sacrificio correctamente", {

  # 1. Crear Censo de prueba (Ovejas)
  mock_census_sheep <- data.frame(
    group = "G1",
    zone = "South",
    identification = c("mature_sheep_male_dairy", "mature_sheep_male_meat",
                       "mature_sheep_female_dairy", "mature_sheep_female_meat"),
    population = c(10, 10, 100, 100),
    animal_type = "sheep",
    stringsAsFactors = FALSE
  )

  # 2. Crear Parámetros (Tasa parto 1.5 - gemelos comunes, Reemplazo 0.2)
  mock_rates_sheep <- data.frame(
    animal_type = "sheep",
    parameter = c("lambing_rate", "lambing_rate", "replacement_rate", "replacement_rate", "replacement_rate", "replacement_rate"),
    animal_subtype = c("dairy", "meat", "dairy", "dairy", "meat", "meat"),
    sex = c(NA, NA, "male", "female", "male", "female"),
    value = c(1.5, 1.2, 0.1, 0.2, 0.1, 0.2),
    stringsAsFactors = FALSE
  )

  # 3. Ejecutar función
  res <- calculate_population_sheep(mock_census_sheep, mock_rates_sheep)

  # --- VERIFICACIONES ---

  # A. Verificar estructura
  expect_s3_class(res, "data.frame")
  expect_true(all(c("group", "zone", "identification", "population") %in% names(res)))

  # B. Verificar Cálculo de Sacrificio (Slaughter) Lechería
  # Nacimientos: 100 hembras * 1.5 tasa = 150 corderos totales
  # Reemplazos: (10 machos * 0.1) + (100 hembras * 0.2) = 1 + 20 = 21 reemplazos
  # Sacrificio: 150 - 21 = 129
  val_slaughter <- res$population[res$identification == "lamb_dairy_slaughter"]
  expect_equal(val_slaughter, 129)

  # C. Verificar que las zonas se mantienen (a diferencia de cattle, no deben ser NA)
  expect_true(all(res$zone == "South"))
})
