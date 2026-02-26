library(testthat)
library(herdr)
library(dplyr)
library(tidyr)

test_that("calculate_population_sheep correctly expands population with class_flex", {

  # 1. Mock Base Census (Sheep)
  # Usamos las 4 identificaciones requeridas
  mock_census_sheep <- data.frame(
    region = "G1",
    subregion = "Z1",
    animal_tag = c("mature_sheep_male_dairy", "mature_sheep_female_dairy",
                   "mature_sheep_male_meat", "mature_sheep_female_meat"),
    class_flex = "grazing", # Mantenemos la llave de identidad
    population = c(5, 100, 10, 200),
    stringsAsFactors = FALSE
  )

  # 2. Mock Rate Parameters (Sheep)
  mock_rates_sheep <- data.frame(
    animal_type = "sheep",
    animal_subtype = c("dairy", "meat", "dairy", "dairy", "meat", "meat"),
    parameter = c("lambing_rate", "lambing_rate", "replacement_rate", "replacement_rate", "replacement_rate", "replacement_rate"),
    sex = c(NA, NA, "male", "female", "male", "female"),
    value = c(1.5, 1.2, 0.1, 0.2, 0.15, 0.25),
    stringsAsFactors = FALSE
  )

  # 3. Execution
  # Nota: Si tu función actual NO tiene class_flex, este test fallará en la columna class_flex.
  # Es vital añadir class_flex al group_by y pivot_longer de la función original.
  res <- calculate_population_sheep(
    census_sheep = mock_census_sheep,
    rate_parameters = mock_rates_sheep
  )

  # 4. Assertions
  # Verificamos que las crías se calculen bien
  # Dairy: 100 hembras * 1.5 lambing_rate = 150 corderos totales
  # Reemplazos: 100 hembras * 0.2 = 20 hembras / 5 machos * 0.1 = 0.5 machos
  # Slaughter: 150 - 20 - 0.5 = 129.5

  slaughter_val <- res %>%
    filter(animal_tag == "lamb_dairy_slaughter") %>%
    pull(population)

  expect_equal(slaughter_val, 129.5)

  # Verificamos que no se pierdan las llaves de identidad
  expect_true("region" %in% colnames(res))
  expect_true("subregion" %in% colnames(res))
  expect_true("animal_tag" %in% colnames(res))
})
