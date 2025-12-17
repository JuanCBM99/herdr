test_that("calculate_population_goat calcula reemplazos correctamente", {

  mock_census_goat <- data.frame(
    group = "G1",
    zone = "Mountain",
    identification = c("mature_goat_male_dairy", "mature_goat_male_meat",
                       "mature_goat_female_dairy", "mature_goat_female_meat"),
    population = c(20, 20, 200, 200),
    animal_type = "goat",
    stringsAsFactors = FALSE
  )

  mock_rates_goat <- data.frame(
    animal_type = "goat",
    parameter = c(rep("replacement_rate", 4), "kidding_rate", "kidding_rate"),
    animal_subtype = c("dairy", "dairy", "meat", "meat", "dairy", "meat"),
    sex = c("male", "female", "male", "female", NA, NA),
    value = c(0.05, 0.1, 0.05, 0.1, 1.2, 1.2),
    stringsAsFactors = FALSE
  )

  res <- calculate_population_goat(mock_census_goat, mock_rates_goat)

  # Verificación de reemplazo específico
  # 200 hembras meat * 0.1 tasa = 20
  val_meat_repl <- res$population[res$identification == "kid_goat_female_meat_replacement"]
  expect_equal(val_meat_repl, 20)

  # Verificar que NO existen filas de slaughter (ya que la función no las crea)
  expect_false(any(grepl("slaughter", res$identification)))
})
