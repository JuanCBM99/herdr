library(testthat)
library(herdr)
library(readr)
library(dplyr)

setup_land_use_final <- function() {
  if (!dir.exists("user_data")) dir.create("user_data")

  # 1. El Super-Archivo de Definiciones (Evita el error de 'wool_yield' y otros)
  write_csv(data.frame(
    region="test", subregion="test", animal_tag="cow", class_flex="none",
    animal_type="cattle", animal_subtype="dairy", diet_tag="diet1",
    milk_yield=5000, fat_content=3.5,
    wool_yield=0,           # <--- La columna que faltaba
    cp_excretion_factor=0.16,
    cfi="c1", ca="a1", work_hours=0,
    c="steer", a="none", b="none",
    c_pregnancy="none", pr=0
  ), "user_data/livestock_definitions.csv")

  # 2. Pesos y Censo (Necesarios para GE y Población)
  write_csv(data.frame(
    region="test", subregion="test", animal_tag="cow", class_flex="none",
    average_weight=600, adult_weight=600, weight_gain=0
  ), "user_data/livestock_weights.csv")

  write_csv(data.frame(
    region="test", subregion="test", animal_tag="cow", class_flex="none",
    population=100, animal_type="cattle", animal_subtype="dairy"
  ), "user_data/livestock_census.csv")

  write_csv(data.frame(
    animal_tag="cow", replacement_rate=0, calving_interval=0,
    mortality_rate=0, first_calving_age=0
  ), "user_data/reproduction_parameters.csv")

  # 3. Dieta y Cultivos (Necesarios para Land Use)
  write_csv(data.frame(
    region="test", subregion="test", class_flex="none", diet_tag="diet1",
    forage_share=100, concentrate_share=0, milk_share=0, milk_replacer_share=0
  ), "user_data/diet_profiles.csv")

  write_csv(data.frame(
    region="test", subregion="test", class_flex="none", diet_tag="diet1",
    ingredient_type="forage", ingredient="alfalfa", ingredient_share=100
  ), "user_data/diet_ingredients.csv")

  write_csv(data.frame(
    ingredient="alfalfa", dry_matter_yield=12000
  ), "user_data/crop_yields.csv")

  # 4. Características y Coeficientes
  write_csv(data.frame(
    ingredient="alfalfa", ingredient_type="forage",
    de=60, cp=18, ndf=40, ash=8, eb=18.4
  ), "user_data/feed_characteristics.csv")

  write_csv(data.frame(
    coefficient="cfi", description=c("c1", "steer", "none"), value=c(0.335, 1, 0)
  ), "user_data/ipcc_coefficients.csv")
}

test_that("calculate_land_use computes m2 based on metabolic demand", {
  setup_land_use_final()

  # Ejecución: Ahora la cadena NE_wool -> GE -> Land Use tiene todas sus columnas
  results <- calculate_land_use(saveoutput = FALSE)

  expect_s3_class(results, "data.frame")
  expect_true("Land_use_per_animal" %in% colnames(results))

  if(nrow(results) > 0) {
    expect_gt(results$Land_use_per_animal[1], 0)
  }

  # Limpieza
  if (dir.exists("user_data")) unlink("user_data", recursive = TRUE)
})
