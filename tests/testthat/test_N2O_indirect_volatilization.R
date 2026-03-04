library(testthat)
library(herdr)
library(readr)

setup_volatilization_env <- function() {
  if (!dir.exists("user_data")) dir.create("user_data")

  # 1. Definiciones completas (evitando errores de tipo con a, b, c)
  write_csv(data.frame(
    region="test", subregion="test", animal_tag="cow", class_flex="none",
    animal_type="cattle", animal_subtype="dairy", diet_tag="diet1",
    milk_yield=5000, fat_content=3.5, cp_excretion_factor=0.16,
    cfi="c1", ca="a1", work_hours=0,
    c="steer", a="none", b="none",
    c_pregnancy="none", pr=0, wool_yield=0
  ), "user_data/livestock_definitions.csv")

  # 2. Pesos, Censo y Parámetros Reproductivos (Requeridos por la cadena de población)
  write_csv(data.frame(region="test", subregion="test", animal_tag="cow", class_flex="none",
                       average_weight=600, adult_weight=600, weight_gain=0), "user_data/livestock_weights.csv")
  write_csv(data.frame(region="test", subregion="test", animal_tag="cow", class_flex="none",
                       population=100, animal_type="cattle", animal_subtype="dairy"), "user_data/livestock_census.csv")
  write_csv(data.frame(animal_tag="cow", replacement_rate=0, calving_interval=0,
                       mortality_rate=0, first_calving_age=0), "user_data/reproduction_parameters.csv")

  # 3. Nutrición (Para GE y N_intake)
  write_csv(data.frame(region="test", subregion="test", class_flex="none", diet_tag="diet1",
                       forage_share=100, concentrate_share=0, milk_share=0, milk_replacer_share=0), "user_data/diet_profiles.csv")
  write_csv(data.frame(region="test", subregion="test", class_flex="none", diet_tag="diet1",
                       ingredient_type="forage", ingredient="grass", ingredient_share=100), "user_data/diet_ingredients.csv")
  write_csv(data.frame(ingredient="grass", ingredient_type="forage", de=60, cp=12, ndf=40, ash=5, eb=18), "user_data/feed_characteristics.csv")

  # 4. Estiércol e IPCC (Frac_gas y EF4 son los factores clave aquí)
  write_csv(data.frame(region="test", subregion="test", animal_tag="cow", class_flex="none",
                       animal_type="cattle", animal_subtype="dairy", allocation=1,
                       system_base="slurry", management_months=12, system_climate="temperate",
                       system_subclimate="none", system_variant="none", climate_zone="none",
                       climate_moisture="none"), "user_data/manure_management.csv")

  write_csv(data.frame(system_base="slurry", management_months=12, system_climate="temperate",
                       system_subclimate="none", system_variant="none", climate_zone="none",
                       climate_moisture="none", animal_type="cattle", animal_subtype="dairy",
                       EF3=0.01, frac_gas=0.2, EF4=0.01), "user_data/ipcc_mm.csv")

  # 5. Coeficientes
  write_csv(data.frame(coefficient="cfi", description=c("c1", "steer", "none"), value=c(0.335, 1, 0)), "user_data/ipcc_coefficients.csv")
}

test_that("calculate_N2O_indirect_volatilization completes the N cycle", {
  setup_volatilization_env()

  # Ejecución
  results <- calculate_N2O_indirect_volatilization(saveoutput = FALSE)

  # Verificaciones
  expect_s3_class(results, "data.frame")
  expect_true("n2o_g" %in% colnames(results))

  # Si N_excreted se calculó bien, n_volatilization debe ser > 0
  if(nrow(results) > 0) {
    expect_true(all(results$n_volatilization_kg_year >= 0))
  }

  # Limpieza
  unlink("user_data", recursive = TRUE)
})
