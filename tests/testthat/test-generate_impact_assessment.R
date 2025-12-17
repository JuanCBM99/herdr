library(testthat)
library(herdr)
library(dplyr)

test_that("generate_impact_assessment consolida todos los impactos correctamente", {

  # 1. Definimos un set de datos mínimo coherente
  keys <- data.frame(
    group = "G1", zone = "Z1", identification = "cow_test",
    animal_type = "cattle", animal_subtype = "dairy",
    stringsAsFactors = FALSE
  )

  # 2. Preparamos lo que cada sub-función debería devolver
  # (Simulamos los outputs que ya validamos en tests anteriores)
  mock_enteric <- keys %>% mutate(emissions_total = 10.5)
  mock_manure  <- keys %>% mutate(Emissions_CH4_Mg_year = 2.3)
  mock_n2o_dir <- keys %>% mutate(N2O_emissions = 0.5)
  mock_n2o_vol <- keys %>% mutate(n2o_g = 0.1)
  mock_n2o_lea <- keys %>% mutate(N2O_L = 0.05)
  mock_land    <- keys %>% mutate(Land_use_Total_m2 = 5000)

  # 3. Ejecución con Mocked Bindings de TODO el ecosistema
  with_mocked_bindings(
    {
      # Test A: Funcionamiento básico (agrupado por identificación)
      res <- generate_impact_assessment(saveoutput = FALSE, group_by_identification = TRUE)

      expect_s3_class(res, "data.frame")
      expect_equal(nrow(res), 1)

      # Verificamos que las columnas de impacto existan y tengan los valores sumados
      expect_equal(res$Emissions_CH4_enteric, 10.5)
      expect_equal(res$Land_use_m2, 5000)
      expect_equal(res$Emissions_N2O_indirect_vol, 0.1)

      # Test B: Filtros (debe devolver 0 filas si filtramos por algo que no existe)
      res_filtered <- generate_impact_assessment(animal = "sheep", saveoutput = FALSE)
      expect_equal(nrow(res_filtered), 0)

      # Test C: Validación de datos (mock de las funciones de validación)
      # Si no hay error en el tryCatch, el mensaje de éxito debe aparecer
    },
    # Mockeamos la validación para que no pida archivos reales
    validate_data = function(...) TRUE,
    calculate_population = function(...) keys %>% mutate(population = 100),
    calculate_weighted_variable = function(...) keys %>% mutate(cp = 16),
    # Mockeamos todas las funciones de impacto
    calculate_emissions_enteric = function(...) mock_enteric,
    calculate_CH4_manure = function(...) mock_manure,
    calculate_N2O_direct_manure = function(...) mock_n2o_dir,
    calculate_N2O_indirect_volatilization = function(...) mock_n2o_vol,
    calculate_N2O_indirect_leaching = function(...) mock_n2o_lea,
    calculate_land_use = function(...) mock_land,
    .package = "herdr"
  )
})
