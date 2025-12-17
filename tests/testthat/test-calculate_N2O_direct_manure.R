library(testthat)
library(herdr)
library(dplyr)

test_that("calculate_N2O_direct_manure calcula emisiones usando la lógica interna de N_retention y N_intake", {

  keys <- data.frame(
    group = "G1", zone = "Z1", identification = "cow_test",
    animal_type = "cattle", animal_subtype = "dairy",
    stringsAsFactors = FALSE
  )

  # Valores estándar
  mock_ge <- cbind(keys, ge = 300)
  mock_cp <- cbind(keys, cp = 18)
  mock_pop <- cbind(keys, population = 100)
  mock_neg <- cbind(keys, NEg = 10)

  # Para que N_retention sea 0 según tu código: weight_gain debe ser 0
  # y no ser oveja/cabra (es cattle).
  mock_cats <- data.frame(
    identification = "cow_test", animal_type = "cattle", animal_subtype = "dairy",
    milk_yield = 0, fat_content = 3.5, stringsAsFactors = FALSE
  )

  mock_weights <- data.frame(
    identification = "cow_test", animal_type = "cattle", animal_subtype = "dairy",
    weight_gain = 0, # <--- ESTO fuerzo el TRUE ~ 0 en tu case_when
    stringsAsFactors = FALSE
  )

  mock_n2o_config <- data.frame(
    identification = "cow_test", animal_type = "cattle", animal_subtype = "dairy",
    management_system = "solid_storage", climate = "warm", management_duration = 12,
    stringsAsFactors = FALSE
  )

  mock_efs <- data.frame(
    management_system = "solid_storage", climate = "warm", value = 0.005,
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    {
      res <- calculate_N2O_direct_manure(saveoutput = FALSE)

      val_n2o <- res$N2O_emissions[res$identification == "cow_test"]

      # Ahora tiene que ser positivo porque N_retention será 0
      expect_gt(val_n2o, 0)

      # Verificamos que N_retention fue efectivamente 0
      expect_equal(res$N_retention[res$identification == "cow_test"], 0)
    },
    calculate_ge = function(...) mock_ge,
    calculate_weighted_variable = function(...) mock_cp,
    calculate_population = function(...) mock_pop,
    calculate_NEg = function(...) mock_neg,
    load_dataset = function(name) {
      switch(name,
             "categories" = mock_cats,
             "weights" = mock_weights,
             "n2o_direct" = mock_n2o_config,
             "emission_factors_direct" = mock_efs)
    },
    .package = "herdr"
  )
})
