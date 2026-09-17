library(testthat)
library(herdr)
library(dplyr)
library(tibble)

test_that("calculate_population_poultry preserves broilers without generating pullets if no mature hens present", {
  census <- tibble::tribble(
    ~animal_tag, ~region, ~subregion, ~class_flex, ~population,
    "broilers", "spain", NA_character_, NA_character_, 20000
  )

  rate_parameters <- tibble::tibble(
    animal_tag = character(),
    parameter = character(),
    value = numeric()
  )

  definitions <- tibble::tribble(
    ~animal_tag, ~animal_type, ~animal_subtype,
    "broilers", "poultry", "meat",
    "replacement_meat_pullets", "poultry", "meat",
    "breeder_meat_hens", "poultry", "meat"
  )

  res <- calculate_population_poultry(census, rate_parameters, definitions)

  expect_equal(nrow(res), 1)
  expect_equal(res$animal_tag, "broilers")
  expect_equal(res$population, 20000)
})

test_that("calculate_population_poultry models breeder_meat_hens to replacement_meat_pullets", {
  census <- tibble::tribble(
    ~animal_tag, ~region, ~subregion, ~class_flex, ~population,
    "breeder_meat_hens", "spain", "north", NA_character_, 10000
  )

  rate_parameters <- tibble::tribble(
    ~animal_tag, ~parameter, ~value,
    "breeder_meat_hens", "replacement_rate", 1.0
  )

  definitions <- tibble::tribble(
    ~animal_tag, ~animal_type, ~animal_subtype,
    "breeder_meat_hens", "poultry", "meat",
    "replacement_meat_pullets", "poultry", "meat"
  )

  res <- calculate_population_poultry(census, rate_parameters, definitions)

  expect_true("breeder_meat_hens" %in% res$animal_tag)
  expect_true("replacement_meat_pullets" %in% res$animal_tag)

  # Default rearing days: 140 -> 10000 * 1.0 * (140 / 365) = 3835.616
  pullet_pop <- res %>% filter(animal_tag == "replacement_meat_pullets") %>% pull(population)
  expect_equal(round(pullet_pop, 2), 3835.62)
})

test_that("calculate_population_poultry models laying_hens to replacement_layer_pullets", {
  census <- tibble::tribble(
    ~animal_tag, ~region, ~subregion, ~class_flex, ~population,
    "laying_hens", "spain", NA_character_, NA_character_, 50000
  )

  rate_parameters <- tibble::tribble(
    ~animal_tag, ~parameter, ~value,
    "laying_hens", "replacement_rate", 1.0
  )

  definitions <- tibble::tribble(
    ~animal_tag, ~animal_type, ~animal_subtype,
    "laying_hens", "poultry", "layer",
    "replacement_layer_pullets", "poultry", "layer"
  )

  res <- calculate_population_poultry(census, rate_parameters, definitions)

  expect_true("laying_hens" %in% res$animal_tag)
  expect_true("replacement_layer_pullets" %in% res$animal_tag)

  # Default rearing days: 119 -> 50000 * 1.0 * (119 / 365) = 16301.37
  pullet_pop <- res %>% filter(animal_tag == "replacement_layer_pullets") %>% pull(population)
  expect_equal(round(pullet_pop, 2), 16301.37)
})

test_that("calculate_population_poultry preserves broilers in mixed farm with breeder hens", {
  census <- tibble::tribble(
    ~animal_tag, ~region, ~subregion, ~class_flex, ~population,
    "breeder_meat_hens", "spain", NA_character_, NA_character_, 10000,
    "broilers", "spain", NA_character_, NA_character_, 30000
  )

  rate_parameters <- tibble::tribble(
    ~animal_tag, ~parameter, ~value,
    "breeder_meat_hens", "replacement_rate", 1.0
  )

  definitions <- tibble::tribble(
    ~animal_tag, ~animal_type, ~animal_subtype,
    "breeder_meat_hens", "poultry", "meat",
    "replacement_meat_pullets", "poultry", "meat",
    "broilers", "poultry", "meat"
  )

  res <- calculate_population_poultry(census, rate_parameters, definitions)

  expect_setequal(res$animal_tag, c("breeder_meat_hens", "replacement_meat_pullets", "broilers"))
  broiler_pop <- res %>% filter(animal_tag == "broilers") %>% pull(population)
  expect_equal(broiler_pop, 30000)
})

test_that("calculate_population_poultry strictly respects user definitions", {
  census <- tibble::tribble(
    ~animal_tag, ~region, ~subregion, ~class_flex, ~population,
    "breeder_meat_hens", "spain", NA_character_, NA_character_, 10000
  )

  rate_parameters <- tibble::tribble(
    ~animal_tag, ~parameter, ~value,
    "breeder_meat_hens", "replacement_rate", 1.0
  )

  # Definitions do NOT include replacement_meat_pullets
  definitions <- tibble::tribble(
    ~animal_tag, ~animal_type, ~animal_subtype,
    "breeder_meat_hens", "poultry", "meat"
  )

  res <- calculate_population_poultry(census, rate_parameters, definitions)

  # Only breeder_meat_hens should be returned
  expect_equal(res$animal_tag, "breeder_meat_hens")
  expect_false("replacement_meat_pullets" %in% res$animal_tag)
})

test_that("calculate_population_poultry automatically estimates replacement rate from laying cycle days when missing", {
  census <- tibble::tribble(
    ~animal_tag, ~region, ~subregion, ~class_flex, ~population,
    "laying_hens", "spain", NA_character_, NA_character_, 10000
  )

  rate_parameters <- tibble::tibble(
    animal_tag = character(),
    parameter = character(),
    value = numeric()
  )

  definitions <- tibble::tribble(
    ~animal_tag, ~animal_type, ~animal_subtype,
    "laying_hens", "poultry", "layer",
    "replacement_layer_pullets", "poultry", "layer"
  )

  res <- calculate_population_poultry(census, rate_parameters, definitions)

  # Estimated rate = 365 / 511 = 0.7142857
  # Annual pullets = 10000 * (365 / 511) = 7142.857
  # Standing AAP = 7142.857 * (119 / 365) = 10000 * (119 / 511) = 2328.767
  pullet_pop <- res %>% filter(animal_tag == "replacement_layer_pullets") %>% pull(population)
  expect_equal(round(pullet_pop, 2), 2328.77)
})

test_that("calculate_population_poultry automatically models broilers from breeder_meat_hens when definitions include broilers", {
  census <- tibble::tribble(
    ~animal_tag, ~region, ~subregion, ~class_flex, ~population,
    "breeder_meat_hens", "spain", NA_character_, NA_character_, 10000
  )

  rate_parameters <- tibble::tribble(
    ~animal_tag, ~parameter, ~value,
    "breeder_meat_hens", "replacement_rate", 1.0
  )

  definitions <- tibble::tribble(
    ~animal_tag, ~animal_type, ~animal_subtype, ~egg_mass_g_day, ~egg_weight_g, ~fertility_rate,
    "breeder_meat_hens", "poultry", "meat", 37.12, 64.0, 0.83,
    "replacement_meat_pullets", "poultry", "meat", 0, NA_real_, NA_real_,
    "broilers", "poultry", "meat", 0, NA_real_, NA_real_
  )

  res <- calculate_population_poultry(census, rate_parameters, definitions)

  expect_setequal(res$animal_tag, c("breeder_meat_hens", "replacement_meat_pullets", "broilers"))

  # Broiler AAP check: 10000 * (37.12 / 64) * 0.83 * 42 = 202287.6
  broiler_pop <- res %>% filter(animal_tag == "broilers") %>% pull(population)
  expected_broilers <- 10000 * (37.12 / 64.0) * 0.83 * 42
  expect_equal(broiler_pop, expected_broilers, tolerance = 1e-2)

  # Replacement pullets check: 10000 * 1.0 * (140 / 365) = 3835.616
  pullet_pop <- res %>% filter(animal_tag == "replacement_meat_pullets") %>% pull(population)
  expect_equal(pullet_pop, 10000 * (140 / 365), tolerance = 1e-2)
})

test_that("calculate_population_poultry uses default fallbacks for egg_weight_g and fertility_rate when omitted", {
  census <- tibble::tribble(
    ~animal_tag, ~region, ~subregion, ~class_flex, ~population,
    "breeder_meat_hens", "spain", NA_character_, NA_character_, 5000
  )

  rate_parameters <- tibble::tribble(
    ~animal_tag, ~parameter, ~value,
    "breeder_meat_hens", "replacement_rate", 1.0
  )

  # No egg_weight_g or fertility_rate columns provided
  definitions <- tibble::tribble(
    ~animal_tag, ~animal_type, ~animal_subtype, ~egg_mass_g_day,
    "breeder_meat_hens", "poultry", "meat", 37.12,
    "replacement_meat_pullets", "poultry", "meat", 0,
    "broilers", "poultry", "meat", 0
  )

  res <- calculate_population_poultry(census, rate_parameters, definitions)

  expect_true("broilers" %in% res$animal_tag)
  broiler_pop <- res %>% filter(animal_tag == "broilers") %>% pull(population)
  # Expected with defaults: 64.0g egg weight and 0.83 fertility
  expected_broilers <- 5000 * (37.12 / 64.0) * 0.83 * 42
  expect_equal(broiler_pop, expected_broilers, tolerance = 1e-2)
})


