library(testthat)
library(herdr)
library(dplyr)
library(readr)
library(tibble)

test_that("calculate_population_swine generates only existing swine tags without extra categories", {
  # Mock definitions matching standard monogastric CSV
  definitions <- tibble::tribble(
    ~animal_tag, ~region, ~subregion, ~class_flex, ~animal_type, ~animal_subtype, ~piglets_born, ~piglets_suckling,
    "breeder_sows", "spain", NA_character_, NA_character_, "swine", NA_character_, 16.26, 13.05,
    "boars", "spain", NA_character_, NA_character_, "swine", NA_character_, NA_real_, NA_real_,
    "replacement_sows", "spain", NA_character_, NA_character_, "swine", NA_character_, NA_real_, NA_real_,
    "fattening_pigs", "spain", NA_character_, NA_character_, "swine", NA_character_, NA_real_, NA_real_
  )

  census_swine <- tibble::tribble(
    ~animal_tag, ~region, ~subregion, ~class_flex, ~population,
    "breeder_sows", "spain", NA_character_, NA_character_, 1000,
    "boars", "spain", NA_character_, NA_character_, 20
  )

  rate_parameters <- tibble::tribble(
    ~animal_tag, ~parameter, ~value,
    "breeder_sows", "replacement_rate", "0.25",
    "breeder_sows", "farrowing_rate", "2.35",
    "breeder_sows", "piglets_per_litter", "12.0"
  )

  res <- calculate_population_swine(census_swine, rate_parameters, definitions)

  expect_s3_class(res, "data.frame")
  expect_true(nrow(res) > 0)

  # Check that only tags in definitions are returned
  allowed_tags <- c("breeder_sows", "boars", "replacement_sows", "fattening_pigs")
  expect_true(all(res$animal_tag %in% allowed_tags))
  expect_false(any(c("piglets", "weaners", "suckling_piglets") %in% res$animal_tag))

  # Mathematical checks
  sows_res <- res %>% filter(animal_tag == "breeder_sows") %>% pull(population)
  expect_equal(sows_res, 1000)

  boars_res <- res %>% filter(animal_tag == "boars") %>% pull(population)
  expect_equal(boars_res, 20)

  repl_res <- res %>% filter(animal_tag == "replacement_sows") %>% pull(population)
  expect_equal(repl_res, 1000 * 0.25)

  fat_res <- res %>% filter(animal_tag == "fattening_pigs") %>% pull(population)
  # annual piglets = 1000 * 2.35 * 12 = 28200
  # slaughter = 28200 - 250 = 27950
  # standing herd = 27950 * (110 / 365)
  expect_true(fat_res > 0)
  expect_true(all(res$population > 0))
})

test_that("calculate_population_swine respects definitions if replacement_sows is not defined", {
  # Definitions WITHOUT replacement_sows
  definitions_limited <- tibble::tribble(
    ~animal_tag, ~region, ~subregion, ~class_flex, ~animal_type, ~animal_subtype, ~piglets_born, ~piglets_suckling,
    "breeder_sows", "spain", NA_character_, NA_character_, "swine", NA_character_, 16.26, 13.05,
    "fattening_pigs", "spain", NA_character_, NA_character_, "swine", NA_character_, NA_real_, NA_real_
  )

  census_swine <- tibble::tribble(
    ~animal_tag, ~region, ~subregion, ~class_flex, ~population,
    "breeder_sows", "spain", NA_character_, NA_character_, 500
  )

  rate_parameters <- tibble::tribble(
    ~animal_tag, ~parameter, ~value,
    "breeder_sows", "replacement_rate", "0.25"
  )

  res <- calculate_population_swine(census_swine, rate_parameters, definitions_limited)

  # Replacement sows MUST NOT be generated because it does not exist in definitions
  expect_false("replacement_sows" %in% res$animal_tag)
  expect_true("breeder_sows" %in% res$animal_tag)
  expect_true("fattening_pigs" %in% res$animal_tag)
})

test_that("calculate_population_swine handles feedlot-only swine farm gracefully", {
  definitions <- tibble::tribble(
    ~animal_tag, ~region, ~subregion, ~class_flex, ~animal_type, ~animal_subtype,
    "fattening_pigs", "spain", NA_character_, NA_character_, "swine", NA_character_
  )

  census_swine <- tibble::tribble(
    ~animal_tag, ~region, ~subregion, ~class_flex, ~population,
    "fattening_pigs", "spain", NA_character_, NA_character_, 5000
  )

  rate_parameters <- tibble::tibble(animal_tag = character(), parameter = character(), value = character())

  res <- calculate_population_swine(census_swine, rate_parameters, definitions)

  expect_equal(nrow(res), 1)
  expect_equal(res$animal_tag, "fattening_pigs")
  expect_equal(res$population, 5000)
})
