library(testthat)
library(herdr)
library(readr)
library(dplyr)

path_census <- test_path("test_data", "user_data/livestock_census.csv")
path_rates  <- test_path("test_data", "user_data/reproduction_parameters.csv")

test_that("calculate_population_goat returns correct structure", {

  mock_census <- read_csv(path_census, show_col_types = FALSE) %>%
    filter(grepl("goat", animal_tag)) %>%
    mutate(population = case_when(
      animal_tag == "mature_goat_female_dairy" ~ 1000,
      animal_tag == "mature_goat_male_dairy"   ~ 100,
      TRUE ~ 0
    ))

  mock_rates <- read_csv(path_rates, show_col_types = FALSE) %>%
    filter(grepl("goat", animal_tag))

  results <- calculate_population_goat(mock_census, mock_rates)

  expect_s3_class(results, "data.frame")
  expect_true(any(grepl("kid", results$animal_tag)))
  expect_true(all(c("region", "subregion", "class_flex", "animal_tag", "population") %in% colnames(results)))
})

test_that("goat population correctly calculates replacement cohorts", {

  mock_census <- read_csv(path_census, show_col_types = FALSE) %>%
    filter(grepl("goat", animal_tag)) %>%
    mutate(population = case_when(
      animal_tag == "mature_goat_female_dairy" ~ 1000,
      animal_tag == "mature_goat_male_dairy"   ~ 100,
      TRUE ~ 0
    ), class_flex = "grazing")

  mock_rates <- read_csv(path_rates, show_col_types = FALSE) %>%
    filter(grepl("goat", animal_tag)) %>%
    mutate(value = case_when(
      animal_tag == "mature_goat_female_dairy" & parameter == "pr_sheep_goat"     ~ 1.5,
      animal_tag == "mature_goat_female_dairy" & parameter == "replacement_rate" ~ 0.2,
      animal_tag == "mature_goat_male_dairy"   & parameter == "replacement_rate" ~ 0.1,
      TRUE ~ 0
    ))

  res <- calculate_population_goat(mock_census, mock_rates)

  f_repl <- res %>% filter(animal_tag == "kid_goat_female_dairy_replacement") %>% pull(population)
  m_repl <- res %>% filter(animal_tag == "kid_goat_male_dairy_replacement") %>% pull(population)
  dairy_slaughter <- res %>% filter(animal_tag == "kid_goat_dairy_slaughter") %>% pull(population)

  expect_equal(f_repl, 200)
  expect_equal(m_repl, 10)
  expect_equal(dairy_slaughter, 1290)
})

test_that("goat function filters zero populations automatically", {

  mock_census <- read_csv(path_census, show_col_types = FALSE) %>%
    filter(grepl("goat", animal_tag)) %>%
    mutate(population = ifelse(animal_tag == "mature_goat_female_dairy", 100, 0))

  mock_rates <- read_csv(path_rates, show_col_types = FALSE) %>%
    filter(grepl("goat", animal_tag)) %>%
    mutate(value = 0.5)

  res <- calculate_population_goat(mock_census, mock_rates)

  expect_true(all(res$population > 0))
  expect_false(any(grepl("_meat", res$animal_tag)))
})

test_that("calculate_population_goat handles accelerated kidding cycles and slaughter AAP scaling", {
  mock_census <- tibble::tribble(
    ~animal_tag, ~region, ~subregion, ~class_flex, ~population,
    "mature_goat_female_dairy", "spain", "andalucia", NA_character_, 1000
  )

  mock_rates <- tibble::tribble(
    ~animal_tag, ~parameter, ~value,
    "mature_goat_female_dairy", "pr_sheep_goat", 1.5,
    "mature_goat_female_dairy", "replacement_rate", 0.2
  )

  mock_weights <- tibble::tribble(
    ~animal_tag, ~productive_period_days,
    "mature_goat_female_dairy", 200,
    "kid_goat_dairy_slaughter", 70
  )

  res <- calculate_population_goat(mock_census, mock_rates, definitions = NULL, weights = mock_weights)

  # Births: 1000 * (365 / 200) * 1.5 = 2737.5
  # Replacements: 1000 * 0.2 = 200
  # Annual slaughter: 2737.5 - 200 = 2537.5
  # AAP slaughter: 2537.5 * (70 / 365) = 486.6438
  dairy_slaughter <- res %>%
    filter(animal_tag == "kid_goat_dairy_slaughter") %>%
    pull(population)

  expect_equal(round(dairy_slaughter, 2), 486.64)
})

