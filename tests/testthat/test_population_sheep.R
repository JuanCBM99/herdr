library(testthat)
library(herdr)
library(readr)
library(dplyr)

path_census <- test_path("test_data", "user_data/livestock_census.csv")
path_defs   <- test_path("test_data", "user_data/ruminant_definitions.csv")

test_that("calculate_population_sheep returns correct structure and math from CSV", {

  census_raw <- read_csv(path_census, show_col_types = FALSE)

  if (!any(grepl("sheep", census_raw$animal_tag))) skip("No sheep tags in census CSV")

  mock_census <- census_raw %>%
    filter(grepl("sheep", animal_tag)) %>%
    mutate(population = ifelse(animal_tag == "mature_sheep_female_meat", 1000, 0))

  defs_raw <- read_csv(path_defs, show_col_types = FALSE)

  mock_defs <- defs_raw %>%
    filter(grepl("sheep", animal_tag)) %>%
    mutate(
      pr_sheep_goat = ifelse(animal_tag == "mature_sheep_female_meat", 1.5, 0),
      replacement_rate = ifelse(animal_tag == "mature_sheep_female_meat", 0.2, 0)
    )

  results <- calculate_population_sheep(mock_census, mock_defs)

  expect_s3_class(results, "data.frame")
  expect_true(all(c("farm_id", "region", "subregion", "animal_tag", "population") %in% colnames(results)))

  meat_slaughter <- results %>%
    filter(animal_tag == "lamb_meat_slaughter") %>%
    pull(population)

  expect_equal(meat_slaughter, 1300)
})

test_that("calculate_population_sheep filters out zero populations using CSV structure", {

  census_raw <- read_csv(path_census, show_col_types = FALSE)
  defs_raw   <- read_csv(path_defs, show_col_types = FALSE)

  mock_census <- census_raw %>%
    filter(grepl("sheep", animal_tag)) %>%
    mutate(population = ifelse(animal_tag == "mature_sheep_female_meat", 100, 0))

  mock_defs <- defs_raw %>%
    filter(grepl("sheep", animal_tag)) %>%
    mutate(replacement_rate = 0.1, pr_sheep_goat = 1.0)

  res <- calculate_population_sheep(mock_census, mock_defs)

  expect_true(all(res$population > 0))
  expect_false("lamb_dairy_slaughter" %in% res$animal_tag)
})

test_that("calculate_population_sheep handles accelerated lambing cycles and slaughter AAP scaling", {
  mock_census <- tibble::tribble(
    ~farm_id, ~animal_tag, ~region, ~subregion, ~population,
    "farm_1", "mature_sheep_female_meat", "spain", "aragon", 1000
  )

  mock_rates <- tibble::tribble(
    ~animal_tag, ~parameter, ~value,
    "mature_sheep_female_meat", "pr_sheep_goat", 1.5,
    "mature_sheep_female_meat", "replacement_rate", 0.2
  )

  mock_weights <- tibble::tribble(
    ~animal_tag, ~productive_period_days,
    "mature_sheep_female_meat", 200,
    "lamb_meat_slaughter", 70
  )

  res <- calculate_population_sheep(mock_census, mock_rates, definitions = NULL, weights = mock_weights)

  # Births: 1000 * (365 / 200) * 1.5 = 2737.5
  # Replacements: 1000 * 0.2 = 200
  # Annual slaughter: 2737.5 - 200 = 2537.5
  # AAP slaughter: 2537.5 * (70 / 365) = 486.6438
  meat_slaughter <- res %>%
    filter(animal_tag == "lamb_meat_slaughter") %>%
    pull(population)

  expect_equal(round(meat_slaughter, 2), 486.64)
})

