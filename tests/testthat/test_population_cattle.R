library(testthat)
library(herdr)
library(readr)
library(dplyr)

path_census <- test_path("test_data", "user_data/livestock_census.csv")
path_defs   <- test_path("test_data", "user_data/ruminant_definitions.csv")

test_that("population math follows biological sex-ratio and replacement rules", {

  mock_census <- read_csv(path_census, show_col_types = FALSE) %>%
    filter(animal_tag == "mature_dairy_cattle") %>%
    mutate(population = 1000)

  mock_defs <- read_csv(path_defs, show_col_types = FALSE) %>%
    filter(animal_tag == "mature_dairy_cattle") %>%
    mutate(
      pregnancy_rate = 1.0,
      replacement_rate = 0.1
    )

  res <- calculate_population_cattle(mock_census, mock_defs)

  males <- res %>% filter(animal_tag == "feedlot_calves_male") %>% pull(population)
  expect_equal(males, 500)

  repl_females <- res %>% filter(animal_tag == "dairy_calves_female_replacement") %>% pull(population)
  expect_equal(repl_females, 100)
})

test_that("region and subregion are preserved from parents for all offspring including feedlot", {
  mock_census <- tibble::tribble(
    ~farm_id, ~animal_tag, ~region, ~subregion, ~population,
    "farm_1", "mature_dairy_cattle", "spain", "Gipuzkoa", 100
  )

  mock_rates <- tibble::tribble(
    ~animal_tag, ~parameter, ~value,
    "mature_dairy_cattle", "pregnancy_rate", "0.9",
    "mature_dairy_cattle", "replacement_rate", "0.2"
  )

  res <- calculate_population_cattle(mock_census, mock_rates)

  feedlot_males <- res %>% filter(animal_tag == "feedlot_calves_male")
  expect_equal(feedlot_males$farm_id[1], "farm_1")
  expect_equal(feedlot_males$region[1], "spain")
  expect_equal(feedlot_males$subregion[1], "Gipuzkoa")

  feedlot_females <- res %>% filter(animal_tag == "feedlot_calves_female")
  expect_equal(feedlot_females$farm_id[1], "farm_1")
  expect_equal(feedlot_females$subregion[1], "Gipuzkoa")

  repl_females <- res %>% filter(animal_tag == "dairy_calves_female_replacement")
  expect_equal(repl_females$farm_id[1], "farm_1")
  expect_equal(repl_females$subregion[1], "Gipuzkoa")
})

test_that("demographic warning is triggered and negative populations are prevented when replacement > births", {
  mock_census <- tibble::tribble(
    ~farm_id, ~animal_tag, ~region, ~subregion, ~population,
    "farm_1", "mature_dairy_cattle", "spain", NA_character_, 100
  )

  # Calving rate = 0.50 (female calves = 25), replacement rate = 0.40 (demand = 40)
  mock_rates <- tibble::tribble(
    ~animal_tag, ~parameter, ~value,
    "mature_dairy_cattle", "pregnancy_rate", "0.5",
    "mature_dairy_cattle", "replacement_rate", "0.4"
  )

  expect_warning(
    res <- calculate_population_cattle(mock_census, mock_rates),
    "Demographic Warning"
  )

  # Female feedlot must be clamped to 0 rather than negative
  female_feedlot <- res %>% filter(animal_tag == "feedlot_calves_female") %>% pull(population)
  if (length(female_feedlot) > 0) {
    expect_true(female_feedlot >= 0)
  }
})

test_that("feedlot and slaughter calves are scaled to AAP according to productive_period_days", {
  mock_census <- tibble::tribble(
    ~farm_id, ~animal_tag, ~region, ~subregion, ~population,
    "farm_1", "mature_dairy_cattle", "spain", NA_character_, 1000
  )

  mock_rates <- tibble::tribble(
    ~animal_tag, ~parameter, ~value,
    "mature_dairy_cattle", "pregnancy_rate", "1.0",
    "mature_dairy_cattle", "replacement_rate", "0.1"
  )

  # Calves fattened for 210 days instead of 365
  mock_weights <- tibble::tribble(
    ~animal_tag, ~productive_period_days,
    "feedlot_calves_male", 210,
    "feedlot_calves_female", 180
  )

  res <- calculate_population_cattle(mock_census, mock_rates, weights = mock_weights)

  males <- res %>% filter(animal_tag == "feedlot_calves_male") %>% pull(population)
  # 500 born * (210 / 365)
  expect_equal(males, 500 * (210 / 365), tolerance = 1e-4)

  females <- res %>% filter(animal_tag == "feedlot_calves_female") %>% pull(population)
  # (500 born - 100 repl = 400 slaughter) * (180 / 365)
  expect_equal(females, 400 * (180 / 365), tolerance = 1e-4)

  # Replacement calves remain full annual cohort (100 head)
  repl_females <- res %>% filter(animal_tag == "dairy_calves_female_replacement") %>% pull(population)
  expect_equal(repl_females, 100)
})

test_that("demographic parameters vary dynamically across strata and years without val[1] lock", {
  mock_census <- tibble::tribble(
    ~farm_id, ~animal_tag, ~region, ~subregion, ~population,
    "farm_1", "mature_dairy_cattle", "spain_1990", "north", 1000,
    "farm_1", "mature_dairy_cattle", "spain_2015", "north", 1000
  )

  mock_defs <- tibble::tribble(
    ~farm_id, ~animal_tag, ~region, ~subregion, ~pregnancy_rate, ~replacement_rate,
    "farm_1", "mature_dairy_cattle", "spain_1990", "north", 1.0, 0.19,
    "farm_1", "mature_dairy_cattle", "spain_2015", "north", 1.0, 0.27
  )

  res <- calculate_population_cattle(mock_census, mock_defs)

  repl_1990 <- res %>%
    filter(animal_tag == "dairy_calves_female_replacement", region == "spain_1990") %>%
    pull(population)
  expect_equal(repl_1990, 190)

  repl_2015 <- res %>%
    filter(animal_tag == "dairy_calves_female_replacement", region == "spain_2015") %>%
    pull(population)
  expect_equal(repl_2015, 270)
})


