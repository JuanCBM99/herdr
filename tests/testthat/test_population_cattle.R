library(testthat)
library(herdr)
library(readr)
library(dplyr)

path_census <- test_path("test_data", "user_data/livestock_census.csv")
path_rates  <- test_path("test_data", "user_data/reproduction_parameters.csv")

test_that("population math follows biological sex-ratio and replacement rules", {

  mock_census <- read_csv(path_census, show_col_types = FALSE) %>%
    filter(animal_tag == "mature_dairy_cattle") %>%
    mutate(population = 1000, class_flex = "lactation")

  mock_rates <- read_csv(path_rates, show_col_types = FALSE) %>%
    filter(animal_tag == "mature_dairy_cattle") %>%
    mutate(value = case_when(
      parameter == "pregnancy_rate" ~ 1.0,
      parameter == "replacement_rate" ~ 0.1,
      TRUE ~ value
    ))

  res <- calculate_population_cattle(mock_census, mock_rates)

  males <- res %>% filter(animal_tag == "feedlot_calves_male") %>% pull(population)
  expect_equal(males, 500)

  repl_females <- res %>% filter(animal_tag == "dairy_calves_female_replacement") %>% pull(population)
  expect_equal(repl_females, 100)
})

test_that("class_flex is preserved for mature animals and NA for offspring", {

  mock_census <- read_csv(path_census, show_col_types = FALSE) %>%
    filter(animal_tag == "mature_dairy_cattle") %>%
    mutate(class_flex = "grazing_test")

  mock_rates <- read_csv(path_rates, show_col_types = FALSE) %>%
    filter(animal_tag == "mature_dairy_cattle")

  res <- calculate_population_cattle(mock_census, mock_rates)

  mature_entry <- res %>% filter(animal_tag == "mature_dairy_cattle")
  expect_equal(mature_entry$class_flex[1], "grazing_test")

  calf_entry <- res %>% filter(animal_tag == "feedlot_calves_male")
  expect_true(is.na(calf_entry$class_flex[1]))
})

test_that("subregion is preserved from parents for all offspring including feedlot", {
  mock_census <- tibble::tribble(
    ~animal_tag, ~region, ~subregion, ~class_flex, ~population,
    "mature_dairy_cattle", "spain", "Gipuzkoa", "lactation", 100
  )

  mock_rates <- tibble::tribble(
    ~animal_tag, ~parameter, ~value,
    "mature_dairy_cattle", "pregnancy_rate", "0.9",
    "mature_dairy_cattle", "replacement_rate", "0.2"
  )

  res <- calculate_population_cattle(mock_census, mock_rates)

  feedlot_males <- res %>% filter(animal_tag == "feedlot_calves_male")
  expect_equal(feedlot_males$subregion[1], "Gipuzkoa")

  feedlot_females <- res %>% filter(animal_tag == "feedlot_calves_female")
  expect_equal(feedlot_females$subregion[1], "Gipuzkoa")

  repl_females <- res %>% filter(animal_tag == "dairy_calves_female_replacement")
  expect_equal(repl_females$subregion[1], "Gipuzkoa")
})

test_that("demographic warning is triggered and negative populations are prevented when replacement > births", {
  mock_census <- tibble::tribble(
    ~animal_tag, ~region, ~subregion, ~class_flex, ~population,
    "mature_dairy_cattle", "spain", NA_character_, "lactation", 100
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
    ~animal_tag, ~region, ~subregion, ~class_flex, ~population,
    "mature_dairy_cattle", "spain", NA_character_, "lactation", 1000
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

