library(testthat)
library(herdr)
library(readr)
library(dplyr)

path_census <- test_path("test_data", "user_data/livestock_census.csv")
path_defs   <- test_path("test_data", "user_data/ruminant_definitions.csv")

test_that("calculate_population_small_ruminants returns correct sheep structure and math from CSV", {

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

  results <- calculate_population_small_ruminants(mock_census, mock_defs, species = "sheep")

  expect_s3_class(results, "data.frame")
  expect_true(all(c("farm_id", "region", "subregion", "animal_tag", "population") %in% colnames(results)))

  meat_slaughter <- results %>%
    filter(animal_tag == "lamb_meat_slaughter") %>%
    pull(population)

  expect_equal(meat_slaughter, 1300)
})

test_that("calculate_population_small_ruminants filters out zero sheep populations using CSV structure", {

  census_raw <- read_csv(path_census, show_col_types = FALSE)
  defs_raw   <- read_csv(path_defs, show_col_types = FALSE)

  mock_census <- census_raw %>%
    filter(grepl("sheep", animal_tag)) %>%
    mutate(population = ifelse(animal_tag == "mature_sheep_female_meat", 100, 0))

  mock_defs <- defs_raw %>%
    filter(grepl("sheep", animal_tag)) %>%
    mutate(replacement_rate = 0.1, pr_sheep_goat = 1.0)

  res <- calculate_population_small_ruminants(mock_census, mock_defs, species = "sheep")

  expect_true(all(res$population > 0))
  expect_false("lamb_dairy_slaughter" %in% res$animal_tag)
})

test_that("calculate_population_small_ruminants handles accelerated lambing cycles and slaughter AAP scaling", {
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

  res <- calculate_population_small_ruminants(mock_census, mock_rates, definitions = NULL, weights = mock_weights, species = "sheep")

  # Births: 1000 * (365 / 200) * 1.5 = 2737.5
  # Replacements: 1000 * 0.2 = 200
  # Annual slaughter: 2737.5 - 200 = 2537.5
  # AAP slaughter: 2537.5 * (70 / 365) = 486.6438
  meat_slaughter <- res %>%
    filter(animal_tag == "lamb_meat_slaughter") %>%
    pull(population)

  expect_equal(round(meat_slaughter, 2), 486.64)
})

test_that("calculate_population_small_ruminants handles multiple sheep farms/strata without scalar condition errors", {
  mock_census <- tibble::tribble(
    ~farm_id, ~animal_tag, ~region, ~subregion, ~population,
    "farm_1", "mature_sheep_female_meat", "spain", "aragon",  400,
    "farm_2", "mature_sheep_female_meat", "spain", "navarra", 200
  )

  mock_defs <- tibble::tribble(
    ~farm_id, ~animal_tag, ~region, ~subregion, ~pr_sheep_goat, ~replacement_rate,
    "farm_1", "mature_sheep_female_meat", "spain", "aragon",  1.4, 0.20,
    "farm_2", "mature_sheep_female_meat", "spain", "navarra", 1.6, 0.25
  )

  mock_weights <- tibble::tribble(
    ~farm_id, ~animal_tag, ~region, ~subregion, ~productive_period_days,
    "farm_1", "mature_sheep_female_meat", "spain", "aragon",  240,
    "farm_1", "lamb_meat_slaughter",      "spain", "aragon",  70,
    "farm_2", "mature_sheep_female_meat", "spain", "navarra", 365,
    "farm_2", "lamb_meat_slaughter",      "spain", "navarra", 90
  )

  res <- expect_no_error(
    calculate_population_small_ruminants(mock_census, definitions = mock_defs, weights = mock_weights, species = "sheep")
  )
  expect_setequal(unique(res$farm_id), c("farm_1", "farm_2"))
  f1_repl <- res %>% filter(farm_id == "farm_1", animal_tag == "lamb_female_meat_replacement") %>% pull(population)
  f2_repl <- res %>% filter(farm_id == "farm_2", animal_tag == "lamb_female_meat_replacement") %>% pull(population)
  expect_equal(f1_repl, 400 * 0.20)
  expect_equal(f2_repl, 200 * 0.25)
})

test_that("calculate_population_small_ruminants returns correct goat structure", {

  mock_census <- read_csv(path_census, show_col_types = FALSE) %>%
    filter(grepl("goat", animal_tag)) %>%
    mutate(population = case_when(
      animal_tag == "mature_goat_female_dairy" ~ 1000,
      animal_tag == "mature_goat_male_dairy"   ~ 100,
      TRUE ~ 0
    ))

  mock_defs <- read_csv(path_defs, show_col_types = FALSE) %>%
    filter(grepl("goat", animal_tag))

  results <- calculate_population_small_ruminants(mock_census, mock_defs, species = "goat")

  expect_s3_class(results, "data.frame")
  expect_true(any(grepl("kid", results$animal_tag)))
  expect_true(all(c("farm_id", "region", "subregion", "animal_tag", "population") %in% colnames(results)))
})

test_that("calculate_population_small_ruminants correctly calculates goat replacement cohorts", {

  mock_census <- read_csv(path_census, show_col_types = FALSE) %>%
    filter(grepl("goat", animal_tag)) %>%
    mutate(population = case_when(
      animal_tag == "mature_goat_female_dairy" ~ 1000,
      animal_tag == "mature_goat_male_dairy"   ~ 100,
      TRUE ~ 0
    ))

  mock_defs <- read_csv(path_defs, show_col_types = FALSE) %>%
    filter(grepl("goat", animal_tag)) %>%
    mutate(
      pr_sheep_goat = ifelse(animal_tag == "mature_goat_female_dairy", 1.5, 0),
      replacement_rate = case_when(
        animal_tag == "mature_goat_female_dairy" ~ 0.2,
        animal_tag == "mature_goat_male_dairy"   ~ 0.1,
        TRUE ~ 0
      )
    )

  res <- calculate_population_small_ruminants(mock_census, mock_defs, species = "goat")

  f_repl <- res %>% filter(animal_tag == "kid_goat_female_dairy_replacement") %>% pull(population)
  m_repl <- res %>% filter(animal_tag == "kid_goat_male_dairy_replacement") %>% pull(population)
  dairy_slaughter <- res %>% filter(animal_tag == "kid_goat_dairy_slaughter") %>% pull(population)

  expect_equal(f_repl, 200)
  expect_equal(m_repl, 10)
  expect_equal(dairy_slaughter, 1290)
})

test_that("calculate_population_small_ruminants filters zero goat populations automatically", {

  mock_census <- read_csv(path_census, show_col_types = FALSE) %>%
    filter(grepl("goat", animal_tag)) %>%
    mutate(population = ifelse(animal_tag == "mature_goat_female_dairy", 100, 0))

  mock_defs <- read_csv(path_defs, show_col_types = FALSE) %>%
    filter(grepl("goat", animal_tag)) %>%
    mutate(replacement_rate = 0.5, pr_sheep_goat = 1.0)

  res <- calculate_population_small_ruminants(mock_census, mock_defs, species = "goat")

  expect_true(all(res$population > 0))
  expect_false(any(grepl("_meat", res$animal_tag)))
})

test_that("calculate_population_small_ruminants handles accelerated kidding cycles and slaughter AAP scaling", {
  mock_census <- tibble::tribble(
    ~farm_id, ~animal_tag, ~region, ~subregion, ~population,
    "farm_1", "mature_goat_female_dairy", "spain", "andalucia", 1000
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

  res <- calculate_population_small_ruminants(mock_census, mock_rates, definitions = NULL, weights = mock_weights, species = "goat")

  # Births: 1000 * (365 / 200) * 1.5 = 2737.5
  # Replacements: 1000 * 0.2 = 200
  # Annual slaughter: 2737.5 - 200 = 2537.5
  # AAP slaughter: 2537.5 * (70 / 365) = 486.6438
  dairy_slaughter <- res %>%
    filter(animal_tag == "kid_goat_dairy_slaughter") %>%
    pull(population)

  expect_equal(round(dairy_slaughter, 2), 486.64)
})

test_that("calculate_population_small_ruminants handles multiple goat farms/strata without scalar condition errors", {
  mock_census <- tibble::tribble(
    ~farm_id, ~animal_tag, ~region, ~subregion, ~population,
    "farm_1", "mature_goat_female_dairy", "spain", "andalucia", 500,
    "farm_2", "mature_goat_female_dairy", "spain", "murcia",    300
  )

  mock_defs <- tibble::tribble(
    ~farm_id, ~animal_tag, ~region, ~subregion, ~pr_sheep_goat, ~replacement_rate,
    "farm_1", "mature_goat_female_dairy", "spain", "andalucia", 1.5, 0.20,
    "farm_2", "mature_goat_female_dairy", "spain", "murcia",    1.8, 0.25
  )

  mock_weights <- tibble::tribble(
    ~farm_id, ~animal_tag, ~region, ~subregion, ~productive_period_days,
    "farm_1", "mature_goat_female_dairy", "spain", "andalucia", 240,
    "farm_1", "kid_goat_dairy_slaughter", "spain", "andalucia", 60,
    "farm_2", "mature_goat_female_dairy", "spain", "murcia",    365,
    "farm_2", "kid_goat_dairy_slaughter", "spain", "murcia",    90
  )

  res <- expect_no_error(
    calculate_population_small_ruminants(mock_census, definitions = mock_defs, weights = mock_weights, species = "goat")
  )
  expect_setequal(unique(res$farm_id), c("farm_1", "farm_2"))
  f1_repl <- res %>% filter(farm_id == "farm_1", animal_tag == "kid_goat_female_dairy_replacement") %>% pull(population)
  f2_repl <- res %>% filter(farm_id == "farm_2", animal_tag == "kid_goat_female_dairy_replacement") %>% pull(population)
  expect_equal(f1_repl, 500 * 0.20)
  expect_equal(f2_repl, 300 * 0.25)
})

test_that("calculate_population_small_ruminants infers species automatically when omitted", {
  census_goat <- tibble::tribble(
    ~farm_id, ~animal_tag, ~region, ~subregion, ~population,
    "farm_1", "mature_goat_female_dairy", "spain", "andalucia", 100
  )
  defs_goat <- tibble::tribble(
    ~farm_id, ~animal_tag, ~region, ~subregion, ~pr_sheep_goat, ~replacement_rate,
    "farm_1", "mature_goat_female_dairy", "spain", "andalucia", 1.5, 0.2
  )
  res_goat <- calculate_population_small_ruminants(census_goat, definitions = defs_goat)
  expect_true("kid_goat_dairy_slaughter" %in% res_goat$animal_tag)

  census_sheep <- tibble::tribble(
    ~farm_id, ~animal_tag, ~region, ~subregion, ~population,
    "farm_1", "mature_sheep_female_meat", "spain", "aragon", 100
  )
  defs_sheep <- tibble::tribble(
    ~farm_id, ~animal_tag, ~region, ~subregion, ~pr_sheep_goat, ~replacement_rate,
    "farm_1", "mature_sheep_female_meat", "spain", "aragon", 1.4, 0.2
  )
  res_sheep <- calculate_population_small_ruminants(census_sheep, definitions = defs_sheep)
  expect_true("lamb_meat_slaughter" %in% res_sheep$animal_tag)
})
