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
      parameter == "calving_rate" ~ 1.0,
      parameter == "replacement_rate" ~ 0.1,
      TRUE ~ value
    ))

  res <- calculate_population_cattle(mock_census, mock_rates, data.frame())

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

  res <- calculate_population_cattle(mock_census, mock_rates, data.frame())

  mature_entry <- res %>% filter(animal_tag == "mature_dairy_cattle")
  expect_equal(mature_entry$class_flex[1], "grazing_test")

  calf_entry <- res %>% filter(animal_tag == "feedlot_calves_male")
  expect_true(is.na(calf_entry$class_flex[1]))
})
