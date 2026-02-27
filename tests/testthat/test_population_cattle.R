library(testthat)
library(herdr)
library(dplyr)
library(tidyr)

test_that("calculate_population_cattle correctly expands population with class_flex", {

  mock_census <- data.frame(
    region = "G1",
    subregion = "Z1",
    animal_tag = c("mature_dairy_cattle", "mature_dairy_cattle", "mature_beef_bull", "mature_beef_cattle"),
    class_flex = c("stall", "grazing", "stall", "grazing"),
    population = c(100, 50, 10, 200),
    stringsAsFactors = FALSE
  )

  mock_rates <- data.frame(
    parameter = c("calving_rate", "calving_rate", "replacement_rate", "replacement_rate", "replacement_rate"),
    animal_tag = c("mature_dairy_cattle", "mature_beef_cattle", "mature_beef_bull", "mature_beef_cattle", "mature_dairy_cattle"),
    value = c(0.9, 0.8, 0.2, 0.25, 0.3),
    stringsAsFactors = FALSE
  )

  res <- calculate_population_cattle(
    census_cattle = mock_census,
    rate_parameters = mock_rates,
    categories = data.frame()
  )

  expect_true(all(c("region", "subregion", "animal_tag", "class_flex") %in% colnames(res)))

  # Total dairy cows = 150. Total female replacement = 150 * 0.3 = 45
  dairy_repl <- res %>%
    filter(animal_tag == "dairy_calves_female_replacement", region == "G1") %>%
    pull(population)
  expect_equal(sum(dairy_repl), 45)

  # Offspring have NA in class_flex
  offspring_check <- res %>%
    filter(animal_tag == "feedlot_calves_male") %>%
    pull(class_flex)
  expect_true(all(is.na(offspring_check)))

  # Parent animals preserve their class_flex
  parent_check <- res %>%
    filter(animal_tag == "mature_dairy_cattle")
  expect_true(all(c("stall", "grazing") %in% parent_check$class_flex))
})
