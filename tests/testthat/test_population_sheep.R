library(testthat)
library(herdr)
library(dplyr)
library(tidyr)

test_that("calculate_population_sheep correctly expands population with class_flex", {

  mock_census_sheep <- data.frame(
    region = "G1",
    subregion = "Z1",
    animal_tag = c("mature_sheep_male_dairy", "mature_sheep_female_dairy",
                   "mature_sheep_male_meat", "mature_sheep_female_meat"),
    class_flex = "grazing",
    population = c(5, 100, 10, 200),
    stringsAsFactors = FALSE
  )

  mock_rates_sheep <- data.frame(
    parameter = c("lambing_rate", "lambing_rate",
                  "replacement_rate", "replacement_rate",
                  "replacement_rate", "replacement_rate"),
    animal_tag = c("mature_sheep_female_dairy", "mature_sheep_female_meat",
                   "mature_sheep_male_dairy", "mature_sheep_female_dairy",
                   "mature_sheep_male_meat", "mature_sheep_female_meat"),
    value = c(1.5, 1.2, 0.1, 0.2, 0.15, 0.25),
    stringsAsFactors = FALSE
  )

  res <- calculate_population_sheep(
    census_sheep = mock_census_sheep,
    rate_parameters = mock_rates_sheep
  )

  # Dairy: 100 females * 1.5 = 150 births
  # Dairy replacements: 100 * 0.2 = 20 female, 5 * 0.1 = 0.5 male
  # Slaughter: 150 - 20 - 0.5 = 129.5
  slaughter_val <- res %>%
    filter(animal_tag == "lamb_dairy_slaughter") %>%
    pull(population)
  expect_equal(slaughter_val, 129.5)

  expect_true("region" %in% colnames(res))
  expect_true("subregion" %in% colnames(res))
  expect_true("animal_tag" %in% colnames(res))
})
