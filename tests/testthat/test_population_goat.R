library(testthat)
library(herdr)
library(dplyr)
library(tidyr)

test_that("calculate_population_goat correctly expands population with class_flex", {

  # 1. Mock Base Census (Goat)
  mock_census_goat <- data.frame(
    region = "G1",
    subregion = "Z1",
    animal_tag = c("mature_goat_male_dairy", "mature_goat_female_dairy",
                   "mature_goat_male_meat", "mature_goat_female_meat"),
    class_flex = "grazing",
    population = c(5, 100, 10, 200),
    stringsAsFactors = FALSE
  )

  # 2. Mock Rate Parameters (Goat) — using animal_tag as the key
  mock_rates_goat <- data.frame(
    parameter = c("replacement_rate", "replacement_rate",
                  "replacement_rate", "replacement_rate"),
    animal_tag = c("mature_goat_male_dairy", "mature_goat_female_dairy",
                   "mature_goat_male_meat", "mature_goat_female_meat"),
    value = c(0.1, 0.2, 0.15, 0.25),
    stringsAsFactors = FALSE
  )

  # 3. Execution
  res <- calculate_population_goat(
    census_goat = mock_census_goat,
    rate_parameters = mock_rates_goat
  )

  # 4. Assertions
  # Verify that identity columns are present
  expect_true(all(c("region", "subregion", "animal_tag", "population") %in% colnames(res)))

  # Verify female dairy replacement: 100 females * 0.2 = 20
  female_dairy_repl <- res %>%
    filter(animal_tag == "kid_goat_female_dairy_replacement") %>%
    pull(population)
  expect_equal(sum(female_dairy_repl), 20)

  # Verify female meat replacement: 200 females * 0.25 = 50
  female_meat_repl <- res %>%
    filter(animal_tag == "kid_goat_female_meat_replacement") %>%
    pull(population)
  expect_equal(sum(female_meat_repl), 50)

  # Verify that parent animals are included
  parent_tags <- c("mature_goat_male_dairy", "mature_goat_female_dairy",
                   "mature_goat_male_meat", "mature_goat_female_meat")
  expect_true(all(parent_tags %in% res$animal_tag))
})
