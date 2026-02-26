library(testthat)
library(herdr)
library(dplyr)
library(tidyr)

test_that("calculate_population_cattle correctly expands population with class_flex", {

  # 1. Mock Base Census (The 3 required identifications)
  # We test with different class_flex to ensure they aren't collapsed
  mock_census <- data.frame(
    region = "G1",
    subregion = "Z1",
    animal_tag = c("mature_dairy_cattle", "mature_dairy_cattle", "mature_beef_bull", "mature_beef_cattle"),
    class_flex = c("stall", "grazing", "stall", "grazing"), # Multiple management types
    population = c(100, 50, 10, 200),
    stringsAsFactors = FALSE
  )

  # 2. Mock Rate Parameters
  mock_rates <- data.frame(
    animal_type = "cattle",
    animal_subtype = c("dairy", "beef", "beef", "beef", "dairy"),
    parameter = c("calving_rate", "calving_rate", "replacement_rate", "replacement_rate", "replacement_rate"),
    sex = c(NA, NA, "male", "female", "female"),
    value = c(0.9, 0.8, 0.2, 0.25, 0.3),
    stringsAsFactors = FALSE
  )

  # 3. Execution
  # This function returns a long dataframe with calculated offspring
  res <- calculate_population_cattle(
    census_cattle = mock_census,
    rate_parameters = mock_rates,
    categories = data.frame() # Not used in current logic but required by signature
  )

  # 4. Assertions
  # Check if identity columns are present
  expect_true(all(c("region", "subregion", "animal_tag", "class_flex") %in% colnames(res)))

  # Validate Dairy expansion for the 'stall' group:
  # 100 dairy cows * 0.9 calving rate / 2 (females) = 45 dairy_calves_female_replacement
  # (Since rate_dairy_female_repl is 0.3, the result should be 100 * 0.3 = 30)
  dairy_repl <- res %>%
    filter(animal_tag == "dairy_calves_female_replacement", region == "G1") %>%
    pull(population)

  # Total dairy cows = 150. Total female replacement = 150 * 0.3 = 45
  expect_equal(sum(dairy_repl), 45)

  # Check that offspring have NA in class_flex (as per your Cambio 4)
  offspring_check <- res %>%
    filter(animal_tag == "feedlot_calves_male") %>%
    pull(class_flex)

  expect_true(all(is.na(offspring_check)))

  # Check that parent animals preserve their class_flex
  parent_check <- res %>%
    filter(animal_tag == "mature_dairy_cattle")

  expect_true(all(c("stall", "grazing") %in% parent_check$class_flex))
})
