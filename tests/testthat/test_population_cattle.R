library(testthat)
library(herdr)
library(dplyr)

# We removed context() to fix the warning.
# In the 3rd edition, the file name serves as the context.

test_that("calculate_population_cattle returns correct structure", {
  # 1. Setup minimal Mock Data
  mock_census <- data.frame(
    region = "spain",
    subregion = "north",
    animal_tag = "mature_dairy_cattle",
    class_flex = "lactation",
    population = 1000
  )

  mock_rates <- data.frame(
    parameter = c("calving_rate", "replacement_rate"),
    animal_tag = c("mature_dairy_cattle", "mature_dairy_cattle"),
    value = c(0.9, 0.2)
  )

  # 2. Run function
  results <- calculate_population_cattle(mock_census, mock_rates, data.frame())

  # --- BASIC STRUCTURE CHECKS ---
  expect_s3_class(results, "data.frame")
  expect_named(results, c("region", "subregion", "animal_tag", "class_flex", "population"))
})

test_that("population math follows biological sex-ratio and replacement rules", {
  # Scenario: 1000 cows, 1.0 calving rate, 0.1 replacement rate
  mock_census <- data.frame(
    region = "spain", subregion = "north",
    animal_tag = "mature_dairy_cattle", class_flex = "lactation", population = 1000
  )
  mock_rates <- data.frame(
    parameter = c("calving_rate", "replacement_rate"),
    animal_tag = "mature_dairy_cattle", value = c(1.0, 0.1)
  )

  res <- calculate_population_cattle(mock_census, mock_rates, data.frame())

  # Calculation logic:
  # 1000 births -> 500 males / 500 females
  # Replacement: 1000 * 0.1 = 100 female calves kept
  # Feedlot males: 500
  # Feedlot females: 500 - 100 = 400

  # --- MATH CHECKS ---
  males <- res %>% filter(animal_tag == "feedlot_calves_male") %>% pull(population)
  expect_equal(males, 500)

  repl_females <- res %>% filter(animal_tag == "dairy_calves_female_replacement") %>% pull(population)
  expect_equal(repl_females, 100)
})



test_that("class_flex is only preserved for mature animals", {
  mock_census <- data.frame(
    region = "spain", subregion = "north",
    animal_tag = "mature_dairy_cattle", class_flex = "lactation", population = 100
  )
  mock_rates <- data.frame(
    parameter = c("calving_rate", "replacement_rate"),
    animal_tag = "mature_dairy_cattle", value = c(1, 0)
  )

  res <- calculate_population_cattle(mock_census, mock_rates, data.frame())

  # Mature cows should keep "lactation"
  mature_entry <- res %>% filter(animal_tag == "mature_dairy_cattle")
  expect_equal(mature_entry$class_flex, "lactation")

  # Calves should have NA in class_flex
  calf_entry <- res %>% filter(animal_tag == "feedlot_calves_male")
  expect_true(is.na(calf_entry$class_flex))
})
