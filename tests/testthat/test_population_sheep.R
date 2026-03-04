library(testthat)
library(herdr)
library(dplyr)

test_that("calculate_population_sheep returns correct structure and math", {

  # 1. Setup Mock Data including ALL required categories (even if 0)
  # This prevents the "object not found" error during the pivot/mutate
  mock_census <- data.frame(
    region = "spain",
    subregion = "extremadura",
    animal_tag = c(
      "mature_sheep_female_meat",
      "mature_sheep_female_dairy",
      "mature_sheep_male_meat",
      "mature_sheep_male_dairy"
    ),
    class_flex = "grazing",
    population = c(1000, 0, 0, 0) # Only meat females have population
  )

  # 2. Setup rates for all categories
  mock_rates <- data.frame(
    parameter = rep(c("lambing_rate", "replacement_rate"), each = 4),
    animal_tag = rep(c(
      "mature_sheep_female_meat", "mature_sheep_female_dairy",
      "mature_sheep_male_meat", "mature_sheep_male_dairy"
    ), 2),
    value = c(
      1.5, 1.2, 0, 0,  # Lambing rates
      0.2, 0.2, 0, 0   # Replacement rates
    )
  )

  # 3. Run function
  # This will now work because all columns exist (even if they are 0)
  results <- calculate_population_sheep(mock_census, mock_rates)

  # --- TEST: Structure ---
  expect_s3_class(results, "data.frame")
  expect_true(all(c("region", "subregion", "animal_tag", "population") %in% colnames(results)))

  # --- TEST: Math logic ---
  # 1000 meat ewes * 1.5 lambs = 1500 total lambs
  # Female replacement = 1000 * 0.2 = 200
  # Slaughter = 1500 - 200 = 1300
  meat_slaughter <- results %>%
    filter(animal_tag == "lamb_meat_slaughter") %>%
    pull(population)

  expect_equal(meat_slaughter, 1300)
})

test_that("calculate_population_sheep filters out zero populations", {

  # Create a scenario where one category is 0
  mock_census <- data.frame(
    region = "spain", subregion = "test",
    animal_tag = c("mature_sheep_female_meat", "mature_sheep_female_dairy",
                   "mature_sheep_male_meat", "mature_sheep_male_dairy"),
    class_flex = NA,
    population = c(100, 0, 0, 0)
  )

  mock_rates <- data.frame(
    parameter = rep(c("lambing_rate", "replacement_rate"), each = 4),
    animal_tag = rep(c("mature_sheep_female_meat", "mature_sheep_female_dairy",
                       "mature_sheep_male_meat", "mature_sheep_male_dairy"), 2),
    value = 0.1
  )

  res <- calculate_population_sheep(mock_census, mock_rates)

  # The output should only contain tags with population > 0
  expect_true(all(res$population > 0))
  # Dairy slaughter should not be in the results because dairy population was 0
  expect_false("lamb_dairy_slaughter" %in% res$animal_tag)
})
