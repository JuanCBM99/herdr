library(testthat)
library(herdr)
library(dplyr)

test_that("calculate_population_goat returns correct structure", {

  # 1. Setup Mock Data with ALL required tags to prevent "object not found" errors
  # Even if population is 0, the column must exist after pivot_wider
  mock_census <- data.frame(
    region = "spain",
    subregion = "canary_islands",
    animal_tag = c(
      "mature_goat_female_dairy", "mature_goat_female_meat",
      "mature_goat_male_dairy", "mature_goat_male_meat"
    ),
    class_flex = NA,
    population = c(1000, 0, 100, 0) # Only dairy goats have population
  )

  # 2. Setup mock rates
  mock_rates <- data.frame(
    parameter = rep(c("kidding_rate", "replacement_rate"), each = 4),
    animal_tag = rep(c(
      "mature_goat_female_dairy", "mature_goat_female_meat",
      "mature_goat_male_dairy", "mature_goat_male_meat"
    ), 2),
    value = c(
      1.8, 1.2, 0, 0,  # Kidding rates
      0.2, 0.2, 0.1, 0 # Replacement rates
    )
  )

  # 3. Run function
  results <- calculate_population_goat(mock_census, mock_rates)

  # --- TEST: Structure ---
  expect_s3_class(results, "data.frame")
  expect_named(results, c("region", "subregion", "class_flex", "animal_tag", "population"))
  expect_true(any(grepl("kid", results$animal_tag)))
})



test_that("goat population correctly calculates replacement cohorts", {

  # Scenario: 1000 Dairy Females (0.2 repl) and 100 Dairy Males (0.1 repl)
  mock_census <- data.frame(
    region = "spain", subregion = "test",
    animal_tag = c(
      "mature_goat_female_dairy", "mature_goat_female_meat",
      "mature_goat_male_dairy", "mature_goat_male_meat"
    ),
    class_flex = "grazing",
    population = c(1000, 0, 100, 0)
  )

  mock_rates <- data.frame(
    parameter = rep(c("kidding_rate", "replacement_rate"), each = 4),
    animal_tag = rep(c(
      "mature_goat_female_dairy", "mature_goat_female_meat",
      "mature_goat_male_dairy", "mature_goat_male_meat"
    ), 2),
    value = c(
      1.5, 0, 0, 0,    # kidding
      0.2, 0, 0.1, 0   # replacement
    )
  )

  res <- calculate_population_goat(mock_census, mock_rates)

  # Math check:
  # Female replacement: 1000 * 0.2 = 200
  # Male replacement: 100 * 0.1 = 10

  # --- TEST: Female Replacement ---
  f_repl <- res %>%
    filter(animal_tag == "kid_goat_female_dairy_replacement") %>%
    pull(population)
  expect_equal(f_repl, 200)

  # --- TEST: Male Replacement ---
  m_repl <- res %>%
    filter(animal_tag == "kid_goat_male_dairy_replacement") %>%
    pull(population)
  expect_equal(m_repl, 10)
})

test_that("goat function filters zero populations automatically", {

  mock_census <- data.frame(
    region = "spain", subregion = "test",
    animal_tag = c(
      "mature_goat_female_dairy", "mature_goat_female_meat",
      "mature_goat_male_dairy", "mature_goat_male_meat"
    ),
    class_flex = NA,
    population = c(100, 0, 0, 0) # Only dairy females
  )

  mock_rates <- data.frame(
    parameter = rep(c("kidding_rate", "replacement_rate"), each = 4),
    animal_tag = rep(c(
      "mature_goat_female_dairy", "mature_goat_female_meat",
      "mature_goat_male_dairy", "mature_goat_male_meat"
    ), 2),
    value = 0.5
  )

  res <- calculate_population_goat(mock_census, mock_rates)

  # The final output should not contain any tags with 0 population
  # (Since you have dplyr::filter(round(population, 5) > 0) at the end)
  expect_true(all(res$population > 0))

  # In this scenario, meat categories should be absent
  expect_false(any(grepl("_meat", res$animal_tag)))
})
