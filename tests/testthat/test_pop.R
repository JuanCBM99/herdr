library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)

test_that("calculate_population automatic cycle works with cleaned test data", {
  withr::local_dir(test_path("test_data"))

  read_csv("user_data/livestock_census.csv", col_types = cols(.default = "c", population = "d"), show_col_types = FALSE) %>%
    write_csv("user_data/livestock_census.csv")

  read_csv("user_data/livestock_definitions.csv", col_types = cols(.default = "c"), show_col_types = FALSE) %>%
    write_csv("user_data/livestock_definitions.csv")

  results <- calculate_population(
    automatic_cycle = TRUE,
    saveoutput = FALSE
  )

  expect_s3_class(results, "data.frame")
  expect_true(any(grepl("calves", results$animal_tag)))
  expect_false(any(is.na(results$animal_type)))
})
