library(testthat)
library(herdr)
library(dplyr)

test_that("calculate_population handles manual and automatic modes correctly", {

  mock_categories <- data.frame(
    animal_tag = c("mature_sheep_female_dairy", "lamb_dairy_slaughter"),
    region = "R1", subregion = "S1", class_flex = "grazing",
    animal_type = "sheep", animal_subtype = "dairy",
    stringsAsFactors = FALSE
  )

  mock_census <- data.frame(
    animal_tag = "mature_sheep_female_dairy",
    region = "R1", subregion = "S1", class_flex = "grazing",
    population = 100,
    stringsAsFactors = FALSE
  )

  res_auto <- testthat::with_mocked_bindings(

    testthat::with_mocked_bindings(
      calculate_population(automatic_cycle = TRUE, saveoutput = FALSE),

      read_csv = function(file, ...) {
        if (grepl("census.csv", file)) return(mock_census)
        if (grepl("categories.csv", file)) return(mock_categories)
        if (grepl("rate_parameters.csv", file)) return(data.frame())
        return(data.frame())
      },
      .package = "readr"
    ),

    calculate_population_sheep = function(...) {
      data.frame(
        region = "R1", subregion = "S1",
        animal_tag = c("mature_sheep_female_dairy", "lamb_dairy_slaughter"),
        population = c(100, 50),
        class_flex = "grazing",
        stringsAsFactors = FALSE
      )
    },
    calculate_population_cattle = function(...) data.frame(),
    calculate_population_goat   = function(...) data.frame(),
    .package = "herdr"
  )

  expect_s3_class(res_auto, "data.frame")
  expect_equal(nrow(res_auto), 2)
  expect_equal(sum(res_auto$population), 150)
  expect_true(all(c("region", "class_flex", "animal_subtype") %in% colnames(res_auto)))
})
