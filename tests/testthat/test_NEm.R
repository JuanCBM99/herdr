library(testthat)
library(herdr)
library(readr)
library(dplyr)

setup_nem_env <- function() {
  if (!dir.exists("user_data")) dir.create("user_data")

  write_csv(data.frame(
    region = "spain", subregion = "north", animal_tag = "mature_dairy_cattle",
    class_flex = "lactation", average_weight = 600
  ), "user_data/livestock_weights.csv")

  write_csv(data.frame(
    region = "spain", subregion = "north", animal_tag = "mature_dairy_cattle",
    class_flex = "lactation", animal_type = "cattle", animal_subtype = "dairy",
    cfi = "dairy_cow"
  ), "user_data/livestock_definitions.csv")

  write_csv(data.frame(
    coefficient = "cfi", description = "dairy_cow", value = 0.335
  ), "user_data/ipcc_coefficients.csv")
}

cleanup_nem_env <- function() {
  if (dir.exists("user_data")) unlink("user_data", recursive = TRUE)
  if (dir.exists("output")) unlink("output", recursive = TRUE)
}

test_that("calculate_NEm computes metabolic energy correctly", {
  setup_nem_env()

  results <- calculate_NEm(saveoutput = FALSE)

  # Usamos tolerancia para evitar fallos por redondeo de milésimas
  # El valor esperado está en el rango [40.611, 40.612]
  expect_equal(results$NEm[1], 40.611, tolerance = 0.002)

  cleanup_nem_env()
})

test_that("calculate_NEm handles missing weights with zero", {
  setup_nem_env()

  write_csv(data.frame(
    region = "spain", subregion = "north", animal_tag = "mature_dairy_cattle",
    class_flex = "lactation", average_weight = NA
  ), "user_data/livestock_weights.csv")

  results <- calculate_NEm(saveoutput = FALSE)

  expect_equal(results$NEm[1], 0)

  cleanup_nem_env()
})
