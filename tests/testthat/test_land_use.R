library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)

test_that("calculate_land_use computes m2 safely without downloading parquet", {
  # 1. CREATE ISOLATED ENVIRONMENT
  # Create a temporary directory that will be deleted when the R session ends
  temp_test_dir <- tempfile()
  dir.create(temp_test_dir)

  # Copy the test data folder to the temporary environment
  file.copy(from = test_path("test_data/user_data"), to = temp_test_dir, recursive = TRUE)

  # Tell R to work INSIDE the temporary folder
  withr::local_dir(temp_test_dir)

  # 2. ANTI-DOWNLOAD SHIELD
  # Modify the temporary COPY of diet_ingredients.csv to remove NAs
  path_diet <- "user_data/diet_ingredients.csv"
  if (file.exists(path_diet)) {
    df <- read_csv(path_diet, col_types = cols(.default = "c"), show_col_types = FALSE)

    # If the column does not exist, create it. If it exists and has NAs, fill with "Spain"
    if (!"country_of_origin" %in% names(df)) {
      df <- df %>% mutate(country_of_origin = "Spain")
    } else {
      df <- df %>% mutate(country_of_origin = ifelse(is.na(country_of_origin), "Spain", country_of_origin))
    }

    # Overwrite only the ghost copy
    write_csv(df, path_diet)
  }

  # 3. EXECUTE THE FUNCTION
  # Since there are no longer NAs in the origin, the function will ignore the Parquet logic
  results <- suppressWarnings(
    calculate_land_use(
      farm_country = "Spain",
      year = 2022,
      saveoutput = FALSE
    )
  )

  # 4. BASIC VALIDATIONS
  # Check that it returns a dataframe
  expect_s3_class(results, "data.frame")

  # Check that key columns exist
  expect_true("land_use_per_animal_m2" %in% colnames(results))
  expect_true("total_land_use_m2" %in% colnames(results))

  # If there are results, check that mathematical logic holds (no negative numbers)
  if(nrow(results) > 0) {
    expect_true(all(results$land_use_per_animal_m2 >= 0))

    # Total use must be per_capita * population
    sample_row <- results[1, ]
    if(sample_row$population > 0) {
      calc_total <- sample_row$land_use_per_animal_m2 * sample_row$population
      expect_equal(sample_row$total_land_use_m2, calc_total, tolerance = 0.01)
    }
  }
})
