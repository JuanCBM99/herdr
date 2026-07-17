library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)

test_that("generate_impact_assessment creates a consistent final report safely", {
  # 1. CREATE ISOLATED ENVIRONMENT
  # Create a temporary directory that will be deleted when the R session ends
  temp_test_dir <- tempfile()
  dir.create(temp_test_dir)

  # Copy the test data folder to the temporary environment
  file.copy(from = test_path("test_data/user_data"), to = temp_test_dir, recursive = TRUE)

  # Tell R to work INSIDE the temporary folder
  withr::local_dir(temp_test_dir)

  # 2. ANTI-DOWNLOAD SHIELD
  # Modify the temporary COPY so that calculate_land_use (which is called internally)
  # does not find NAs and skips the Parquet download.
  path_diet <- "user_data/diet_ingredients.csv"
  if (file.exists(path_diet)) {
    df <- read_csv(path_diet, col_types = cols(.default = "c"), show_col_types = FALSE)

    if (!"country_of_origin" %in% names(df)) {
      df <- df %>% mutate(country_of_origin = "Spain")
    } else {
      df <- df %>% mutate(country_of_origin = ifelse(is.na(country_of_origin), "Spain", country_of_origin))
    }

    # Overwrite only the ghost copy
    write_csv(df, path_diet)
  }

  # 3. EXECUTE THE MAIN FUNCTION
  results <- suppressWarnings(
    generate_impact_assessment(
      farm_country = "Spain",
      year = 2022,
      saveoutput = FALSE,
      group_by_identification = TRUE
    )
  )

  # 4. STRUCTURE VALIDATIONS
  expect_s3_class(results, "data.frame")

  # Verify that all important columns have been joined and calculated
  expected_cols <- c(
    "CH4_enteric_Gg", "CH4_manure_Gg", "N2O_direct_Gg",
    "N2O_vol_Gg", "N2O_lea_Gg", "Land_m2", "CO2eq_Total_Gg"
  )
  for (col in expected_cols) {
    expect_true(col %in% colnames(results))
  }

  # 5. LOGICAL AND MATHEMATICAL VALIDATION
  if (nrow(results) > 0) {
    # Emissions and land use cannot be negative
    expect_true(all(results$CO2eq_Total_Gg >= 0))
    expect_true(all(results$Land_m2 >= 0))

    # Check that the IPCC mathematical formula for CO2eq has been applied correctly:
    # (CH4_ent + CH4_man)*28 + (N2Os)*265
    sample_row <- results[1, ]
    calculated_co2 <- (sample_row$CH4_enteric_Gg * 28) +
      (sample_row$CH4_manure_Gg * 28) +
      ((sample_row$N2O_direct_Gg + sample_row$N2O_vol_Gg + sample_row$N2O_lea_Gg) * 265)

    expect_equal(sample_row$CO2eq_Total_Gg, calculated_co2, tolerance = 0.01)
  }
})
