library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)

test_that("generate_impact_assessment creates a consistent final report", {
  # 1. Use the local test data directory
  withr::local_dir(test_path("test_data"))

  # 2. Run the master function
  # We use 'Spain' as it matches the data in our test CSV files
  results <- suppressWarnings(
    generate_impact_assessment(
      crop_yield_country = "Spain",
      saveoutput = TRUE,
      group_by_identification = TRUE
    )
  )

  # 3. Structural checks
  # Check if the output is a data frame and contains the core consolidated columns
  expect_s3_class(results, "data.frame")

  metrics <- c("CH4_enteric_Gg", "CH4_manure_Gg", "N2O_direct_Gg",
               "Land_m2", "CO2eq_Total_Gg")
  expect_true(all(metrics %in% colnames(results)))

  # 4. Mathematical logic check (CO2 Equivalent)
  # Global Warming Potential (GWP) check: Total CO2eq = (CH4 * 28) + (N2O * 265)
  if(nrow(results) > 0) {
    # Calculate expected CO2eq from individual gas columns
    calculated_co2 = (results$CH4_enteric_Gg[1] + results$CH4_manure_Gg[1]) * 28 +
      (results$N2O_direct_Gg[1] + results$N2O_vol_Gg[1] + results$N2O_lea_Gg[1]) * 265

    # Verify that the function's total matches our manual calculation
    expect_equal(results$CO2eq_Total_Gg[1], calculated_co2, tolerance = 0.001)
  }

  # 5. File persistence check
  # Ensure the report is physically saved to the output folder
  expect_true(file.exists("output/impact_assessment_summary.csv"))
})
