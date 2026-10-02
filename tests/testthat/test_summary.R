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
    "N2O_vol_Gg", "N2O_lea_Gg", "Land_m2",
    "Land_cropland_m2", "Land_grassland_convertible_m2", "Land_grassland_unconvertible_m2", "Land_other_m2",
    "CO2eq_Total_Gg",
    "total_protein_kg", "population", "primary_product",
    "DMI_kgday", "feed_intake_kg",
    "feed_CP_total_kg", "feed_CP_cropland_kg",
    "GHG_intensity_protein", "Land_intensity_protein",
    "GHG_intensity_head", "Land_intensity_head",
    "GHG_intensity_product", "Land_intensity_product",
    # Enfoque A (IDF Bulletin 520 / 2022) biophysical allocation
    "AF_milk", "AF_meat", "AF_wool", "AF_egg",
    "GHG_intensity_milk", "Land_intensity_milk",
    "GHG_intensity_meat", "Land_intensity_meat",
    "GHG_intensity_wool", "Land_intensity_wool",
    "GHG_intensity_egg", "Land_intensity_egg",
    # Mottet et al. (2017) Protein Feed Conversion
    "Protein_FCR_total", "Protein_FCR_cropland", "Protein_net_balance_kg"
  )
  for (col in expected_cols) {
    expect_true(col %in% colnames(results))
  }

  # 5. LOGICAL AND MATHEMATICAL VALIDATION
  if (nrow(results) > 0) {
    # Emissions and land use cannot be negative
    expect_true(all(results$CO2eq_Total_Gg >= 0))
    expect_true(all(results$Land_m2 >= 0))
    expect_true(all(results$Land_cropland_m2 >= 0))
    expect_true(all(results$Land_grassland_convertible_m2 >= 0))
    expect_true(all(results$Land_grassland_unconvertible_m2 >= 0))
    expect_true(all(results$Land_other_m2 >= 0))
    expect_equal(results$Land_m2, results$Land_cropland_m2 + results$Land_grassland_convertible_m2 + results$Land_grassland_unconvertible_m2 + results$Land_other_m2, tolerance = 1e-4)
    expect_true(all(results$feed_intake_kg >= 0))
    expect_true(all(results$DMI_kgday >= 0))

    # Check that the IPCC mathematical formula for CO2eq has been applied correctly:
    # (CH4_ent + CH4_man)*28 + (N2Os)*265
    sample_row <- results[1, ]
    calculated_co2 <- (sample_row$CH4_enteric_Gg * 28) +
      (sample_row$CH4_manure_Gg * 28) +
      ((sample_row$N2O_direct_Gg + sample_row$N2O_vol_Gg + sample_row$N2O_lea_Gg) * 265)

    expect_equal(sample_row$CO2eq_Total_Gg, calculated_co2, tolerance = 0.01)

    # Check Protein FCR metrics
    if (!is.na(sample_row$Protein_FCR_total)) {
      expect_true(sample_row$Protein_FCR_total > 0)
    }
    if (!is.na(sample_row$Protein_FCR_cropland)) {
      expect_true(sample_row$Protein_FCR_cropland >= 0)
    }
    if (!is.na(sample_row$Protein_net_balance_kg)) {
      expect_true(is.numeric(sample_row$Protein_net_balance_kg))
    }

    # Allocation factors sum to 1.0 (or 0 for non-milking/rearing)
    af_sum <- sample_row$AF_milk + sample_row$AF_meat + sample_row$AF_wool + sample_row$AF_egg
    if (af_sum > 0) {
      expect_equal(af_sum, 1.0, tolerance = 1e-3)
    }
  }

  # 6. FILTERING AND AGGREGATION OPTIONS
  filtered_res <- suppressWarnings(
    generate_impact_assessment(
      farm_country = "Spain",
      year = 2022,
      animal = "cattle",
      group_by_identification = FALSE,
      saveoutput = TRUE
    )
  )
  expect_s3_class(filtered_res, "data.frame")
  expect_true(all(filtered_res$animal_type == "cattle"))
  expect_true(file.exists("output/impact_assessment_summary.csv"))

  # 7. GWP REPORT SELECTION (AR6, AR5, AR4, SAR, CUSTOM)
  res_ar6 <- suppressWarnings(
    generate_impact_assessment(
      farm_country = "Spain",
      year = 2022,
      saveoutput = FALSE,
      ar = "AR6"
    )
  )
  expect_equal(attr(res_ar6, "gwp_report"), "AR6")
  row_ar6 <- res_ar6[1, ]
  exp_ar6 <- (row_ar6$CH4_enteric_Gg * 27) +
    (row_ar6$CH4_manure_Gg * 27) +
    ((row_ar6$N2O_direct_Gg + row_ar6$N2O_vol_Gg + row_ar6$N2O_lea_Gg) * 273)
  expect_equal(row_ar6$CO2eq_Total_Gg, exp_ar6, tolerance = 0.01)

  # Custom GWP vector
  res_custom <- suppressWarnings(
    generate_impact_assessment(
      farm_country = "Spain",
      year = 2022,
      saveoutput = FALSE,
      gwp_report = c(CH4 = 30, N2O = 250)
    )
  )
  row_cust <- res_custom[1, ]
  exp_cust <- (row_cust$CH4_enteric_Gg * 30) +
    (row_cust$CH4_manure_Gg * 30) +
    ((row_cust$N2O_direct_Gg + row_cust$N2O_vol_Gg + row_cust$N2O_lea_Gg) * 250)
  expect_equal(row_cust$CO2eq_Total_Gg, exp_cust, tolerance = 0.01)

  # Invalid GWP should stop with informative error
  expect_error(
    generate_impact_assessment(farm_country = "Spain", year = 2022, gwp_report = "INVALID"),
    "Invalid GWP report"
  )
})


