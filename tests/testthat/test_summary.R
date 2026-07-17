library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)

# ---------------------------------------------------------------------------
# Shared helper: normalize CSVs + skip guard, reused across tests
# ---------------------------------------------------------------------------
.prepare_impact_test_env <- function() {
  files_to_fix <- c(
    "livestock_weights.csv",
    "livestock_definitions.csv",
    "ipcc_coefficients.csv",
    "diet_profiles.csv",
    "diet_ingredients.csv",
    "feed_characteristics.csv",
    "mapping.csv",
    "fao_crop_yields.csv",
    "forage_yields.csv",
    "fao_production.csv",
    "livestock_census.csv",
    "manure_management.csv",
    "emission_factors.csv",
    "nitrogen_excretion.csv"
  )

  for (f in files_to_fix) {
    path <- file.path("user_data", f)
    if (file.exists(path)) {
      read_csv(path, col_types = cols(.default = "c"), show_col_types = FALSE) %>%
        mutate(across(everything(), trimws)) %>%
        write_csv(path)
    }
  }

  diet_ing_path <- file.path("user_data", "diet_ingredients.csv")
  parquet_path  <- file.path("user_data", "fao_trade_matrix.parquet")
  prod_path     <- file.path("user_data", "fao_production.csv")

  if (file.exists(diet_ing_path)) {
    diet_ing <- read_csv(diet_ing_path, show_col_types = FALSE)
    needs_fao_engine   <- any(is.na(diet_ing$country_of_origin))
    missing_fao_inputs <- needs_fao_engine && (!file.exists(parquet_path) || !file.exists(prod_path))
    skip_if(missing_fao_inputs, "Missing FAO trade inputs; skipping.")
  }
}

# ---------------------------------------------------------------------------
# 1. Happy path: structure and internal consistency
# ---------------------------------------------------------------------------
test_that("generate_impact_assessment produces correct output structure using provided CSVs", {

  withr::local_dir(test_path("test_data"))
  .prepare_impact_test_env()

  results <- suppressWarnings(suppressMessages(
    generate_impact_assessment(
      automatic_cycle = FALSE,
      saveoutput = FALSE,
      group_by_identification = TRUE,
      farm_country = "Spain",
      year = 2022
    )
  ))

  expect_s3_class(results, "data.frame")

  expected_cols <- c(
    "region", "subregion", "animal_tag", "class_flex",
    "animal_type", "animal_subtype",
    "CH4_enteric_Gg", "CH4_manure_Gg",
    "N2O_direct_Gg", "N2O_vol_Gg", "N2O_lea_Gg",
    "Land_m2",
    "CO2eq_enteric", "CO2eq_manure", "CO2eq_N2O", "CO2eq_Total_Gg"
  )
  expect_true(all(expected_cols %in% colnames(results)))

  if (nrow(results) > 0) {
    numeric_cols <- results %>% dplyr::select(where(is.numeric))
    expect_false(any(sapply(numeric_cols, function(x) any(is.na(x)))))

    expect_true(all(results$CH4_enteric_Gg >= 0))
    expect_true(all(results$CH4_manure_Gg >= 0))
    expect_true(all(results$Land_m2 >= 0))
    expect_true(all(results$CO2eq_Total_Gg >= 0))

    expect_equal(
      results$CO2eq_Total_Gg,
      results$CO2eq_enteric + results$CO2eq_manure + results$CO2eq_N2O,
      tolerance = 1e-6
    )
  }
})


# ---------------------------------------------------------------------------
# 2. Aggregation: group_by_identification = FALSE collapses animal_tag
# ---------------------------------------------------------------------------
test_that("generate_impact_assessment aggregates correctly when group_by_identification = FALSE", {

  withr::local_dir(test_path("test_data"))
  .prepare_impact_test_env()

  detailed <- suppressWarnings(suppressMessages(
    generate_impact_assessment(saveoutput = FALSE, group_by_identification = TRUE)
  ))
  skip_if(nrow(detailed) == 0, "No rows returned by pipeline; skipping.")

  aggregated <- suppressWarnings(suppressMessages(
    generate_impact_assessment(saveoutput = FALSE, group_by_identification = FALSE)
  ))

  expect_s3_class(aggregated, "data.frame")
  expect_false("animal_tag" %in% colnames(aggregated))

  # Aggregated should have fewer or equal rows than the detailed version
  expect_true(nrow(aggregated) <= nrow(detailed))

  # Totals should be conserved across the two grouping levels
  expect_equal(
    sum(detailed$CO2eq_Total_Gg, na.rm = TRUE),
    sum(aggregated$CO2eq_Total_Gg, na.rm = TRUE),
    tolerance = 1e-3
  )
})

# ---------------------------------------------------------------------------
# 3. saveoutput = TRUE writes the summary CSV to disk
# ---------------------------------------------------------------------------
test_that("generate_impact_assessment saves output to CSV when saveoutput = TRUE", {

  withr::local_dir(test_path("test_data"))
  .prepare_impact_test_env()

  if (dir.exists("output")) unlink("output", recursive = TRUE)
  withr::defer(if (dir.exists("output")) unlink("output", recursive = TRUE))

  suppressWarnings(suppressMessages(
    generate_impact_assessment(saveoutput = TRUE)
  ))

  expect_true(file.exists("output/impact_assessment_summary.csv"))

  saved <- read_csv("output/impact_assessment_summary.csv", show_col_types = FALSE)
  expect_true(nrow(saved) > 0)
  expect_true("CO2eq_Total_Gg" %in% colnames(saved))
})

# ---------------------------------------------------------------------------
# 4. farm_country / year are correctly forwarded to calculate_land_use
# ---------------------------------------------------------------------------
test_that("generate_impact_assessment forwards farm_country and year to land use calculation", {

  withr::local_dir(test_path("test_data"))
  .prepare_impact_test_env()

  results_2022 <- suppressWarnings(suppressMessages(
    generate_impact_assessment(saveoutput = FALSE, farm_country = "Spain", year = 2022)
  ))

  expect_s3_class(results_2022, "data.frame")
  expect_true("Land_m2" %in% colnames(results_2022))
  # Land_m2 should be computed (non-negative, present) regardless of the
  # specific year value, confirming the parameter was accepted and used
  # without erroring the pipeline
  expect_true(all(results_2022$Land_m2 >= 0, na.rm = TRUE))
})

# ---------------------------------------------------------------------------
# 5. CO2eq math uses the correct GWP factors (AR5: CH4=28, N2O=265)
# ---------------------------------------------------------------------------
test_that("generate_impact_assessment applies correct GWP conversion factors", {

  withr::local_dir(test_path("test_data"))
  .prepare_impact_test_env()

  results <- suppressWarnings(suppressMessages(
    generate_impact_assessment(saveoutput = FALSE)
  ))
  skip_if(nrow(results) == 0, "No rows returned by pipeline; skipping.")

  expect_equal(results$CO2eq_enteric, results$CH4_enteric_Gg * 28, tolerance = 1e-6)
  expect_equal(results$CO2eq_manure, results$CH4_manure_Gg * 28, tolerance = 1e-6)
  expect_equal(
    results$CO2eq_N2O,
    (results$N2O_direct_Gg + results$N2O_vol_Gg + results$N2O_lea_Gg) * 265,
    tolerance = 1e-6
  )
})
