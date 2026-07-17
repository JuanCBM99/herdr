library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)

test_that("generate_impact_assessment produces correct output structure using provided CSVs", {

  # 1. Set test directory to use existing CSV files
  withr::local_dir(test_path("test_data"))

  # 2. Normalize existing files (trim whitespace, force character cols)
  #    Includes all inputs required across the whole pipeline
  #    (DMI, population, emissions, manure, N2O, land use)
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

  # 3. Guard: skip if diet_ingredients.csv has missing origins and no local
  #    FAO trade inputs are available (avoids network download / crash)
  diet_ing_path <- file.path("user_data", "diet_ingredients.csv")
  parquet_path  <- file.path("user_data", "fao_trade_matrix.parquet")
  prod_path     <- file.path("user_data", "fao_production.csv")

  if (file.exists(diet_ing_path)) {
    diet_ing <- read_csv(diet_ing_path, show_col_types = FALSE)
    needs_fao_engine   <- any(is.na(diet_ing$country_of_origin))
    missing_fao_inputs <- needs_fao_engine && (!file.exists(parquet_path) || !file.exists(prod_path))
    skip_if(missing_fao_inputs, "Missing country_of_origin values and no local FAO trade inputs; skipping to avoid network download in tests.")
  }

  # 4. Execute full impact assessment pipeline
  results <- suppressWarnings(suppressMessages(
    generate_impact_assessment(
      automatic_cycle = FALSE,
      saveoutput = FALSE,
      group_by_identification = TRUE,
      farm_country = "Spain",
      year = 2022
    )
  ))

  # 5. Assertions
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

  # Basic consistency checks
  if (nrow(results) > 0) {
    numeric_cols <- results %>% dplyr::select(where(is.numeric))
    expect_false(any(sapply(numeric_cols, function(x) any(is.na(x)))))

    expect_true(all(results$CH4_enteric_Gg >= 0))
    expect_true(all(results$CH4_manure_Gg >= 0))
    expect_true(all(results$Land_m2 >= 0))
    expect_true(all(results$CO2eq_Total_Gg >= 0))

    # CO2eq_Total_Gg should equal the sum of its components (within tolerance)
    expect_equal(
      results$CO2eq_Total_Gg,
      results$CO2eq_enteric + results$CO2eq_manure + results$CO2eq_N2O,
      tolerance = 1e-6
    )
  }
})

test_that("generate_impact_assessment applies filters correctly", {

  withr::local_dir(test_path("test_data"))

  diet_ing_path <- file.path("user_data", "diet_ingredients.csv")
  parquet_path  <- file.path("user_data", "fao_trade_matrix.parquet")
  prod_path     <- file.path("user_data", "fao_production.csv")

  if (file.exists(diet_ing_path)) {
    diet_ing <- read_csv(diet_ing_path, show_col_types = FALSE)
    needs_fao_engine   <- any(is.na(diet_ing$country_of_origin))
    missing_fao_inputs <- needs_fao_engine && (!file.exists(parquet_path) || !file.exists(prod_path))
    skip_if(missing_fao_inputs, "Missing FAO trade inputs; skipping filter test.")
  }

  full_results <- suppressWarnings(suppressMessages(
    generate_impact_assessment(saveoutput = FALSE)
  ))

  skip_if(nrow(full_results) == 0, "No rows returned by pipeline; skipping filter test.")

  sample_animal <- full_results$animal_type[1]

  filtered_results <- suppressWarnings(suppressMessages(
    generate_impact_assessment(saveoutput = FALSE, animal = sample_animal)
  ))

  expect_true(all(filtered_results$animal_type == sample_animal))
  expect_true(nrow(filtered_results) <= nrow(full_results))
})
