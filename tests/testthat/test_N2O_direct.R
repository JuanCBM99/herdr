library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)

test_that("calculate_N2O_direct_manure computes direct emissions from nitrogen excretion", {
  # 1. Set test directory in an isolated temporary location
  temp_test_dir <- tempfile()
  dir.create(temp_test_dir)
  file.copy(from = test_path("test_data/user_data"), to = temp_test_dir, recursive = TRUE)
  withr::local_dir(temp_test_dir)

  # 2. Normalize and prepare input CSVs
  files_to_fix <- c(
    "ruminant_definitions.csv", "livestock_weights.csv",
    "livestock_census.csv", "manure_management.csv",
    "ipcc_mm.csv", "diet_profiles.csv",
    "diet_ingredients.csv", "feed_characteristics.csv",
    "ipcc_coefficients.csv"
  )

  for (f in files_to_fix) {
    path <- file.path("user_data", f)
    if (file.exists(path)) {
      read_csv(path, col_types = cols(.default = "c"), show_col_types = FALSE) %>%
        mutate(across(everything(), trimws)) %>%
        write_csv(path)
    }
  }

  # 3. Execute N2O Direct Manure calculation
  # We use suppressWarnings to skip nutritional alerts during the GE/CP sub-calls
  results <- suppressWarnings(
    calculate_N2O_direct_manure(automatic_cycle = FALSE, saveoutput = FALSE)
  )

  # 4. Assertions: Structure
  expect_s3_class(results, "data.frame")

  # Check for mandatory IPCC columns for Nitrogen reporting
  required_cols <- c("N_intake_kgheadday", "N_retention_kg_day", "N_excreted_kgheadyear",
                     "EF3", "direct_N2O_kgyear")
  expect_true(all(required_cols %in% colnames(results)))

  # 5. Assertions: Biological & Mathematical Logic
  if(nrow(results) > 0) {
    # N_intake must be positive if animal eats (GE > 0 and CP > 0)
    expect_true(all(results$N_intake_kgheadday >= 0))

    # N_retention logic check:
    # For sheep/goats it should be 10% of N intake according to IPCC 2019 Table 10.20
    small_ruminants <- results %>% filter(tolower(animal_type) %in% c("sheep", "goat"))
    if(nrow(small_ruminants) > 0) {
      expect_equal(small_ruminants$N_retention_kg_day, round(0.10 * small_ruminants$N_intake_kgheadday, 4), tolerance = 1e-4)
    }

    # Total N2O must be numeric and not NA
    expect_false(any(is.na(results$direct_N2O_kgyear)))

    # Unit check: direct_N2O_kgyear includes the molecular weight conversion (44/28)
    # If N_excreted and EF3 are > 0, emissions must be > 0
    sample_active <- results %>% filter(N_excreted_kgheadyear > 0 & EF3 > 0) %>% head(1)
    if(nrow(sample_active) > 0) {
      expect_gt(sample_active$direct_N2O_kgyear, 0)
    }
  }
})

test_that("calculate_N2O_direct_manure detects nitrogen retention exceeding intake", {
  temp_test_dir <- tempfile()
  dir.create(temp_test_dir)
  file.copy(from = test_path("test_data/user_data"), to = temp_test_dir, recursive = TRUE)
  withr::local_dir(temp_test_dir)

  # Artificially set an extreme milk yield with low protein intake
  rum_path <- "user_data/ruminant_definitions.csv"
  rum <- read_csv(rum_path, show_col_types = FALSE)
  rum$milk_yield_kg_year <- 250000
  write_csv(rum, rum_path)

  diet_path <- "user_data/feed_characteristics.csv"
  feed <- read_csv(diet_path, show_col_types = FALSE)
  feed$CP_pct <- 0.1
  write_csv(feed, diet_path)

  expect_error(
    suppressWarnings(calculate_N2O_direct_manure(automatic_cycle = FALSE, saveoutput = FALSE)),
    "Biological Imbalance Error: Daily nitrogen retention exceeds daily nitrogen intake"
  )
})

test_that("herdr_validate_manure_coverage stops when a populated cohort has no manure system", {
  pop <- data.frame(
    region = "R1", subregion = "S1",
    animal_tag = c("dairy_cows", "beef_cows", "empty_cohort"),
    population = c(100, 50, 0),
    stringsAsFactors = FALSE
  )
  manure <- data.frame(
    region = "R1", subregion = "S1",
    animal_tag = "dairy_cows", system_base = "Lagoon",
    stringsAsFactors = FALSE
  )

  # beef_cows has animals but no manure system -> error naming only that cohort
  expect_error(
    herdr:::herdr_validate_manure_coverage(pop, manure),
    "beef_cows"
  )
  expect_error(
    herdr:::herdr_validate_manure_coverage(pop, manure),
    "no manure management system"
  )

  # Cohort with zero animals does not need a system (empty_cohort is not reported)
  err <- tryCatch(herdr:::herdr_validate_manure_coverage(pop, manure), error = function(e) conditionMessage(e))
  expect_false(grepl("empty_cohort", err))

  # Fully covered herd passes silently
  manure_full <- rbind(manure, transform(manure, animal_tag = "beef_cows"))
  expect_true(herdr:::herdr_validate_manure_coverage(pop, manure_full))
})

