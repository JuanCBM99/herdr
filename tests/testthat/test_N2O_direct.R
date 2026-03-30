library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)

test_that("calculate_N2O_direct_manure computes direct emissions from nitrogen excretion", {
  # 1. Set test directory
  withr::local_dir(test_path("test_data"))

  # 2. Normalize and prepare input CSVs
  # N2O calculation requires almost all master files to be consistent
  files_to_fix <- c(
    "livestock_definitions.csv", "livestock_weights.csv",
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
  required_cols <- c("N_intake_kgheadday", "N_retention", "N_excreted_kgheadday",
                     "EF3", "direct_N2O_kgyear")
  expect_true(all(required_cols %in% colnames(results)))

  # 5. Assertions: Biological & Mathematical Logic
  if(nrow(results) > 0) {
    # N_intake must be positive if animal eats (GE > 0 and CP > 0)
    expect_true(all(results$N_intake_kgheadday >= 0))

    # N_retention logic check:
    # For sheep/goats it should be a fixed 0.1 according to your function
    small_ruminants <- results %>% filter(tolower(animal_type) %in% c("sheep", "goat"))
    if(nrow(small_ruminants) > 0) {
      expect_equal(unique(small_ruminants$N_retention), 0.1)
    }

    # Total N2O must be numeric and not NA
    expect_false(any(is.na(results$direct_N2O_kgyear)))

    # Unit check: direct_N2O_kgyear includes the molecular weight conversion (44/28)
    # If N_excreted and EF3 are > 0, emissions must be > 0
    sample_active <- results %>% filter(N_excreted_kgheadday > 0 & EF3 > 0) %>% head(1)
    if(nrow(sample_active) > 0) {
      expect_gt(sample_active$direct_N2O_kgyear, 0)
    }
  }
})
