library(testthat)
library(herdr)
library(readr)
library(dplyr)

setup_work_env <- function() {
  if (!dir.exists("user_data")) dir.create("user_data")

  # 1. Weights (Required by NEm)
  write_csv(data.frame(
    region = "global", subregion = "all", animal_tag = "oxen_work",
    class_flex = "draft", average_weight = 500
  ), "user_data/livestock_weights.csv")

  # 2. Definitions (Mapping cfi for NEm and work_hours for NE_work)
  write_csv(data.frame(
    region = "global", subregion = "all", animal_tag = "oxen_work",
    class_flex = "draft", animal_type = "cattle", animal_subtype = "beef",
    cfi = "draft_ox", work_hours = 4
  ), "user_data/livestock_definitions.csv")

  # 3. IPCC Coefficients (Required by NEm)
  write_csv(data.frame(
    coefficient = "cfi",
    description = "draft_ox",
    value = 0.335
  ), "user_data/ipcc_coefficients.csv")
}

cleanup_work_env <- function() {
  if (dir.exists("user_data")) unlink("user_data", recursive = TRUE)
  if (dir.exists("output")) unlink("output", recursive = TRUE)
}

# --- TESTS ---

test_that("calculate_NE_work computes work energy correctly", {
  setup_work_env()

  # Execute
  results <- calculate_NE_work(saveoutput = FALSE)

  # Math check:
  # 1. NEm = 0.335 * (500 ^ 0.75) ≈ 35.422
  # 2. NE_work = 4 * 35.422 = 141.688

  expect_s3_class(results, "data.frame")
  expect_equal(results$NE_work[1], 141.688, tolerance = 0.01)

  cleanup_work_env()
})

test_that("calculate_NE_work handles missing work hours with zero", {
  setup_work_env()

  # Overwrite with NA in work_hours
  write_csv(data.frame(
    region = "global", subregion = "all", animal_tag = "oxen_work",
    class_flex = "draft", animal_type = "cattle", animal_subtype = "beef",
    cfi = "draft_ox", work_hours = NA
  ), "user_data/livestock_definitions.csv")

  results <- calculate_NE_work(saveoutput = FALSE)

  # NE_work should be 0 due to replace_na
  expect_equal(results$NE_work[1], 0)

  cleanup_work_env()
})
