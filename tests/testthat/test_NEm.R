library(testthat)
library(herdr)
library(dplyr)

test_that("calculate_NEm correctly joins keys and applies IPCC maintenance formula", {

  # 1. Mock Data Setup
  # Identity keys common to all mocks
  test_keys <- data.frame(
    region = "Reg1", subregion = "Sub1",
    animal_tag = "dairy_cow_test", class_flex = "grazing",
    stringsAsFactors = FALSE
  )

  # Weights: IPCC Eq 10.3 requires live weight
  mock_weights <- mutate(test_keys, average_weight = 600)

  # Categories: Links animal to its CFI (Maintenance Coefficient)
  mock_cats <- mutate(test_keys,
                      animal_type = "cattle",
                      animal_subtype = "dairy",
                      cfi = "cfi_dairy_cows")

  # Coefficients: Standard IPCC values
  # Dairy cattle maintenance coefficient is usually ~0.335 for lactating cows
  mock_coeffs <- data.frame(
    coefficient = "cfi",
    description = "cfi_dairy_cows",
    value = 0.335,
    stringsAsFactors = FALSE
  )

  # 2. Execution with Mocked readr
  res <- testthat::with_mocked_bindings(
    calculate_NEm(saveoutput = FALSE),

    # Intercepting read_csv calls within the readr package
    read_csv = function(file, ...) {
      if (grepl("weights.csv", file)) return(mock_weights)
      if (grepl("categories.csv", file)) return(mock_cats)
      if (grepl("ipcc_coefficients.csv", file)) return(mock_coeffs)
      return(data.frame())
    },
    .package = "readr"
  )

  # 3. Assertions
  # Verify identity keys are preserved
  expect_true(all(c("region", "subregion", "animal_tag", "class_flex") %in% colnames(res)))

  # Verify Calculation: NEm = Cfi * (Weight ^ 0.75)
  # 0.335 * (600 ^ 0.75) = 0.335 * 121.22 = 40.609
  expected_nem <- round(0.335 * (600 ^ 0.75), 3)

  val_calc <- res$NEm[1]
  expect_equal(val_calc, expected_nem, tolerance = 0.001)

  # Ensure character columns are preserved for subsequent joins
  expect_equal(res$animal_type[1], "cattle")
})
