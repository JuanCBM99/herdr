library(testthat)
library(herdr)
library(dplyr)

test_that("calculate_NEl correctly computes lactation energy for cattle and small ruminants", {

  # 1. Setup Identity and Mock Data
  test_keys <- data.frame(
    region = "Reg1", subregion = "Sub1", class_flex = "stall",
    stringsAsFactors = FALSE
  )

  # Mock Categories: Una vaca con grasa variable y una cabra
  mock_cats <- data.frame(
    test_keys,
    animal_tag = c("cow_milk", "goat_milk"),
    animal_type = c("cattle", "goat"),
    animal_subtype = c("dairy", "dairy"),
    milk_yield = c(8000, 1000), # kg/año
    fat_content = c(4.0, 3.5),   # % grasa
    stringsAsFactors = FALSE
  )

  # Mock Weights: Solo se usa para el inner_join de identidad
  mock_weights <- data.frame(
    test_keys,
    animal_tag = c("cow_milk", "goat_milk"),
    stringsAsFactors = FALSE
  )

  # 2. Execution with Mocked readr
  res <- testthat::with_mocked_bindings(
    calculate_NEl(saveoutput = FALSE),

    read_csv = function(file, ...) {
      if (grepl("categories.csv", file)) return(mock_cats)
      if (grepl("weights.csv", file)) return(mock_weights)
      return(data.frame())
    },
    .package = "readr"
  )

  # 3. Assertions
  # --- Cattle Check ---
  # Yield_day = 8000 / 365 = 21.9178
  # NEl = 21.9178 * (1.47 + 0.4 * 4.0) = 21.9178 * 3.07 = 67.288
  val_cattle <- res %>% filter(animal_type == "cattle") %>% pull(NEl)
  expect_equal(val_cattle, 67.288, tolerance = 0.001)

  # --- Goat Check ---
  # Yield_day = 1000 / 365 = 2.7397
  # NEl = 2.7397 * 4.6 = 12.603
  val_goat <- res %>% filter(animal_type == "goat") %>% pull(NEl)
  expect_equal(val_goat, 12.603, tolerance = 0.001)

  # 4. Identity Check
  expect_true(all(c("region", "animal_tag", "NEl") %in% colnames(res)))
  expect_equal(nrow(res), 2)
})
