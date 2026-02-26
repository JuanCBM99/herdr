library(testthat)
library(herdr)
library(dplyr)

test_that("calculate_vs computes volatile solids correctly using IPCC formula", {

  # 1. Setup Identity
  test_keys <- data.frame(
    region = "R1", subregion = "S1",
    animal_tag = "cow_vs_test", class_flex = "stall",
    animal_type = "cattle", animal_subtype = "dairy",
    stringsAsFactors = FALSE
  )

  # 2. Prepare Mocks
  # Simulamos GE = 250 MJ/day, DE = 65%
  mock_ge   <- mutate(test_keys, ge = 250, de = 65)
  # Simulamos Ash = 8%
  mock_diet <- mutate(test_keys, ash = 8)

  # 3. Execution with Mocked Internal herdr Functions
  res <- testthat::with_mocked_bindings(
    calculate_vs(urinary_energy = 0.04, saveoutput = FALSE),

    calculate_ge = function(...) mock_ge,
    calculate_weighted_variable = function(...) mock_diet,

    .package = "herdr"
  )

  # 4. Assertions
  # --- Verificación del Cálculo ---
  # GE = 250, DE = 65, UE = 0.04, ASH = 8
  # VS = [250 * (1 - 0.65) + (0.04 * 250)] * [(1 - 0.08) / 18.45]
  # VS = [250 * 0.35 + 10] * [0.92 / 18.45]
  # VS = [87.5 + 10] * 0.04986 = 97.5 * 0.04986 = 4.861

  expect_s3_class(res, "data.frame")
  expect_true("vs" %in% colnames(res))

  val_vs <- res$vs[1]
  expect_equal(val_vs, 4.861, tolerance = 0.001)

  # Verificación de llaves y metadatos
  expect_equal(res$animal_tag[1], "cow_vs_test")
  expect_equal(res$urinary_energy[1], 0.04)
  expect_true(all(c("ge", "de", "ash") %in% colnames(res)))
})
