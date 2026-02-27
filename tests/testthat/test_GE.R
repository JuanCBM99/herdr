library(testthat)
library(herdr)
library(dplyr)

test_that("calculate_ge correctly consolidates all NE components into Gross Energy", {

  test_keys <- data.frame(
    region = "Reg1", subregion = "Sub1",
    animal_tag = "master_cow", class_flex = "grazing",
    animal_type = "cattle", animal_subtype = "dairy",
    stringsAsFactors = FALSE
  )

  mock_de    <- mutate(test_keys, de = 65)
  mock_nem   <- mutate(test_keys, NEm = 40)
  mock_nea   <- mutate(test_keys, NEa = 14)
  mock_neg   <- mutate(test_keys, NEg = 10)
  mock_nel   <- mutate(test_keys, NEl = 20)
  mock_work  <- mutate(test_keys, NE_work = 0)
  mock_preg  <- mutate(test_keys, NE_pregnancy = 4)
  mock_wool  <- mutate(test_keys, NE_wool = 0)

  res <- testthat::with_mocked_bindings(
    calculate_ge(saveoutput = FALSE),

    calculate_weighted_variable = function(...) mock_de,
    calculate_NEm = function(...) mock_nem,
    calculate_NEa = function(...) mock_nea,
    calculate_NEg = function(...) mock_neg,
    calculate_NEl = function(...) mock_nel,
    calculate_NE_work = function(...) mock_work,
    calculate_NE_pregnancy = function(...) mock_preg,
    calculate_NE_wool = function(...) mock_wool,

    .package = "herdr"
  )

  # de = 65 -> de_percent = 0.65
  # rem = 1.123 - (4.092e-3 * 65) + (1.126e-5 * 65^2) - (25.4 / 65) = 0.514
  # reg = 1.164 - (5.16e-3 * 65) + (1.308e-5 * 65^2) - (37.4 / 65) = 0.308
  # GE = ( (40+14+20+0+4)/0.514 + (10+0)/0.308 ) / 0.65 = 283.424

  expect_s3_class(res, "data.frame")
  expect_true("ge" %in% colnames(res))

  val_ge <- res$ge[1]
  expect_gt(val_ge, 200)
  expect_equal(val_ge, 283.424, tolerance = 0.1)

  expect_equal(res$animal_tag[1], "master_cow")
  expect_equal(res$class_flex[1], "grazing")
})
