library(testthat)
library(herdr)
library(withr)

test_that("herdr_init initializes project structure and copies example templates", {
  withr::with_tempdir({
    expect_message(herdr_init(), "Initializing herdr project structure")

    expect_true(dir.exists("user_data"))
    expect_true(dir.exists("Examples"))

    # Verify that essential template files were copied into user_data
    ud_files <- list.files("user_data")
    expect_true(length(ud_files) > 0)
    expect_true(any(grepl("livestock_census\\.csv", ud_files)))
  })
})

test_that("reference data paths resolve bundled datasets correctly", {
  forage_path <- herdr_get_forages_path()
  expect_true(file.exists(forage_path))

  ecoalim_path <- herdr_get_ecoalim_path()
  expect_true(file.exists(ecoalim_path))

  df_eco <- arrow::read_parquet(ecoalim_path)
  expect_equal(nrow(df_eco), 224)
  expect_equal(ncol(df_eco), 38)
})

test_that("herdr_cache_dir returns a valid directory path", {
  cache <- herdr_cache_dir()
  expect_type(cache, "character")
  expect_true(dir.exists(cache))
})

test_that("reference data paths prioritize local data_dir when present", {
  withr::with_tempdir({
    dir.create("custom_data")
    mock_file <- file.path("custom_data", "forages.parquet")
    file.copy(herdr_get_forages_path(), mock_file)

    resolved <- herdr_get_forages_path(data_dir = "custom_data")
    expect_equal(normalizePath(resolved), normalizePath(mock_file))
  })
})
