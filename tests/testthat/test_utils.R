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
