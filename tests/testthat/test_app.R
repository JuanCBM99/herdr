library(testthat)
library(shiny)
library(herdr)
library(dplyr)
library(readr)
library(withr)

# Source app script from inst/app or local path
app_file <- system.file("app", "app.R", package = "herdr")
if (app_file == "" || !file.exists(app_file)) {
  # Fallback for active source development
  app_file <- testthat::test_path("../../inst/app/app.R")
}

# If the app is structured as a standalone script, source it in a clean environment
app_env <- new.env()
sys.source(app_file, envir = app_env)
app_server <- app_env$server

test_that("Shiny server initializes tables and runs calculation cycle safely", {
  withr::local_dir(test_path("test_data"))

  shiny::testServer(app_server, {
    # 1. Verification of Initial Reactives
    expect_true(exists("rv"))
    expect_true(exists("load_all_data"))

    # Check that initial data frames loaded properly
    expect_s3_class(rv$census, "data.frame")
    expect_s3_class(rv$def, "data.frame")
    expect_s3_class(rv$ipcc_coef, "data.frame")

    # 2. Test Single Function Execution (Gross Energy)
    session$setInputs(
      function_choice = "calculate_ge",
      auto_cycle = FALSE,
      farm_country = "Spain",
      year = 2022,
      plot_groups = c("animal_tag", "class_flex"),
      calculate = 1
    )

    # Check model output data
    calc_res <- model_data()
    expect_false(is.null(calc_res))
    expect_s3_class(calc_res, "data.frame")
    expect_true("GE_MJday" %in% names(calc_res))

    # Check reactive plot generation
    plot_obj <- current_plot()
    expect_s3_class(plot_obj, "ggplot")
    expect_match(plot_obj$labels$title, "Results for GE MJday")

    # 3. Test Full Assessment Pipeline Execution
    session$setInputs(
      function_choice = "generate_impact_assessment",
      calculate = 2
    )

    summary_res <- model_data()
    expect_false(is.null(summary_res))
    expect_s3_class(summary_res, "data.frame")

    # 4. Test Data Reset Action
    session$setInputs(reset_data = 1)
    expect_equal(nrow(rv$census), 0)
    expect_null(model_data())
  })
})
