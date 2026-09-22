library(testthat)
library(shiny)
library(herdr)
library(dplyr)
library(readr)
library(withr)

# Source app script from local source path (for development) or installed package
local_app <- testthat::test_path("../../inst/app/app.R")
if (file.exists(local_app)) {
  app_file <- normalizePath(local_app, winslash = "/")
  .GlobalEnv$.HERDR_APP_DIR <- normalizePath(dirname(local_app), winslash = "/")
  on.exit(rm(list = ".HERDR_APP_DIR", envir = .GlobalEnv), add = TRUE)
} else {
  app_file <- system.file("app", "app.R", package = "herdr")
}

# If the app is structured as a standalone script, source it in a clean environment
app_env <- new.env()
sys.source(app_file, envir = app_env)
app_server <- app_env$server

test_that("Shiny server initializes tables and runs calculation cycle safely", {
  temp_test_dir <- tempfile()
  dir.create(temp_test_dir)
  file.copy(from = test_path("test_data/user_data"), to = temp_test_dir, recursive = TRUE)
  withr::local_dir(temp_test_dir)

  # Anti-download shield: prevent triggering 187 MB FAO trade matrix download during tests
  path_diet <- "user_data/diet_ingredients.csv"
  if (file.exists(path_diet)) {
    df <- readr::read_csv(path_diet, col_types = readr::cols(.default = "c"), show_col_types = FALSE)
    if (!"country_of_origin" %in% names(df)) {
      df <- df %>% dplyr::mutate(country_of_origin = "Spain")
    } else {
      df <- df %>% dplyr::mutate(country_of_origin = ifelse(is.na(country_of_origin), "Spain", country_of_origin))
    }
    readr::write_csv(df, path_diet)
  }

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

test_that("Enhanced data health validation flags invalid animal types, invented animals, and 200% ingredients", {
  temp_test_dir <- tempfile()
  dir.create(temp_test_dir)
  file.copy(from = test_path("test_data/user_data"), to = temp_test_dir, recursive = TRUE)
  withr::local_dir(temp_test_dir)

  app_env <- new.env()
  sys.source(app_file, envir = app_env)
  app_server <- app_env$server

  shiny::testServer(app_server, {
    # 1. Clean data should have 0 issues
    issues_clean <- project_validation_issues()
    expect_equal(length(issues_clean), 0)

    # 2. Entering 200% on ingredient share should trigger an error on diet_ingr
    rv$diet_ingr$ingredient_share[1] <- "200"
    issues_200 <- project_validation_issues()
    expect_true(any(sapply(issues_200, function(x) x$table_id == "diet_ingr" && grepl("100%", x$title))))

    # 3. Adding an invented animal in census triggers missing definition error in Ruminants (def)
    rv$diet_ingr$ingredient_share[1] <- "100"
    new_row <- rv$census[1, ]
    new_row$animal_tag <- "unicorn"
    new_row$population <- 50
    rv$census <- rbind(new_row, rv$census)
    issues_invented <- project_validation_issues()
    expect_true(any(sapply(issues_invented, function(x) x$table_id == "def" && grepl("unicorn", x$message))))
    expect_false(any(sapply(issues_invented, function(x) x$table_id == "census" && grepl("unicorn", x$message))))

    # 4. Adding invalid animal_type in ruminants triggers error
    rv$def$animal_type[1] <- "dinosaur"
    issues_type <- project_validation_issues()
    expect_true(any(sapply(issues_type, function(x) x$table_id == "def" && grepl("dinosaur", x$message))))

    # 5. Typing an invalid variant like 'a' in manure triggers error
    rv$manure$system_variant[1] <- "a"
    issues_manure <- project_validation_issues()
    expect_true(any(sapply(issues_manure, function(x) x$table_id == "manure" && grepl("system_variant", x$message))))

    # 6. Trailing space in manure triggers whitespace error
    rv$manure$system_variant[1] <- NA
    rv$manure$system_variant[12] <- "uncovered "
    issues_space <- project_validation_issues()
    expect_true(any(sapply(issues_space, function(x) x$table_id == "manure" && grepl("Whitespace", x$title))))

    # 7. Cohort defined downstream in def that is NOT in Census triggers error
    new_def <- rv$def[1, ]
    new_def$animal_tag <- "perro"
    rv$def <- rbind(new_def, rv$def)
    issues_downstream <- project_validation_issues()
    expect_true(any(sapply(issues_downstream, function(x) x$table_id == "def" && grepl("Cohort Not in Census", x$title) && grepl("perro", x$message))))
  })
})

