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

test_that("Shiny server initializes tables and runs calculation cycle safely", {
  temp_test_dir <- tempfile()
  dir.create(temp_test_dir)
  file.copy(from = test_path("test_data/user_data"), to = temp_test_dir, recursive = TRUE)
  withr::local_dir(temp_test_dir)

  # If the app is structured as a standalone script, source it in a clean environment
  app_env <- new.env()
  sys.source(app_file, envir = app_env)
  app_server <- app_env$server

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
      plot_groups = c("animal_tag", "region"),
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
    session$setInputs(reset_data = 1, confirm_reset_data = 1)
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
    rv$def$animal_type[1] <- "cattle"

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
    new_def$animal_tag <- "unknown_cohort"
    rv$def <- rbind(new_def, rv$def)
    issues_downstream <- project_validation_issues()
    expect_true(any(sapply(issues_downstream, function(x) x$table_id == "def" && grepl("Cohort Not in Census", x$title) && grepl("unknown_cohort", x$message))))

    # 8. Demographic cohorts (feedlot calves, replacement heifers) defined downstream:
    rv$def <- rv$def[rv$def$animal_tag != "unknown_cohort", ]
    rv$census <- rv$census[rv$census$animal_tag != "feedlot_calves_male", ]

    new_demo_def <- rv$def[1, ]
    new_demo_def$animal_tag <- "feedlot_calves_male"
    rv$def <- rbind(rv$def, new_demo_def)

    new_demo_w <- rv$weights[1, ]
    new_demo_w$animal_tag <- "feedlot_calves_male"
    rv$weights <- rbind(rv$weights, new_demo_w)

    # When auto_cycle is FALSE, demographic cohort raises an advisory warning, NOT a blocking error
    session$setInputs(auto_cycle = FALSE)
    issues_demo_off <- project_validation_issues()
    expect_false(any(sapply(issues_demo_off, function(x) x$severity == "error" && grepl("feedlot_calves_male", x$message))))
    expect_true(any(sapply(issues_demo_off, function(x) x$severity == "warning" && grepl("feedlot_calves_male", x$message))))

    # When auto_cycle is TRUE, demographic cohort is completely valid (0 errors, 0 warnings for feedlot_calves_male)
    session$setInputs(auto_cycle = TRUE)
    issues_demo_on <- project_validation_issues()
    expect_false(any(sapply(issues_demo_on, function(x) grepl("feedlot_calves_male", x$message))))

    # 9. Modifying manure allocation to != 100% triggers a blocking error in manure
    orig_alloc <- rv$manure$allocation[13]
    rv$manure$allocation[13] <- "0.5" # Alters mature_dairy_cattle sum to ~1.45
    issues_bad_alloc <- project_validation_issues()
    expect_true(any(sapply(issues_bad_alloc, function(x) x$table_id == "manure" && x$severity == "error" && grepl("100%", x$title))))

    # 10. Negative manure allocation triggers a blocking error
    rv$manure$allocation[13] <- "-0.1"
    issues_neg_alloc <- project_validation_issues()
    expect_true(any(sapply(issues_neg_alloc, function(x) x$table_id == "manure" && x$severity == "error" && grepl("Negative", x$title))))

    # Restore clean allocation
    rv$manure$allocation[13] <- orig_alloc

    # 11. Selecting incompatible growth coefficient 'a' in cattle triggers a blocking error in def
    rv$def$a[1] <- "a_females" # Row 1 is cattle (feedlot_calves_male)
    issues_bad_a <- project_validation_issues()
    expect_true(any(sapply(issues_bad_a, function(x) x$table_id == "def" && x$severity == "error" && grepl("Growth Coefficient in Cattle", x$title))))
    rv$def$a[1] <- NA

    # 12. Selecting incompatible cfi (e.g. goats) in cattle triggers a blocking error in def
    orig_cfi <- rv$def$cfi[1]
    rv$def$cfi[1] <- "goats"
    issues_bad_cfi <- project_validation_issues()
    expect_true(any(sapply(issues_bad_cfi, function(x) x$table_id == "def" && x$severity == "error" && grepl("Incompatible Coefficient in Cattle", x$title))))
    rv$def$cfi[1] <- orig_cfi

    # 13. Selecting completely unknown tag triggers Unknown Coefficient Tag error
    rv$def$cfi[1] <- "invented_cfi"
    issues_unknown_cfi <- project_validation_issues()
    expect_true(any(sapply(issues_unknown_cfi, function(x) x$table_id == "def" && x$severity == "error" && grepl("Unknown Coefficient Tag", x$title))))
    rv$def$cfi[1] <- orig_cfi
  })
})

test_that("Shiny Block B regressions: dictionary units, 0-row save_clean, NA-safe validation, leading zeros, and cascades", {
  temp_test_dir <- tempfile()
  dir.create(temp_test_dir)
  file.copy(from = test_path("test_data/user_data"), to = temp_test_dir, recursive = TRUE)
  withr::local_dir(temp_test_dir)

  app_env <- new.env()
  sys.source(app_file, envir = app_env)

  # 1. Data dictionary accurately documents frac_fat_pct / frac_protein_pct as 0-1 fractions (not 0-100%)
  dict_html <- as.character(app_env$build_dictionary_panel())
  expect_match(dict_html, "frac_fat_pct")
  expect_match(dict_html, "decimal fraction \\(0")
  expect_false(grepl("body weight gain \\(percentage, 0", dict_html))
  expect_match(dict_html, "cattle_buffalo \\[lactating cows\\]")
  expect_match(dict_html, "pasture_range_paddock")
  expect_false(grepl("corn_national", dict_html, fixed = TRUE))

  # 2. save_clean() writes 0-row CSVs (overwriting stale data) and read_clean() preserves leading zeros
  tmp_csv <- file.path(temp_test_dir, "zero_row_test.csv")
  df_full <- data.frame(farm_id = "001", subregion = "08019", animal_tag = "cow_1", population = 10, stringsAsFactors = FALSE)
  app_env$save_clean(df_full, tmp_csv)
  read_back <- app_env$read_clean(tmp_csv)
  expect_identical(read_back$farm_id, "001")
  expect_identical(read_back$subregion, "08019")

  app_env$save_clean(df_full[0, , drop = FALSE], tmp_csv)
  read_zero <- app_env$read_clean(tmp_csv)
  expect_equal(nrow(read_zero), 0)
  expect_true(all(c("farm_id", "subregion", "animal_tag", "population") %in% names(read_zero)))

  # 3. Server-level tests: reset wipes disk CSVs, blank/non-numeric shares never crash aggregate, NA-safe subsetting
  app_server <- app_env$server
  shiny::testServer(app_server, {
    # 3a. All-NA / blank ingredient_share must NOT crash aggregate() and must report != 100%
    orig_ingr <- rv$diet_ingr
    rv$diet_ingr$ingredient_share <- NA_character_
    issues_na_ingr <- project_validation_issues()
    expect_true(any(sapply(issues_na_ingr, function(x) x$table_id == "diet_ingr" && grepl("100%", x$title))))

    # 3b. Non-numeric ingredient_share flags explicit error without crashing
    rv$diet_ingr$ingredient_share[1] <- "abc"
    issues_nan_ingr <- project_validation_issues()
    expect_true(any(sapply(issues_nan_ingr, function(x) x$table_id == "diet_ingr" && grepl("Non-numeric", x$title))))
    rv$diet_ingr <- orig_ingr

    # 3c. Non-numeric population must flag Non-numeric Population Count (NOT phantom NA rows / Negative Population)
    orig_census <- rv$census
    rv$census$population[1] <- "fifty"
    issues_nan_pop <- project_validation_issues()
    expect_true(any(sapply(issues_nan_pop, function(x) x$table_id == "census" && grepl("Non-numeric Population", x$title))))
    expect_false(any(sapply(issues_nan_pop, function(x) x$table_id == "census" && grepl("Negative Population", x$title))))
    rv$census <- orig_census

    # 3d. Non-numeric productive_period_days must report Missing Productive Period cleanly without NA labels
    orig_weights <- rv$weights
    active_idx <- which(rv$weights$animal_tag == "mature_dairy_cattle")[1]
    rv$weights$productive_period_days[active_idx] <- "invalid_days"
    issues_bad_ppd <- project_validation_issues()
    ppd_issues <- Filter(function(x) x$table_id == "weights" && grepl("Productive Period", x$title), issues_bad_ppd)
    expect_true(length(ppd_issues) > 0)
    expect_false(grepl("\\bNA\\b", ppd_issues[[1]]$message))
    rv$weights <- orig_weights

    # 3e. Opening manure modal and triggering cascade observers works without error
    session$setInputs(open_modal_manure = 1)
    session$setInputs(modal_input_manure_system_base = "liquid_slurry")
    expect_true(TRUE)

    # 3f. Confirming Clear All Data writes 0-row CSVs to user_data_tmp on disk
    session$setInputs(reset_data = 1, confirm_reset_data = 1)
    census_disk <- app_env$read_clean(file.path(user_data_tmp, "livestock_census.csv"))
    expect_equal(nrow(census_disk), 0)
  })
})

