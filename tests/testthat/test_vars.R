library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)

test_that("calculate_weighted_variable handles integrity errors and biological warnings", {
  # Set test directory in an isolated temporary location
  temp_test_dir <- tempfile()
  dir.create(temp_test_dir)
  file.copy(from = test_path("test_data/user_data"), to = temp_test_dir, recursive = TRUE)
  withr::local_dir(temp_test_dir)

  # Fix column types to avoid logical vs character errors
  read_csv("user_data/diet_profiles.csv", col_types = cols(.default = "c"), show_col_types = FALSE) %>%
    write_csv("user_data/diet_profiles.csv")
  read_csv("user_data/diet_ingredients.csv", col_types = cols(.default = "c"), show_col_types = FALSE) %>%
    write_csv("user_data/diet_ingredients.csv")
  read_csv("user_data/feed_characteristics.csv", col_types = cols(.default = "c"), show_col_types = FALSE) %>%
    write_csv("user_data/feed_characteristics.csv")
  read_csv("user_data/ruminant_definitions.csv", col_types = cols(.default = "c"), show_col_types = FALSE) %>%
    write_csv("user_data/ruminant_definitions.csv")

  # Load base data for manipulation
  diet_path <- "user_data/diet_profiles.csv"
  original_diets <- read_csv(diet_path, show_col_types = FALSE)

  # Block: Test Integrity Error (Shares < 100%)
  bad_integrity <- original_diets[1, ] %>%
    mutate(forage_share = 50, concentrate_share = 10, milk_share = 0, milk_replacer_share = 0)
  write_csv(bad_integrity, diet_path)

  expect_error(
    calculate_weighted_variable(saveoutput = FALSE),
    "Diet Profile Error"
  )

  # Block: Test Biological Warning (High concentrate)
  bad_biology <- original_diets[1, ] %>%
    mutate(forage_share = 10, concentrate_share = 90, milk_share = 0, milk_replacer_share = 0)
  write_csv(bad_biology, diet_path)

  expect_warning(
   expect_warning(
    calculate_weighted_variable(saveoutput = FALSE),
    "Warning \\(Forage\\)"
  ),
  "Warning \\(Protein\\)"
  )
})

test_that("calculate_weighted_variable works with normalized diets without region/subregion", {
  temp_test_dir <- tempfile()
  dir.create(temp_test_dir)
  file.copy(from = test_path("test_data/user_data"), to = temp_test_dir, recursive = TRUE)
  withr::local_dir(temp_test_dir)

  diet_path <- "user_data/diet_profiles.csv"
  ingr_path <- "user_data/diet_ingredients.csv"
  original_diets <- read_csv(diet_path, show_col_types = FALSE)
  original_ingrs <- read_csv(ingr_path, show_col_types = FALSE)

  clean_diets <- original_diets %>% dplyr::select(-dplyr::any_of(c("region", "subregion")))
  clean_ingrs <- original_ingrs %>% dplyr::select(-dplyr::any_of(c("region", "subregion")))

  write_csv(clean_diets, diet_path)
  write_csv(clean_ingrs, ingr_path)

  res <- suppressWarnings(calculate_weighted_variable(saveoutput = FALSE))
  expect_s3_class(res, "data.frame")
  expect_true("diet_tag" %in% names(res))
  expect_true("GE_feed_kcal_kg" %in% names(res))
  expect_true(nrow(res) > 0)
})

test_that("calculate_weighted_variable computes human-edible CP from mapping.csv human_edible", {
  temp_test_dir <- tempfile()
  dir.create(temp_test_dir)
  file.copy(from = test_path("test_data/user_data"), to = temp_test_dir, recursive = TRUE)
  withr::local_dir(temp_test_dir)

  diets <- read_csv("user_data/diet_profiles.csv", show_col_types = FALSE)
  target_diet <- diets$diet_tag[1]
  diets_one <- diets[1, ] %>%
    mutate(forage_share = 0, concentrate_share = 100, milk_share = 0, milk_replacer_share = 0)
  write_csv(diets_one, "user_data/diet_profiles.csv")

  # Mix of: alfalfa_meal (human_edible=0), bakery_byproduct (human_edible=0),
  # linseed_meal (human_edible=1), and skimmed_milk (land_type=none, human_edible=1)
  custom_ing <- tibble::tibble(
    diet_tag = target_diet,
    ingredient_type = rep("concentrate", 4),
    ingredient = c("alfalfa_meal_17_5_cp", "bakery_byproduct_2_5_ash", "linseed_meal", "skimmed_milk"),
    ingredient_share = c(25, 25, 25, 25),
    country_of_origin = rep("Spain", 4),
    custom_yield_kg_ha = rep(NA_real_, 4)
  )
  write_csv(custom_ing, "user_data/diet_ingredients.csv")

  fc <- read_csv("user_data/feed_characteristics.csv", show_col_types = FALSE) %>%
    dplyr::filter(ingredient %in% custom_ing$ingredient)
  cp_by_ing <- setNames(as.numeric(fc$CP_pct), fc$ingredient)

  res_he <- suppressWarnings(calculate_weighted_variable(saveoutput = FALSE))
  row_he <- res_he %>% dplyr::filter(diet_tag == target_diet) %>% dplyr::slice(1)

  # Only linseed_meal (he=1) and skimmed_milk (he=1, despite land_type=none) should count,
  # weighted by each ingredient's own CP_pct (not the scalar diet average)
  expected_he_cp <- 0.25 * cp_by_ing[["linseed_meal"]] + 0.25 * cp_by_ing[["skimmed_milk"]]
  expect_equal(row_he$CP_human_edible_pct, expected_he_cp, tolerance = 1e-6)
  expect_equal(row_he$CP_pct, mean(unname(cp_by_ing[custom_ing$ingredient])), tolerance = 1e-6)
  # Because linseed_meal (37.99% CP) has diaas_pct = NA (>50% of edible CP), coverage < 80% -> HDP_feed_pct is NA
  expect_true(is.na(row_he$HDP_feed_pct))
})

test_that("calculate_weighted_variable computes HDP_feed_pct accurately from diaas_pct and handles partial coverage", {
  temp_test_dir <- tempfile()
  dir.create(temp_test_dir)
  file.copy(from = test_path("test_data/user_data"), to = temp_test_dir, recursive = TRUE)
  withr::local_dir(temp_test_dir)

  diets <- read_csv("user_data/diet_profiles.csv", show_col_types = FALSE)
  target_diet <- diets$diet_tag[1]
  diets_one <- diets[1, ] %>%
    mutate(forage_share = 0, concentrate_share = 100, milk_share = 0, milk_replacer_share = 0)
  write_csv(diets_one, "user_data/diet_profiles.csv")

  # 1. Full DIAAS coverage: 50% corn (diaas=42.4) + 50% soybean_meal_44_cp (diaas=97.0)
  ing_full <- tibble::tibble(
    diet_tag = target_diet,
    ingredient_type = c("concentrate", "concentrate"),
    ingredient = c("corn", "soybean_meal_44_cp"),
    ingredient_share = c(50, 50),
    country_of_origin = c("Spain", "Brazil"),
    custom_yield_kg_ha = c(NA_real_, NA_real_)
  )
  write_csv(ing_full, "user_data/diet_ingredients.csv")

  fc <- read_csv("user_data/feed_characteristics.csv", show_col_types = FALSE) %>%
    dplyr::filter(ingredient %in% c("corn", "soybean_meal_44_cp", "tapioca_65"))
  cp_by_ing <- setNames(as.numeric(fc$CP_pct), fc$ingredient)

  res_full <- suppressWarnings(calculate_weighted_variable(saveoutput = FALSE))
  row_full <- res_full %>% dplyr::filter(diet_tag == target_diet) %>% dplyr::slice(1)

  exp_hdp <- 0.50 * cp_by_ing[["corn"]] * 0.424 + 0.50 * cp_by_ing[["soybean_meal_44_cp"]] * 0.970
  expect_equal(row_full$HDP_feed_pct, exp_hdp, tolerance = 1e-6)

  # 2. High coverage (>=80% but <100%): 45% corn + 45% soybean_meal_44_cp + 10% tapioca_65 (2.72% CP, diaas=NA)
  # tapioca_65 is ~1% of edible CP -> coverage ~99% -> scales weighted-average DIAAS cleanly (no NA, no >5% warning)
  ing_part <- tibble::tibble(
    diet_tag = target_diet,
    ingredient_type = rep("concentrate", 3),
    ingredient = c("corn", "soybean_meal_44_cp", "tapioca_65"),
    ingredient_share = c(45, 45, 10),
    country_of_origin = rep("Spain", 3),
    custom_yield_kg_ha = rep(NA_real_, 3)
  )
  write_csv(ing_part, "user_data/diet_ingredients.csv")

  res_part <- suppressWarnings(calculate_weighted_variable(saveoutput = FALSE))
  row_part <- res_part %>% dplyr::filter(diet_tag == target_diet) %>% dplyr::slice(1)

  cp_known <- 0.45 * cp_by_ing[["corn"]] + 0.45 * cp_by_ing[["soybean_meal_44_cp"]]
  hdp_known <- 0.45 * cp_by_ing[["corn"]] * 0.424 + 0.45 * cp_by_ing[["soybean_meal_44_cp"]] * 0.970
  cp_total_edible <- cp_known + 0.10 * cp_by_ing[["tapioca_65"]]
  expect_equal(row_part$HDP_feed_pct, (hdp_known / cp_known) * cp_total_edible, tolerance = 1e-6)

  # 3. Unused template diet with missing DIAAS must NOT trigger Warning (DIAAS),
  #    whereas an active cohort diet with missing DIAAS (>5% edible CP) MUST warn.
  unused_diet <- diets_one %>%
    mutate(diet_tag = "unused_template_diet_no_diaas")
  write_csv(dplyr::bind_rows(diets_one, unused_diet), "user_data/diet_profiles.csv")

  ing_unused_missing <- tibble::tibble(
    diet_tag = "unused_template_diet_no_diaas",
    ingredient_type = "concentrate",
    ingredient = "linseed_meal",
    ingredient_share = 100,
    country_of_origin = "Spain",
    custom_yield_kg_ha = NA_real_
  )
  write_csv(dplyr::bind_rows(ing_full, ing_unused_missing), "user_data/diet_ingredients.csv")

  caught_warnings <- character()
  withCallingHandlers(
    calculate_weighted_variable(saveoutput = FALSE),
    warning = function(w) {
      caught_warnings <<- c(caught_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_false(any(grepl("Warning \\(DIAAS\\)", caught_warnings)))

  # Now assign missing-DIAAS ingredient to the active diet -> Warning (DIAAS) must fire
  ing_active_missing <- tibble::tibble(
    diet_tag = target_diet,
    ingredient_type = c("concentrate", "concentrate"),
    ingredient = c("corn", "linseed_meal"),
    ingredient_share = c(50, 50),
    country_of_origin = c("Spain", "Spain"),
    custom_yield_kg_ha = c(NA_real_, NA_real_)
  )
  write_csv(dplyr::bind_rows(ing_active_missing, ing_unused_missing), "user_data/diet_ingredients.csv")

  caught_warnings_active <- character()
  withCallingHandlers(
    calculate_weighted_variable(saveoutput = FALSE),
    warning = function(w) {
      caught_warnings_active <<- c(caught_warnings_active, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  diaas_warns <- grep("Warning \\(DIAAS\\)", caught_warnings_active, value = TRUE)
  expect_length(diaas_warns, 1)
  expect_true(grepl(target_diet, diaas_warns, fixed = TRUE))
  expect_true(grepl("linseed_meal", diaas_warns, fixed = TRUE))
  expect_false(grepl("unused_template_diet_no_diaas", diaas_warns, fixed = TRUE))
})

test_that("calculate_weighted_variable preserves cohorts with optional NA region and feed_characteristics has valid GE", {
  temp_test_dir <- tempfile()
  dir.create(temp_test_dir)
  dir.create(file.path(temp_test_dir, "user_data"))
  dairy_files <- list.files(system.file("Examples", "Level1_Spain_Dairy_Cattle_2015", package = "herdr"), full.names = TRUE)
  if (length(dairy_files) == 0) {
    dairy_files <- list.files("inst/Examples/Level1_Spain_Dairy_Cattle_2015", full.names = TRUE)
  }
  file.copy(dairy_files, file.path(temp_test_dir, "user_data"))

  withr::with_dir(temp_test_dir, {
    fc <- read_csv("user_data/feed_characteristics.csv", show_col_types = FALSE)
    expect_true(all(fc$GE_feed_kcal_kg >= 0, na.rm = TRUE))
    expect_equal(fc$GE_feed_kcal_kg[fc$ingredient == "urea"], 2530)
    zero_check <- c(
      "calcium_carbonate", "dolomitic_carbonate", "sodium_bicarbonate",
      "sodium_carbonate", "iron_carbonate_ii_siderita", "cobalt_ii_carbonate",
      "coated_granulated_cobalt_ii_carbonate", "potassium_carbonate",
      "dicalcium_phosphate_dihydrated", "mg_oxide_calcined_magnesite",
      "ammonium_chloride", "Vitamin/mineral_supplement"
    )
    expect_true(all(fc$GE_feed_kcal_kg[fc$ingredient %in% zero_check] == 0))
    expect_equal(fc$GE_feed_kcal_kg[fc$ingredient == "dl_methionine"], 5670)
    expect_equal(fc$GE_feed_kcal_kg[fc$ingredient == "l_lysine_hcl"], 4810)
    expect_equal(fc$GE_feed_kcal_kg[fc$ingredient == "l_triptophane"], 6530)
    expect_equal(fc$GE_feed_kcal_kg[fc$ingredient == "formic_acid"], 1320)
    expect_equal(fc$GE_feed_kcal_kg[fc$ingredient == "butiric_acid"], 5980)
    expect_equal(fc$GE_feed_kcal_kg[fc$ingredient == "choline_chloride_75"], 0)
    expect_equal(fc$GE_feed_kcal_kg[fc$ingredient == "sodium_butyrate"], 0)
    expect_true("source_GE" %in% names(fc))
    expect_false(any(is.na(fc$source_GE)))
    expect_equal(fc$source_GE[fc$ingredient == "urea"], "INRAE-CIRAD-AFZ FeedTables")
    expect_equal(fc$source_GE[fc$ingredient == "formic_acid"], "NIST / FEDNA (combustion enthalpy)")
    expect_equal(fc$source_GE[fc$ingredient == "choline_chloride_75"], "Non-energetic additive (0)")
    expect_equal(fc$source_GE[fc$ingredient == "corn"], "Ewan (1989)")

    # Clear region across definitions -> cohort must NOT be dropped
    rum <- read_csv("user_data/ruminant_definitions.csv", col_types = cols(.default = "c"), show_col_types = FALSE) %>%
      mutate(region = NA_character_)
    write_csv(rum, "user_data/ruminant_definitions.csv")

    res <- suppressWarnings(calculate_weighted_variable(saveoutput = FALSE))
    expect_equal(nrow(res), 1)
    expect_equal(res$animal_tag, "mature_dairy_cattle")
    expect_true(is.na(res$region))
  })
})
