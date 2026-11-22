library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)

test_that("generate_impact_assessment creates two consistent final reports safely", {
  # 1. CREATE ISOLATED ENVIRONMENT
  temp_test_dir <- tempfile()
  dir.create(temp_test_dir)

  # Copy the test data folder to the temporary environment
  file.copy(from = test_path("test_data/user_data"), to = temp_test_dir, recursive = TRUE)

  # Tell R to work INSIDE the temporary folder
  withr::local_dir(temp_test_dir)

  # 2. ANTI-DOWNLOAD SHIELD
  path_diet <- "user_data/diet_ingredients.csv"
  if (file.exists(path_diet)) {
    df <- read_csv(path_diet, col_types = cols(.default = "c"), show_col_types = FALSE)

    if (!"country_of_origin" %in% names(df)) {
      df <- df %>% mutate(country_of_origin = "Spain")
    } else {
      df <- df %>% mutate(country_of_origin = ifelse(is.na(country_of_origin), "Spain", country_of_origin))
    }

    # Overwrite only the ghost copy
    write_csv(df, path_diet)
  }

  # 3. EXECUTE THE MAIN FUNCTION
  results <- suppressWarnings(
    generate_impact_assessment(
      farm_country = "Spain",
      year = 2022,
      saveoutput = TRUE,
      group_by_identification = TRUE
    )
  )

  # 4. STRUCTURE VALIDATIONS
  expect_type(results, "list")
  expect_true(all(c("totals", "intensities") %in% names(results)))
  expect_s3_class(results$totals, "data.frame")
  expect_s3_class(results$intensities, "data.frame")

  # Verify files on disk
  expect_true(file.exists("output/total_emissions_and_land_use.csv"))
  expect_true(file.exists("output/product_intensities.csv"))

  # Verify inventory columns in Table 1
  expected_cols_totals <- c(
    "farm_id", "region", "subregion", "animal_tag", "animal_type", "animal_subtype", "production_role",
    "population", "DMI_kgday", "feed_intake_kg", "feed_CP_total_kg", "feed_CP_cropland_kg",
    "CH4_enteric_Gg", "CH4_manure_Gg", "N2O_direct_Gg", "N2O_vol_Gg", "N2O_lea_Gg", "N2O_indirect_Gg",
    "CO2eq_enteric", "CO2eq_manure", "CO2eq_N2O_direct", "CO2eq_N2O_indirect", "CO2eq_N2O",
    "CO2eq_feed_fossil_Gg", "CO2eq_feed_luc_Gg", "CO2eq_feed_Gg",
    "CO2eq_Total_Gg", "CO2eq_cradle_to_farm_Gg",
    "Land_cropland_m2", "Land_grassland_convertible_m2", "Land_grassland_unconvertible_m2", "Land_other_m2", "Land_m2",
    "Water_feed_m3",
    "CO2eq_Total_head", "CO2eq_cradle_to_farm_head", "Land_head", "Water_head"
  )
  for (col in expected_cols_totals) {
    expect_true(col %in% colnames(results$totals), info = paste("Missing totals col:", col))
  }

  # Verify product intensity columns in Table 2
  expected_cols_intensities <- c(
    "farm_id", "region", "subregion", "animal_tag", "animal_type", "animal_subtype", "production_role",
    "population",
    "milk_FPCM_kg", "meat_carcass_weight_kg", "egg_fresh_kg", "wool_kg", "total_protein_kg",
    "GHG_intensity_milk", "GHG_cradle_to_farm_milk", "Land_intensity_milk", "Water_intensity_milk",
    "GHG_intensity_meat", "GHG_cradle_to_farm_meat", "Land_intensity_meat", "Water_intensity_meat",
    "GHG_intensity_egg", "GHG_cradle_to_farm_egg", "Land_intensity_egg", "Water_intensity_egg",
    "GHG_intensity_wool", "GHG_cradle_to_farm_wool", "Land_intensity_wool", "Water_intensity_wool",
    "GHG_intensity_protein", "GHG_cradle_to_farm_protein", "Land_intensity_protein", "Water_intensity_protein",
    "Protein_FCR_total", "Protein_FCR_cropland"
  )
  for (col in expected_cols_intensities) {
    expect_true(col %in% colnames(results$intensities), info = paste("Missing intensities col:", col))
  }

  # 5. LOGICAL AND MATHEMATICAL VALIDATION
  totals <- results$totals
  intensities <- results$intensities

  if (nrow(totals) > 0) {
    # Emissions and land use cannot be negative
    expect_true(all(totals$CO2eq_Total_Gg >= 0))
    expect_true(all(totals$Land_m2 >= 0))
    expect_true(all(totals$Land_cropland_m2 >= 0))
    expect_true(all(totals$Land_grassland_convertible_m2 >= 0))
    expect_true(all(totals$Land_grassland_unconvertible_m2 >= 0))
    expect_true(all(totals$Land_other_m2 >= 0))
    expect_equal(totals$Land_m2, totals$Land_cropland_m2 + totals$Land_grassland_convertible_m2 + totals$Land_grassland_unconvertible_m2 + totals$Land_other_m2, tolerance = 1e-4)
    expect_true(all(totals$feed_intake_kg >= 0))
    expect_true(all(totals$DMI_kgday >= 0))

    # Check IPCC CO2eq math on first row
    sample_row <- totals[1, ]
    calculated_co2 <- (sample_row$CH4_enteric_Gg * 28) +
      (sample_row$CH4_manure_Gg * 28) +
      ((sample_row$N2O_direct_Gg + sample_row$N2O_vol_Gg + sample_row$N2O_lea_Gg) * 265)

    expect_equal(sample_row$CO2eq_Total_Gg, calculated_co2, tolerance = 0.01)
  }

  if (nrow(intensities) > 0) {
    mature_rows <- intensities[intensities$production_role == "mature" & intensities$population > 0, ]
    if (nrow(mature_rows) > 0) {
      farm_sample <- mature_rows[1, ]
      if (!is.na(farm_sample$Protein_FCR_total)) {
        expect_true(farm_sample$Protein_FCR_total > 0)
      }
      if (!is.na(farm_sample$Protein_FCR_cropland)) {
        expect_true(farm_sample$Protein_FCR_cropland >= 0)
      }
      if (!is.na(farm_sample$GHG_intensity_milk)) {
        expect_true(farm_sample$GHG_intensity_milk > 0)
      }
    }
  }

  # 6. FILTERING AND AGGREGATION OPTIONS
  filtered_res <- suppressWarnings(
    generate_impact_assessment(
      farm_country = "Spain",
      year = 2022,
      animal = "cattle",
      group_by_identification = FALSE,
      saveoutput = TRUE
    )
  )
  expect_type(filtered_res, "list")
  expect_true(all(filtered_res$totals$animal_type == "cattle"))
  expect_true(file.exists("output/total_emissions_and_land_use.csv"))
  expect_true(file.exists("output/product_intensities.csv"))

  # 7. GWP REPORT SELECTION (AR6, AR5, AR4, SAR, CUSTOM)
  res_ar6 <- suppressWarnings(
    generate_impact_assessment(
      farm_country = "Spain",
      year = 2022,
      saveoutput = FALSE,
      ar = "AR6"
    )
  )
  expect_equal(attr(res_ar6, "gwp_report"), "AR6")
  row_ar6 <- res_ar6$totals[1, ]
  exp_ar6 <- (row_ar6$CH4_enteric_Gg * 27) +
    (row_ar6$CH4_manure_Gg * 27) +
    ((row_ar6$N2O_direct_Gg + row_ar6$N2O_vol_Gg + row_ar6$N2O_lea_Gg) * 273)
  expect_equal(row_ar6$CO2eq_Total_Gg, exp_ar6, tolerance = 0.01)

  # Custom GWP vector
  res_custom <- suppressWarnings(
    generate_impact_assessment(
      farm_country = "Spain",
      year = 2022,
      saveoutput = FALSE,
      gwp_report = c(CH4 = 30, N2O = 250)
    )
  )
  row_cust <- res_custom$totals[1, ]
  exp_cust <- (row_cust$CH4_enteric_Gg * 30) +
    (row_cust$CH4_manure_Gg * 30) +
    ((row_cust$N2O_direct_Gg + row_cust$N2O_vol_Gg + row_cust$N2O_lea_Gg) * 250)
  expect_equal(row_cust$CO2eq_Total_Gg, exp_cust, tolerance = 0.01)

  # Invalid GWP should stop with informative error
  expect_error(
    generate_impact_assessment(farm_country = "Spain", year = 2022, gwp_report = "INVALID"),
    "Invalid GWP report"
  )
})

test_that("generate_impact_assessment preserves and filters by farm_id cleanly", {
  temp_test_dir <- tempfile()
  dir.create(temp_test_dir)
  dir.create(file.path(temp_test_dir, "user_data"))

  # Copy Level2 (farm_north & farm_south) into temp environment
  level2_files <- list.files(system.file("Examples", "Level2_Spain_Dairy_Cattle_2015", package = "herdr"), full.names = TRUE)
  if (length(level2_files) == 0) {
    level2_files <- list.files("inst/Examples/Level2_Spain_Dairy_Cattle_2015", full.names = TRUE)
  }
  file.copy(level2_files, file.path(temp_test_dir, "user_data"))

  withr::with_dir(temp_test_dir, {
    # 1. Full run across all farms
    all_farms <- suppressWarnings(
      generate_impact_assessment(farm_country = "Spain", year = 2022, saveoutput = FALSE)
    )
    expect_type(all_farms, "list")
    expect_true("farm_id" %in% names(all_farms$totals))
    expect_true("farm_id" %in% names(all_farms$intensities))
    expect_setequal(unique(all_farms$totals$farm_id), c("farm_north", "farm_south"))
    expect_setequal(unique(all_farms$intensities$farm_id), c("farm_north", "farm_south"))

    # 2. Filter by farm_north
    north_only <- suppressWarnings(
      generate_impact_assessment(farm_id = "farm_north", farm_country = "Spain", year = 2022, saveoutput = FALSE)
    )
    expect_equal(unique(north_only$totals$farm_id), "farm_north")
    expect_equal(unique(north_only$intensities$farm_id), "farm_north")

    # 3. Filter by farm_south
    south_only <- suppressWarnings(
      generate_impact_assessment(farm_id = "farm_south", farm_country = "Spain", year = 2022, saveoutput = FALSE)
    )
    expect_equal(unique(south_only$totals$farm_id), "farm_south")
    expect_equal(unique(south_only$intensities$farm_id), "farm_south")

    # 4. Aggregated enterprise level (group_by_identification = FALSE) preserves separate farm rows
    enterprise_summary <- suppressWarnings(
      generate_impact_assessment(
        farm_country = "Spain", year = 2022, saveoutput = FALSE, group_by_identification = FALSE
      )
    )
    expect_true("farm_id" %in% names(enterprise_summary$totals))
    expect_setequal(unique(enterprise_summary$totals$farm_id), c("farm_north", "farm_south"))
    expect_equal(nrow(enterprise_summary$intensities), 2)
  })
})

test_that("Table 2 integrates breeding + replacement + slaughter in meat systems while keeping dairy/layer rules", {
  temp_test_dir <- tempfile()
  dir.create(temp_test_dir)
  dir.create(file.path(temp_test_dir, "user_data"))

  mono_files <- list.files(system.file("Examples", "Monogastric_Example", package = "herdr"), full.names = TRUE)
  if (length(mono_files) == 0) {
    mono_files <- list.files("inst/Examples/Monogastric_Example", full.names = TRUE)
  }
  file.copy(mono_files, file.path(temp_test_dir, "user_data"))

  withr::with_dir(temp_test_dir, {
    path_diet <- "user_data/diet_ingredients.csv"
    if (file.exists(path_diet)) {
      df <- read_csv(path_diet, col_types = cols(.default = "c"), show_col_types = FALSE)
      if (!"country_of_origin" %in% names(df)) {
        df <- df %>% mutate(country_of_origin = "Spain")
      } else {
        df <- df %>% mutate(country_of_origin = ifelse(is.na(country_of_origin), "Spain", country_of_origin))
      }
      write_csv(df, path_diet)
    }

    res <- suppressWarnings(
      generate_impact_assessment(farm_country = "Spain", year = 2022, saveoutput = FALSE)
    )

    swine_tot <- res$totals %>% filter(animal_type == "swine")
    swine_int <- res$intensities %>% filter(animal_type == "swine")

    # Swine meat enterprise integrates breeding + replacement + slaughter with biophysical allocation
    tot_swine_s1 <- sum(swine_tot$CO2eq_Total_Gg) * 1e6
    fatten_row <- swine_int %>% filter(production_role == "slaughter")
    sows_row <- swine_int %>% filter(animal_tag == "breeder_sows")

    # Both slaughter and cull meat intensities are positive and close mass balance
    expect_true(fatten_row$GHG_intensity_meat > 0)
    expect_true(sows_row$GHG_intensity_meat > 0)

    alloc_fatten_s1 <- fatten_row$GHG_intensity_meat * fatten_row$meat_carcass_weight_kg
    alloc_sows_s1   <- sows_row$GHG_intensity_meat * sows_row$meat_carcass_weight_kg
    expect_equal(alloc_fatten_s1 + alloc_sows_s1, tot_swine_s1, tolerance = 1e-2 * tot_swine_s1)

    # Enterprise-level Protein FCR: all protein-producing cohorts share whole-herd FCR (including replacement)
    expect_equal(fatten_row$Protein_FCR_total, sows_row$Protein_FCR_total)
    expect_equal(fatten_row$Protein_FCR_cropland, sows_row$Protein_FCR_cropland)
    exp_swine_fcr_tot <- round(sum(swine_tot$feed_CP_total_kg) / sum(swine_int$total_protein_kg), 2)
    exp_swine_fcr_crop <- round(sum(swine_tot$feed_CP_cropland_kg) / sum(swine_int$total_protein_kg), 2)
    expect_equal(fatten_row$Protein_FCR_total, exp_swine_fcr_tot)
    expect_equal(fatten_row$Protein_FCR_cropland, exp_swine_fcr_crop)
  })
})

test_that("Table 2 integrates mature dairy + replacement + slaughter calves into a single enterprise pool", {
  temp_test_dir <- tempfile()
  dir.create(temp_test_dir)
  dir.create(file.path(temp_test_dir, "user_data"))

  dairy_files <- list.files(system.file("Examples", "Level1_Spain_Dairy_Cattle_2015", package = "herdr"), full.names = TRUE)
  if (length(dairy_files) == 0) {
    dairy_files <- list.files("inst/Examples/Level1_Spain_Dairy_Cattle_2015", full.names = TRUE)
  }
  file.copy(dairy_files, file.path(temp_test_dir, "user_data"))

  withr::with_dir(temp_test_dir, {
    # Add a slaughter calf cohort and a replacement heifer cohort to the dairy farm
    census <- read_csv("user_data/livestock_census.csv", col_types = cols(.default = "c"), show_col_types = FALSE)
    calves_census <- census %>%
      mutate(
        animal_tag = "dairy_calves_fattening",
        population = "50"
      )
    heifers_census <- census %>%
      mutate(
        animal_tag = "dairy_heifers",
        population = "30"
      )
    write_csv(bind_rows(census, heifers_census, calves_census), "user_data/livestock_census.csv")

    defs <- read_csv("user_data/ruminant_definitions.csv", col_types = cols(.default = "c"), show_col_types = FALSE)
    calves_def <- defs %>%
      mutate(
        animal_tag = "dairy_calves_fattening",
        production_role = "slaughter",
        milk_yield_kg_year = "0"
      )
    heifers_def <- defs %>%
      mutate(
        animal_tag = "dairy_heifers",
        production_role = "replacement",
        milk_yield_kg_year = "0"
      )
    write_csv(bind_rows(defs, heifers_def, calves_def), "user_data/ruminant_definitions.csv")

    manure <- read_csv("user_data/manure_management.csv", col_types = cols(.default = "c"), show_col_types = FALSE)
    calves_manure <- manure %>% mutate(animal_tag = "dairy_calves_fattening")
    heifers_manure <- manure %>% mutate(animal_tag = "dairy_heifers")
    write_csv(bind_rows(manure, heifers_manure, calves_manure), "user_data/manure_management.csv")

    weights <- read_csv("user_data/livestock_weights.csv", col_types = cols(.default = "c"), show_col_types = FALSE)
    calves_weights <- weights %>%
      mutate(
        animal_tag = "dairy_calves_fattening",
        initial_weight_kg = "50",
        final_weight_kg = "450",
        adult_weight_kg = "450",
        productive_period_days = "365"
      )
    heifers_weights <- weights %>%
      mutate(
        animal_tag = "dairy_heifers",
        initial_weight_kg = "200",
        final_weight_kg = "550",
        adult_weight_kg = "550",
        productive_period_days = "365"
      )
    write_csv(bind_rows(weights, heifers_weights, calves_weights), "user_data/livestock_weights.csv")

    res <- suppressWarnings(
      generate_impact_assessment(farm_country = "Spain", year = 2022, saveoutput = FALSE)
    )

    tot <- res$totals %>% filter(farm_id == "farm_1", animal_type == "cattle", animal_subtype == "dairy")
    inten <- res$intensities %>% filter(farm_id == "farm_1", animal_type == "cattle", animal_subtype == "dairy")

    expect_equal(nrow(inten), 3)

    # All three cohorts exist
    mat_row <- inten %>% filter(production_role == "mature")
    repl_row <- inten %>% filter(production_role == "replacement")
    slau_row <- inten %>% filter(production_role == "slaughter")

    # Replacement has NA commercial intensities
    expect_true(is.na(repl_row$GHG_intensity_milk))
    expect_true(is.na(repl_row$GHG_intensity_meat))

    # Mature has milk and cull meat intensities
    expect_true(!is.na(mat_row$GHG_intensity_milk) && mat_row$GHG_intensity_milk > 0)
    expect_true(!is.na(mat_row$GHG_intensity_meat) && mat_row$GHG_intensity_meat > 0)

    # Slaughter has young meat intensity
    expect_true(!is.na(slau_row$GHG_intensity_meat) && slau_row$GHG_intensity_meat > 0)
    expect_true(is.na(slau_row$GHG_intensity_milk))

    # Verify closed mass balance: total allocated Scope 1 emissions equals total enterprise Scope 1 emissions
    tot_s1 <- sum(tot$CO2eq_Total_Gg) * 1e6
    alloc_milk_s1 <- mat_row$GHG_intensity_milk * mat_row$milk_FPCM_kg
    alloc_cull_s1 <- mat_row$GHG_intensity_meat * mat_row$meat_carcass_weight_kg
    alloc_slau_s1 <- slau_row$GHG_intensity_meat * slau_row$meat_carcass_weight_kg

    sum_alloc <- alloc_milk_s1 + alloc_cull_s1 + alloc_slau_s1
    expect_equal(sum_alloc, tot_s1, tolerance = 1e-2 * tot_s1)

    # Verify unified enterprise-level Protein FCR includes mature + replacement + slaughter
    expect_true(is.na(repl_row$Protein_FCR_total))
    expect_equal(mat_row$Protein_FCR_total, slau_row$Protein_FCR_total)
    expect_equal(mat_row$Protein_FCR_cropland, slau_row$Protein_FCR_cropland)
    exp_dairy_fcr_tot <- round(sum(tot$feed_CP_total_kg) / sum(inten$total_protein_kg), 2)
    exp_dairy_fcr_crop <- round(sum(tot$feed_CP_cropland_kg) / sum(inten$total_protein_kg), 2)
    expect_equal(mat_row$Protein_FCR_total, exp_dairy_fcr_tot)
    expect_equal(mat_row$Protein_FCR_cropland, exp_dairy_fcr_crop)
  })
})
