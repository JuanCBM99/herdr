library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)
library(ggplot2)

test_that("plot_herdr_results generates dynamic plots from calculated pipeline outputs", {
  # 1. Set test directory
  withr::local_dir(test_path("test_data"))

  # 2. Normalize and prepare input CSVs
  files_to_fix <- c(
    "ruminant_definitions.csv", "livestock_weights.csv",
    "livestock_census.csv", "manure_management.csv",
    "ipcc_mm.csv", "diet_profiles.csv",
    "diet_ingredients.csv", "feed_characteristics.csv",
    "ipcc_coefficients.csv"
  )

  for (f in files_to_fix) {
    path <- file.path("user_data", f)
    if (file.exists(path)) {
      read_csv(path, col_types = cols(.default = "c"), show_col_types = FALSE) %>%
        mutate(across(everything(), trimws)) %>%
        write_csv(path)
    }
  }

  # 3. Execute pipeline functions to get real data structures
  ge_results <- suppressWarnings(calculate_ge(saveoutput = FALSE))
  dmi_results <- suppressWarnings(calculate_DMI(saveoutput = FALSE))

  # 4. Assertions: Edge cases
  expect_null(plot_herdr_results(data.frame()))
  expect_null(plot_herdr_results(ge_results, group_cols = c("non_existent_group")))

  # 5. Assertions: Universal plot with dynamic dictionary
  p_ge <- plot_herdr_results(ge_results, group_cols = c("animal_tag", "class_flex"), func_name = "calculate_ge")
  expect_s3_class(p_ge, "ggplot")
  expect_match(p_ge$labels$title, "Results for GE MJday")
  expect_true("plot_label" %in% names(p_ge$data))

  p_dmi <- plot_herdr_results(dmi_results, group_cols = c("animal_tag"), func_name = "calculate_DMI")
  expect_s3_class(p_dmi, "ggplot")
  expect_match(p_dmi$labels$title, "Results for DMI kgday")

  # 6. Assertions: Emissions breakdown special case
  emissions_df <- data.frame(
    animal_tag = c("cow_dairy", "sheep_meat"),
    region = c("Europe", "Europe"),
    subregion = c("Spain", "Spain"),
    class_flex = c("dairy", "meat"),
    ch4_enteric = c(120.5, 15.2),
    ch4_manure = c(30.1, 3.0),
    n2o_manure = c(10.4, 1.1)
  )
  p_emissions <- plot_herdr_results(emissions_df)
  expect_s3_class(p_emissions, "ggplot")
  expect_equal(p_emissions$labels$title, "Greenhouse Gas Emissions")
  expect_true("Gg_CO2e" %in% names(p_emissions$data))

  # 7. Assertions: Aggregation logic (sum for totals, mean for rates)
  rep_df <- data.frame(
    animal_tag = c("tag_1", "tag_1"),
    region = c("Europe", "Europe"),
    subregion = c("Spain", "Spain"),
    class_flex = c("dairy", "dairy"),
    GE_MJday = c(100, 200),
    DE_pct = c(60, 80)
  )
  p_agg <- plot_herdr_results(rep_df, group_cols = "animal_tag", func_name = "calculate_ge")
  expect_s3_class(p_agg, "ggplot")
  expect_equal(nrow(p_agg$data), 1)
  expect_equal(p_agg$data$GE_MJday, 300)
  expect_equal(p_agg$data$DE_pct, 70)

  # 8. Assertions: Diet composition profile plot
  diet_df <- data.frame(
    animal_tag = c("cow_1", "cow_2"),
    region = c("spain", "spain"),
    DE_pct = c(70, 75),
    CP_pct = c(16, 18),
    NDF_pct = c(35, 32)
  )
  p_diet <- plot_herdr_results(diet_df, group_cols = "animal_tag")
  expect_s3_class(p_diet, "ggplot")
  expect_equal(p_diet$labels$title, "Diet Composition Profiles")

  # 9. Assertions: Land use breakdown plot
  land_df <- data.frame(
    animal_tag = c("pig_1", "pig_1"),
    region = c("spain", "spain"),
    land_type = c("cropland", "grassland_convertible"),
    total_land_use_m2 = c(50000, 20000)
  )
  p_land <- plot_herdr_results(land_df, group_cols = "animal_tag")
  expect_s3_class(p_land, "ggplot")
  expect_match(p_land$labels$title, "Land Use")

  # 10. Assertions: Edible protein production plot and calculate_production registration
  protein_df <- data.frame(
    animal_tag = c("dairy_cow", "broiler_chicken"),
    region = c("spain", "spain"),
    subregion = c("north", "south"),
    class_flex = c("dairy", "meat"),
    milk_protein_kg = c(2500, 0),
    meat_protein_kg = c(300, 800),
    egg_protein_kg = c(0, 0),
    total_protein_kg = c(2800, 800)
  )
  p_protein <- plot_herdr_results(protein_df, group_cols = "animal_tag", func_name = "calculate_production")
  expect_s3_class(p_protein, "ggplot")
  expect_equal(p_protein$labels$title, "Edible Protein Production")
  expect_true("Protein_kg" %in% names(p_protein$data))

  # 11. Assertions: Environmental Impact Assessment plot (stacked emissions + land footprint)
  impact_df <- data.frame(
    animal_tag = c("dairy_cows", "fattening_pigs"),
    region = c("spain", "spain"),
    subregion = c("north", "south"),
    class_flex = c("dairy", "meat"),
    CH4_enteric_Gg = c(2.5, 0.05),
    CH4_manure_Gg = c(0.6, 0.4),
    N2O_direct_Gg = c(0.03, 0.02),
    N2O_vol_Gg = c(0.01, 0.005),
    N2O_lea_Gg = c(0.005, 0.002),
    Land_m2 = c(5000000, 1500000)
  )
  p_impact <- plot_herdr_results(impact_df, group_cols = "animal_tag", func_name = "generate_impact_assessment")
  expect_s3_class(p_impact, "ggplot")
  expect_equal(p_impact$labels$title, "Environmental Impact Assessment")
  expect_true("Land Footprint (ha)" %in% levels(p_impact$data$panel))
  expect_true("GHG Emissions (Gg CO2e)" %in% levels(p_impact$data$panel))
  expect_true("CH4 Enteric" %in% levels(p_impact$data$component))
  expect_true("Land Use" %in% levels(p_impact$data$component))

  p_impact_ar6 <- plot_herdr_results(impact_df, group_cols = "animal_tag", func_name = "generate_impact_assessment", ar = "AR6")
  expect_s3_class(p_impact_ar6, "ggplot")

  # 11b. Assertions: Environmental Impact Assessment with Land Use breakdown
  impact_breakdown_df <- impact_df %>%
    dplyr::mutate(
      Land_cropland_m2                = c(3000000, 1000000),
      Land_grassland_convertible_m2   = c(1500000, 400000),
      Land_grassland_unconvertible_m2 = c(500000, 100000)
    )
  p_impact_breakdown <- plot_herdr_results(impact_breakdown_df, group_cols = "animal_tag", func_name = "generate_impact_assessment")
  expect_s3_class(p_impact_breakdown, "ggplot")
  expect_true("Cropland" %in% levels(p_impact_breakdown$data$component))
  expect_true("Grassland (Conv.)" %in% levels(p_impact_breakdown$data$component))
  expect_true("Grassland (Unconv.)" %in% levels(p_impact_breakdown$data$component))

  # 12. Assertions: Functional unit plots (protein, product, head)
  impact_fu_df <- impact_df %>%
    dplyr::mutate(
      population = c(1000, 5000),
      milk_FPCM_kg = c(8000000, 0),
      meat_carcass_weight_kg = c(200000, 450000),
      egg_fresh_kg = c(0, 0),
      total_protein_kg = c(260000, 80000)
    )

  p_fu_prot <- plot_herdr_results(impact_fu_df, group_cols = "animal_tag", func_name = "generate_impact_assessment", functional_unit = "protein")
  expect_s3_class(p_fu_prot, "ggplot")
  expect_true("GHG Intensity (kg CO2e / kg protein)" %in% levels(p_fu_prot$data$panel))
  expect_true("Land Footprint (m2 / kg protein)" %in% levels(p_fu_prot$data$panel))

  p_fu_head <- plot_herdr_results(impact_fu_df, group_cols = "animal_tag", func_name = "generate_impact_assessment", functional_unit = "head")
  expect_s3_class(p_fu_head, "ggplot")
  expect_true("GHG per Head (kg CO2e / head)" %in% levels(p_fu_head$data$panel))
  expect_true("Land per Head (m2 / head)" %in% levels(p_fu_head$data$panel))

  p_fu_prod <- plot_herdr_results(impact_fu_df, group_cols = "animal_tag", func_name = "generate_impact_assessment", functional_unit = "product")
  expect_s3_class(p_fu_prod, "ggplot")
  expect_true("GHG Intensity (kg CO2e / kg product)" %in% levels(p_fu_prod$data$panel))
  expect_true("Land Footprint (m2 / kg product)" %in% levels(p_fu_prod$data$panel))

  # 13. Assertions: Protein Feed Efficiency (Mottet FCR)
  impact_fcr_df <- impact_fu_df %>%
    dplyr::mutate(
      feed_CP_total_kg    = c(910000, 320000),
      feed_CP_cropland_kg = c(650000, 80000)
    )
  p_fu_fcr <- plot_herdr_results(impact_fcr_df, group_cols = "animal_tag", func_name = "generate_impact_assessment", functional_unit = "fcr")
  expect_s3_class(p_fu_fcr, "ggplot")
  expect_match(p_fu_fcr$labels$title, "Protein Feed Efficiency")
  expect_true("Cropland Protein FCR (Human Competition)" %in% levels(p_fu_fcr$data$metric))
})

