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
})
