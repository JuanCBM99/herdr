library(testthat)
library(herdr)
library(readr)
library(dplyr)

setup_omni_env_final <- function() {
  if (!dir.exists("user_data")) dir.create("user_data")

  # 1. Definitions (The Super-File with all required columns)
  write_csv(data.frame(
    region="test_reg", subregion="test_sub", animal_tag="cow_01", class_flex="grazing",
    animal_type="cattle", animal_subtype="dairy", diet_tag="standard",
    milk_yield=5000, fat_content=3.5, wool_yield=0,
    cp_excretion_factor=0.16, cfi="cow_c", ca="stall_a", work_hours=0,
    c="steer", a="none", b="none", c_pregnancy="none", pr=0
  ), "user_data/livestock_definitions.csv")

  # 2. Weights, Census, and Repro Parameters
  write_csv(data.frame(region="test_reg", subregion="test_sub", animal_tag="cow_01", class_flex="grazing",
                       average_weight=600, adult_weight=600, weight_gain=0.1), "user_data/livestock_weights.csv")
  write_csv(data.frame(region="test_reg", subregion="test_sub", animal_tag="cow_01", class_flex="grazing",
                       population=100, animal_type="cattle", animal_subtype="dairy"), "user_data/livestock_census.csv")
  write_csv(data.frame(animal_tag="cow_01", replacement_rate=0, calving_interval=0,
                       mortality_rate=0, first_calving_age=0), "user_data/reproduction_parameters.csv")

  # 3. Nutrition & Crop Yields (For GE, N_excreted, and Land Use)
  write_csv(data.frame(region="test_reg", subregion="test_sub", class_flex="grazing", diet_tag="standard",
                       forage_share=100, concentrate_share=0, milk_share=0, milk_replacer_share=0), "user_data/diet_profiles.csv")
  write_csv(data.frame(region="test_reg", subregion="test_sub", class_flex="grazing", diet_tag="standard",
                       ingredient_type="forage", ingredient="grass", ingredient_share=100), "user_data/diet_ingredients.csv")
  write_csv(data.frame(ingredient="grass", ingredient_type="forage", de=60, cp=12, ndf=40, ash=5, eb=18.4), "user_data/feed_characteristics.csv")
  write_csv(data.frame(ingredient="grass", dry_matter_yield=10000), "user_data/crop_yields.csv")

  # 4. Manure & IPCC Master Table (CH4 and N2O factors)
  write_csv(data.frame(region="test_reg", subregion="test_sub", animal_tag="cow_01", class_flex="grazing",
                       animal_type="cattle", animal_subtype="dairy", allocation=1,
                       system_base="pasture", management_months=12, system_climate="temperate",
                       system_subclimate="none", system_variant="none", climate_zone="none",
                       climate_moisture="none"), "user_data/manure_management.csv")

  write_csv(data.frame(system_base="pasture", management_months=12, system_climate="temperate",
                       system_subclimate="none", system_variant="none", climate_zone="none",
                       climate_moisture="none", animal_type="cattle", animal_subtype="dairy",
                       EF3=0.01, mcf=0.01, frac_leach=0.1, EF5=0.01, frac_gas=0.1, EF4=0.01), "user_data/ipcc_mm.csv")

  # 5. Coefficients (Crucial fix: including animal_type and animal_subtype for B0 select)
  write_csv(data.frame(
    coefficient = c("cfi", "ca", "ym", "c_pregnancy", "b_0"),
    description = c("cow_c", "stall_a", "cattle", "none", "dairy_b0"),
    value = c(0.335, 0.17, 6.5, 0, 0.24),
    animal_type = "cattle",
    animal_subtype = "dairy"
  ), "user_data/ipcc_coefficients.csv")
}

# --- TEST ---

test_that("generate_impact_assessment completes the full environmental profile", {
  setup_omni_env_final()

  # This call triggers the entire package pipeline
  summary_res <- generate_impact_assessment(saveoutput = FALSE)

  # Validation: Check for the key impact columns
  expect_s3_class(summary_res, "data.frame")
  expect_true(all(c("CO2eq_Total_Gg", "Land_m2", "Carbon_Footprint_m2") %in% colnames(summary_res)))

  # Logic check: Carbon footprint should be positive
  if(nrow(summary_res) > 0) {
    expect_gt(summary_res$Carbon_Footprint_m2[1], 0)
  }

  # Clean up
  if (dir.exists("user_data")) unlink("user_data", recursive = TRUE)
})
