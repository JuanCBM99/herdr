library(testthat)
library(herdr)
library(readr)

setup_dmi_high_intake_env <- function() {
  if (!dir.exists("user_data")) dir.create("user_data")

  # 1. High production (60kg milk) and low weight (400kg) to force high DMI%
  write_csv(data.frame(
    region="test", subregion="test", animal_tag="greedy_cow",
    class_flex="none", animal_type="Cattle", animal_subtype="dairy",
    diet_tag="diet1", milk_yield=60, fat_content=3.5,
    cp_excretion_factor=0.16, cfi="c1", ca="a1", work_hours=0,
    c="steer", a="none", b="none", c_pregnancy="none", pr=0, wool_yield=0
  ), "user_data/livestock_definitions.csv")

  write_csv(data.frame(
    region="test", subregion="test", animal_tag="greedy_cow",
    class_flex="none", average_weight=400, adult_weight=400, weight_gain=0
  ), "user_data/livestock_weights.csv")

  # 2. Extremely low ed (Energy density) forces higher mass intake
  write_csv(data.frame(region="test", subregion="test", class_flex="none", diet_tag="diet1",
                       forage_share=100, concentrate_share=0, milk_share=0, milk_replacer_share=0), "user_data/diet_profiles.csv")
  write_csv(data.frame(region="test", subregion="test", class_flex="none", diet_tag="diet1",
                       ingredient_type="forage", ingredient="straw", ingredient_share=100), "user_data/diet_ingredients.csv")
  write_csv(data.frame(ingredient="straw", ingredient_type="forage", de=30, cp=4, ndf=80, ash=10, ed=5, fat=1), "user_data/feed_characteristics.csv")

  # 3. Required boilerplate
  write_csv(data.frame(region="test", subregion="test", animal_tag="greedy_cow",
                       class_flex="none", population=1, animal_type="Cattle", animal_subtype="dairy"), "user_data/livestock_census.csv")
  write_csv(data.frame(animal_tag="greedy_cow", replacement_rate=0, calving_interval=0,
                       mortality_rate=0, first_calving_age=0), "user_data/reproduction_parameters.csv")
  write_csv(data.frame(coefficient="cfi", description=c("c1", "steer", "none"), value=c(0.335, 1, 0)), "user_data/ipcc_coefficients.csv")
}

test_that("calculate_dmi correctly identifies high intake physiological limits", {
  setup_dmi_high_intake_env()

  # This setup forces DMI well above 4.4% for cattle
  expect_warning(
    results <- calculate_dmi(saveoutput = FALSE),
    "Intake Warning"
  )

  expect_s3_class(results, "data.frame")
  expect_gt(results$dmi_bw_pct[1], 4.4)

  unlink("user_data", recursive = TRUE)
})
