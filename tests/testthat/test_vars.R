library(testthat)
library(herdr)
library(readr)

# --- Helper: Setup environment relative to test directory ---
setup_nutritional_test_env <- function(type = "integrity") {
  # El test se ejecuta en tests/testthat, creamos user_data ahí
  target_dir <- "user_data"

  if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE)

  # Archivos CSV
  if (type == "integrity") {
    write_csv(data.frame(
      region="test", subregion="test", class_flex="grazing", diet_tag="bad_diet",
      forage_share=50, concentrate_share=10, milk_share=0, milk_replacer_share=0
    ), file.path(target_dir, "diet_profiles.csv"))
  } else {
    write_csv(data.frame(
      region="test", subregion="test", class_flex="grazing", diet_tag="bad_bio",
      forage_share=10, concentrate_share=90, milk_share=0, milk_replacer_share=0
    ), file.path(target_dir, "diet_profiles.csv"))
  }

  write_csv(data.frame(diet_tag=c("bad_diet", "bad_bio"), region="test", subregion="test",
                       class_flex="grazing", ingredient="grass", ingredient_type="forage",
                       ingredient_share=100), file.path(target_dir, "diet_ingredients.csv"))

  write_csv(data.frame(ingredient="grass", ingredient_type="forage", de=60, cp=5,
                       ndf=40, ash=15, ed=18),
            file.path(target_dir, "feed_characteristics.csv"))

  write_csv(data.frame(region="test", subregion="test", animal_tag="mature_cow",
                       class_flex="grazing", animal_type="Cattle", animal_subtype="dairy",
                       diet_tag=c("bad_diet", "bad_bio")),
            file.path(target_dir, "livestock_definitions.csv"))
}

# --- TESTS ---

test_that("Data integrity assertions catch non-100% shares (ERROR)", {
  setup_nutritional_test_env(type = "integrity")

  # Nos aseguramos de borrar todo al terminar este bloque
  on.exit(unlink("user_data", recursive = TRUE, force = TRUE))

  expect_error(
    calculate_weighted_variable(saveoutput = FALSE),
    "Diet Profile Error"
  )
})

test_that("Nutritional alerts trigger warnings for mature animals (WARNING)", {
  setup_nutritional_test_env(type = "biological")

  on.exit(unlink("user_data", recursive = TRUE, force = TRUE))

  expect_warning(
    calculate_weighted_variable(saveoutput = FALSE),
    "Warning \\(Forage\\)|Warning \\(Protein\\)"
  )
})
