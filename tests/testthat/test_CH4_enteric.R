library(testthat)
library(herdr)
library(readr)
library(dplyr)
library(withr)

test_that("calculate_emissions_enteric computes methane emissions using CSV data", {
  # 1. Directorio de test
  withr::local_dir(test_path("test_data"))

  # 2. Ejecutar función
  results <- suppressWarnings(calculate_emissions_enteric(saveoutput = FALSE))

  # 3. Verificaciones de Estructura
  expect_s3_class(results, "data.frame")
  expect_true(all(c("Ym_pct", "EF_kgheadyear", "total_CH4_enteric_Ggyear") %in% colnames(results)))

  # 4. Verificación de Ym (Lógica Biológica)
  if(nrow(results) > 0) {
    # No deben haber NAs (esto indica que el animal_type en el CSV se leyó bien)
    expect_false(any(is.na(results$Ym_pct)),
                 info = "Error: Ym_pct is NA. Check if animal_type in CSV is 'cattle', 'sheep' or 'goat'.")

    # Lista completa de valores que tu función puede devolver (IPCC Tier 2)
    # Incluimos: 6.7 (sheep), 5.5 (goat), 6.0 (dairy high NDF), 4.0 (beef high DE)
    expected_values <- c(3.0, 4.0, 5.5, 5.7, 6.0, 6.3, 6.5, 6.7, 7.0)

    expect_true(all(results$Ym_pct %in% expected_values),
                info = paste("Unexpected Ym values found:", paste(unique(results$Ym_pct), collapse=", ")))
  }

  # 5. Verificación de Cálculo de Emisión
  # EF = (GE * Ym/100 * 365) / 55.65
  if(nrow(results) > 0 && results$GE_MJday[1] > 0) {
    res1 <- results[1, ]
    calc_ef <- (res1$GE_MJday * (res1$Ym_pct / 100) * 365) / 55.65
    expect_equal(res1$EF_kgheadyear, calc_ef, tolerance = 0.01)
  }
})
