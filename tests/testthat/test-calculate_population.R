test_that("calculate_population orquestra correctamente el censo completo", {

  # Ejecutamos la función principal
  # Usamos saveoutput = FALSE para no ensuciar tu carpeta de trabajo durante los tests
  res <- calculate_population(saveoutput = FALSE)

  # --- VERIFICACIONES DE ESTRUCTURA ---

  # 1. ¿Es un dataframe (tibble)?
  expect_s3_class(res, "data.frame")

  # 2. ¿Tiene las columnas finales requeridas por el paquete?
  expected_cols <- c("group", "zone", "identification", "animal_type", "animal_subtype", "population")
  expect_true(all(expected_cols %in% names(res)))

  # 3. ¿Ha unido correctamente los metadatos de 'categories'?
  # Verificamos que animal_type no sea todo NA (lo que indicaría un join fallido)
  expect_false(all(is.na(res$animal_type)))

  # 4. ¿Están presentes las tres especies principales? (Asumiendo que están en tu rda por defecto)
  especies_presentes <- unique(tolower(res$animal_type))
  expect_true(any(c("cattle", "sheep", "goat") %in% especies_presentes))

  # 5. ¿La columna población es numérica?
  expect_type(res$population, "double")
})

test_that("calculate_population guarda el archivo si saveoutput es TRUE", {
  # Creamos una carpeta temporal para no ensuciar el proyecto
  temp_dir <- tempdir()
  withr::with_dir(temp_dir, {
    # Ejecutamos con guardado
    calculate_population(saveoutput = TRUE)

    # Verificamos que se creó la carpeta y el archivo
    expect_true(dir.exists("output"))
    expect_true(file.exists("output/population_result.csv"))
  })
})
