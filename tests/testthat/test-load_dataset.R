library(testthat)
library(herdr)

test_that("load_dataset maneja correctamente la prioridad y los NA", {

  # 1. Probar carga de datos internos (suponiendo que 'categories' existe en el paquete)
  # Usamos expect_no_error para asegurar que la ruta por defecto funciona
  expect_no_error(df_internal <- load_dataset("categories"))
  expect_s3_class(df_internal, "data.frame")

  # 2. Probar la lógica de conversión de NA
  # Simulamos un dataframe con strings vacíos para ver si la función los limpia
  # Nota: Como load_dataset usa get() del namespace, esto prueba la lógica
  # sobre los datos que ya están en el paquete.

  if (exists("categories", envir = asNamespace("herdr"))) {
    df <- load_dataset("categories")
    # Verificamos que no haya strings vacíos en columnas de tipo carácter
    char_cols <- sapply(df, is.character)
    if (any(char_cols)) {
      expect_false(any(df[, char_cols] == "", na.rm = TRUE))
    }
  }
})

test_that("load_dataset falla con nombres inexistentes", {
  # Intentar cargar algo que no existe ni en disco ni en el paquete
  expect_error(suppressMessages(load_dataset("data_inexistente_123")))
})
