library(testthat)
library(herdr)

test_that("download_templates crea carpeta y gestiona la estructura de archivos", {
  # 1. Carpeta temporal única para el test
  dest <- file.path(tempdir(), "user_data_test")
  if (dir.exists(dest)) unlink(dest, recursive = TRUE)

  # 2. Mockeamos la función de descarga apuntando al paquete 'utils'
  # O mejor: mockeamos la llamada interna si es posible,
  # pero aquí forzamos el binding a 'utils' que es su casa real.
  with_mocked_bindings(
    {
      path <- download_templates(dest_folder = dest)

      # VERIFICACIONES
      expect_true(dir.exists(dest))

      # Verificamos que se intentaron crear los archivos
      # (el mock crea archivos vacíos con file.create)
      files <- list.files(dest)
      expect_gt(length(files), 0)
      expect_true("categories.csv" %in% files)
    },
    download.file = function(url, destfile, ...) {
      file.create(destfile)
      return(0)
    },
    .package = "utils" # <--- CAMBIADO de "base" a "utils"
  )

  # Limpieza
  unlink(dest, recursive = TRUE)
})
