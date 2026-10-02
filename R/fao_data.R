#' FAO and Reference Data Cache Management
#'
#' Functions to locate, cache, and manage large reference datasets (e.g., FAO crops,
#' trade matrices, forages) in a user-level central cache directory.
#'
#' @name fao_data_cache
#' @keywords internal
NULL

#' Get herdr user data cache directory
#'
#' Returns the platform-appropriate user cache directory for herdr data
#' using \code{tools::R_user_dir("herdr", which = "data")}.
#'
#' @return Character path to the user cache directory.
#' @export
herdr_cache_dir <- function() {
  cache_dir <- tools::R_user_dir("herdr", which = "data")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  cache_dir
}

#' Locate or download the FAO crops dataset (fao_crops.parquet)
#'
#' Checks in order:
#' \enumerate{
#'   \item Local data directory (\code{file.path(data_dir, "fao_crops.parquet")})
#'   \item Central user cache (\code{tools::R_user_dir("herdr", "data")})
#'   \item Package bundled extdata (\code{system.file("extdata", "fao_crops.parquet", package = "herdr")})
#'   \item Package example directories (development fallback)
#'   \item Automatic download from GitHub Release to central user cache if missing.
#' }
#'
#' @param data_dir Character. Path to the user data directory. Default is \code{"user_data"}.
#' @param download_if_missing Logical. Whether to attempt downloading if not found locally or in cache.
#' @return Absolute path to \code{fao_crops.parquet}, or NULL if not found.
#' @export
herdr_get_fao_crops_path <- function(data_dir = "user_data", download_if_missing = TRUE) {
  # 1. Local data directory (custom user data or test mocks)
  local_path <- file.path(data_dir, "fao_crops.parquet")
  if (file.exists(local_path)) return(local_path)

  # 2. Central user cache
  cache_dir <- herdr_cache_dir()
  cache_file <- file.path(cache_dir, "fao_crops.parquet")
  if (file.exists(cache_file)) return(cache_file)

  # 3. Package bundled extdata (if packaged)
  bundled <- system.file("extdata", "fao_crops.parquet", package = "herdr")
  if (nzchar(bundled) && file.exists(bundled)) return(bundled)

  # 4. Download from GitHub Release to central cache if allowed
  # nocov start
  if (download_if_missing) {
    url_release <- "https://github.com/JuanCBM99/herdr/releases/latest/download/fao_crops.parquet"
    message("\u23f3 FAO crops dataset not found locally. Downloading to central cache (24 MB)...")
    tryCatch({
      utils::download.file(url_release, destfile = cache_file, mode = "wb")
      message("\u2705 Download completed successfully.")
      return(cache_file)
    }, error = function(e) {
      warning("\u26A0 Could not download fao_crops.parquet: ", e$message)
    })
  }
  # nocov end

  NULL
}

#' Locate or download the FAO trade matrix dataset (fao_trade_matrix.parquet)
#'
#' Checks in order:
#' \enumerate{
#'   \item Local data directory (\code{file.path(data_dir, "fao_trade_matrix.parquet")})
#'   \item Central user cache (\code{tools::R_user_dir("herdr", "data")})
#'   \item Automatic download from GitHub Release to central user cache if missing.
#' }
#'
#' @param data_dir Character. Path to the user data directory. Default is \code{"user_data"}.
#' @param download_if_missing Logical. Whether to attempt downloading if not found locally or in cache.
#' @return Absolute path to \code{fao_trade_matrix.parquet}, or NULL if not found.
#' @export
herdr_get_fao_trade_matrix_path <- function(data_dir = "user_data", download_if_missing = TRUE) {
  # 1. Local data directory
  local_path <- file.path(data_dir, "fao_trade_matrix.parquet")
  if (file.exists(local_path)) return(local_path)

  # 2. Central user cache
  cache_dir <- herdr_cache_dir()
  cache_file <- file.path(cache_dir, "fao_trade_matrix.parquet")
  if (file.exists(cache_file)) return(cache_file)

  # 3. Download from GitHub Release to central cache if allowed
  # nocov start
  if (download_if_missing) {
    url_release <- "https://github.com/JuanCBM99/herdr/releases/latest/download/fao_trade_matrix.parquet"
    message("\u23f3 FAO trade matrix not found locally.")
    message("Downloading background database (187 MB) to central cache... This will only happen once.")
    tryCatch({
      utils::download.file(url_release, destfile = cache_file, mode = "wb")
      message("\u2705 Download completed successfully.")
      return(cache_file)
    }, error = function(e) {
      stop("Error downloading the trade matrix. Please check your internet connection: ", e$message)
    })
  }
  # nocov end

  NULL
}

#' Locate the forages dataset (forages.parquet)
#'
#' Checks in order:
#' \enumerate{
#'   \item Local data directory (\code{file.path(data_dir, "forages.parquet")})
#'   \item Package bundled extdata (\code{system.file("extdata", "forages.parquet", package = "herdr")})
#' }
#'
#' @param data_dir Character. Path to the user data directory. Default is \code{"user_data"}.
#' @return Absolute path to forages parquet file.
#' @export
herdr_get_forages_path <- function(data_dir = "user_data") {
  # 1. Local data directory
  local_path <- file.path(data_dir, "forages.parquet")
  if (file.exists(local_path)) return(local_path)

  # 2. Bundled extdata inside the package
  bundled <- system.file("extdata", "forages.parquet", package = "herdr")
  if (nzchar(bundled) && file.exists(bundled)) return(bundled)

  stop("Could not locate forages.parquet. Please provide it in '", data_dir, "' or reinstall herdr.")
}
