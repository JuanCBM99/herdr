# Locate or download the FAO crops dataset (fao_crops.parquet)

Checks in order:

1.  Local data directory (`file.path(data_dir, "fao_crops.parquet")`)

2.  Central user cache (`tools::R_user_dir("herdr", "data")`)

3.  Package bundled extdata
    (`system.file("extdata", "fao_crops.parquet", package = "herdr")`)

4.  Package example directories (development fallback)

5.  Automatic download from GitHub Release to central user cache if
    missing.

## Usage

``` r
herdr_get_fao_crops_path(data_dir = "user_data", download_if_missing = TRUE)
```

## Arguments

- data_dir:

  Character. Path to the user data directory. Default is `"user_data"`.

- download_if_missing:

  Logical. Whether to attempt downloading if not found locally or in
  cache.

## Value

Absolute path to `fao_crops.parquet`, or NULL if not found.
