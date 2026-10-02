# Locate the forages dataset (forages.parquet)

Checks in order:

1.  Local data directory (`file.path(data_dir, "forages.parquet")`)

2.  Package bundled extdata
    (`system.file("extdata", "forages.parquet", package = "herdr")`)

## Usage

``` r
herdr_get_forages_path(data_dir = "user_data")
```

## Arguments

- data_dir:

  Character. Path to the user data directory. Default is `"user_data"`.

## Value

Absolute path to forages parquet file.
