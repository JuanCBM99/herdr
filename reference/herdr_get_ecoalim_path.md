# Locate the ECOALIM feed LCA dataset (ecoalim.parquet)

Checks in order:

1.  Local data directory (`file.path(data_dir, "ecoalim.parquet")`)

2.  Package bundled extdata
    (`system.file("extdata", "ecoalim.parquet", package = "herdr")`)

## Usage

``` r
herdr_get_ecoalim_path(data_dir = "user_data")
```

## Source

INRAE, ADEME, GIS REVALIM (2024). ECOALIM V9 dataset (EF 3.1).
Distributed under Licence Ouverte / Open Licence Etalab 2.0. URL:
<https://ecoalim.hub.inrae.fr/>

## Arguments

- data_dir:

  Character. Path to the user data directory. Default is `"user_data"`.

## Value

Absolute path to ECOALIM parquet file.
