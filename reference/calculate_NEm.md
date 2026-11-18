# Calculate Net Energy for Maintenance (NEm)

Computes net energy for maintenance based on animal weight and CFI
coefficient.

## Usage

``` r
calculate_NEm(
  saveoutput = TRUE,
  data_dir = "user_data",
  weights = NULL,
  categories = NULL,
  coefficients = NULL,
  validate = TRUE
)
```

## Arguments

- saveoutput:

  If TRUE (default) the results are saved in the output folder.

- data_dir:

  Character. Path to the folder containing input CSV files. Default is
  `"user_data"`.

- weights:

  Optional preloaded `livestock_weights` dataframe.

- categories:

  Optional preloaded `ruminant_definitions` dataframe.

- coefficients:

  Optional preloaded `ipcc_coefficients` dataframe.

- validate:

  Logical. Whether to run coefficient validation. Default is TRUE.

## Value

A tibble with columns `farm_id`, `region`, `subregion`, `animal_tag`,
`animal_type`, `animal_subtype`, and `NEm_MJday` (MJ/day).

## Examples

``` r
if (FALSE) { # \dontrun{
calculate_NEm(saveoutput = FALSE, data_dir = "user_data")
} # }
```
