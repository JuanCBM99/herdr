# Calculate Net Energy for Pregnancy (NE_pregnancy)

Calculate Net Energy for Pregnancy (NE_pregnancy)

## Usage

``` r
calculate_NE_pregnancy(
  saveoutput = TRUE,
  data_dir = "user_data",
  nem_df = NULL,
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

- nem_df:

  Optional precalculated NEm dataframe to avoid recomputation.

- categories:

  Optional preloaded `ruminant_definitions` dataframe.

- coefficients:

  Optional preloaded `ipcc_coefficients` dataframe.

- validate:

  Logical. Whether to run coefficient validation. Default is TRUE.

## Value

A tibble with columns `farm_id`, `region`, `subregion`, `animal_tag`,
`animal_type`, `animal_subtype`, and `NEpregnancy_MJday` (MJ/day).

## Examples

``` r
if (FALSE) { # \dontrun{
calculate_NE_pregnancy(saveoutput = FALSE, data_dir = "user_data")
} # }
```
