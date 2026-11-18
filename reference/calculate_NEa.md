# Calculate Net Energy for Activity (NEa)

Calculate Net Energy for Activity (NEa)

## Usage

``` r
calculate_NEa(
  saveoutput = TRUE,
  data_dir = "user_data",
  nem_df = NULL,
  weights = NULL,
  categories = NULL,
  coefficients = NULL
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

- weights:

  Optional preloaded `livestock_weights` dataframe.

- categories:

  Optional preloaded `ruminant_definitions` dataframe.

- coefficients:

  Optional preloaded `ipcc_coefficients` dataframe.

## Value

A tibble with columns `farm_id`, `region`, `subregion`, `animal_tag`,
`animal_type`, `animal_subtype`, and `NEa_MJday` (MJ/day).

## Examples

``` r
if (FALSE) { # \dontrun{
calculate_NEa(saveoutput = FALSE, data_dir = "user_data")
} # }
```
