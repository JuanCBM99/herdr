# Calculate Net Energy for Wool (NE_wool)

Calculate Net Energy for Wool (NE_wool)

## Usage

``` r
calculate_NE_wool(
  saveoutput = TRUE,
  data_dir = "user_data",
  categories = NULL,
  weights = NULL
)
```

## Arguments

- saveoutput:

  If TRUE (default) the results are saved in the output folder.

- data_dir:

  Character. Path to the folder containing input CSV files. Default is
  `"user_data"`.

- categories:

  Optional preloaded `ruminant_definitions` dataframe.

- weights:

  Optional preloaded `livestock_weights` dataframe.

## Value

A tibble with columns `farm_id`, `region`, `subregion`, `animal_tag`,
`animal_type`, `animal_subtype`, and `NEwool_MJday` (MJ/day).

## Examples

``` r
if (FALSE) { # \dontrun{
calculate_NE_wool(saveoutput = FALSE, data_dir = "user_data")
} # }
```
