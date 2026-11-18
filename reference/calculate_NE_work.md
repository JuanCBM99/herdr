# Calculate Net Energy for Work (NE_work)

Computes NE_work based on NEm and hours of activity.

## Usage

``` r
calculate_NE_work(
  saveoutput = TRUE,
  data_dir = "user_data",
  nem_df = NULL,
  categories = NULL
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

## Value

A tibble with columns `farm_id`, `region`, `subregion`, `animal_tag`,
`animal_type`, `animal_subtype`, and `NEwork_MJday` (MJ/day).

## Examples

``` r
if (FALSE) { # \dontrun{
calculate_NE_work(saveoutput = FALSE, data_dir = "user_data")
} # }
```
