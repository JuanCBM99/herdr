# Calculate Weighted Nutritional Variables

Computes weighted averages of nutritional variables (DE, CP,
human-edible CP, DIAAS-adjusted human-digestible protein, NDF, ASH, GE,
and monogastric DE/ME) by mapping ingredients to diets and diets to
animals, combining ruminant and monogastric (swine and poultry)
definitions.

## Usage

``` r
calculate_weighted_variable(saveoutput = TRUE, data_dir = "user_data")
```

## Arguments

- saveoutput:

  If TRUE (default) the results are saved in the output folder.

- data_dir:

  Character. Path to the folder containing input CSV files. Default is
  `"user_data"`.

## Value

Tibble with weighted nutritional variables for all animal cohorts,
including `CP_human_edible_pct` (human-edible feed CP, % DM) and
`HDP_feed_pct` (DIAAS-adjusted human-digestible feed protein, % DM, FAO
2013 0.5-3 yr child pattern).

## Examples

``` r
if (FALSE) { # \dontrun{
calculate_weighted_variable(saveoutput = FALSE, data_dir = "user_data")
} # }
```
