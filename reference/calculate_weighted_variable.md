# Calculate Weighted Nutritional Variables

Computes weighted averages of nutritional variables (DE, CP, NDF, ASH,
ED, Fat, NFC) by mapping ingredients to diets and diets to animals,
combining ruminant and poultry definitions.

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
