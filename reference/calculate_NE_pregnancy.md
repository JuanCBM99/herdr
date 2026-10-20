# Calculate Net Energy for Pregnancy (NE_pregnancy)

Calculate Net Energy for Pregnancy (NE_pregnancy)

## Usage

``` r
calculate_NE_pregnancy(
  saveoutput = TRUE,
  data_dir = "user_data",
  nem_df = NULL
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
