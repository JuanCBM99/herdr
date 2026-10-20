# Calculate CH4 Emissions from Manure Management

Computes CH4 emissions from manure based on Volatile Solids (VS),
population, and management factors (B0, MCF, AWMS) using IPCC Eq 10.23.

## Usage

``` r
calculate_CH4_manure(
  automatic_cycle = FALSE,
  saveoutput = TRUE,
  data_dir = "user_data",
  vs_df = NULL,
  pop_df = NULL
)
```

## Arguments

- automatic_cycle:

  Logical. If TRUE, uses the built-in model for automatic farm cycle
  calculation. Default is FALSE.

- saveoutput:

  If TRUE (default) the results are saved in the output folder.

- data_dir:

  Path to the directory containing input CSV/data files. Defaults to
  "user_data".

- vs_df:

  Optional precalculated Volatile Solids dataframe.

- pop_df:

  Optional precalculated population dataframe.
