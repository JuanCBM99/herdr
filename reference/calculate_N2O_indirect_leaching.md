# Calculate indirect N2O emissions from leaching

Computes indirect N2O emissions derived from nitrogen leaching (IPCC Eq
10.27 and 10.29).

## Usage

``` r
calculate_N2O_indirect_leaching(
  automatic_cycle = FALSE,
  saveoutput = TRUE,
  data_dir = "user_data",
  direct_N2O_df = NULL,
  pop_df = NULL
)
```

## Arguments

- automatic_cycle:

  Logical. If TRUE, uses the built-in model for automatic farm cycle
  calculation. Default is FALSE.

- saveoutput:

  Logical. If TRUE (default), results are saved to output folder.

- data_dir:

  Path to the directory containing input CSV/data files. Defaults to
  \`"user_data"\`.

- direct_N2O_df:

  Optional precalculated direct N2O manure dataframe.

- pop_df:

  Optional precalculated population dataframe.
