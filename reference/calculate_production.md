# Calculate Total Livestock Production

Computes total production of milk, meat, eggs, and fibre in physical
units (fresh milk, FPCM, live weight, carcass weight) and nutritional
units (kg of edible protein and fibre) based on FAO/GLEAM (Equations 9.1
to 9.5) and IDF standards.

## Usage

``` r
calculate_production(
  automatic_cycle = FALSE,
  saveoutput = TRUE,
  data_dir = "user_data"
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

## Value

Tibble with animal production summary.
