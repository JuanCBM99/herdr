# Calculate land use

Computes total land use (m2) per animal based on validated DMI.

## Usage

``` r
calculate_land_use(
  automatic_cycle = FALSE,
  crop_yield_country,
  saveoutput = TRUE
)
```

## Arguments

- automatic_cycle:

  Logical. If TRUE, uses the built-in model for automatic farm cycle
  calculation. Default is FALSE.

- crop_yield_country:

  Character. FAO Area to use for crop yields.

- saveoutput:

  If TRUE (default) the results are saved in the output folder.
