# Calculate land use

Computes total land use (m2) per animal based on validated DMI. This
version integrates calculate_dmi() to ensure physiological consistency.

## Usage

``` r
calculate_land_use(automatic_cycle = FALSE, saveoutput = TRUE)
```

## Arguments

- automatic_cycle:

  Logical. If TRUE, uses the built-in model for automatic farm cycle
  calculation. Default is FALSE.

- saveoutput:

  If TRUE (default) the results are saved in the output folder.
