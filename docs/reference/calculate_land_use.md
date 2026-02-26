# Calculate land use

Computes total land use (m2) per animal based on derived DMI (ge / eb).
This version automatically cleans duplicates in CSVs and normalizes
dietary shares.

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
