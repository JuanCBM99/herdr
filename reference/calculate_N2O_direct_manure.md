# Calculate direct N2O emissions from manure

Computes direct N2O emissions based on nitrogen excretion logic,
emission factors, management system, and climate (IPCC Eq 10.25).

## Usage

``` r
calculate_N2O_direct_manure(automatic_cycle = FALSE, saveoutput = TRUE)
```

## Arguments

- automatic_cycle:

  Logical. If TRUE, uses the built-in model for automatic farm cycle
  calculation. Default is FALSE.

- saveoutput:

  If TRUE (default) the results are saved in the output folder.
