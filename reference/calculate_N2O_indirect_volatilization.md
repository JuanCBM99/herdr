# Calculate indirect N2O emissions from volatilization

Computes indirect N2O emissions derived from volatilization of excreted
nitrogen (IPCC Eq 10.26 and 10.28).

## Usage

``` r
calculate_N2O_indirect_volatilization(
  automatic_cycle = FALSE,
  saveoutput = TRUE
)
```

## Arguments

- automatic_cycle:

  Logical. If TRUE, uses the built-in model for automatic farm cycle
  calculation. Default is FALSE.

- saveoutput:

  If TRUE (default) the results are saved in the output folder.
