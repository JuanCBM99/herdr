# Calculate methane emissions from enteric fermentation

Computes enteric methane emissions based on Gross Energy (GE),
Digestible Energy (DE), NDF, and Ym factor using IPCC Tier 2 logic for
ruminants, and IPCC Tier 1 with metabolic weight scaling for swine
(Table 10.10 & Section 10.2.4).

## Usage

``` r
calculate_emissions_enteric(automatic_cycle = FALSE, saveoutput = TRUE)
```

## Arguments

- automatic_cycle:

  Logical. If TRUE, uses the built-in model for automatic farm cycle
  calculation. Default is FALSE.

- saveoutput:

  If TRUE (default) the results are saved in the output folder.
