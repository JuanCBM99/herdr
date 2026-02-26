# Calculate Volatile Solids (VS) for Animals

Computes volatile solids (VS) based on Gross Energy (GE), Digestible
Energy (DE), and Ash content using IPCC Tier 2 methodology.

## Usage

``` r
calculate_vs(urinary_energy = 0.04, saveoutput = TRUE)
```

## Arguments

- urinary_energy:

  Numeric. Fraction of energy lost in urine. Default 0.04.

- saveoutput:

  If TRUE (default) the results are saved in the output folder.

## Value

Tibble with VS for all animal categories.
