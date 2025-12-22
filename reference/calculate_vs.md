# Calculate Volatile Solids (VS) for Animals (Refactored)

Computes volatile solids (VS) based on Gross Energy (GE), Digestible
Energy (DE), and Ash content.

## Usage

``` r
calculate_vs(urinary_energy = 0.04, saveoutput = TRUE)
```

## Arguments

- urinary_energy:

  Numeric. Fraction of energy lost in urine. Default 0.04.

- saveoutput:

  Logical. If TRUE, saves the result as CSV. Default TRUE.

## Value

Tibble with VS for all animal categories.
