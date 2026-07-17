# Calculate land use

Computes total land use (m2) per animal based on validated DMI and
specific origin countries.

## Usage

``` r
calculate_land_use(
  automatic_cycle = FALSE,
  saveoutput = TRUE,
  farm_country = "Spain",
  year = 2022
)
```

## Arguments

- automatic_cycle:

  Logical. If TRUE, uses the built-in model for automatic farm cycle
  calculation. Default is FALSE.

- saveoutput:

  If TRUE (default) the results are saved in the output folder.

- farm_country:

  Character. The country of the farm/study (e.g., "Spain"). Default is
  "Spain".

- year:

  Numeric. The reference year for FAO trade data calculation if origins
  are missing. Default is 2022.
