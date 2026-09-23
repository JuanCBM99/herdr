# Calculate land use

Computes total land use (m2) per animal based on validated DMI and
specific origin countries.

## Usage

``` r
calculate_land_use(
  automatic_cycle = FALSE,
  saveoutput = TRUE,
  farm_country = "Spain",
  year = 2024,
  max_trace_hops = 4,
  ssr_threshold = 0.7,
  data_dir = "user_data"
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
  are missing. Default is 2024.

- max_trace_hops:

  Numeric. When an ingredient's country of origin has to be inferred
  from trade data, this caps how many countries the algorithm will
  follow through re-export hubs. Default is 4.

- ssr_threshold:

  Numeric. Self-Sufficiency Ratio (Production / Apparent Consumption)
  above which a country is considered a genuine producer. Default is
  0.70.

- data_dir:

  Path to the directory containing input CSV/data files. Defaults to
  \`"user_data"\`.
