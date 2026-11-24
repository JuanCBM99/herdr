# Calculate CH4 Emissions from Manure Management

Computes CH4 emissions from manure based on Volatile Solids (VS),
population, and management factors (B0, MCF, AWMS) using IPCC Eq 10.23.

## Usage

``` r
calculate_CH4_manure(
  automatic_cycle = FALSE,
  saveoutput = TRUE,
  data_dir = "user_data",
  vs_df = NULL,
  pop_df = NULL
)
```

## Arguments

- automatic_cycle:

  Logical. If TRUE, uses the built-in model for automatic farm cycle
  calculation. Default is FALSE.

- saveoutput:

  If TRUE (default) the results are saved in the output folder.

- data_dir:

  Path to the directory containing input CSV/data files. Defaults to
  "user_data".

- vs_df:

  Optional precalculated Volatile Solids dataframe.

- pop_df:

  Optional precalculated population dataframe.

## Value

A tibble with manure management methane emission factors (`EF_kgyear`,
kg CH4/head/year), volatile solids (`VS_kgday`), `B0_m3kg`, `MCF_pct`,
and total emissions (`total_CH4_mm_kgyear`, kg CH4/year) by animal
cohort and management system.

## Examples

``` r
if (FALSE) { # \dontrun{
calculate_CH4_manure(saveoutput = FALSE, data_dir = "user_data")
} # }
```
