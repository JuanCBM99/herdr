# Calculate direct N2O emissions from manure

Computes direct N2O emissions based on nitrogen excretion logic,
emission factors, management system, and climate (IPCC Eq 10.25).

## Usage

``` r
calculate_N2O_direct_manure(
  automatic_cycle = FALSE,
  saveoutput = TRUE,
  data_dir = "user_data",
  ge_df = NULL,
  cp_df = NULL,
  pop_df = NULL,
  neg_df = NULL,
  dmi_df = NULL
)
```

## Arguments

- automatic_cycle:

  Logical. If TRUE, uses the built-in model for automatic farm cycle
  calculation. Default is FALSE.

- saveoutput:

  Logical. If TRUE (default), results are saved to output folder.

- data_dir:

  Path to the directory containing input CSV/data files. Defaults to
  \`"user_data"\`.

- ge_df:

  Optional precalculated Gross Energy dataframe.

- cp_df:

  Optional precalculated weighted nutritional variables dataframe.

- pop_df:

  Optional precalculated population dataframe.

- neg_df:

  Optional precalculated NEg dataframe.

- dmi_df:

  Optional precalculated DMI dataframe.

## Value

A tibble with annual nitrogen excretion (`N_excreted_kgheadyear`, kg
N/head/year) and direct manure N2O emissions (`N2O_direct_kgyear`, kg
N2O/year) by cohort and management system.

## Examples

``` r
if (FALSE) { # \dontrun{
calculate_N2O_direct_manure(saveoutput = FALSE, data_dir = "user_data")
} # }
```
