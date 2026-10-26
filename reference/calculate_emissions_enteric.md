# Calculate methane emissions from enteric fermentation

Computes enteric methane emissions based on Gross Energy (GE),
Digestible Energy (DE), NDF, and Ym factor using IPCC Tier 2 logic for
ruminants, and the IPCC Tier 1 fixed emission factor for swine (1.5 kg
CH4/head/year, high productivity systems; IPCC 2019 Refinement, Vol 4,
Ch 10, Table 10.10 and Eq. 10.19).

## Usage

``` r
calculate_emissions_enteric(
  automatic_cycle = FALSE,
  saveoutput = TRUE,
  data_dir = "user_data",
  diet_vars = NULL,
  ge_df = NULL,
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

- diet_vars:

  Optional precalculated weighted nutritional variables dataframe.

- ge_df:

  Optional precalculated Gross Energy dataframe.

- pop_df:

  Optional precalculated population dataframe.
