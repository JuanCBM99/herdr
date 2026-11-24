# Calculate Total Livestock Production

Computes total production of milk, meat, eggs, and fibre in physical
units (fresh milk, FPCM, live weight, carcass weight) and nutritional
units (kg of edible protein, DIAAS-adjusted human-digestible protein,
and fibre) based on FAO/GLEAM (Equations 9.1 to 9.5), IDF standards, and
Ertl et al. (2016b, Table 5; FAO 2013 0.5-3 yr child scoring pattern,
untruncated).

## Usage

``` r
calculate_production(
  automatic_cycle = FALSE,
  saveoutput = TRUE,
  data_dir = "user_data",
  pop_df = NULL
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
  "user_data".

- pop_df:

  Optional precalculated population dataframe.

## Value

Tibble with annual animal production per cohort, including physical
outputs (`milk_fresh_kg`, `milk_FPCM_kg`, `meat_live_weight_kg`,
`meat_carcass_weight_kg`, `egg_fresh_kg`, `wool_kg`), edible protein
(`milk_protein_kg`, `meat_protein_kg`, `egg_protein_kg`,
`total_protein_kg`, kg/year), and DIAAS-adjusted human-digestible animal
protein (`total_HDP_kg`, kg HDP/year).

## Examples

``` r
if (FALSE) { # \dontrun{
calculate_production(saveoutput = FALSE, data_dir = "user_data")
} # }
```
