# Calculate Dry Matter Intake (DMI)

Computes daily Dry Matter Intake (kg DM/day) based on metabolic demand
(GE/ED) for ruminants and metabolizable energy requirements (FEDNA /
NRC) for monogastrics (swine and poultry).

## Usage

``` r
calculate_DMI(
  saveoutput = TRUE,
  data_dir = "user_data",
  ge_req = NULL,
  diet_char = NULL,
  mono = NULL,
  weights = NULL
)
```

## Arguments

- saveoutput:

  Logical. If TRUE (default), the results are saved in the output
  folder.

- data_dir:

  Character. Path to the folder containing input CSV files. Default is
  `"user_data"`.

- ge_req:

  Optional precalculated Gross Energy dataframe.

- diet_char:

  Optional precalculated weighted nutritional variables dataframe.

- mono:

  Optional precalculated monogastric energy dataframe.

- weights:

  Optional preloaded livestock weights dataframe.

## Value

A tibble with daily Dry Matter Intake (kg DM/day) and intake relative to
body weight (% BW).
