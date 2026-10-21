# Calculate Feed Environmental Footprint (GHG Emissions and Water Use)

Computes greenhouse gas emissions (total, fossil, and land-use change /
deforestation) and water deprivation footprint (m3) resulting from
animal feed consumption based on the INRAE/ADEME ECOALIM V9 Life Cycle
Assessment (EF 3.1) database.

## Usage

``` r
calculate_feed_emissions(
  automatic_cycle = FALSE,
  saveoutput = TRUE,
  data_dir = "user_data",
  DMI_df = NULL,
  population_df = NULL
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

- DMI_df:

  Optional precalculated DMI dataframe.

- population_df:

  Optional precalculated population dataframe.

## Value

Tibble containing feed environmental footprints by animal category,
including total GHG (kg CO2e), fossil GHG, LUC GHG, and water
consumption (m3).
