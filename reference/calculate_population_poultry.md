# Calculate Poultry Population Structure

Models the complete poultry flock structure from mature breeding or
laying hens based on biological laying cycles, incubation fertility, and
rearing durations.

## Usage

``` r
calculate_population_poultry(
  census_poultry,
  rate_parameters,
  definitions = NULL,
  weights = NULL
)
```

## Arguments

- census_poultry:

  Filtered census data for poultry.

- rate_parameters:

  Reproduction and replacement rates table.

- definitions:

  Optional unified or monogastric definitions table.

- weights:

  Optional weights table (e.g., livestock_weights) containing
  `productive_period_days`.

## Value

A tibble with the modeled poultry population structure.

## Demographic Flow Logic

- **Mature Layers / Breeders**: `laying_hens`, `breeder_meat_hens`.

- **Replacement Pullets**: Rearing cohorts (`replacement_layer_pullets`,
  `replacement_meat_pullets`) generated from the all-in all-out cycle
  turnover and rearing days (`days / 365`).

- **Broilers**: Meat chickens generated from breeder meat hens based on
  daily egg mass (`egg_mass_g_day`), average egg weight
  (`egg_weight_g`), incubation hatchability (`fertility_rate`), and
  fattening duration (`broiler_days`).

- **Manual Broiler Protection**: If the user explicitly enters a
  positive count for `broilers` in the census (e.g. an independent
  commercial grow-out farm), their manual figure is preserved intact
  without being overwritten.
