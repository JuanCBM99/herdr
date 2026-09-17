# Calculate Swine Population Structure

Models the complete swine herd structure from mature breeding animals
using farrowing rates, litter sizes, sow replacement rates, and
fattening durations.

## Usage

``` r
calculate_population_swine(
  census_swine,
  rate_parameters,
  definitions = NULL,
  weights = NULL
)
```

## Arguments

- census_swine:

  Filtered census data for swine.

- rate_parameters:

  Reproduction and replacement rates table.

- definitions:

  Optional unified or monogastric definitions table.

- weights:

  Optional weights table (e.g., livestock_weights) containing
  `productive_period_days`.

## Value

A tibble with the modeled swine population structure.

## Demographic Flow Logic

- **Mature Parents**: `breeder_sows`, `boars`.

- **Annual Piglets**: `breeder_sows * farrowing_rate * litter_size`.

- **Replacement Sows**: `breeder_sows * replacement_rate` retained
  annually.

- **Fattening Pigs**: Slaughter pigs (`Annual Piglets - Replacements`)
  scaled to Average Annual Population (AAP, IPCC Eq. 10.1) based on
  fattening days (`days / 365`).
