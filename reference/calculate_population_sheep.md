# Calculate Sheep Population Structure

Models the complete sheep flock structure from mature breeding animals
using biological prolificacy rates, reproductive cycle duration (lambing
interval), replacement dynamics, and feeding period days for slaughter
lambs.

## Usage

``` r
calculate_population_sheep(
  census_sheep,
  rate_parameters,
  definitions = NULL,
  weights = NULL
)
```

## Arguments

- census_sheep:

  Filtered census data for sheep.

- rate_parameters:

  Reproduction and replacement rates table.

- definitions:

  Optional definitions table (e.g., ruminant_definitions) containing
  `pr_sheep_goat`.

- weights:

  Optional weights table (e.g., livestock_weights) containing
  `productive_period_days`.

## Value

A tibble with the modeled sheep population structure.

## Demographic Flow Logic

- **Mature Parents**: `mature_sheep_female_dairy`,
  `mature_sheep_female_meat`, `mature_sheep_male_dairy`,
  `mature_sheep_male_meat`.

- **Lambing Frequency**: Number of lambings per ewe per year
  (`365 / productive_period_days`), supporting accelerated lambing
  systems (e.g. 240 days = 1.5 lambings/year).

- **Replacements**: Ewe and ram lambs retained annually to sustain the
  breeding flock.

- **Births**: Total lambs born per year
  (`ewes * lambing_frequency * prolificacy`).

- **Slaughter Lambs**: Remaining offspring (`Births - Replacements`)
  scaled to Average Annual Population (AAP, IPCC Eq. 10.1) based on
  feeding duration (`days / 365`).
