# Calculate Goat Population Structure

Models the complete goat herd structure from mature breeding animals
using biological prolificacy rates, reproductive cycle duration (kidding
interval), replacement dynamics, and feeding period days for slaughter
kids.

## Usage

``` r
calculate_population_goat(
  census_goat,
  rate_parameters,
  definitions = NULL,
  weights = NULL
)
```

## Arguments

- census_goat:

  Filtered census data for goat.

- rate_parameters:

  Reproduction and replacement rates table.

- definitions:

  Optional definitions table (e.g., ruminant_definitions) containing
  `pr_sheep_goat`.

- weights:

  Optional weights table (e.g., livestock_weights) containing
  `productive_period_days`.

## Value

A tibble with the modeled goat population structure.

## Demographic Flow Logic

- **Mature Parents**: `mature_goat_female_dairy`,
  `mature_goat_female_meat`, `mature_goat_male_dairy`,
  `mature_goat_male_meat`.

- **Kidding Frequency**: Number of kiddings per doe per year
  (`365 / productive_period_days`), supporting accelerated kidding
  systems (e.g. 240 days = 1.5 kiddings/year).

- **Replacements**: Doeling and buck kids retained annually to sustain
  the breeding herd.

- **Births**: Total kids born per year
  (`does * kidding_frequency * prolificacy`).

- **Slaughter Kids**: Remaining offspring (`Births - Replacements`)
  scaled to Average Annual Population (AAP, IPCC Eq. 10.1) based on
  feeding duration (`days / 365`).
