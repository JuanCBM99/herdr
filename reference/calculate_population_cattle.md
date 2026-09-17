# Calculate Cattle Population Structure

Models the complete cattle herd structure from mature breeding animals
using biological reproduction rates, replacement dynamics, and feeding
period durations.

## Usage

``` r
calculate_population_cattle(
  census_cattle,
  rate_parameters,
  definitions = NULL,
  weights = NULL
)
```

## Arguments

- census_cattle:

  Filtered census data for cattle.

- rate_parameters:

  Reproduction and replacement rates table.

- definitions:

  Optional definitions table (e.g., ruminant_definitions) containing
  `pregnancy_rate`.

- weights:

  Optional weights table (e.g., livestock_weights) containing
  `productive_period_days`.

## Value

A tibble with the modeled cattle population structure.

## Demographic Flow Logic

- **Mature Parents**: `mature_dairy_cattle`, `mature_beef_cattle`,
  `mature_beef_bull`.

- **Replacements**: Female and male calves needed annually to maintain
  mature stock.

- **Births**: Calculated assuming an equal 50:50 sex ratio (calves
  divided by 2).

- **Slaughter / Feedlot Calves**: Non-replacement offspring scaled to
  Average Annual Population (AAP, IPCC Eq. 10.1) based on feeding
  duration (`days / 365`).

- **Yearlings**: Retained replacement calves mapped 1:1 into yearling
  cohorts.
