# Summarize CH4, N2O Emissions, and Land Use

Summarize CH4, N2O Emissions, and Land Use

## Usage

``` r
generate_impact_assessment(
  automatic_cycle = FALSE,
  region = NULL,
  subregion = NULL,
  animal = NULL,
  type = NULL,
  class_flex = NULL,
  saveoutput = TRUE,
  group_by_identification = TRUE
)
```

## Arguments

- automatic_cycle:

  Logical. TRUE for built-in model, FALSE for manual
  livestock_census.csv.

- region:

  Character/Numeric vector to filter.

- subregion:

  Character vector to filter.

- animal:

  Livestock type (animal_type).

- type:

  Livestock subtype (animal_subtype).

- class_flex:

  Management class (e.g., 'grazing', 'stall').

- saveoutput:

  If TRUE saves to output folder.

- group_by_identification:

  If TRUE returns by animal_tag.
