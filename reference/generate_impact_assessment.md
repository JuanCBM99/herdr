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
  group_by_identification = TRUE,
  farm_country = "Spain",
  year = 2024
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

- farm_country:

  Character. The country of the farm/study (e.g., "Spain"). Default is
  "Spain".

- year:

  Numeric. The reference year for FAO trade data calculation if origins
  are missing. Default is 2022.
