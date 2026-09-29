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
  year = 2024,
  gwp_report = "AR5",
  ar = NULL,
  data_dir = "user_data"
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

- gwp_report:

  Character string or named numeric vector. IPCC Assessment Report
  version for Global Warming Potential (GWP100) factors. Options are
  \`"AR5"\` (default, CH4=28, N2O=265), \`"AR6"\` (CH4=27, N2O=273),
  \`"AR4"\` (CH4=25, N2O=298), or \`"SAR"\` (CH4=21, N2O=310).
  Alternatively, a custom named vector like \`c(CH4 = 27, N2O = 273)\`
  can be provided.

- ar:

  Optional shorthand alias for \`gwp_report\` (e.g., \`ar = "AR6"\`).

- data_dir:

  Path to the directory containing input CSV/data files. Defaults to
  \`"user_data"\`.
