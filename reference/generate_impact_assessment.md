# Summarize CH4, N2O Emissions, Land Use, and Farm Product Intensities

Integrates all upstream Tier 2 GHG emissions, agroecological land
footprints, and GLEAM production outputs. Generates two distinct
reports: an absolute environmental inventory (totals) and farm-level
product, nutritional, and per-head environmental intensities.

## Usage

``` r
generate_impact_assessment(
  automatic_cycle = FALSE,
  farm_id = NULL,
  region = NULL,
  subregion = NULL,
  animal = NULL,
  type = NULL,
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

  Logical. TRUE for built-in demographic models, FALSE for manual
  livestock_census.csv.

- farm_id:

  Character vector to filter by farm_id. Default is NULL.

- region:

  Character/Numeric vector to filter by geographical region (e.g.,
  country or province). Together with `subregion`, defines the spatial
  location of the farm; management or housing variants within the same
  farm must be distinguished via `animal_tag`, not `subregion`. Default
  is NULL.

- subregion:

  Character vector to filter by geographical subregion (e.g.,
  municipality or district). Default is NULL.

- animal:

  Livestock type (animal_type). Default is NULL.

- type:

  Livestock subtype (animal_subtype). Default is NULL.

- saveoutput:

  If TRUE saves to output folder:
  'output/total_emissions_and_land_use.csv' and
  'output/product_intensities.csv'.

- group_by_identification:

  If TRUE returns the inventory totals by individual animal_tag cohort.
  If FALSE aggregates totals by farm_id, region, subregion, animal_type,
  animal_subtype.

- farm_country:

  Character. The country of the farm/study (e.g., "Spain"). Default is
  "Spain".

- year:

  Numeric. The reference year for FAO trade data calculation if origins
  are missing. Default is 2024.

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

## Value

A named list containing two dataframes:

- totals:

  Absolute environmental inventory of emissions, feed, and land use.

- intensities:

  Farm-level product, nutritional, and per-head environmental
  intensities.
