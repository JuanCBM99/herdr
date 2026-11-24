# Summarize CH4, N2O Emissions, Land Use, and Farm Product Intensities

Integrates all upstream Tier 2 GHG emissions, agroecological land
footprints, and GLEAM production outputs. Generates two distinct
reports: an absolute environmental inventory (totals) and farm-level
product, nutritional, and per-head environmental intensities.

Product intensities and Protein FCR (Table 2) are obtained by allocating
the whole enterprise pool (GHG, land, water, feed protein) among its
co-products using a biophysical (energy-based) allocation factor:
`AF_k = Energy_k / Energy_total`. Specifically, `Protein_FCR_total`
divides whole-herd total feed crude protein by edible animal protein
(allocated by `af_protein`), `Protein_FCR_human_edible` weights feed
crude protein by the explicit `human_edible` column in `mapping.csv`
(default binary `0/1` classification anchored on arable non-forage
concentrates with `economic_allocation > 0.33`, excluding inedible/waste
items and including commercial food-grade dairy feeds), and `EPCR_DIAAS`
adjusts both human-edible feed protein (`diaas_pct` in `mapping.csv`)
and animal product protein (Ertl et al., 2016b, Table 5) by their
Digestible Indispensable Amino Acid Score (FAO 2013 0.5-3 yr child
pattern, untruncated), alongside the Protein Quality Ratio
(`PQR = DIAAS_output / DIAAS_input`).

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

  Absolute environmental inventory of emissions (Gg CH4, Gg N2O, Gg
  CO2eq), feed (kg/year, including `feed_CP_total_kg`,
  `feed_CP_human_edible_kg`, and `feed_HDP_kg`), land (m2) and water
  (m3).

- intensities:

  Farm-level product (per kg FPCM milk, carcass meat, eggs, wool or
  edible protein) and enterprise-level nutritional intensities
  (`Protein_FCR_total` across all feed CP; `Protein_FCR_human_edible`
  weighted by `human_edible` in `mapping.csv`; `EPCR_DIAAS` adjusting
  both feed and animal protein by DIAAS under FAO 2013 0.5-3 yr pattern;
  and `PQR = DIAAS_output / DIAAS_input`).

## Examples

``` r
if (FALSE) { # \dontrun{
res <- generate_impact_assessment(
  farm_country = "Spain", year = 2022,
  saveoutput = FALSE, ar = "AR6"
)
res$totals
res$intensities
} # }
```
