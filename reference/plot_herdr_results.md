# Plot herdr results dynamically with custom grouping and premium aesthetics

Plot herdr results dynamically with custom grouping and premium
aesthetics

## Usage

``` r
plot_herdr_results(
  df,
  group_cols = c("farm_id", "animal_tag", "region", "subregion"),
  func_name = NULL,
  gwp_report = "AR5",
  ar = NULL,
  functional_unit = "total",
  impact_category = NULL
)
```

## Arguments

- df:

  Dataframe containing the results.

- group_cols:

  Character vector of columns to group the plot by.

- func_name:

  Name of the function that generated the data.

- gwp_report:

  Character string or named numeric vector. IPCC GWP standard used as
  fallback when GWP columns are not pre-calculated. Options: \`"AR5"\`,
  \`"AR6"\`, \`"AR4"\`, \`"SAR"\`. Default is \`"AR5"\`.

- ar:

  Optional alias for \`gwp_report\`.

- functional_unit:

  Character string specifying the functional unit for impact assessment
  plots: \`"total"\` (Gg CO2e, ha, m3), \`"protein"\` (per kg edible
  protein), \`"product"\` (per kg commercial product), \`"fcr"\` (Mottet
  et al. 2017 Protein Feed Conversion Ratio and human food competition),
  \`"milk"\`, \`"meat"\`, \`"wool"\`, \`"egg"\`, or \`"head"\` (per
  animal head). Default is \`"total"\`.

- impact_category:

  Character string specifying the environmental impact category to
  visualize: \`"ghg"\` (carbon footprint), \`"land"\` (land use
  footprint), \`"water"\` (feed water deprivation footprint),
  \`"overview"\` (multi-criteria comparison of all footprints), or
  \`"fcr"\` (protein feed efficiency). Default is \`NULL\`
  (auto-selected).

## Value

A ggplot2 object.
