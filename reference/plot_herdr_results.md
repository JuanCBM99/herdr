# Plot herdr results dynamically with custom grouping and premium aesthetics

Plot herdr results dynamically with custom grouping and premium
aesthetics

## Usage

``` r
plot_herdr_results(
  df,
  group_cols = c("animal_tag", "region", "subregion", "class_flex"),
  func_name = NULL,
  gwp_report = "AR5",
  ar = NULL
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

## Value

A ggplot2 object.
