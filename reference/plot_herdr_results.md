# Plot herdr results dynamically with custom grouping and premium aesthetics

Plot herdr results dynamically with custom grouping and premium
aesthetics

## Usage

``` r
plot_herdr_results(
  df,
  group_cols = c("animal_tag", "region", "subregion", "class_flex"),
  func_name = NULL
)
```

## Arguments

- df:

  Dataframe containing the results.

- group_cols:

  Character vector of columns to group the plot by.

- func_name:

  Name of the function that generated the data.

## Value

A ggplot2 object.
