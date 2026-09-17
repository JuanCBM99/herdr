# Calculate Total Animal Population

Orchestrates the animal population calculation for the farm. Provides
two distinct modes:

- **Manual Census (`automatic_cycle = FALSE`)**: Directly uses the
  animal counts provided by the user in `livestock_census.csv`. Allows
  arbitrary custom categories as long as they are declared in the
  definitions and parameter tables.

- **Automatic Farm Cycle (`automatic_cycle = TRUE`)**: Models the
  complete herd or flock structure (offspring, replacements,
  fattening/slaughter cohorts) automatically from mature breeding
  animals using biological reproduction and replacement dynamics.

## Usage

``` r
calculate_population(automatic_cycle = FALSE, saveoutput = TRUE)
```

## Arguments

- automatic_cycle:

  Logical. If `TRUE`, runs the built-in biological demographic model for
  each species. Default is `FALSE`.

- saveoutput:

  Logical. If `TRUE`, exports the final population dataset to
  `output/population_result.csv`. Default is `TRUE`.

## Value

A tibble containing the complete animal population structure with
columns: `region`, `subregion`, `animal_tag`, `class_flex`,
`animal_type`, `animal_subtype`, and `population`.

## Automatic Demographic Modeling

When `automatic_cycle = TRUE`, the model requires standard mature
categories (e.g., `mature_dairy_cattle`, `mature_beef_cattle`,
`mature_sheep_female_dairy`, `mature_goat_female_dairy`, `breeder_sows`,
`breeder_meat_hens`, `laying_hens`). All generated offspring
automatically inherit the `region` and `subregion` of their parent
cohorts, ensuring seamless downstream integration with diets, weights,
and manure management.
