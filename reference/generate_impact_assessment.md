# Summarize CH₄, N₂O Emissions, and Land Use

Calculates a summary of CH₄ and N₂O emissions (direct and indirect) and
land use, grouped by code, animal type, subtype, and zone. Optionally
allows for aggregation to a more general level.

## Usage

``` r
generate_impact_assessment(
  group = NULL,
  zone = NULL,
  animal = NULL,
  type = NULL,
  saveoutput = TRUE,
  group_by_identification = TRUE
)
```

## Arguments

- group:

  Character/Numeric vector (optional). Groups to filter. Default
  \`NULL\`.

- zone:

  Character vector (optional). Zones to filter. Default \`NULL\`.

- animal:

  Character string (optional). Livestock type (\`animal_type\`). Default
  \`NULL\`.

- type:

  Character string (optional). Livestock subtype (\`animal_subtype\`).
  Default \`NULL\`.

- saveoutput:

  Logical. If \`TRUE\`, saves the result to
  \`"output/generate_impact_assessment.csv"\`. Default \`TRUE\`.

- group_by_identification:

  Logical. If \`TRUE\`, returns results at the \`identification\` level.
  If \`FALSE\`, aggregates all \`identification\`s.

## Value

A \`tibble\` with columns for each emission type and land use.
