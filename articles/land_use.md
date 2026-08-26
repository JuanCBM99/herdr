# Land Use Methodology

## Land Use Methodology

### Introduction

In addition to greenhouse gas emissions, `herdr` estimates the
**feed-related agricultural land use footprint** associated with
livestock operations, across both ruminant and monogastric systems.

Land use is defined as the physical agricultural area (in m²) required
to produce the feed ration consumed by a herd. The engine combines
animal dry matter intake (DMI), crop and forage yields, multi-output
economic allocation, and — when an ingredient’s origin isn’t specified —
dynamic international trade flows.

This vignette is the detailed reference behind the shorter mentions of
land use scattered through [Technical
Reference](https://juancbm99.github.io/herdr/articles/Technical_reference.md)
and the [General
Workflow](https://juancbm99.github.io/herdr/articles/Workflow.md).

------------------------------------------------------------------------

### Mathematical Formulation

Land footprint calculations follow four sequential steps for every
ingredient $`i`$ in diet $`d`$.

#### 1. Land required per kilogram of feed

The baseline land required to produce one kilogram of dry matter is the
inverse of the crop’s yield:

``` math
LandPerKg_i = \begin{cases} \dfrac{1}{Yield_i} & \text{if } Yield_i > 0 \\ 0 & \text{otherwise} \end{cases}
```

where $`Yield_i`$ is expressed in kg DM/ha.

#### 2. Economic allocation adjustment

Many feed ingredients are co-products of a crop grown mainly for
something else — soybean meal alongside soybean oil, or a cereal grain
alongside its straw. Attributing the *entire* cultivated area to the
feed ingredient would overstate its footprint, so `herdr` applies the
`economic_allocation` factor from
[`mapping.csv`](https://juancbm99.github.io/herdr/articles/Technical_reference.html#mapping.csv--database-connector)
($`EconomicAllocation_i \in [0, 1]`$):

``` math
AdjustedLandPerKg_i = LandPerKg_i \times EconomicAllocation_i
```

#### 3. Annual ingredient intake

How much of ingredient $`i`$ a cohort consumes over a year depends on
daily dry matter intake ($`DMI_{\text{kg/day}}`$), the feed category’s
share of the diet ($`Share_{cat}`$, from `diet_profiles.csv`), and the
ingredient’s share within that category ($`Share_i`$, from
`diet_ingredients.csv`):

``` math
AnnualConsumption_i = (DMI_{\text{kg/day}} \times 365) \times \left(\frac{Share_{cat}}{100}\right) \times \left(\frac{Share_i}{100}\right)
```

$`Share_{cat}`$ corresponds to whichever of forage, concentrate, milk,
or milk replacer the ingredient belongs to.

#### 4. Cohort land footprint

The area is computed and converted from hectares to square metres (1 ha
= 10,000 m²), then scaled up to the full cohort:

``` math
LandUsePerAnimal_i \; (\text{m}^2) = AdjustedLandPerKg_i \times AnnualConsumption_i \times 10{,}000
```

``` math
TotalLandUse_i \; (\text{m}^2) = LandUsePerAnimal_i \times Population
```

------------------------------------------------------------------------

### Yield Resolution Hierarchy

`herdr` resolves $`Yield_i`$ through a three-tiered hierarchy, checked
in order:

1.  **User custom yields.** If `custom_yield_kg_ha` is filled in for
    that ingredient in `diet_ingredients.csv`, herdr uses it directly
    and labels the origin `"Custom Data"` — no database lookup happens
    at all.
2.  **FAOSTAT crop yields.** Otherwise, crop productivities are queried
    from `fao_crops.parquet` for the relevant country and reporting
    year.
3.  **Curated forage database.** For forages and grasses not well
    covered by FAOSTAT, yields come from `fao_forages.parquet`, compiled
    from literature and technical sources.

See [Adding a New
Ingredient](https://juancbm99.github.io/herdr/articles/Adding_Ingredient.md)
for a worked example of connecting a new ingredient to this hierarchy
via `mapping.csv`.

------------------------------------------------------------------------

### Feed Origin & Dynamic Trade Allocation

When `country_of_origin` is left as `NA` in `diet_ingredients.csv`,
herdr estimates it from international trade flows rather than leaving it
undefined.

1.  **Apparent consumption** — how much of the crop the country actually
    uses, regardless of who grew it:

    ``` math
    ApparentConsumption = \max(1, Production + TotalImports - TotalExports)
    ```

2.  **Self-Sufficiency Ratio (SSR)** — the share of that consumption the
    country produces itself:

    ``` math
    SSR = \frac{Production}{ApparentConsumption}
    ```

3.  **Origin assignment rule:**

    - If $`SSR \ge 0.70`$ (70% self-sufficient or more): the origin is
      set to `farm_country` — the country is assumed to be growing
      enough of the crop itself.
    - If $`SSR < 0.70`$: the origin is set to the country’s **primary
      source of imports** (`Top_Partner`) — the trade partner it imports
      the most of that crop from.

> **Note:** the trade matrix database (`fao_trade_matrix.parquet`, ~187
> MB) is downloaded automatically from GitHub Releases the first time an
> assessment needs trade resolution — see [Technical
> Reference](https://juancbm99.github.io/herdr/articles/Technical_reference.html#fao_trade_matrixparquet--dynamic-trade-background-auto-downloaded)
> for details.

------------------------------------------------------------------------

### Execution

``` r

# Standalone run
land_results <- calculate_land_use(
  farm_country = "Spain",
  year = 2022,
  saveoutput = TRUE
)

# Integrated execution via full assessment
full_results <- generate_impact_assessment(
  farm_country = "Spain",
  year = 2022
)
```

------------------------------------------------------------------------

### Next steps

- [Technical
  Reference](https://juancbm99.github.io/herdr/articles/Technical_reference.md)
  — full definitions for `mapping.csv`, `fao_crops.parquet`,
  `fao_forages.parquet`, and `fao_trade_matrix.parquet`.
- [Adding a New
  Ingredient](https://juancbm99.github.io/herdr/articles/Adding_Ingredient.md)
  — how `economic_allocation` and `yield_name` connect a new ingredient
  to this methodology.
- [General
  Workflow](https://juancbm99.github.io/herdr/articles/Workflow.md) —
  where land use fits into a full assessment.
