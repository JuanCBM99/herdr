# Land Use Methodology

## Introduction

In addition to greenhouse gas emissions, `herdr` estimates the
**feed-related agricultural land use footprint** associated with
livestock operations, across both ruminant and monogastric systems.

Gross land occupation alone does not capture whether feed production
directly competes with human food security. To address this, `herdr`
pairs physical agricultural area (in m2) with the agroecological land
competition framework of **Mottet et al. (2017)**. The engine combines
animal dry matter intake (DMI), crop and forage productivities,
multi-output economic allocation, agroecological soil classification,
and – when an ingredient’s origin is unspecified – dynamic international
trade flows.

This vignette is the detailed reference behind the shorter mentions of
land use scattered through [Technical
Reference](https://juancbm99.github.io/herdr/articles/Technical_reference.md)
and the [General
Workflow](https://juancbm99.github.io/herdr/articles/Workflow.md).

------------------------------------------------------------------------

## Agroecological Land Classification

To distinguish between feed grown on arable land suitable for direct
human crop production and feed derived from marginal or non-cultivable
terrain, `herdr` classifies every feedstuff into an agroecological
category via the `land_type` column in
`user_data/feed_characteristics.csv`:

- **`cropland`:** Cultivated agricultural soils with arable potential
  suitable for human food crops (cereal grains, oilseeds, pulses, roots,
  agroindustrial by-products, and annual arable silages).
- **`grassland_convertible`:** Pastures situated on fertile ground that
  could agronomically be converted to arable crop production.
- **`grassland_unconvertible`:** Permanent native pastures, upland
  scrub, savannas, and extensive rangelands situated on steep, rocky, or
  marginal soils unsuitable for crop cultivation (including natural
  fresh pastures such as `atlantic_pasture_fresh`,
  `mediterranean_pasture_fresh`, and `mountain_pasture_fresh`).
- **`none`:** Inputs requiring zero direct agricultural land footprint.
  This includes synthetic amino acids, mineral supplements, vitamins,
  technological additives (e.g., bentonites and mycotoxin binders),
  post-consumer recycled fats (`yellow_grease`), and animal/marine
  co-products (excluded from feed land accounting to prevent
  double-counting livestock impacts).

------------------------------------------------------------------------

## Mathematical Formulation

Land footprint calculations follow four sequential steps for every
ingredient $`i`$ in diet $`d`$.

### 1. Land required per kilogram of feed

The baseline land required to produce one kilogram of dry matter is the
inverse of the crop’s yield, conditioned by its agroecological category:

``` math
LandPerKg_i =
\begin{cases}
0 & \text{if } land\_type_i \in \{\text{none}, \text{no\_land}\} \\
\dfrac{1}{Yield_i} & \text{if } Yield_i > 0 \\
0 & \text{otherwise}
\end{cases}
```

where $`Yield_i`$ is expressed in kg DM/ha. For non-land inputs
(`none`), the yield is strictly zeroed to avoid unnecessary database
lookups.

#### Dry Matter Yield Resolution

Depending on whether an ingredient is an arable crop or a forage,
`herdr` applies distinct dry matter handling:

- **Arable crops (`fao_crops.parquet`):** Because FAOSTAT productivities
  report yields on an as-harvested fresh weight basis ($`RawYield_i`$),
  `herdr` converts them to a dry matter yield ($`Yield_i`$) using the
  ingredient’s dry matter fraction ($`DM\_pct_i`$) from
  `user_data/feed_characteristics.csv`:

``` math
Yield_i = RawYield_i \times \left(\frac{DM\_pct_i}{100}\right)
```

- **Curated forages (`forages.parquet`) & Custom yields
  (`custom_yield_kg_ha`):** National forage statistics (such as
  MAPA 2024) and user-supplied custom yields are **already reported on a
  100% dry matter basis** (kg DM/ha). Therefore, `herdr` assigns
  $`Yield_i = RawYield_i`$ directly, avoiding redundant or erroneous
  $`DM\_pct`$ discounts.

### 2. Economic allocation adjustment

Many feed ingredients are co-products of a crop grown mainly for
something else – soybean meal alongside soybean oil, or a cereal grain
alongside its straw. Attributing the *entire* cultivated area to the
feed ingredient would overstate its footprint, so `herdr` applies the
`economic_allocation` factor from
[`mapping.csv`](https://juancbm99.github.io/herdr/articles/Technical_reference.html#mapping.csv--database-connector)
($`EconomicAllocation_i \in [0, 1]`$):

``` math
AdjustedLandPerKg_i =
\begin{cases}
0 & \text{if } land\_type_i \in \{\text{none}, \text{no\_land}\} \\
LandPerKg_i \times EconomicAllocation_i & \text{otherwise}
\end{cases}
```

If `economic_allocation` is omitted or left as `NA`, `herdr` defaults to
$`1.0`$.

### 3. Annual ingredient intake

How much of ingredient $`i`$ a cohort consumes over a year depends on
daily dry matter intake ($`DMI_{\text{kg/day}}`$), the feed category’s
share of the diet ($`Share_{cat}`$, from `diet_profiles.csv`), and the
ingredient’s share within that category ($`Share_i`$, from
`diet_ingredients.csv`):

``` math
AnnualConsumption_i =
(DMI_{\text{kg/day}} \times 365)
\times
\left(\frac{Share_{cat}}{100}\right)
\times
\left(\frac{Share_i}{100}\right)
```

$`Share_{cat}`$ corresponds to whichever of forage, concentrate, milk,
or milk replacer the ingredient belongs to. Diets link seamlessly to
cohorts via `diet_tag`. Within any given cohort and category, `herdr`
validates that $`\sum Share_i = 100\% \pm 0.1\%`$.

### 4. Cohort land footprint

The area is computed and converted from hectares to square metres (1 ha
= 10,000 m2), then scaled up to the full cohort:

``` math
LandUsePerAnimal_i \; (\text{m2}) =
AdjustedLandPerKg_i
\times
AnnualConsumption_i
\times
10{,}000
```

``` math
TotalLandUse_i \; (\text{m2}) =
LandUsePerAnimal_i
\times
Population
```

------------------------------------------------------------------------

## Yield Resolution Hierarchy

`herdr` resolves $`Yield_i`$ through a five-tiered hierarchy, checked in
order:

1.  **Non-land inputs:** Ingredients with
    `land_type %in% c("none", "no_land")` are immediately assigned a
    yield of 0 kg DM/ha and bypass all database lookups.
2.  **User custom yields:** If `custom_yield_kg_ha` is provided in
    `diet_ingredients.csv`, `herdr` uses it directly (labeled
    `"Custom Data"`) and bypasses database queries.
3.  **FAOSTAT crop yields:** Arable crop productivities are queried from
    `fao_crops.parquet` for the resolved country and reporting year
    (filtering out regional aggregate areas where `Area Code >= 5000`).
4.  **Curated forage database (`forages.parquet`):** Because FAOSTAT
    does not provide consistent national forage productivities across
    countries, `herdr` bundles `forages.parquet`, containing official
    national statistics for Spain (**MAPA 2024**, Anuario de Estadística
    Agraria), covering pastures (Atlantic, Mediterranean, Mountain),
    annual silages, and hays:
    - **Proxy fallback for international studies:** When analyzing a
      farm outside Spain whose diets contain bundled forages without
      `custom_yield_kg_ha`, `herdr` applies the Spanish national forage
      yield as a proxy fallback and emits an informative warning
      disclaimer. Users can provide local forage yields at any time via
      `custom_yield_kg_ha`.
    - **Backward compatibility:** For existing projects, the engine
      automatically checks for `forages.parquet` and gracefully accepts
      `fao_forages.parquet` if present.
5.  **Global mix fallback:** If an arable crop has no recorded yield in
    the resolved country for that reference year, `herdr` computes a
    production-weighted mean yield from the top 3 global producing
    countries of that crop, tagged as `"Global Mix (Top 3 Producers)"`:

``` math
Yield_{\text{fallback}} = \frac{\sum_{k=1}^3 Production_k \times Yield_k}{\sum_{k=1}^3 Production_k}
```

See [Adding a New
Ingredient](https://juancbm99.github.io/herdr/articles/Adding_Ingredient.md)
for a practical, worked guide on connecting a new feed ingredient to
this hierarchy.

------------------------------------------------------------------------

## Feed Origin & Dynamic Trade Allocation

When `country_of_origin` is left as `NA` in `diet_ingredients.csv`,
`herdr` dynamically traces the true agricultural producer through
international bilateral trade flows rather than naively assigning
imports to transit hubs:

### 1. Apparent Consumption & Self-Sufficiency Ratio (SSR)

For each candidate country and crop item, domestic consumption and
domestic production sufficiency are evaluated:

``` math
ApparentConsumption =
\max(1, Production + TotalImports - TotalExports)
```

``` math
SSR =
\frac{Production}{ApparentConsumption}
```

### 2. Bounded Recursive Traceback

Many major importing nations function as re-export or processing hubs
(for instance, the Netherlands importing raw soybeans through the Port
of Rotterdam and crushing/re-exporting meal to other European
countries). Assigning the transit hub’s national yield would
misrepresent the crop’s actual agricultural footprint.

To prevent this, `herdr` uses a bounded recursive traceback algorithm:

1.  Starting from `farm_country` (default `"Spain"`), the algorithm
    checks whether domestic $`SSR \ge \text{ssr\_threshold}`$ (default
    $`0.70`$ / 70%).
2.  If $`SSR \ge 0.70`$, the origin is resolved to that country.
3.  If $`SSR < 0.70`$, the crop is primarily an import. The algorithm
    identifies that country’s largest bilateral supplier (`Top_Partner`)
    and steps into it.
4.  If that supplier also has $`SSR < 0.70`$ (indicating another
    re-export hub), the algorithm recursively steps to *its* top
    supplier, tracking visited countries to avoid circular trading
    loops.
5.  The search continues up to `max_trace_hops` (default 4 hops).
6.  If the supply chain loops or terminates without reaching a nation
    with $`SSR \ge 0.70`$, `herdr` defaults to the production-weighted
    **Global Mix (Top 3 Producers)** described above, or `farm_country`
    if global data is missing.

> **Note:** The bilateral trade database (`fao_trade_matrix.parquet`,
> ~187 MB) is downloaded automatically from GitHub Releases the first
> time an assessment requires trade resolution – see [Technical
> Reference](https://juancbm99.github.io/herdr/articles/Technical_reference.html#fao_trade_matrixparquet--dynamic-trade-background-auto-downloaded).

------------------------------------------------------------------------

## Execution & Visualization

### Function Arguments

[`calculate_land_use()`](https://juancbm99.github.io/herdr/reference/calculate_land_use.md)
supports the following arguments:

- `automatic_cycle`: Logical (default `FALSE`). When `TRUE`, links
  directly to `herdr`’s demographic population engine
  ([`calculate_population()`](https://juancbm99.github.io/herdr/reference/calculate_population.md))
  to compute life-cycle herd replacement structures.
- `farm_country`: Character (default `"Spain"`). Country where the farm
  is located, used as the starting point for trade origin tracing and
  domestic yield lookups.
- `year`: Numeric (default `2024`). Reference year for FAO
  productivities and trade data.
- `data_dir`: Path (default `"user_data"`). Location of editable input
  CSVs and reference parquet files.
- `max_trace_hops`: Numeric (default `4`). Maximum hops through trade
  partner hubs.
- `ssr_threshold`: Numeric (default `0.70`). Minimum self-sufficiency
  ratio to identify a true producer country.
- `saveoutput`: Logical (default `TRUE`). When `TRUE`, writes results to
  `output/land_use.csv`.

### Running the Assessment

``` r

# Standalone land use calculation
land_results <- calculate_land_use(
  farm_country    = "Spain",
  year            = 2024,
  automatic_cycle = FALSE,
  data_dir        = "user_data",
  saveoutput      = TRUE
)

# Integrated execution via full farm impact assessment
full_results <- generate_impact_assessment(
  farm_country    = "Spain",
  year            = 2024,
  automatic_cycle = FALSE,
  data_dir        = "user_data"
)
```

### Output Schema

The output dataframe (and `output/land_use.csv`) provides a complete
agroecological and spatial breakdown:

| Column | Description |
|:---|:---|
| `region`, `subregion`, `animal_tag`, `class_flex` | Cohort identification and geographic stratum |
| `animal_type`, `animal_subtype` | Species and production system category |
| `ingredient` | Specific feedstuff name from diet |
| `land_type` | Agroecological classification (`cropland`, `grassland_convertible`, `grassland_unconvertible`, `none`) |
| `country_of_origin` | Resolved producing country (or `"Custom Data"` / `"Global Mix"`) |
| `DM_pct` | Dry matter percentage of the ingredient |
| `dm_yield` | Applied dry matter yield (kg DM/ha) |
| `population` | Headcount of the cohort |
| `land_use_per_animal_m2` | Land occupation per individual animal (m2/year) |
| `total_land_use_m2` | Total cohort land footprint (m2/year) |

### Visualizing Agroecological Profiles

[`plot_herdr_results()`](https://juancbm99.github.io/herdr/reference/plot_herdr_results.md)
automatically detects the presence of `land_type` and renders stacked
bar charts categorized by agroecological land competition:

``` r

# Stacked land use breakdown across cohorts
land <- calculate_land_use()
plot_herdr_results(land)
```

Non-land inputs (`none`) are automatically filtered from the
visualization to focus exclusively on productive land footprint.

------------------------------------------------------------------------

## References

- Mottet, A., de Haan, C., Falcucci, A., Tempio, G., Opio, C., &
  Gerber, P. (2017). *More fuel than food: Global food security and the
  role of livestock feed*. **Global Food Security**, 14, 1–8.
- MAPA (2024). *Anuario de Estadística Agraria*. Ministerio de
  Agricultura, Pesca y Alimentación, Gobierno de España.

## Next steps

- [Technical
  Reference](https://juancbm99.github.io/herdr/articles/Technical_reference.md)
  – full definitions for `mapping.csv`, `feed_characteristics.csv`, and
  background databases.
- [Adding a New
  Ingredient](https://juancbm99.github.io/herdr/articles/Adding_Ingredient.md)
  – how `land_type`, `economic_allocation`, and `yield_name` connect a
  new feed ingredient.
- [General
  Workflow](https://juancbm99.github.io/herdr/articles/Workflow.md) –
  where land use fits into the complete farm impact pipeline.
