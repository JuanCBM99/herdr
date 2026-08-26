# Dairy Cattle Spain: North/South in different Life Stages

## Advanced Assessment: Managing Complexity with `class_flex`

This guide tackles a high-complexity scenario for **mature dairy
cattle**, splitting the population both by **geography** (North
vs. South Spain) and by **physiological state**, using the `class_flex`
column to distinguish the **lactation phase** from the **dry phase**.

> 📂 **All files referenced below live in `user_data/`.**

### What you’ll edit

| Category | Files |
|:---|:---|
| **User inputs** *(fill these in)* | `livestock_census.csv` · `diet_profiles.csv` · `diet_ingredients.csv` · `ruminant_definitions.csv` · `livestock_weights.csv` · `manure_management.csv` |
| **If using the automatic herd cycle** | `reproduction_parameters.csv` |
| **Reference libraries** *(consult only)* | `feed_characteristics.csv` · `mapping.csv` · `fao_forages.parquet` · `fao_crops.parquet` · `ipcc_coefficients.csv` · `ipcc_mm.csv` |

Full details on every file above are in the [Technical
Reference](https://juancbm99.github.io/herdr/articles/Technical_reference.md)
vignette. This guide builds directly on the [Moderate
Example](https://juancbm99.github.io/herdr/articles/Moderate_Example.md)
— start there first if you haven’t used `class_flex` before.

------------------------------------------------------------------------

### Step 1 — Multi-Dimensional Census (`livestock_census.csv`)

The census links each population figure to a subregion **and** a
physiological state — essential for accurate annual emission averages.

| animal_tag            | region  | subregion | class_flex        | population |
|:----------------------|:--------|:----------|:------------------|-----------:|
| `mature_dairy_cattle` | `spain` | `north`   | `dry_phase`       |  88,065.87 |
| `mature_dairy_cattle` | `spain` | `north`   | `lactation_phase` | 448,921.13 |
| `mature_dairy_cattle` | `spain` | `south`   | `dry_phase`       |  51,118.64 |
| `mature_dairy_cattle` | `spain` | `south`   | `lactation_phase` | 260,580.36 |

------------------------------------------------------------------------

### Step 2 — Phase-Specific Nutrition

`class_flex` lets you define a different forage/concentrate ratio, and
different ingredients, for each life stage.

#### A. Diet profiles — `diet_profiles.csv`

| diet_tag | region | subregion | class_flex | forage | concentrate | milk | milk_replacer |
|:---|:---|:---|:---|---:|---:|---:|---:|
| `diet_dairy_mature` | `spain` | `north` | `dry_phase` | 70 | 30 | 0 | 0 |
| `diet_dairy_mature` | `spain` | `north` | `lactation_phase` | 55 | 45 | 0 | 0 |
| `diet_dairy_mature` | `spain` | `south` | `dry_phase` | 54 | 46 | 0 | 0 |
| `diet_dairy_mature` | `spain` | `south` | `lactation_phase` | 50 | 50 | 0 | 0 |

#### B. Ingredient breakdown — `diet_ingredients.csv`

Ingredients must be assigned precisely to their `subregion` and
`class_flex`. Here’s the South/lactation profile:

| diet_tag | region | subregion | class_flex | feed_category | ingredient | percentage | country_of_origin | custom_yield_kg_ha |
|:---|:---|:---|:---|:---|:---|---:|:---|---:|
| `diet_dairy_mature` | `spain` | `south` | `lactation_phase` | concentrate | `corn_national` | 44.85 | `spain` | `NA` |
| `diet_dairy_mature` | `spain` | `south` | `lactation_phase` | concentrate | `soybean_meal_44_cp` | 17.12 | `NA` | `NA` |
| `diet_dairy_mature` | `spain` | `south` | `lactation_phase` | forage | `corn_silage_25_30` | 55.00 | `spain` | 12000 |

> 🌍 **Missing origins:** as before, an `NA` in `country_of_origin` is
> resolved automatically by the dynamic FAO background allocation engine
> — see [Land Use
> Methodology](https://juancbm99.github.io/herdr/articles/land_use.md)
> for how.
>
> 🎯 **Overriding the yield:** the corn silage row sets
> `custom_yield_kg_ha` to `12000`, so herdr uses that farm-specific
> figure directly for the land-use footprint instead of a
> FAO/trade-based lookup.

------------------------------------------------------------------------

### Step 3 — Physiological Definitions (`ruminant_definitions.csv`)

This is the most technical file — it determines energy requirements
under IPCC Tier 2. Notice how `milk_yield` and `cfi` change between
phases.

| animal_tag | region | subregion | class_flex | cfi | ca | c | milk_yield | fat_content | c_pregnancy |
|:---|:---|:---|:---|:---|:---|:---|---:|---:|:---|
| `mature_dairy_cattle` | `spain` | `north` | `lactation_phase` | `cattle_buffalo [lactating]` | `stall` | `females` | 8,295 | 3.73 | `cattle and buffalo` |
| `mature_dairy_cattle` | `spain` | `north` | `dry_phase` | `cattle/buffalo` | `stall` | `females` | 0 | 0 | `cattle and buffalo` |
| `mature_dairy_cattle` | `spain` | `south` | `lactation_phase` | `cattle_buffalo [lactating]` | `stall` | `females` | 9,044 | 3.73 | `cattle and buffalo` |
| `mature_dairy_cattle` | `spain` | `south` | `dry_phase` | `cattle/buffalo` | `stall` | `females` | 0 | 0 | `cattle and buffalo` |

------------------------------------------------------------------------

### Step 4 — Body Weights (`livestock_weights.csv`)

Accurate weights are essential to calculate Maintenance Energy
($`NE_m`$) and to validate the animal’s maximum physical intake capacity
(DMI as % of body weight).

| animal_tag | region | subregion | class_flex | adult_weight | productive_period | initial_weight | final_weight |
|:---|:---|:---|:---|---:|---:|---:|---:|
| `mature_dairy_cattle` | `spain` | `north` | `dry_phase` | 675 | 365 | 675 | 675 |
| `mature_dairy_cattle` | `spain` | `north` | `lactation_phase` | 675 | 365 | 675 | 675 |
| `mature_dairy_cattle` | `spain` | `south` | `dry_phase` | 675 | 365 | 675 | 675 |
| `mature_dairy_cattle` | `spain` | `south` | `lactation_phase` | 675 | 365 | 675 | 675 |

------------------------------------------------------------------------

### Step 5 — Advanced Manure Management (`manure_management.csv`)

At this level of granularity you can specify climate zone and system
variant per cohort, which determines the methane conversion factors
used.

| animal_tag | region | subregion | class_flex | system_base | system_variant | management_months | system_climate | system_subclimate | climate_zone | climate_moisture | b_0 | allocation |
|:---|:---|:---|:---|:---|:---|---:|:---|:---|:---|:---|:---|---:|
| `mature_dairy_cattle` | `spain` | `north` | `lactation_phase` | `anaerobic_lagoon` | `uncovered` | — | `cool` | `temperate` | `zone_moist` | `wet` | `dairy_cattle_high_productivity` | 0.0537 |
| `mature_dairy_cattle` | `spain` | `north` | `lactation_phase` | `liquid_slurry` | `with_natural_crust_cover` | 3 | `cool` | `temperate` | `zone_moist` | `wet` | `dairy_cattle_high_productivity` | 0.3432 |
| `mature_dairy_cattle` | `spain` | `south` | `dry_phase` | `solid_storage` | — | — | `warm` | — | — | `dry` | `dairy_cattle_high_productivity` | 0.0694 |
| `mature_dairy_cattle` | `spain` | `south` | `dry_phase` | `deep_bedding` | `no_mixing` | `>1` | `warm` | `temperate` | `zone_dry` | `dry` | `dairy_cattle_high_productivity` | 0.0131 |

> ⚠️ **Illustrative snippet:** as in the Moderate example, the rows
> above don’t sum to 1.0 per cohort — some rows were omitted here for
> brevity. In a real project, `allocation` must sum to exactly 1.0 for
> every `animal_tag`/`region`/`subregion`/`class_flex` combination.

------------------------------------------------------------------------

### Step 6 — Final Execution

When running the analysis, herdr processes each row as a unique
“animal–state–region” combination before aggregating the results.

``` r

library(herdr)

# The model calculates impact for each phase separately
results <- generate_impact_assessment(
  automatic_cycle = FALSE,
  farm_country = "Spain",
  year = 2015
)
```

------------------------------------------------------------------------

### Next steps

- [Moderate
  Example](https://juancbm99.github.io/herdr/articles/Moderate_Example.md)
  — the North/South split this example builds on, without `class_flex`.
- [Basic
  Assessment](https://juancbm99.github.io/herdr/articles/Easy_Example.md)
  — the simplest single-region starting point.
- [Technical
  Reference](https://juancbm99.github.io/herdr/articles/Technical_reference.md)
  — every file and column explained in detail.
