# Basic Assessment: A Step-by-Step Guide

## Getting Started with a Basic Assessment

This guide walks through a **Basic Assessment** for **mature dairy
cattle in Spain (2015)**, using a single national-level identifier to
link census data, nutrition, and environmental impact from start to
finish.

> 📂 **All files referenced below live in `user_data/`.**

### What you’ll edit

| Category | Files |
|:---|:---|
| **User inputs** *(fill these in)* | `livestock_census.csv` · `diet_profiles.csv` · `diet_ingredients.csv` · `ruminant_definitions.csv` · `livestock_weights.csv` · `manure_management.csv` |
| **If using the automatic herd cycle** | `reproduction_parameters.csv` |
| **Reference libraries** *(consult only)* | `feed_characteristics.csv` · `mapping.csv` · `fao_forages.parquet` · `fao_crops.parquet` · `ipcc_coefficients.csv` · `ipcc_mm.csv` |

Full details on every file above are in the [Technical
Reference](https://juancbm99.github.io/herdr/articles/Technical_reference.md)
vignette.

------------------------------------------------------------------------

### Step 1 — The Census

Open `livestock_census.csv`. This is where you define who is on the
farm. For this basic example, we work at national level, so `subregion`
and `class_flex` are left blank.

| animal_tag            | region  | subregion | class_flex | population |
|:----------------------|:--------|:----------|:-----------|-----------:|
| `mature_dairy_cattle` | `spain` |           |            |    848,686 |

------------------------------------------------------------------------

### Step 2 — Designing the Diet

The diet is filled in manually based on nutritional reports
(e.g. *Informe Zootécnico 2015*).

#### A. Check available ingredients

Open `feed_characteristics.csv` to see the list of available feed items.
You can also add new ingredients here, as long as every required column
is filled in.

#### B. Define the profile — `diet_profiles.csv`

| diet_tag            | region  | forage | concentrate | milk | milk_replacer |
|:--------------------|:--------|-------:|------------:|-----:|--------------:|
| `diet_dairy_mature` | `spain` |     60 |          40 |    0 |             0 |

#### C. Ingredient breakdown — `diet_ingredients.csv`

Repeat the same `diet_tag` and `region` once per ingredient.

| diet_tag | region | feed_category | ingredient | percentage | country_of_origin | custom_yield_kg_ha |
|:---|:---|:---|:---|---:|:---|---:|
| `diet_dairy_mature` | `spain` | forage | `corn_silage` | 100 | `spain` | `NA` |
| `diet_dairy_mature` | `spain` | concentrate | `barley_grain` | 50 | `NA` | `NA` |
| `diet_dairy_mature` | `spain` | concentrate | `soybean_meal` | 50 | `NA` | 5000 |

> ✅ **Percentages sum to 100% *within* each category** — here, 50%
> barley + 50% soybean meal make up 100% of the concentrate portion.
>
> 🌍 **Missing origins:** leave `country_of_origin` as `NA` when you
> don’t know where a feed comes from. herdr resolves it automatically
> using FAO trade data — see [Land Use
> Methodology](https://juancbm99.github.io/herdr/articles/land_use.md)
> for how.
>
> 🎯 **Overriding the yield:** `custom_yield_kg_ha` lets you skip the
> FAO/trade lookup entirely for a given ingredient. If you already know
> the real yield for your farm or region — e.g. `5000` kg/ha — enter it
> here and herdr uses that value directly for the land-use calculation,
> ignoring `country_of_origin` and any FAO database lookup for that row.

------------------------------------------------------------------------

### Step 3 — Animal Categories and Coefficients

Open `ruminant_definitions.csv`. This file links your animal to the IPCC
Tier 2 equations — each of `cfi`, `ca`, `c`, and `c_pregnancy` is a
separate lookup into `ipcc_coefficients.csv`, so use the **exact
description** for each as it appears there.

| animal_tag | region | diet_tag | cfi | ca | c | milk_yield | fat_content | c_pregnancy |
|:---|:---|:---|:---|:---|:---|---:|---:|:---|
| `mature_dairy_cattle` | `spain` | `diet_dairy_mature` | `cattle_buffalo [lactating]` | `stall` | `females` | 8,894.38 | 3.73 | `cattle and buffalo` |

------------------------------------------------------------------------

### Step 4 — Body Weights

Open `livestock_weights.csv`. Make sure the identifying keys match the
census exactly.

| animal_tag | region | adult_weight | productive_period | initial_weight | final_weight |
|:---|:---|---:|---:|---:|---:|
| `mature_dairy_cattle` | `spain` | 675 | 365 | 675 | 675 |

------------------------------------------------------------------------

### Step 5 — Manure Management (`manure_management.csv`)

1.  Check `ipcc_mm.csv` for valid `system_base`, `system_variant`, and
    `climate_zone` combinations — or browse the [Manure System
    Guide](https://juancbm99.github.io/herdr/articles/Manure.md) for the
    same information in narrative form.
2.  If a cohort splits its manure across more than one system,
    **duplicate the row** and divide the `allocation` between them so it
    sums to 1.0.

| animal_tag | region | system_base | system_variant | management_months | system_climate | system_subclimate | climate_zone | climate_moisture | b_0 | allocation |
|:---|:---|:---|:---|---:|:---|:---|:---|:---|:---|---:|
| `mature_dairy_cattle` | `spain` | `liquid_slurry` | `with_natural_crust_cover` | 3 | `warm` | `temperate` | `zone_dry` | `dry` | `dairy_cattle_high_productivity` | 0.3 |
| `mature_dairy_cattle` | `spain` | `solid_storage` | — | — | `warm` | — | — | `default` | `dairy_cattle_high_productivity` | 0.7 |

> ℹ️ `solid_storage` is a Standard system (see the [Manure System
> Guide](https://juancbm99.github.io/herdr/articles/Manure.md)) — it
> doesn’t need `management_months`, `system_variant`,
> `system_subclimate`, or `climate_zone`, so those columns are left
> blank.

------------------------------------------------------------------------

### Step 6 — Running the Analysis

Once every CSV above is complete, run the assessment from R:

``` r

library(herdr)

results <- generate_impact_assessment(
  automatic_cycle = FALSE,
  farm_country = "Spain",
  year = 2015
)
```

------------------------------------------------------------------------

### Next steps

- [General
  Workflow](https://juancbm99.github.io/herdr/articles/Workflow.md) —
  the full step-by-step guide this example is based on.
- [Technical
  Reference](https://juancbm99.github.io/herdr/articles/Technical_reference.md)
  — every file and column explained in detail.
- [Moderate
  Example](https://juancbm99.github.io/herdr/articles/Moderate_Example.md)
  / [Hard
  Example](https://juancbm99.github.io/herdr/articles/Difficult_Example.md)
  — build on this with multi-region herds and mixed manure systems.
