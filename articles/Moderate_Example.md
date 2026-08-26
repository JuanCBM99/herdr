# Dairy Cattle Spain: North/South

## Moderate Assessment: North/South Regional Split

This guide walks through a **Moderate Assessment** for **mature dairy
cattle in Spain (2015)**, using the multi-regional structure (North and
South) to improve precision over a single national figure.

> 📂 **All files referenced below live in `user_data/`.**

### What you’ll edit

| Category | Files |
|:---|:---|
| **User inputs** *(fill these in)* | `livestock_census.csv` · `diet_profiles.csv` · `diet_ingredients.csv` · `ruminant_definitions.csv` · `livestock_weights.csv` · `manure_management.csv` |
| **If using the automatic herd cycle** | `reproduction_parameters.csv` |
| **Reference libraries** *(consult only)* | `feed_characteristics.csv` · `mapping.csv` · `fao_forages.parquet` · `fao_crops.parquet` · `ipcc_coefficients.csv` · `ipcc_mm.csv` |

Full details on every file above are in the [Technical
Reference](https://juancbm99.github.io/herdr/articles/Technical_reference.md)
vignette. This guide builds on the [Basic
Assessment](https://juancbm99.github.io/herdr/articles/Easy_Example.md)
— start there first if this is your first time using herdr.

------------------------------------------------------------------------

### Step 1 — The Census

Open `livestock_census.csv`. Here we split the Spanish dairy population
into two climatic regions using `subregion`.

| animal_tag            | region  | subregion | class_flex | population |
|:----------------------|:--------|:----------|:-----------|-----------:|
| `mature_dairy_cattle` | `spain` | `north`   |            |    536,987 |
| `mature_dairy_cattle` | `spain` | `south`   |            |    311,699 |

------------------------------------------------------------------------

### Step 2 — Designing the Diet

Forage availability differs between North and South, so both the diet
profile and its ingredients are defined per subregion.

#### A. Define the profile — `diet_profiles.csv`

The forage proportion is higher in the North (55%) than in the South
(50%).

| diet_tag            | region  | subregion | forage | concentrate | milk |
|:--------------------|:--------|:----------|-------:|------------:|-----:|
| `diet_dairy_mature` | `spain` | `north`   |     55 |          45 |    0 |
| `diet_dairy_mature` | `spain` | `south`   |     50 |          50 |    0 |

#### B. Ingredient breakdown — `diet_ingredients.csv`

Within each `feed_category`, `percentage` must sum to 100%. Here’s the
concentrate portion for Spain South:

| diet_tag | region | subregion | feed_category | ingredient | percentage | country_of_origin | custom_yield_kg_ha |
|:---|:---|:---|:---|:---|---:|:---|---:|
| `diet_dairy_mature` | `spain` | `south` | concentrate | `corn_national` | 44.85 | `spain` | `NA` |
| `diet_dairy_mature` | `spain` | `south` | concentrate | `soybean_meal_44_cp` | 17.12 | `NA` | `NA` |
| `diet_dairy_mature` | `spain` | `south` | concentrate | `rapeseed_meal_00_33_cp` | 28.71 | `NA` | 3200 |

> 🌍 **Missing origins:** `country_of_origin` for the soybean and
> rapeseed meals is left as `NA`. herdr resolves both automatically
> using the FAO dynamic background allocation engine and a 70%
> self-sufficiency rule — see [Land Use
> Methodology](https://juancbm99.github.io/herdr/articles/land_use.md)
> for how.
>
> 🎯 **Overriding the yield:** the rapeseed meal row sets
> `custom_yield_kg_ha` to `3200`, so herdr uses that figure directly for
> its land-use footprint instead of looking up a FAO/trade-based yield —
> regardless of what `country_of_origin` says.

------------------------------------------------------------------------

### Step 3 — Animal Categories and Coefficients

Open `ruminant_definitions.csv`. This file links your animal tags to the
IPCC physiological equations — each of `cfi`, `ca`, `c`, and
`c_pregnancy` is a separate lookup into `ipcc_coefficients.csv`.

| animal_tag | region | subregion | diet_tag | cfi | ca | c | milk_yield | fat_content | c_pregnancy | animal_type |
|:---|:---|:---|:---|:---|:---|:---|---:|---:|:---|:---|
| `mature_dairy_cattle` | `spain` | `north` | `diet_dairy_mature` | `cattle_buffalo [lactating]` | `stall` | `females` | 8,295.0 | 3.73 | `cattle and buffalo` | `cattle` |
| `mature_dairy_cattle` | `spain` | `south` | `diet_dairy_mature` | `cattle_buffalo [lactating]` | `stall` | `females` | 9,044.0 | 3.73 | `cattle and buffalo` | `cattle` |

------------------------------------------------------------------------

### Step 4 — Body Weights

Open `livestock_weights.csv`. Make sure the identifying keys match the
census exactly — these weights validate the dry matter intake (DMI)
limits.

| animal_tag | region | subregion | adult_weight | productive_period | initial_weight | final_weight |
|:---|:---|:---|---:|---:|---:|---:|
| `mature_dairy_cattle` | `spain` | `north` | 675 | 365 | 675 | 675 |
| `mature_dairy_cattle` | `spain` | `south` | 675 | 365 | 675 | 675 |

------------------------------------------------------------------------

### Step 5 — Manure Management (`manure_management.csv`)

You can define multiple manure systems for the same cohort by splitting
the `allocation` across rows — the sum per
`animal_tag`/`region`/`subregion` combination must equal 1.0.

| animal_tag | region | subregion | system_base | system_variant | management_months | system_climate | system_subclimate | climate_zone | climate_moisture | allocation |
|:---|:---|:---|:---|:---|---:|:---|:---|:---|:---|---:|
| `mature_dairy_cattle` | `spain` | `north` | `anaerobic_lagoon` | `uncovered` | — | `cool` | `temperate` | `zone_moist` | `wet` | 0.0537 |
| `mature_dairy_cattle` | `spain` | `north` | `liquid_slurry` | `with_natural_crust_cover` | 3 | `cool` | `temperate` | `zone_moist` | `wet` | 0.3432 |
| `mature_dairy_cattle` | `spain` | `north` | `solid_storage` | — | — | `cool` | — | — | `default` | 0.3551 |
| `mature_dairy_cattle` | `spain` | `south` | `solid_storage` | — | — | `warm` | — | — | `default` | 0.3551 |

> ⚠️ **Illustrative snippet:** the rows above don’t sum to 1.0 per
> cohort — some rows were omitted here for brevity. In a real project,
> always check that `allocation` sums to exactly 1.0 for every
> `animal_tag`/`region`/`subregion` combination before running the
> model.

------------------------------------------------------------------------

### Step 6 — Running the Analysis

Once your CSVs are updated in `user_data/`, run the assessment. herdr
automatically performs biological validations (e.g. checking that DMI
doesn’t exceed ~5.5% of body weight) and assigns trade impacts for any
missing origins.

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

- [Basic
  Assessment](https://juancbm99.github.io/herdr/articles/Easy_Example.md)
  — the single-region version this example builds on.
- [Hard
  Example](https://juancbm99.github.io/herdr/articles/Difficult_Example.md)
  — adds physiological life stages (`class_flex`) on top of this
  North/South split.
- [Technical
  Reference](https://juancbm99.github.io/herdr/articles/Technical_reference.md)
  — every file and column explained in detail.
