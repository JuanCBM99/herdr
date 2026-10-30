# Dairy Cattle Spain: North/South

## Moderate Assessment: North/South Regional Split

This guide walks through a **Moderate Assessment** for **mature dairy
cattle in Spain (2015)**, using a multi-farm / multi-regional structure
(`North` vs. `South`) to capture differences in diet composition, milk
yield, and manure climate zones.

> 📂 **All files referenced below live in `user_data/` and correspond to
> the pre-configured `inst/Examples/Level2_Spain_Dairy_Cattle_2015/`
> baseline.**

### What you’ll edit

| Category | Files |
|:---|:---|
| **User inputs** *(fill these in)* | `livestock_census.csv` · `diet_profiles.csv` · `diet_ingredients.csv` · `ruminant_definitions.csv` · `livestock_weights.csv` · `manure_management.csv` |
| **Reference libraries** *(consult only)* | `feed_characteristics.csv` · `mapping.csv` · `forages.parquet` · `fao_crops.parquet` · `ipcc_coefficients.csv` · `ipcc_mm.csv` · `ecoalim.parquet` |

Full details on every file above are in the [Technical
Reference](https://juancbm99.github.io/herdr/articles/Technical_reference.md)
vignette. This guide builds on the [Basic
Assessment](https://juancbm99.github.io/herdr/articles/Easy_Example.md)
— start there first if this is your first time using `herdr`.

------------------------------------------------------------------------

### Step 1 — The Census (`livestock_census.csv`)

Open `livestock_census.csv`. Here we split the Spanish dairy population
into two climatic subregions (`north` and `south`) and two farm
identifiers (`farm_north` and `farm_south`). The row order you declare
here is preserved from top to bottom in all output tables and charts.

| farm_id      | animal_tag            | region  | subregion | population |
|:-------------|:----------------------|:--------|:----------|-----------:|
| `farm_north` | `mature_dairy_cattle` | `spain` | `north`   |    536,987 |
| `farm_south` | `mature_dairy_cattle` | `spain` | `south`   |    311,699 |

------------------------------------------------------------------------

### Step 2 — Designing Regional Diets

Forage availability differs between Atlantic/Northern Spain and
Mediterranean/Southern Spain, so separate diets are defined for each
region.

#### A. Define the profile — `diet_profiles.csv`

The forage proportion is higher in the North (55%) than in the South
(50%).

| diet_tag | forage_share | concentrate_share | milk_share | milk_replacer_share |
|:---|---:|---:|---:|---:|
| `diet_dairy_mature_north` | 55 | 45 | 0 | 0 |
| `diet_dairy_mature_south` | 50 | 50 | 0 | 0 |

#### B. Ingredient breakdown — `diet_ingredients.csv`

Within each `ingredient_type`, `ingredient_share` must sum to 100%. Here
is an excerpt of the concentrate portion for Spain South:

| diet_tag | ingredient_type | ingredient | ingredient_share | country_of_origin | custom_yield_kg_ha |
|:---|:---|:---|---:|:---|---:|
| `diet_dairy_mature_south` | `concentrate` | `corn` | 44.85 | `spain` | `NA` |
| `diet_dairy_mature_south` | `concentrate` | `soybean_meal_44_cp` | 17.12 | `NA` | `NA` |
| `diet_dairy_mature_south` | `concentrate` | `rapeseed_meal_00_33_cp` | 28.71 | `NA` | 3200 |

> 🌍 **Missing origins:** `country_of_origin` for soybean and rapeseed
> meals is left as `NA`. `herdr` resolves both automatically using the
> FAO dynamic bilateral trade engine and a 70% domestic self-sufficiency
> threshold — see [Land Use
> Methodology](https://juancbm99.github.io/herdr/articles/land_use.md).
>
> 🎯 **Overriding the yield:** setting `custom_yield_kg_ha = 3200`
> instructs `herdr` to use that farm-specific dry matter yield directly
> instead of querying FAOSTAT.

------------------------------------------------------------------------

### Step 3 — Animal Categories and Coefficients (`ruminant_definitions.csv`)

Open `ruminant_definitions.csv`. Each cohort specifies its canonical
taxonomy (`animal_type = "cattle"`, `animal_subtype = "dairy"`,
`production_role = "mature"`), its diet, milk yield, and IPCC
coefficients.

| farm_id | animal_tag | region | subregion | animal_type | animal_subtype | production_role | diet_tag | cfi | ca | c | milk_yield_kg_year | fat_content_pct | protein_content_pct | c_pregnancy_cattle | pregnancy_rate | replacement_rate |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|---:|---:|---:|:---|---:|---:|
| `farm_north` | `mature_dairy_cattle` | `spain` | `north` | `cattle` | `dairy` | `mature` | `diet_dairy_mature_north` | `cattle_buffalo [lactating cows]` | `stall` | `females` | 8,295.0 | 3.73 | 3.39 | `cattle and buffalo` | 0.90 | 0.27 |
| `farm_south` | `mature_dairy_cattle` | `spain` | `south` | `cattle` | `dairy` | `mature` | `diet_dairy_mature_south` | `cattle_buffalo [lactating cows]` | `stall` | `females` | 9,044.0 | 3.73 | 3.39 | `cattle and buffalo` | 0.90 | 0.27 |

------------------------------------------------------------------------

### Step 4 — Body Weights (`livestock_weights.csv`)

Open `livestock_weights.csv`. Make sure the four primary demographic
keys (`farm_id`, `animal_tag`, `region`, `subregion`) match the census
exactly.

| farm_id | animal_tag | region | subregion | adult_weight_kg | productive_period_days | initial_weight_kg | final_weight_kg |
|:---|:---|:---|:---|---:|---:|---:|---:|
| `farm_north` | `mature_dairy_cattle` | `spain` | `north` | 675 | 365 | 675 | 675 |
| `farm_south` | `mature_dairy_cattle` | `spain` | `south` | 675 | 365 | 675 | 675 |

------------------------------------------------------------------------

### Step 5 — Manure Management (`manure_management.csv`)

You can define multiple manure systems for the same cohort by splitting
the `allocation` across rows — the sum per
`(farm_id, animal_tag, region, subregion)` cohort must equal exactly
**1.0**. Notice how `system_climate` (`cool` in the North vs. `warm` in
the South) and `climate_moisture` (`wet` vs. `dry`) reflect regional
agroclimatic conditions:

| farm_id | animal_tag | region | subregion | system_base | system_variant | management_months | system_climate | system_subclimate | climate_zone | climate_moisture | b_0 | allocation |
|:---|:---|:---|:---|:---|:---|---:|:---|:---|:---|:---|:---|---:|
| `farm_north` | `mature_dairy_cattle` | `spain` | `north` | `anaerobic_lagoon` | `uncovered` | — | `cool` | `temperate` | `zone_moist` | `wet` | `dairy_cattle_high_productivity` | 0.0537 |
| `farm_north` | `mature_dairy_cattle` | `spain` | `north` | `liquid_slurry` | `with_natural_crust_cover` | 3 | `cool` | `temperate` | `zone_moist` | `wet` | `dairy_cattle_high_productivity` | 0.3432 |
| `farm_north` | `mature_dairy_cattle` | `spain` | `north` | `solid_storage` | — | — | `cool` | — | — | `default` | `dairy_cattle_high_productivity` | 0.3551 |
| `farm_south` | `mature_dairy_cattle` | `spain` | `south` | `solid_storage` | — | — | `warm` | — | — | `default` | `dairy_cattle_high_productivity` | 0.3551 |

> ⚠️ **Illustrative snippet:** Some rows were omitted above for brevity.
> In
> `inst/Examples/Level2_Spain_Dairy_Cattle_2015/manure_management.csv`,
> all allocations for `farm_north` and `farm_south` sum to exactly
> **1.0**.

------------------------------------------------------------------------

### Step 6 — Running the Analysis & Comparing Regions

``` r

library(herdr)

results <- generate_impact_assessment(
  automatic_cycle = FALSE,
  farm_country    = "Spain",
  year            = 2015,
  gwp_report      = "AR5"
)

# Inspect regional totals and product intensities
results$totals
results$intensities

# Compare North vs. South milk carbon intensity (kg CO2e / kg FPCM milk)
plot_herdr_results(
  df              = results,
  group_cols      = c("farm_id", "subregion"),
  func_name       = "generate_impact_assessment",
  impact_category = "ghg",
  functional_unit = "milk"
)
```

------------------------------------------------------------------------

### Next steps

- [Basic
  Assessment](https://juancbm99.github.io/herdr/articles/Easy_Example.md)
  — the single-region version this example builds on.
- [Advanced Multi-Cohort
  Example](https://juancbm99.github.io/herdr/articles/Difficult_Example.md)
  — multi-cohort swine & poultry assessment demonstrating `mature`,
  `replacement`, and `slaughter` lifecycle integration.
- [Technical
  Reference](https://juancbm99.github.io/herdr/articles/Technical_reference.md)
  — every file and column explained in detail.
