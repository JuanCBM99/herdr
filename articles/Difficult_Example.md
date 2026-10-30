# Intensive Multi-Cohort Assessment: Swine & Poultry Systems

## Advanced Assessment: Multi-Cohort Swine & Poultry Systems (`Monogastric_Example`)

This guide walks through an **Advanced Multi-Cohort Assessment** for
**monogastric livestock in Spain (`swine` and `poultry`)**,
demonstrating how `herdr` models full pyramidal herd structures
combining **`mature`** breeding stock, **`replacement`** rearing
cohorts, and **`slaughter`** finishing cohorts within a single project.

> 📂 **All files referenced below correspond to the pre-configured
> `inst/Examples/Monogastric_Example/` dataset.**

### What you’ll edit

| Category | Files |
|:---|:---|
| **User inputs** *(fill these in)* | `livestock_census.csv` · `diet_profiles.csv` · `diet_ingredients.csv` · `monogastric_definitions.csv` · `livestock_weights.csv` · `manure_management.csv` |
| **Reference libraries** *(consult only)* | `feed_characteristics.csv` · `mapping.csv` · `fao_crops.parquet` · `ipcc_coefficients.csv` · `ipcc_mm.csv` · `ecoalim.parquet` |

------------------------------------------------------------------------

### Step 1 — Multi-Cohort Census (`livestock_census.csv`)

In `livestock_census.csv`, we declare the **Annual Average Population
(AAP)**—i.e., the average number of occupied barn places present on any
given day of the year—for 10 cohorts across three distinct monogastric
production enterprises on `farm_1`:

1.  **Poultry Meat Enterprise (`poultry` / `meat`):** `broilers`
    (`slaughter`), `replacement_meat_pullets` (`replacement`), and
    `breeder_meat_hens` (`mature`).
2.  **Poultry Layer Enterprise (`poultry` / `layer`):**
    `replacement_layer_pullets` (`replacement`), `laying_hens`
    (`mature`), and `free_range_laying_hens` (`mature`).
3.  **Swine Meat Enterprise (`swine` / `meat`):** `fattening_pigs`
    (`slaughter`), `replacement_sows` (`replacement`), `breeder_sows`
    (`mature`), and `boars` (`mature`).

| farm_id  | animal_tag                  | region  | subregion | population |
|:---------|:----------------------------|:--------|:----------|-----------:|
| `farm_1` | `broilers`                  | `spain` |           | 73,219,430 |
| `farm_1` | `replacement_meat_pullets`  | `spain` |           |  2,088,485 |
| `farm_1` | `breeder_meat_hens`         | `spain` |           |  3,999,963 |
| `farm_1` | `replacement_layer_pullets` | `spain` |           |  9,265,000 |
| `farm_1` | `laying_hens`               | `spain` |           | 35,640,000 |
| `farm_1` | `free_range_laying_hens`    | `spain` |           |  3,960,000 |
| `farm_1` | `fattening_pigs`            | `spain` |           |  5,237,113 |
| `farm_1` | `replacement_sows`          | `spain` |           |    287,689 |
| `farm_1` | `breeder_sows`              | `spain` |           |  1,299,996 |
| `farm_1` | `boars`                     | `spain` |           |     17,187 |

> 📌 **Preserving Input Order:** `herdr` preserves the exact row order
> of `livestock_census.csv` from top to bottom in both `results$totals`
> (Table 1), `results$intensities` (Table 2), and all
> [`plot_herdr_results()`](https://juancbm99.github.io/herdr/reference/plot_herdr_results.md)
> bar charts.

------------------------------------------------------------------------

### Step 2 — Monogastric Definitions & Strict Role Taxonomy (`monogastric_definitions.csv`)

In `monogastric_definitions.csv`, every cohort is assigned:

- **`animal_type`**: strictly `poultry` or `swine`.
- **`animal_subtype`**:
  - For `swine`: strictly **`meat`**.
  - For `poultry`: strictly **`meat`** or **`layer`**.
- **`production_role`**: strictly **`mature`**, **`replacement`**, or
  **`slaughter`**.

| farm_id | animal_tag | region | animal_type | animal_subtype | production_role | diet_tag | cfi_maintenance | frac_fat_pct | frac_protein_pct | eggs_per_year | egg_weight_g | fertility_rate | alpha | piglets_born | piglets_suckling | replacement_rate |
|:---|:---|:---|:---|:---|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| `farm_1` | `broilers` | `spain` | `poultry` | `meat` | `slaughter` | `diet_broilers` | 113 | 0.115 | 0.181 | 0 | — | — | — | — | — | — |
| `farm_1` | `replacement_meat_pullets` | `spain` | `poultry` | `meat` | `replacement` | `diet_replacement_meat_pullets` | 113 | 0.085 | 0.140 | 0 | — | — | — | — | — | — |
| `farm_1` | `breeder_meat_hens` | `spain` | `poultry` | `meat` | `mature` | `diet_breeder_meat_hens` | 125 | 0.175 | 0.166 | 211.70 | 64 | 0.83 | — | — | — | 1.2126 |
| `farm_1` | `replacement_layer_pullets` | `spain` | `poultry` | `layer` | `replacement` | `diet_replacement_layer_pullets` | 113 | 0.085 | 0.140 | 0 | — | — | — | — | — | — |
| `farm_1` | `laying_hens` | `spain` | `poultry` | `layer` | `mature` | `diet_laying_white_hens` | 125 | 0.175 | 0.166 | 297.62 | 60 | — | — | — | — | 0.7140 |
| `farm_1` | `free_range_laying_hens` | `spain` | `poultry` | `layer` | `mature` | `diet_free_range_laying_hens` | 131 | 0.175 | 0.166 | 313.82 | 60 | — | — | — | — | 0.7140 |
| `farm_1` | `fattening_pigs` | `spain` | `swine` | `meat` | `slaughter` | `diet_fattening_pigs_80_to_109_kg` | 206 | 0.266 | 0.153 | 0 | — | — | 0.60 | — | — | — |
| `farm_1` | `replacement_sows` | `spain` | `swine` | `meat` | `replacement` | `diet_replacement_sows` | 206 | 0.280 | 0.153 | 0 | — | — | 0.60 | — | — | — |
| `farm_1` | `breeder_sows` | `spain` | `swine` | `meat` | `mature` | `diet_gest2_sows` | 104.5 | 0.000 | 0.000 | 0 | — | — | 0.75 | 16.26 | 13.05 | 0.2500 |
| `farm_1` | `boars` | `spain` | `swine` | `meat` | `mature` | `diet_boar_adult` | 104.5 | 0.203 | 0.161 | 0 | — | — | 0.75 | — | — | 0.3000 |

------------------------------------------------------------------------

### Step 3 — Weights & Batch Turnover (`livestock_weights.csv`)

For short-cycle monogastrics, `productive_period_days` controls both
**Average Daily Gain (ADG)** for FEDNA Metabolizable Energy equations
and **annual batch turnover**
($`365 / \text{productive\_period\_days}`$) for slaughter output:

- **`broilers` (`slaughter`):** `productive_period_days = 42` days
  ($`365 / 42 \approx 8.69`$ batches finished per barn place per year,
  from `0.044` kg to `2.60` kg).
- **`fattening_pigs` (`slaughter`):** `productive_period_days = 110`
  days ($`365 / 110 \approx 3.32`$ batches finished per barn place per
  year, from `20.0` kg to `111.82` kg).
- **`breeder_sows` (`mature`):** `productive_period_days = 148.9` days
  (farrowing interval, yielding $`365 / 148.9 \approx 2.45`$ litters per
  sow per year), with `sows_gestation_days = 115.1` and
  `sows_lactation_days = 25`.

| farm_id | animal_tag | region | adult_weight_kg | productive_period_days | initial_weight_kg | final_weight_kg | sows_gestation_days | sows_lactation_days | piglet_birth_weight_kg | piglet_weaning_weight_kg | sow_reserve_gain_kg |
|:---|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| `farm_1` | `broilers` | `spain` | 2.60 | 42.0 | 0.044 | 2.60 | — | — | — | — | — |
| `farm_1` | `replacement_meat_pullets` | `spain` | 2.17 | 140.0 | 0.044 | 2.17 | — | — | — | — | — |
| `farm_1` | `breeder_meat_hens` | `spain` | 4.00 | 301.0 | 2.170 | 4.00 | — | — | — | — | — |
| `farm_1` | `replacement_layer_pullets` | `spain` | 1.24 | 119.0 | 0.038 | 1.24 | — | — | — | — | — |
| `farm_1` | `laying_hens` | `spain` | 1.72 | 511.0 | 1.240 | 1.72 | — | — | — | — | — |
| `farm_1` | `free_range_laying_hens` | `spain` | 2.15 | 441.0 | 1.400 | 2.15 | — | — | — | — | — |
| `farm_1` | `fattening_pigs` | `spain` | — | 110.0 | 20.000 | 111.82 | — | — | — | — | — |
| `farm_1` | `replacement_sows` | `spain` | — | 74.7 | 111.820 | 145.93 | — | — | — | — | — |
| `farm_1` | `breeder_sows` | `spain` | — | 148.9 | 200.840 | 223.19 | 115.1 | 25 | 1.4 | 5.89 | 5.0 |
| `farm_1` | `boars` | `spain` | — | 365.0 | 237.500 | 287.50 | — | — | — | — | — |

------------------------------------------------------------------------

### Step 4 — How `herdr` Attributes Intensities in Table 2 (`results$intensities`)

When you run
[`generate_impact_assessment()`](https://juancbm99.github.io/herdr/reference/generate_impact_assessment.md),
`herdr` produces two complementary tables:

1.  **Table 1 (`results$totals`):** Shows the **direct physical
    inventory** of each of the 10 cohorts (enteric $`\text{CH}_4`$,
    manure $`\text{CH}_4`$, direct/indirect $`\text{N}_2\text{O}`$,
    upstream feed fossil & LUC $`\text{CO}_2\text{e}`$, agroecological
    land, AWARE water, and per-head footprints).
2.  **Table 2 (`results$intensities`):** Computes
    **enterprise-integrated product and protein intensities** within
    each `(farm_id, region, subregion, animal_type, animal_subtype)`
    group:
    - **Poultry Meat (`poultry`, `meat`):** The overhead footprint of
      `breeder_meat_hens` (`mature`) and `replacement_meat_pullets`
      (`replacement`) is integrated with `broilers` (`slaughter`) over
      the total meat produced (broilers + culled breeders).
      `replacement_meat_pullets` shows `NA` for product intensities
      because its emissions are already absorbed into the meat
      enterprise.
    - **Poultry Layers (`poultry`, `layer`):** Each laying cohort
      (`laying_hens` and `free_range_laying_hens`) keeps its own direct
      footprint plus a population-proportional share (`90%` and `10%`,
      respectively) of the `replacement_layer_pullets` overhead, divided
      by its respective egg production (`egg_fresh_kg`).
    - **Swine Meat (`swine`, `meat`):** The breeding herd overhead
      (`breeder_sows` + `boars` + `replacement_sows`) is integrated with
      `fattening_pigs` over total carcass pork output (finished pigs +
      culled sows/boars).

------------------------------------------------------------------------

### Step 5 — Running the Assessment & Visualizing Results

``` r

library(herdr)

# Run assessment directly on the bundled Monogastric_Example
mono_dir <- system.file("Examples/Monogastric_Example", package = "herdr")

results <- generate_impact_assessment(
  data_dir        = mono_dir,
  automatic_cycle = FALSE,
  farm_country    = "Spain",
  year            = 2024,
  gwp_report      = "AR5",
  saveoutput      = FALSE
)

# Inspect Table 1 (Cohort Totals) and Table 2 (Enterprise Product Intensities)
results$totals
results$intensities

# 1. Compare Carbon Footprint per kg of Edible Protein across cohorts
plot_herdr_results(
  df              = results,
  group_cols      = "animal_tag",
  func_name       = "generate_impact_assessment",
  impact_category = "ghg",
  functional_unit = "protein"
)

# 2. Compare Egg Carbon Intensity between caged/barn vs. free-range hens
plot_herdr_results(
  df              = results,
  group_cols      = "animal_tag",
  func_name       = "generate_impact_assessment",
  impact_category = "ghg",
  functional_unit = "egg"
)
```

------------------------------------------------------------------------

### Next steps

- [Herd Demography & Life
  Cycles](https://juancbm99.github.io/herdr/articles/Herd_Demography.md)
  — how `automatic_cycle = TRUE` derives `replacement_sows`,
  `fattening_pigs`, and `broilers` automatically from breeding stock.
- [Basic
  Assessment](https://juancbm99.github.io/herdr/articles/Easy_Example.md)
  / [Moderate
  Example](https://juancbm99.github.io/herdr/articles/Moderate_Example.md)
  — dairy cattle case studies.
- [Technical
  Reference](https://juancbm99.github.io/herdr/articles/Technical_reference.md)
  — complete column-by-column dictionary.
