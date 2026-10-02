# General Workflow: Step-by-Step Scripting Guide

## Introduction

This guide walks you through building and executing a complete livestock
environmental and nutritional assessment in R from scratch using
**`herdr`**.

Whether you are modeling a single dairy farm, an intensive swine
operation, or a national inventory across regions, following this
structured workflow ensures full consistency across IPCC Tier 2
greenhouse gases, agroecological land competition, and edible protein
outputs.

------------------------------------------------------------------------

## 1. Installation & Environment Setup

### Step 1: Install and Load `herdr`

Install the package directly from GitHub:

``` r

if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

remotes::install_github("JuanCBM99/herdr")
library(herdr)
```

### Step 2: Initialize Your Working Directory

Run
[`herdr_init()`](https://juancbm99.github.io/herdr/reference/herdr_init.md)
in your R console or project root:

``` r

herdr_init()
```

This generates three standard folders:

- **`user_data/`**: The active working directory containing editable CSV
  scenario templates and reference libraries (`ipcc_coefficients.csv`,
  `ipcc_mm.csv`, `feed_characteristics.csv`, `mapping.csv`). Background
  databases (`forages.parquet`, `fao_crops.parquet`,
  `fao_trade_matrix.parquet`) are resolved automatically via bundled
  assets and central cache.
- **`Examples/`**: Pre-configured baseline datasets across species
  (dairy cattle, beef cattle, swine, sheep) ready to use as templates.
- **`output/`**: Destination directory where results, emission
  inventories, and land-use summaries are automatically saved.

------------------------------------------------------------------------

## 2. Project Architecture & Demographic Keys

All data tables in `herdr` are linked using **four standardized
demographic keys**:

``` text
┌────────────────────────────────────────────────────────────────────────────────────────┐
│                              PRIMARY DEMOGRAPHIC COHORT KEYS                           │
├─────────────────┬──────────────┬─────────────────┬─────────────────────────────────────┤
│  animal_tag     │  region      │  subregion      │  class_flex                         │
│  (Mandatory)    │  (Optional)  │  (Optional)     │  (Optional)                         │
│  e.g., "dairy"  │  e.g., spain │  e.g., galicia  │  e.g., "lactation_phase"            │
└─────────────────┴──────────────┴─────────────────┴─────────────────────────────────────┘
```

> ⚠️ **Critical Join Rule:** Every unique combination of `animal_tag`,
> `region`, `subregion`, and `class_flex` declared in
> `livestock_census.csv` must match the definition, weight, and manure
> tables exactly. Identifiers are **strictly lowercase** with
> **underscores** (no spaces or uppercase letters).

------------------------------------------------------------------------

## 3. Preparing Input Tables (`user_data/`)

You can edit the CSV templates located in `user_data/` using any
spreadsheet editor (Excel, LibreOffice) or programmatically in R. For
the full column-by-column schema, see the [Technical Data
Dictionary](https://juancbm99.github.io/herdr/articles/Technical_reference.md).

#### Summary of Required Input Tables:

[TABLE]

#### Two Golden Rules for Diets & Manure:

1.  **Diet Proportions:** In `diet_profiles.csv`, forage + concentrate +
    milk + milk_replacer must sum to **100%**. In
    `diet_ingredients.csv`, ingredient shares must sum to **100%**
    within each category.
2.  **Manure Allocations:** In `manure_management.csv`, the sum of
    `allocation` for each unique cohort must equal exactly **1.0**
    (100%).

------------------------------------------------------------------------

## 4. Executing the Environmental Assessment

### 4.1. Consolidated Pipeline (`generate_impact_assessment`)

The fastest and most reliable way to run the model is
[`generate_impact_assessment()`](https://juancbm99.github.io/herdr/reference/generate_impact_assessment.md).
It executes the entire upstream chain (energy balance, DMI, enteric
fermentation, manure management, land use, and GLEAM production) and
compiles multi-metric outputs in one call:

``` r

library(herdr)

# Run full assessment for reference year 2024
results <- generate_impact_assessment(
  farm_country    = "Spain",
  year            = 2024,
  automatic_cycle = FALSE,
  gwp_report      = "AR5",
  saveoutput      = TRUE
)

# Preview generated summary table
head(results)
```

#### Key Output Metrics Returned:

- **Greenhouse Gas Emissions (Gg CO2e):** `CO2eq_enteric`,
  `CO2eq_manure`, `CO2eq_N2O_direct`, `CO2eq_N2O_indirect`, and
  `CO2eq_Total_Gg`.
- **Agroecological Land Footprint (m2):** `Land_cropland_m2`,
  `Land_grassland_convertible_m2`, `Land_grassland_unconvertible_m2`,
  `Land_other_m2`, and `Land_m2`.
- **Physical & Edible Protein Production:** `milk_FPCM_kg`,
  `meat_carcass_weight_kg`, `egg_fresh_kg`, and `total_protein_kg`.
- **Protein Feed Conversion & Food Security (Mottet et al. 2017):**
  `Protein_FCR_total` (total CP intake / edible protein) and
  `Protein_FCR_cropland` (cropland CP intake / edible protein).
- **Intensities across Functional Units:** `GHG_intensity_protein`,
  `Land_intensity_protein`, `GHG_intensity_product`,
  `Land_intensity_product`, `GHG_intensity_head`, and
  `Land_intensity_head`.

------------------------------------------------------------------------

### 4.2. Modular Step-by-Step Execution

Researchers seeking intermediate physiological outputs can run
individual modules sequentially:

``` r

# 1. Animal Demography & Population Balance
pop_data <- calculate_population(automatic_cycle = FALSE)

# 2. Weighted Nutritional Diet Characteristics
feed_vars <- calculate_weighted_variable()

# 3. Gross Energy Intake (GE)
ge_data <- calculate_ge()

# 4. Dry Matter Intake (DMI)
dmi_data <- calculate_DMI()

# 5. Enteric Methane Emissions (CH4)
enteric_emiss <- calculate_emissions_enteric()

# 6. Manure Methane & Nitrous Oxide (CH4, Direct & Indirect N2O)
manure_ch4 <- calculate_CH4_manure()
manure_n2o_dir <- calculate_N2O_direct_manure()
manure_n2o_vol <- calculate_N2O_indirect_volatilization()
manure_n2o_lea <- calculate_N2O_indirect_leaching()

# 7. Feed Land Use Footprint & FAO Trade Origin
land_data <- calculate_land_use(farm_country = "Spain", year = 2024)

# 8. Animal Productivity & Edible Human Protein (GLEAM)
prod_data <- calculate_production()
```

------------------------------------------------------------------------

## 5. Visualizing & Benchmarking Results

Use
[`plot_herdr_results()`](https://juancbm99.github.io/herdr/reference/plot_herdr_results.md)
to generate publication-ready stacked ggplot2 charts. The function
allows dynamic aggregation across any demographic or species grouping,
and supports **five standardized functional units**:

``` r

# 1. Absolute whole-inventory footprint (Gg CO2e and ha)
p_total <- plot_herdr_results(
  df              = results,
  group_cols      = c("animal_type", "region"),
  func_name       = "generate_impact_assessment",
  functional_unit = "total"
)

# 2. Commercial product intensity (kg CO2e and m2 per kg milk, carcass meat, or eggs)
# Applying IDF Bulletin 520/2022 physical Net Energy allocation for dairy herds
p_product <- plot_herdr_results(
  df              = results,
  group_cols      = c("animal_type", "animal_subtype"),
  func_name       = "generate_impact_assessment",
  functional_unit = "product"
)

# 3. Nutritional edible protein intensity (kg CO2e and m2 per kg human protein)
p_protein <- plot_herdr_results(
  df              = results,
  group_cols      = c("animal_tag"),
  func_name       = "generate_impact_assessment",
  functional_unit = "protein"
)

# 4. Per animal head footprint (kg CO2e and m2 per head per year)
p_head <- plot_herdr_results(
  df              = results,
  group_cols      = c("animal_type"),
  func_name       = "generate_impact_assessment",
  functional_unit = "head"
)

# 5. Protein Feed Conversion & Human Food Competition (Mottet et al. 2017)
# Renders dual bars: Biological FCR vs Cropland FCR with a 1.0 food producer threshold line
p_fcr <- plot_herdr_results(
  df              = results,
  group_cols      = c("animal_type", "animal_subtype"),
  func_name       = "generate_impact_assessment",
  functional_unit = "fcr"
)

# Display chart
print(p_product)

# Export chart to disk
ggplot2::ggsave("output/product_intensity_chart.png", plot = p_product, width = 10, height = 6, dpi = 300)
```

------------------------------------------------------------------------

## 6. Common Issues & Troubleshooting

| Issue / Alert | Cause | Solution |
|:---|:---|:---|
| **Missing Cohorts Warning** | An `animal_tag` in Census is missing in definitions, weights, or manure tables. | Ensure all four keys (`animal_tag`, `region`, `subregion`, `class_flex`) match across all input tables. |
| **Shares do not sum to 100%** | Diet category or ingredient proportions do not sum to 100. | Adjust `forage_share`, `concentrate_share`, etc. in `diet_profiles.csv`, or ingredient shares in `diet_ingredients.csv`. |
| **Manure allocation != 1.0** | Allocations for a cohort do not sum to 1.0. | Adjust `allocation` values in `manure_management.csv` so their sum equals exactly 1.0 per cohort. |
| **Unknown Ingredient** | An ingredient in diets is missing in reference libraries. | Add the ingredient to `feed_characteristics.csv` and `mapping.csv` (see [Adding a New Ingredient](https://juancbm99.github.io/herdr/articles/Adding_Ingredient.md)). |

------------------------------------------------------------------------

## Next Steps

- [Technical Data
  Dictionary](https://juancbm99.github.io/herdr/articles/Technical_reference.md)
  — detailed column-by-column schema and database connectors.
- [Interactive Shiny
  App](https://juancbm99.github.io/herdr/articles/app.md) — walk through
  the graphical interface.
- [Herd Demography & Population
  Dynamics](https://juancbm99.github.io/herdr/articles/Herd_Demography.md)
  — reproductive cycles, AAP rules, and automatic closures.
- [Theoretical
  Basis](https://juancbm99.github.io/herdr/articles/Theoretical_basis.md)
  — complete mathematical formulation (IPCC Tier 2, GLEAM, and IDF
  2022).
