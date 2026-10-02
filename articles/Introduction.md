# Introduction to herdr

## Welcome to herdr

**`herdr`** is an integrated computational framework in R for modeling
**greenhouse gas (GHG) emissions**, **agricultural land competition**,
and **nutritional edible protein** across livestock production systems.

Most existing calculators evaluate emissions or feed land requirements
in isolation. `herdr` bridges animal physiology, nutritional energy
balances, herd demography, manure management, and international trade
flows into a **single, internally consistent modeling engine**.

------------------------------------------------------------------------

## Core Capabilities

``` text
┌────────────────────────────────────────────────────────────────────────────────────────┐
│                                 herdr ENGINE OVERVIEW                                  │
├──────────────────────────┬─────────────────────────────┬───────────────────────────────┤
│  1. IPCC TIER 2 GHGs     │  2. AGROECOLOGICAL LAND     │  3. FOOD PRODUCTIVITY & FCR   │
│  • Enteric CH4           │  • Cropland vs Grassland    │  • GLEAM Milk, Meat, Eggs     │
│  • Manure CH4            │  • Mottet (2017) Framework  │  • IDF (2022) Physical Alloc. │
│  • Direct & Indirect N2O │  • FAOSTAT Bilateral Trade  │  • 3 Feed Conversion Ratios   │
└──────────────────────────┴─────────────────────────────┴───────────────────────────────┘
```

1.  **IPCC Tier 2 Greenhouse Gas Inventories:** Implements the *2019
    Refinement to the 2006 IPCC Guidelines for National Greenhouse Gas
    Inventories* for ruminants (cattle, sheep, goats) and monogastrics
    (swine, poultry, adopting FEDNA nutritional standards).
2.  **Agroecological Land Footprint & Competition:** Quantifies feed
    land requirements (m2) and classifies land according to human food
    competition (Mottet et al., 2017) into **Cropland**, **Convertible
    Grassland**, **Unconvertible Rangeland**, and **Other**. When
    ingredient origins are unspecified, `herdr` traces true producer
    countries using recursive FAO bilateral trade matrices and national
    self-sufficiency thresholds.
3.  **Animal Productivity & Edible Human Protein:** Implements the **FAO
    GLEAM v3.0** methodology to translate herd throughput into physical
    commodities (Fat and Protein Corrected Milk, cold dressed carcass
    weight, boneless meat, and fresh eggs) and total digestible human
    protein (kg protein/year).
4.  **Biophysical Co-Product Allocation & Protein Feed Efficiency
    (Mottet et al., 2017):** Applies the updated **International Dairy
    Federation (IDF Bulletin 520/2022)** Net Energy physical allocation
    and reports protein conversion and human food competition metrics
    (`Protein_FCR_total` and `Protein_FCR_cropland`).

------------------------------------------------------------------------

## Supported Livestock Species

`herdr` natively supports full life cycles across major domestic
species:

- 🐄 **Cattle:** Mature dairy cows (lactating & dry phases), replacement
  heifers, breeding bulls, suckler beef cows, and feedlot fattening
  calves.
- 🐑 **Sheep:** Dairy ewes, meat ewes, breeding rams, replacement
  hoggets, and slaughter lambs.
- 🐐 **Goats:** Dairy does, meat does, bucks, kids for replacement, and
  dairy/meat fattening kids.
- 🐖 **Swine:** Breeding sows (gestation & lactation phases), service
  boars, replacement gilts, weaners, and fattening pigs.
- 🐔 **Poultry:** Commercial layer flocks, layer replacement pullets,
  broiler meat flocks, and meat breeder hens.

------------------------------------------------------------------------

## Quick Start: Choose Your Path

`herdr` can be operated either through an interactive web-based
graphical interface (GUI) or programmatically via R scripts.

#### Option A: Interactive Web Application (No Coding Required)

``` r

library(herdr)

# Initialize local workspace with templates
herdr_init()

# Launch the interactive Shiny interface
run_herdr_app()
```

The Shiny app provides guided forms, real-time input validation,
interactive stacked charts, and one-click data downloads. See the
[Interactive App
Guide](https://juancbm99.github.io/herdr/articles/app.md) for a complete
walkthrough.

#### Option B: Scripted R Pipeline (Reproducible Research)

``` r

library(herdr)

# 1. Initialize local project directories and templates
herdr_init()

# 2. Run consolidated environmental and nutritional assessment
results <- generate_impact_assessment(
  farm_country = "Spain",
  year         = 2024,
  gwp_report   = "AR5"
)

# 3. Visualize results by species across functional units
p <- plot_herdr_results(
  df              = results,
  group_cols      = c("animal_type", "region"),
  func_name       = "generate_impact_assessment",
  functional_unit = "protein" # kg CO2e and m2 per kg edible protein
)

print(p)
```

See the [Scripting
Workflow](https://juancbm99.github.io/herdr/articles/Workflow.md) for
complete step-by-step instructions.

------------------------------------------------------------------------

## Documentation Map

The documentation is organized into four complementary pillars designed
to support different stages of your work:

| Pillar | Guide | Purpose |
|:---|:---|:---|
| **Getting Started** | [R Scripting Workflow](https://juancbm99.github.io/herdr/articles/Workflow.md) | End-to-end tutorial for script-based projects in R. |
|  | [Interactive Shiny App](https://juancbm99.github.io/herdr/articles/app.md) | Guided tour of the graphical user interface. |
| **How-To Guides** | [Herd Demography & Life Cycles](https://juancbm99.github.io/herdr/articles/Herd_Demography.md) | How to configure biological cycles, AAP population dynamics, and automatic herd closures. |
|  | [Manure Management Pathways](https://juancbm99.github.io/herdr/articles/Manure.md) | How to select valid IPCC storage, treatment, and climate combinations in CSV inputs. |
|  | [Feed Land Competition & Trade](https://juancbm99.github.io/herdr/articles/land_use.md) | How agroecological land categories and dynamic FAO trade tracing operate. |
|  | [Adding a Custom Feed Ingredient](https://juancbm99.github.io/herdr/articles/Adding_Ingredient.md) | How to expand nutritional and LCA reference databases with custom feedstuffs. |
| **Case Studies** | [1. Dairy Cattle (Basic)](https://juancbm99.github.io/herdr/articles/Easy_Example.md) | Single-farm assessment highlighting FPCM and IDF co-product allocation. |
|  | [2. Regional Variations (Moderate)](https://juancbm99.github.io/herdr/articles/Moderate_Example.md) | Multi-farm spatial stratification across diverse climatic regions. |
|  | [3. Intensive Multi-Cohort AAP (Advanced)](https://juancbm99.github.io/herdr/articles/Difficult_Example.md) | High-throughput batch rearing in swine and poultry systems. |
| **Methodology & Reference** | [Technical Data Dictionary](https://juancbm99.github.io/herdr/articles/Technical_reference.md) | Comprehensive field-by-field schema of all CSV templates and background databases. |
|  | [Theoretical Basis](https://juancbm99.github.io/herdr/articles/Theoretical_basis.md) | Complete mathematical formulations, IPCC Tier 2 equations, and GLEAM algorithms. |
