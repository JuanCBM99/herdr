# Technical Reference: Files & Parameters

`herdr` operates on a data-driven approach. To ensure the model works
correctly, files are divided into two categories:

- **User Inputs** — files you fill in under `user_data/`.
- **Reference Libraries** — internal files you consult (but do not
  normally edit).

------------------------------------------------------------------------

## I. User Input Files (`user_data/`)

These are the templates you must complete to run your specific analysis.
Follow the [Workflow
Guide](https://juancbm99.github.io/herdr/articles/Workflow.md) for
step-by-step instructions.

### Population & Metrics

| File | Purpose |
|:---|:---|
| `livestock_census.csv` | Defines the `animal_tag`, location (`region`), and the number of heads (`population`). |
| `livestock_weights.csv` | Defines the physical scale of the animals: `adult_weight`, `initial_weight`, `final_weight`, `productive_period`, plus additional parameters for breeder swine. |
| `livestock_definitions.csv` | Bridge file for **ruminant** animals. Links each `animal_tag` to a `diet_tag` and an IPCC description. |
| `monogastric_definitions.csv` | Bridge file for **monogastric** animals. Links each `animal_tag` to a `diet_tag` and the species-specific parameters required for monogastric energy calculations. |

### Nutrition & Diets

| File | Purpose |
|:---|:---|
| `diet_profiles.csv` | Sets the high-level percentage balance between `forage`, `concentrate`, `milk`, and `milk_replacer`. |
| `diet_ingredients.csv` | Micro-breakdown of exactly which ingredients (from the reference library) make up each macro category. |

> **Note on `origin_country`:** if you don’t know where a feed
> ingredient comes from, leave the `origin_country` column as `NA`. The
> package will automatically resolve it using international trade data
> (see [`fao_trade_matrix.parquet`](#fao_trade_matrix) below).

### Manure Management

| File | Purpose |
|:---|:---|
| `manure_management.csv` | Defines how waste is handled: system, climate, and `allocation` (0 to 1). |

### Reproduction Parameters

| File | Purpose |
|:---|:---|
| `reproduction_parameters.csv` | Default offspring and replacement rates, used to estimate missing animal categories under the automatic herd cycle. |

Only needs editing if the population of some animal categories is
unknown, or if offspring/replacement rates need adjusting for a specific
study.

------------------------------------------------------------------------

## II. Reference Libraries (Consult Only)

These files are the “brain” of the package. You should **not** edit them
unless you are an advanced user — but you must **consult them** to copy
the exact names required in your input files.

### `feed_characteristics.csv` — Nutritional Values

Consult this library to find the correct ingredient names for
`diet_ingredients.csv`.

**Key columns:**

| Column               | Meaning                                      |
|:---------------------|:---------------------------------------------|
| `ingredient`         | Ingredient name                              |
| `feed_category`      | Feed category                                |
| `ASH_pct`            | Ash, % DM                                    |
| `CP_pct`             | Crude Protein, % DM                          |
| `EE_pct`             | Ether Extract, % DM                          |
| `NDF_pct`            | Neutral Detergent Fiber, % DM                |
| `DE_pct`             | Digestible Energy, %                         |
| `GE_feed_kcal_kg`    | Gross Energy, kcal/kg DM                     |
| `swine_ME_kcal_kg`   | Metabolizable Energy for swine, kcal/kg DM   |
| `swine_DE_kcal_kg`   | Digestible Energy for swine, kcal/kg DM      |
| `poultry_ME_kcal_kg` | Metabolizable Energy for poultry, kcal/kg DM |

**Sources:** most nutritional values come from the **FEDNA Tables
(2019)**. `DE_pct` values are taken from **Feedipedia**.
`GE_feed_kcal_kg` is calculated using the **NRC (1998) Ewan equation**:

``` math
GE\;(\text{kcal/kg DM}) = 4140 + (56 \times EE\%) + (15 \times CP\%) - (44 \times ASH\%)
```

where `EE%`, `CP%`, and `ASH%` correspond to the `EE_pct`, `CP_pct`, and
`ASH_pct` columns above.

### `ipcc_coefficients.csv` — Metabolic Constants

Consult this to find the `description` you need to copy into
`livestock_definitions.csv`.

- **Key columns:** `description`, `coefficient` ($`C_a`$, $`C_{fi}`$,
  etc.), `value`.
- **Why it matters:** contains the Tier 2 constants that define energy
  needs for maintenance, pregnancy, and lactation, as well as $`B_0`$
  (Maximum Methane Producing Capacity) for manure management
  calculations.

### `ipcc_mm.csv` — Manure Reference

The master list for the manure management phase. Contains every valid
combination of manure systems.

- **Key columns:** `system_base`, `system_variant`, `climate_zone`,
  `management_months`.
- **Why it matters:** your entry in `manure_management.csv` must match a
  row here exactly, or the model will return zero emissions for that
  cohort.

### `mapping.csv` — Database Connector

The bridge that links your diet ingredient names to the agricultural
yield databases.

- **Key columns:** `ingredient`, `yield_name`, `allocation` (0–1
  factor).
- **Why it matters:** tells the model which crop productivity to use,
  and how much of that land footprint is attributed to the animal (e.g.,
  grain vs. straw allocation).

### `forage_yields.csv` — Grass & Silage Data (BC3)

Provisional database for forages, supplemented by BC3 researchers.

- **Key columns:** `Area (Country)`, `Item (Crop name)`,
  `Value (kg DM/ha)`.
- **Why it matters:** provides essential yield data for grazing and
  forage-based systems, where official FAOSTAT records are often
  incomplete or missing.

### `fao_crop_yields.csv` — Official Statutory Yields

Direct yield data for grains and pulses from FAOSTAT (2024).

- **Key columns:** `Area`, `Item`, `Year`, `Value (kg DM/ha)`.
- **Why it matters:** sets the international standard for calculating
  the land footprint (m²) of concentrate feeds and commercial crops.

### `fao_trade_matrix.parquet` — Dynamic Trade Background (Auto-Downloaded)

Unlike the other libraries, this file is **not** tracked in the
repository, due to its size (~187 MB).

- **How it works:** automatically downloaded from GitHub Releases the
  first time `herdr` encounters an `NA` in the `origin_country` column
  of your diets.
- **Why it matters:** powers the hybrid background allocation engine,
  using FAO production and trade data to estimate missing feed origins
  under a 70% self-sufficiency rule.

------------------------------------------------------------------------

## III. Quick Reference Table

Use this table to know where to look when filling out your data:

| If you want to… | Consult this library | To fill this input file |
|:---|:---|:---|
| Identify an animal type | `ipcc_coefficients.csv` | `livestock_definitions.csv` |
| Pick a feed ingredient | `feed_characteristics.csv` | `diet_ingredients.csv` |
| Choose a manure system | `ipcc_mm.csv` | `manure_management.csv` |
| Add a custom crop | `mapping.csv`, `forage_yields.csv`, `fao_crop_yields.csv` | `feed_characteristics.csv` |

------------------------------------------------------------------------

## Tip

The model is case-sensitive and does not tolerate spaces. Always use
**lowercase** and **underscores** (e.g., `maize_silage` instead of
`Maize Silage`).
