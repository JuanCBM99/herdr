# Technical Reference: Files & Parameters

**herdr** operates on a data-driven approach. To ensure the model works
correctly, files are divided into two categories: **User Inputs** (files
you fill in `user_data/`) and **Reference Libraries** (internal files
you consult).

------------------------------------------------------------------------

## I. User Input Files (`user_data/`)

These are the templates you must complete to run your specific analysis.
Follow the [Workflow
Guide](https://juancbm99.github.io/herdr/articles/Workflow.md) for
step-by-step instructions.

### 🐄 Population & Metrics

- **`livestock_census.csv`**: Defines the `animal_tag`, location
  (`region`), and the number of heads (`population`).
- **`weights.csv`**: Defines the physical scale of the animals,
  including `adult_weight`, `average_weight`, and `weight_gain`.
- **`livestock_definitions.csv`**: The bridge file. It links each
  `animal_tag` to a `diet_tag` and an **IPCC Description**.

### 🍚 Nutrition & Diets

- **`diet_macro.csv`**: Sets the high-level balance between Forage,
  Concentrate, Milk, and Milk Replacer.
- **`diet_ingredients.csv`**: The micro-breakdown of exactly which
  ingredients (from the library) make up the macro categories.

### 💩 Manure Management

- **`manure_management.csv`**: Defines how waste is handled, specifying
  the system, the climate, and the `allocation` (0 to 1).

------------------------------------------------------------------------

## II. Reference Libraries (Consult Only)

These files are the “brain” of the package. You should **not** edit them
unless you are an advanced user, but you must **consult them** to copy
the exact names for your inputs.

### 🧪 `feed_characteristics.csv` — Nutritional Values

Consult this to find the correct names for your ingredients to use in
`diet_ingredients.csv`.

- **Key Columns:** `ingredient`, `ingredient_type`, `cp` (Crude Protein
  %), `de` (Digestible Energy %), `ndf` (Neutral detergent fiber) % and
  ge (Gross energy MJ/kg).
- **Why it matters:** `de` (Digestible Energy) is the main driver for
  the Methane Conversion Factor ($`Y_m`$).

### 🧬 `ipcc_coefficients.csv` — Metabolic Constants

Consult this to find the `description` you need to copy into your
`livestock_definitions.csv`.

- **Key Columns:** `description`, `coefficient` ($`C_a`$, $`C_{fi}`$,
  etc.), and `value`.
- **Why it matters:** It contains the Tier 2 constants that define
  energy needs for maintenance, pregnancy, and lactation.

### 💩 `ipcc_mm.csv` — Manure Reference

The master list for Phase 4. It contains every valid combination of
manure systems.

- **Key Columns:** `system_base`, `system_variant`, `climate_zone`, and
  `management_months`.
- **Why it matters:** Your entry in `manure_management.csv` must match a
  row here exactly, or the model will return zero emissions.

### 🌳 `crop_yields.csv` — Land Use Factors

Links your ingredients to the land requirements.

- **Key Columns:** `ingredient`, `dry_matter_yield` (kg/ha).
- **Why it matters:** Essential for calculating the total land footprint
  ($`m^2`$) of your livestock.

------------------------------------------------------------------------

## III. Quick Reference Table

Use this table to know where to look when filling out your data:

| If you want to… | Consult this library: | To fill this input file: |
|:---|:---|:---|
| **Identify an animal type** | `ipcc_coefficients.csv` | `livestock_definitions.csv` |
| **Pick a feed ingredient** | `feed_characteristics.csv` | `diet_ingredients.csv` |
| **Choose a manure system** | `ipcc_mm.csv` | `manure_management.csv` |
| **Add a custom crop** | *Add a new row to both* | `feed_characteristics` & `crop_yields` |

------------------------------------------------------------------------

### 💡 Tip

The model is case-sensitive and does not like spaces. Always use
**lowercase** and **underscores** (e.g., `maize_silage` instead of
`Maize Silage`).
