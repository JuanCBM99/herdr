# Dairy Cattle Spain: North/South in different Life Satages

## Advanced Assessment: Managing Complexity with `class_flex`

In this guide, we tackle a high-complexity scenario for **Mature Dairy
Cattle**. We will split the population by **geography** (North vs. South
Spain) and by **physiological state** using the `class_flex` column to
distinguish between the **Lactation Phase** and the **Dry Phase**.

> **Note on File Locations:** All files are located in the `user_data/`
> folder.

#### 📂 Folder Structure

- **User Files (Update these):** `livestock_census.csv`,
  `diet_profiles.csv`, `diet_ingredients.csv`,
  `livestock_definitions.csv`, `livestock_weights.csv`,
  `manure_management.csv`.
- **In case of usage of the automatic cycle:**
  `reproduction_parameters.csv`
- **Reference Files (Expert use only):** `feed_characteristics.csv`,
  `forage_yields.csv`, `mapping.csv`, `fao_crop_yields.csv`,
  `ipcc_coefficients.csv`, `ipcc_mm.csv`.

------------------------------------------------------------------------

### Step 1: Multi-Dimensional Census (`livestock_census.csv`)

The census file links the population to a specific subregion and a
physiological state. This is vital for accurate annual emission
averages.

| animal_tag          | region | subregion | class_flex      | population |
|:--------------------|:-------|:----------|:----------------|:-----------|
| mature_dairy_cattle | spain  | north     | dry_phase       | 88065.87   |
| mature_dairy_cattle | spain  | north     | lactation_phase | 448921.13  |
| mature_dairy_cattle | spain  | south     | dry_phase       | 51118.64   |
| mature_dairy_cattle | spain  | south     | lactation_phase | 260580.36  |

------------------------------------------------------------------------

### Step 2: Phase-Specific Nutrition

The `class_flex` column allows you to define different
forage/concentrate ratios and specific ingredients for each life stage.

#### A. Diet Profiles (`diet_profiles.csv`)

| diet_tag          | region | subregion | class_flex      | forage_share | concentrate_share | milk_share | milk_replacer_share |
|:------------------|:-------|:----------|:----------------|:-------------|:------------------|:-----------|:--------------------|
| diet_dairy_mature | spain  | north     | dry_phase       | 70           | 30                | 0          | 0                   |
| diet_dairy_mature | spain  | north     | lactation_phase | 55           | 45                | 0          | 0                   |
| diet_dairy_mature | spain  | south     | dry_phase       | 54           | 46                | 0          | 0                   |
| diet_dairy_mature | spain  | south     | lactation_phase | 50           | 50                | 0          | 0                   |

#### B. Ingredient Breakdown (`diet_ingredients.csv`)

Ingredients must be assigned precisely to the `subregion` and
`class_flex`. *Example for the South/Lactation profile:*

| diet_tag          | region | subregion | class_flex      | ingredient         | ingredient_share | ingredient_type |
|:------------------|:-------|:----------|:----------------|:-------------------|:-----------------|:----------------|
| diet_dairy_mature | spain  | south     | lactation_phase | corn_national      | 44.85            | concentrate     |
| diet_dairy_mature | spain  | south     | lactation_phase | soybean_meal_44_cp | 17.12            | concentrate     |
| diet_dairy_mature | spain  | south     | lactation_phase | corn_silage_25_30  | 55.00            | forage          |

------------------------------------------------------------------------

### Step 3: Physiological Definitions (`livestock_definitions.csv`)

This is the most technical file. It determines the energy requirements
(IPCC Tier 2). Note the difference in `milk_yield` and `cfi`.

| animal_tag          | region | subregion | class_flex      | cfi                          | ca    | c       | milk_yield | fat_content | c_pregnancy        |
|:--------------------|:-------|:----------|:----------------|:-----------------------------|:------|:--------|:-----------|:------------|:-------------------|
| mature_dairy_cattle | spain  | north     | lactation_phase | cattle_buffalo \[lactating\] | stall | females | 8295       | 3.73        | cattle and buffalo |
| mature_dairy_cattle | spain  | north     | dry_phase       | cattle/buffalo               | stall | females | 0          | 0           | cattle and buffalo |
| mature_dairy_cattle | spain  | south     | lactation_phase | cattle_buffalo \[lactating\] | stall | females | 9044       | 3.73        | cattle and buffalo |
| mature_dairy_cattle | spain  | north     | dry_phase       | cattle/buffalo               | stall | females | 0          | 0           | cattle and buffalo |

------------------------------------------------------------------------

### Step 4: Body Weights (`livestock_weights.csv`)

Accurate weights are essential to calculate Maintenance Energy
($NE_{m}$) and validate the maximum physical capacity of the animal
($DMI$ as $\%$ of Body Weight).

| animal_tag          | region | subregion | class_flex      | adult_weight | weight_gain | average_weight |
|:--------------------|:-------|:----------|:----------------|:-------------|:------------|:---------------|
| mature_dairy_cattle | spain  | north     | dry_phase       | 675          | 0           | 675            |
| mature_dairy_cattle | spain  | north     | lactation_phase | 675          | 0           | 675            |
| mature_dairy_cattle | spain  | south     | lactation_phase | 675          | 0           | 675            |

------------------------------------------------------------------------

### Step 5: Advanced Manure Management (`manure_management.csv`)

You can define management systems with high granularity, specifying the
climate zone and variants for methane conversion factors.

| animal_tag          | region | subregion | class_flex      | system_base      | management_months | system_climate | system_subclimate | climate_zone | system_variant           | climate_zone | allocation |
|:--------------------|:-------|:----------|:----------------|:-----------------|:------------------|:---------------|:------------------|:-------------|:-------------------------|:-------------|:-----------|
| mature_dairy_cattle | spain  | north     | lactation_phase | anaerobic_lagoon |                   | cool           | temperate         | zone_wet     | uncovered                | wet          | 0.0537     |
| mature_dairy_cattle | spain  | north     | lactation_phase | liquid_slurry    | 3                 | cool           | temperate         | zone_wet     | with_natural_crust_cover | wet          | 0.3432     |
| mature_dairy_cattle | spain  | south     | dry_phase       | solid_storage    |                   | warm           | temperate         |              |                          | dry          | 0.0694     |
| mature_dairy_cattle | spain  | south     | dry_phase       | deep_bedding     | \>1               | warm           | temperate         | zone_dry     | no_mixing                | dry          | 0.0131     |

------------------------------------------------------------------------

### Step 6: Final Execution

When running the analysis, `herdr` will process each row as a unique
“animal-state-region” combination before aggregating the results.

``` r
library(herdr)

# The model calculates impact for each phase separately
results <- generate_impact_assessment(automatic_cycle = FALSE)
```
