# Dairy Cattle Spain: North vs South

## Basic Assessment: A Step-by-Step Guide

This guide will walk you through a **Moderate Assessment** for **Mature
Dairy Cattle in Spain (2015)** using the multi-regional structure (North
and South).

> **Note on File Locations:** All files must be located in the
> `user_data/` folder of your project.

#### 📂 Folder Structure

- **User Files (Update these):** `livestock_census.csv`,
  `diet_profiles.csv`, `diet_ingredients.csv`,
  `livestock_definitions.csv`, `livestock_weights.csv`,
  `manure_management.csv`.
- **In case of usage of the automatic cycle:**
  `reproduction_parameters.csv`
- **Reference Files (Expert use only):** `feed_characteristics.csv`,
  `crop_yields.csv`, `ipcc_coefficients.csv`, `ipcc_mm.csv`.

------------------------------------------------------------------------

### Step 1: The Census

Open `livestock_census.csv`. For this example, we have split the Spanish
dairy population into two climatic regions to improve precision.

| animal_tag          | region | subregion | class_flex | population |
|:--------------------|:-------|:----------|:-----------|:-----------|
| mature_dairy_cattle | spain  | north     |            | 536987     |
| mature_dairy_cattle | spain  | south     |            | 311699     |

------------------------------------------------------------------------

### Step 2: Designing the Diet

The diet is defined by the user. In this dataset, we’ve identified
differences in forage availability between the North and South.

#### A. Define the Profile (`diet_profiles.csv`)

Note that the `forage_share` is higher in the North (55%) compared to
the South (50%).

| diet_tag          | region | subregion | forage_share | concentrate_share | milk_share |
|:------------------|:-------|:----------|:-------------|:------------------|:-----------|
| diet_dairy_mature | spain  | north     | 55           | 45                | 0          |
| diet_dairy_mature | spain  | south     | 50           | 50                | 0          |

#### B. Ingredient Breakdown (`diet_ingredients.csv`)

For each category (forage/concentrate), the `ingredient_share` must sum
to **100%**.

**Example for Spain South (Concentrate portion):**

| diet_tag | region | subregion | ingredient | ingredient_share | ingredient_type |
|:---|:---|:---|:---|:---|:---|
| diet_dairy_mature | spain | south | corn_national | 44.85 | concentrate |
| diet_dairy_mature | spain | south | soybean_meal_44_cp | 17.12 | concentrate |
| diet_dairy_mature | spain | south | rapeseed_meal_00_33_cp | 28.71 | concentrate |

------------------------------------------------------------------------

### Step 3: Animal Categories and Coefficients

Open `livestock_definitions.csv`. This file links your animal tags to
the IPCC physiological equations.

| animal_tag | region | subregion | diet_tag | c_pregnancy | milk_yield | fat_content | animal_type |
|:---|:---|:---|:---|:---|:---|:---|:---|
| mature_dairy_cattle | spain | north | diet_dairy_mature | cattle and buffalo | 8295.0 | 3.73 | cattle |
| mature_dairy_cattle | spain | south | diet_dairy_mature | cattle and buffalo | 9044.0 | 3.73 | cattle |

------------------------------------------------------------------------

### Step 4: Body Weights

Open `livestock_weights.csv`. Ensure the keys match the census. These
weights are used to validate your dry matter intake (DMI) limits.

| animal_tag          | region | subregion | adult_weight | weight_gain | average_weight |
|:--------------------|:-------|:----------|:-------------|:------------|:---------------|
| mature_dairy_cattle | spain  | north     | 675          | 0           | 675            |
| mature_dairy_cattle | spain  | south     | 675          | 0           | 675            |

------------------------------------------------------------------------

### Step 5: Manure Management (`manure_management.csv`)

You can define multiple manure systems for the same animal by splitting
the `allocation` (the sum per animal/region must be 1.0).

| animal_tag | region | subregion | system_base | management_months | system_climate | allocation |
|:---|:---|:---|:---|:---|:---|:---|
| mature_dairy_cattle | spain | north | anaerobic_lagoon |  | cool | 0.0537 |
| mature_dairy_cattle | spain | north | liquid_slurry | 3 | cool | 0.3432 |
| mature_dairy_cattle | spain | north | solid_storage |  | cool | 0.3551 |
| mature_dairy_cattle | spain | south | solid_storage |  | cool | 0.3551 |

------------------------------------------------------------------------

### Step 6: Running the Analysis

Once your CSVs are updated in the `user_data/` folder, run the analysis.
The package will automatically perform biological validations (like
checking if your cow is eating more than 5.5% of its body weight).

``` r
library(herdr)

results <- generate_impact_assessment(
  automatic_cycle = FALSE)
```
