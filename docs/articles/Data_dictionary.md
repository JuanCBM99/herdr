# Data Dictionary: Internal CSV Files

## Data Dictionary: Internal CSV Files

**herdr** operates on a data-driven approach. While the
calculations are universal (IPCC Tier 2), the package comes pre-loaded
with **default datasets representing Spanish national averages** for
livestock performance, diets, and emission factors.

This document serves as the **Technical Reference** for all these
internal CSV files.

------------------------------------------------------------------------

### I. Herd Demography and Performance

These files define the animal inventory, population structure, and
biological requirements for energy calculation.

#### 🐄 📁 `census.csv` — Inventory

| Column | Description / Notes |
|:---|:---|
| `identification` | Unique identifier for each animal (must match `categories.csv`). |
| `animal_type` | General animal class (e.g., `Cattle`, `Sheep`). |
| `animal_subtype` | Specific production type. |
| `sex` | Sex of the animals (`male` / `female`). |
| `group` | Grouping variable (e.g., Farm ID, country, region). |
| `zone` | Geographic or management zone. |
| `population` | Number of animals in this category/group/zone. |

#### 🥩 📁 `categories.csv` — Production and Energy Parameters

| Column | Description / Notes |
|:---|:---|
| `identification` | Unique identifier for each animal. |
| `animal_type` | General animal class. |
| `animal_subtype` | Specific production type. |
| `diet_tag` | Links to `diet.csv` / `ingredients.csv`. |
| `cfi`, `ca` | Maintenance and Activity energy coefficient tags (links to `coefficients.csv`). |
| `c`, `a`, `b` | IPCC energy partitioning coefficients (links to `coefficients.csv`). |
| `c_pregnancy` | Pregnancy energy coefficient tag (links to `coefficients.csv`). Only for cattle. |
| `dm_ingested_total` | Total dry matter intake (kg DM/day). Required for **Land Use** calculation. |
| `grazing_months` | Months spent grazing (0–12). |
| `milk_yield` | Daily milk yield (L/day). |
| `fat_content` | Milk fat percentage (%). |
| `wool_yield` | Wool production (kg/year). |
| `hours` | Working hours per day (for draft animals). |
| `pr` | Reproductive status indicator (e.g., probability of conception). |
| `single birth fraction` | Fraction of births resulting in a single offspring (0–1). (Small Ruminants). |
| `double birth fraction` | Fraction of births resulting in twins (0–1). (Small Ruminants). |

#### 📈 📁 `weights.csv` — Growth and Age Structure

| Column              | Description / Notes                                 |
|:--------------------|:----------------------------------------------------|
| `identification`    | Unique identifier for each animal.                  |
| `adult_weight`      | Adult weight of the animal (kg).                    |
| `birth_weight`      | Birth weight of the animal (kg).                    |
| `initial_weight`    | Weight at the start of the observation period (kg). |
| `final_weight`      | Weight at the end of the observation period (kg).   |
| `wf-wi`             | Difference between final and initial weight (kg).   |
| `ei`, `ef`          | Initial and Final age of the category (days).       |
| `productive_period` | Duration of the productive period (days).           |
| `weight_gain`       | Total weight gain during the period (kg).           |
| `average_weight`    | Average weight during the period (kg).              |
| `animal_type`       | General animal class.                               |
| `animal_subtype`    | Specific production type.                           |

#### 🏷️ 📁 `rate_parameters.csv` — Demographic Rates

| Column | Description / Notes |
|:---|:---|
| `parameter` | Name of the demographic parameter (e.g., `calving_rate`, `mortality`). |
| `animal_type` | General animal class. |
| `animal_subtype` | Specific production type. |
| `sex` | Sex of the animals. |
| `value` | Parameter value (e.g., fraction or rate per year). |

------------------------------------------------------------------------

### II. Nutrition and Feed Characteristics

These files define the composition of the diet and the nutritional
characteristics required for Gross Energy (GE) and Nitrogen (N)
excretion calculations.

#### 🍚 📁 `diet.csv` — High-Level Diet Composition

| Column                | Description / Notes                                |
|:----------------------|:---------------------------------------------------|
| `diet_tag`            | Unique identifier for the diet.                    |
| `zone`                | Geographic or management zone.                     |
| `forage_share`        | Fraction of the diet composed of forage.           |
| `feed_share`          | Fraction of the diet composed of concentrate feed. |
| `milk_share`          | Fraction of the diet composed of milk.             |
| `milk_replacer_share` | Fraction of the diet composed of milk replacer.    |
| `group`               | Grouping variable.                                 |
| `animal_type`         | General animal class.                              |
| `animal_subtype`      | Specific production type.                          |

#### 🌾 📁 `ingredients.csv` — Diet Detail

| Column             | Description / Notes                                  |
|:-------------------|:-----------------------------------------------------|
| `diet_tag`         | Unique identifier for the diet.                      |
| `zone`             | Geographic or management zone.                       |
| `ingredient`       | Name of the ingredient (e.g., maize, soybean meal).  |
| `ingredient_share` | Fraction of the diet represented by this ingredient. |
| `ingredient_type`  | Type of ingredient (e.g., feed, forage, milk).       |
| `group`            | Grouping variable.                                   |
| `animal_type`      | General animal class.                                |
| `animal_subtype`   | Specific production type.                            |

#### 🧪 📁 `characteristics.csv` — Nutritional Values

| Column | Description / Notes |
|:---|:---|
| `ingredient_type` | Type of ingredient. |
| `ingredient` | Name of the ingredient. |
| `moisture` | Moisture content (%). |
| `cp_fm`, `adf_fm`, `ee_fm`, `ndf_fm`, etc. | Nutrient content on **Fresh Matter** basis (%). |
| `me_fm`, `nel_fm`, `nem_fm`, `neg_fm` | Energy content on **Fresh Matter** basis (MJ/kg). |
| `ash`, `cp`, `adf`, `ee`, `ndf`, etc. | Nutrient content on **Dry Matter** basis (%). |
| `me`, `nel`, `nem`, `neg` | Energy content on **Dry Matter** basis (MJ/kg). |
| `de` | Digestible energy (%). Key for Methane ($`Y_m`$) calculation. |
| `ge` | Gross energy (MJ/kg). |

------------------------------------------------------------------------

### III. Manure Management and Emission Factors

#### 💨 📁 `mcf.csv` — Methane Factors

| Column | Description / Notes |
|:---|:---|
| `management_system` | Type of manure management system (e.g., liquid, solid, pasture). |
| `system_climate` | Climate categories available (e.g., temperate, tropical). |
| `mcf` | Methane conversion factor (%) for the system and climate. |

#### 💩 📁 `ch4_mm.csv` — Methane Management Configuration

| Column | Description / Notes |
|:---|:---|
| `identification` | Unique animal identifier. |
| `management_system` | Manure management system currently used. |
| `system_climate` | Climate category of the system. |
| `management_duration` | Duration of the manure management system (days or months). |
| `animal_category` | Category of the animal for baseline Methane Production ($`B_0`$). |
| `animal_type`, `animal_subtype` | Animal classification. |

#### 🌧️ 📁 `fractions.csv` — Nitrogen Loss Factors

| Column | Description / Notes |
|:---|:---|
| `management_system` | Manure management system. |
| `frac_gas_ms` | Fraction of nitrogen lost as volatilized gases ($`NH_3`$ + $`NO_x`$). |
| `frac_leach_ms` | Fraction of nitrogen lost through leaching and runoff. |

#### 🎯 📁 `emission_factors_direct.csv` — N2O Direct (EF3)

| Column              | Description / Notes                           |
|:--------------------|:----------------------------------------------|
| `management_system` | Available manure management system.           |
| `climate`           | Climate categories available.                 |
| `value`             | Emission factor (kg N2O-N per kg N excreted). |

#### 🌀 📁 `emission_factors_indirect.csv` — N2O Indirect (EF4, EF5)

| Column | Description / Notes |
|:---|:---|
| `climate` | Climate category (e.g., wet, dry). |
| `value` | Emission factor value (fraction of N volatilized or leached). |
| `description` | Identifier: `EF4` (volatilization), `EF5` (leaching). |

------------------------------------------------------------------------

### IV. Scientific Constants and Land Use

#### 🧬 📁 `coefficients.csv` — IPCC Constants and Parameters

| Column | Description / Notes |
|:---|:---|
| `description` | Name or description of the coefficient (e.g., `lactating_cow`). |
| `value` | Numerical value. |
| `coefficient` | Coefficient tag (e.g., `cfi`, $`B_0`$, $`Y_m`$). |
| `units` | Units of the coefficient. |
| `animal_type` | General animal class. |

#### 🌳 📁 `crops.csv` — Land Use Factors

| Column | Description / Notes |
|:---|:---|
| `ingredient_type` | Type of ingredient. |
| `ingredient` | Name of the ingredient. |
| `description` | Description of the crop. |
| `land_use_category` | Land use category (e.g., arable, permanent pasture). |
| `moisture` | Moisture content (%). |
| `harvested_yield_rainfed` | Yield under rainfed conditions (kg/ha). |
| `harvested_yield_irrigated` | Yield under irrigated conditions (kg/ha). |
| `harvested_area_rainfed` | Area harvested under rainfed conditions (ha). |
| `harvested_area_irrigated` | Area harvested under irrigated conditions (ha). |
| `dry_matter_yield` | Dry matter yield (kg/ha). |
| `origin` | Country or region of origin. |
