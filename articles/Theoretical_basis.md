# Theoretical Basis: IPCC Tier 2 Methodology

## I. Net Energy for Maintenance (NEm)

**Net Energy for Maintenance (NEm)** is the energy required to maintain
the animal’s energy balance.

**Formula:**  
`NEm = Cfi * Weight^0.75 (MJ/day)`

| Variable | Description                         | Source in the Package |
|----------|-------------------------------------|-----------------------|
| NEm      | Net energy for maintenance (MJ/day) | Calculated            |
| Cfi      | Maintenance coefficient (MJ/day/kg) | `coefficients.csv`    |
| Weight   | Live weight of the animal (kg)      | `weights.csv`         |

------------------------------------------------------------------------

## II. Net Energy for Activity (NEa)

**Net Energy for Activity (NEa)** represents energy used for grazing,
walking, or other animal activities.

- **Cattle and Buffalo**

`NEa = Ca * NEm (MJ/day)`

- **Sheep and Goats**

`NEa = Ca * Weight (MJ/day)`

| Variable     | Description                      | Source in the Package |
|--------------|----------------------------------|-----------------------|
| NEa          | Net energy for activity (MJ/day) | Calculated            |
| Ca           | Activity coefficient             | `coefficients.csv`    |
| NEm / Weight | Base energy or body weight       | `weights.csv`         |

------------------------------------------------------------------------

## III. Net Energy for Growth (NEg)

**Net Energy for Growth (NEg)** is energy for tissue deposition.

- **Cattle and Buffalo**

`NEg = BW^0.75 * (1.097 / 22.02) * (WG / (C * MW))^2 (MJ/day)`

- **Sheep and Goats**

`NEg = (WGlamb / 365) * (a + b * ((BWi + BWf)/2)) (MJ/day)`

| Variable     | Description           | Source in the Package |
|--------------|-----------------------|-----------------------|
| NEg          | Net energy for growth | Calculated            |
| BW / BWi/BWf | Body weight           | `weights.csv`         |
| WG / WGlamb  | Weight gain           | `weights.csv`         |
| C, a, b      | Species/sex constants | `coefficients.csv`    |

------------------------------------------------------------------------

## IV. Net Energy for Lactation (NEl)

Energy required for milk production:

- **Cattle and Buffalo**

`NEl = Milk * (1.47 + 0.40 * Fat) (MJ/day)`

- **Sheep and Goats**

`NEl = Milk * EV_milk (MJ/day)`

| Variable      | Description                          | Source in the Package                 |
|---------------|--------------------------------------|---------------------------------------|
| NEl           | Net energy for lactation             | Calculated                            |
| Milk          | Daily milk production (kg/day)       | `categories.csv`                      |
| Fat / EV_milk | Milk fat or energy value per kg milk | `categories.csv` / `coefficients.csv` |

------------------------------------------------------------------------

## V. Net Energy for Work and Wool Production

- **Work (NEwork) for draft animals**

`NEwork = 0.10 * NEm * Hours (MJ/day)`

- **Wool (NEwool) for sheep/goats**

`NEwool = Pr_wool * EV_wool / 365 (MJ/day)`

| Variable        | Description                                  | Source in the Package |
|-----------------|----------------------------------------------|-----------------------|
| NEwork / NEwool | Energy for work or wool                      | Calculated            |
| Hours / Pr_wool | Work hours per day or annual wool production | `categories.csv`      |
| EV_wool         | Energy per kg of wool                        | `coefficients.csv`    |

------------------------------------------------------------------------

## VI. Net Energy for Pregnancy (NEp)

Energy required for gestation:

`NEp = C_pregnancy * NEm (MJ/day)`

| Variable    | Description           | Source in the Package                                        |
|-------------|-----------------------|--------------------------------------------------------------|
| NEp         | Energy for pregnancy  | Calculated                                                   |
| C_pregnancy | Pregnancy coefficient | `coefficients.csv`                                           |
| NEm         | Maintenance energy    | Result of [`calculate_NEm()`](../reference/calculate_NEm.md) |

------------------------------------------------------------------------

## VII. Gross Energy (GE)

Gross Energy (GE) is the total daily energy intake required to cover all
Net Energy needs:

`GE = [(NE_m + NE_a + NE_l + NEwork + NEp)/R_EM + (NEg + NEwool)/R_EG] / DE (MJ/day)`

| Variable    | Description                | Source in the Package   |
|-------------|----------------------------|-------------------------|
| GE          | Gross energy intake        | Calculated              |
| NE\_\*      | All Net Energy components  | Results of calculations |
| R_EM / R_EG | NE to DE conversion ratios | `coefficients.csv`      |
| DE          | Digestibility of feed      | `characteristics.csv`   |

------------------------------------------------------------------------

## VIII. Methane Module: Enteric Emissions (CH₄)

**Tier 1 Structure with Tier 2 EF calculation**: methane emissions
depend on population and **diet-specific emission factor**:

`E_CH4 = Σ (N_animals * EF) (Gg CH₄/year)`

`EF = (GE * Ym / 100 * 365) / 55.65 (kg CH₄/head/year)`

| Variable | Description               | Source in the Package |
|----------|---------------------------|-----------------------|
| EF       | Methane emission factor   | Calculated            |
| GE       | Gross energy intake       | Calculated            |
| Ym       | Methane conversion factor | `characteristics.csv` |
| 55.65    | Energy content of methane | Constant              |

------------------------------------------------------------------------

## IX. Methane Module: Manure Management (CH₄-MM)

**Volatile Solids (VS)** excretion:

`VS = (GE / 18.45) * [(1 - DE/100) + (UE * GE / GE)] * (1 - ASH) (kg/day)`

**Manure CH₄ Emission Factor:**

`EF_CH4_manure = (VS * B0 * 365 / 100) * 0.67 * Σ(AWMS * MCF) (kg CH₄/year)`

| Variable | Description                  | Source in the Package |
|----------|------------------------------|-----------------------|
| VS       | Volatile solids excreted     | Calculated            |
| B0       | Maximum methane potential    | `coefficients.csv`    |
| AWMS     | Fraction of manure managed   | `ch4_mm.csv`          |
| MCF      | Methane conversion factor    | `mcf.csv`             |
| 0.67     | Conversion m³ CH₄ 192 kg CH₄ | Constant              |

------------------------------------------------------------------------

## X. N₂O Module: Nitrogen-Based Emissions

### I. Annual Nitrogen Excretion (Nex)

`Nintake = GE / 18.45 * (6.25 / 100) * CP% (kg N/day)`

**Nitrogen retention:**

- **Cattle/Buffalo:**

`Nretention = [(NEg + Milk * PR) * 7.03 / (1000 * 6.38)] - [Milk * PR * 6.25 / 100] (g N/day)`

- **Sheep/Goats:**

`Nretention_frac = 0.1 (fraction)`  
`Nex = Nintake * (1 - Nretention_frac) * 365 (kg N/year)`

| Variable        | Description               | Source in the Package                              |
|-----------------|---------------------------|----------------------------------------------------|
| Nintake         | Nitrogen ingested per day | Calculated                                         |
| NEg             | Net energy for growth     | [`calculate_NEg()`](../reference/calculate_NEg.md) |
| Milk            | Daily milk yield          | `categories.csv`                                   |
| PR              | Milk protein content      | `categories.csv`                                   |
| Nretention_frac | Fraction of N retained    | `coefficients.csv`                                 |

------------------------------------------------------------------------

### II. Direct N₂O Emissions

`N2O_direct = 44/28 * Σ [N_animals * Nex * AWMS * EF3] (kg N2O/year)`

| Variable | Description                     | Source in the Package         |
|----------|---------------------------------|-------------------------------|
| AWMS     | Fraction of N managed in system | `ch4_mm.csv`                  |
| EF3      | Direct N₂O emission factor      | `emission_factors_direct.csv` |

------------------------------------------------------------------------

### III. Indirect N₂O Emissions

**Volatilization:**

`N_volatilization = Σ [ (N_animals * Nex + N_codigestate) * fraction_managed * frac_gas ]`  
`N2O_volatilization = 44/28 * N_volatilization * EF4`

**Leaching / Runoff:**

`N_leaching = Σ [ (N_animals * Nex + N_codigestate) * fraction_managed * frac_leach ]`  
`N2O_leaching = 44/28 * N_leaching * EF5`

| Variable              | Description                       | Source in the Package           |
|-----------------------|-----------------------------------|---------------------------------|
| N_codigestate         | Nitrogen from co-digestates       | User input / CSV                |
| fraction_managed      | Fraction of N managed in system   | `ch4_mm.csv`                    |
| frac_gas / frac_leach | Fraction lost as gas or leached   | `fractions.csv`                 |
| EF4 / EF5             | Emission factors for indirect N₂O | `emission_factors_indirect.csv` |

------------------------------------------------------------------------

## XI. Land Use Module

The **Land Use Module** calculates the agricultural land required to
support each animal category based on their diet composition and crop
yields. It provides **land use per animal** and **total land use for the
population**.

------------------------------------------------------------------------

### I. Data Inputs

The calculation requires the following datasets:

| Dataset           | Description                                                                     | Source in the Package                                                           |
|-------------------|---------------------------------------------------------------------------------|---------------------------------------------------------------------------------|
| `categories.csv`  | Animal characteristics, including total dry matter intake (`dm_ingested_total`) | categories.csv                                                                  |
| `ingredients.csv` | Different ingredients share that compose the defined diets                      | diet.csv                                                                        |
| `diet.csv`        | Diet composition including feed, forage, milk, and milk replacer shares         | diet.csv                                                                        |
| `crops.csv`       | Crop yield data (`dry_matter_yield` in kg/ha) from Spain                        | crops.csv                                                                       |
| Population        | Number of animals per category                                                  | Calculated via [`calculate_population()`](../reference/calculate_population.md) |

------------------------------------------------------------------------

### II. Land Use per Ingredient

For each ingredient in the diet, the **weighted consumption** is
calculated:

`consumption_kg = dm_ingested_total * ingredient_share * share_weight / 10000 (kg)`

| Variable            | Description                                                                                                       | Source in the Package |
|---------------------|-------------------------------------------------------------------------------------------------------------------|-----------------------|
| `dm_ingested_total` | Total dry matter ingested by the animal (kg/year)                                                                 | categories.csv        |
| `ingredient_share`  | Fraction of the diet contributed by the ingredient                                                                | ingredients.csv       |
| `share_weight`      | Weighting factor depending on ingredient type (`feed_share`, `forage_share`, `milk_share`, `milk_replacer_share`) | diet.csv              |

The **land use per ingredient** is then calculated using crop yields:

`land_use_m2_per_unit = ifelse(dry_matter_yield > 0, (consumption_kg / dry_matter_yield) * 10000, 0) (m²/animal)`

| Variable               | Description                                           | Source in the Package |
|------------------------|-------------------------------------------------------|-----------------------|
| `dry_matter_yield`     | Crop yield in kg DM/ha                                | crops.csv             |
| `land_use_m2_per_unit` | Land area required per ingredient for one animal (m²) | Calculated            |

------------------------------------------------------------------------

### III. Total Land Use per Animal

The **total land use per animal** is obtained by summing the land
required for all ingredients:

`Land_use_per_animal = sum(land_use_m2_per_unit) (m²/animal)`

| Variable              | Description                                    | Source in the Package |
|-----------------------|------------------------------------------------|-----------------------|
| `Land_use_per_animal` | Total land required per individual animal (m²) | Calculated            |

------------------------------------------------------------------------

### IV. Total Land Use for the Population

The total land required for a livestock category is:

`Land_use_total = Land_use_per_animal * population (m²)`

| Variable         | Description                                      | Source in the Package                                                           |
|------------------|--------------------------------------------------|---------------------------------------------------------------------------------|
| `population`     | Number of animals in the category                | Calculated via [`calculate_population()`](../reference/calculate_population.md) |
| `Land_use_total` | Total land area required for the population (m²) | Calculated                                                                      |

------------------------------------------------------------------------
