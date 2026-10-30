# Technical Data Dictionary: Input Files & Databases

## Overview & Architecture

`herdr` operates on a structured, file-based data engine. All project
data is separated into two categories:

1.  **User Input Tables (`user_data/`):** CSV templates that you create
    or edit to describe your specific livestock scenario.
2.  **Reference Libraries:** Package-bundled reference databases (`.csv`
    and `.parquet`) providing official IPCC coefficients, nutritional
    tables (FEDNA), crop productivities (FAOSTAT / MAPA), bilateral
    trade flows, and feed life cycle inventories (ECOALIM v9 /
    AGRIBALYSE v3.1).

``` text
┌────────────────────────────────────────────────────────────────────────────────────────┐
│                          PRIMARY DEMOGRAPHIC COHORT KEYS                               │
├─────────────────┬─────────────────────────┬──────────────┬─────────────────────────────┤
│  farm_id        │  animal_tag             │  region      │  subregion                  │
│  (Mandatory)    │  (Mandatory)            │  (Optional)  │  (Optional)                 │
│  e.g., "farm_1" │  e.g., "dairy_cows"     │  e.g., spain │  e.g., "north"              │
└─────────────────┴─────────────────────────┴──────────────┴─────────────────────────────┘
```

> ⚠️ **Critical Rules on Identifiers & Ordering:** 1. All table joins
> rely on exact string matching across the four demographic keys
> (`farm_id`, `animal_tag`, `region`, `subregion`). Identifiers are
> **strictly lowercase** and use **underscores** instead of spaces
> (e.g., `mature_dairy_cattle`, NOT `Mature Dairy Cattle`). 2. The row
> order in `livestock_census.csv` defines the exact top-to-bottom order
> of cohorts in all output tables (`totals` and `intensities`) and plots
> ([`plot_herdr_results()`](https://juancbm99.github.io/herdr/reference/plot_herdr_results.md)).

------------------------------------------------------------------------

## I. User Input Schema (`user_data/`)

### 1. Herd Census (`livestock_census.csv`)

Declares the standing population count for each animal group and
establishes the display order across outputs.

| Column | Type | Unit | Required? | Description & Valid Values |
|:---|:--:|:--:|:--:|:---|
| `farm_id` | character | — | **Yes** | Farm or enterprise holding identifier (e.g., `farm_1`, `farm_north`). |
| `animal_tag` | character | — | **Yes** | Primary cohort identifier (e.g., `mature_dairy_cattle`, `fattening_pigs`). |
| `region` | character | — | No | Broad geographic stratum or country (e.g., `spain`). |
| `subregion` | character | — | No | Administrative province, autonomous community, or regional sub-unit (e.g., `north`, `south`). |
| `population` | numeric | head | **Yes** | Annual Average Population (AAP) of live animals present on farm. |

------------------------------------------------------------------------

### 2. Body Weights & Biological Cycles (`livestock_weights.csv`)

Defines the weight boundaries and life cycle durations used for
maintenance energy, growth, and throughput modeling.

[TABLE]

------------------------------------------------------------------------

### 3. Ruminant Definitions (`ruminant_definitions.csv`)

Links ruminant cohorts to IPCC Tier 2 metabolic equations, milk yields,
and reproductive traits.

| Column | Type | Unit | Required? | Description & Valid Values |
|:---|:--:|:--:|:--:|:---|
| `farm_id`, `animal_tag`, `region`, `subregion` | character | — | **Yes** | Demographic keys matching `livestock_census.csv`. |
| `animal_type` | character | — | **Yes** | Broad species (strict): `cattle`, `sheep`, or `goat`. |
| `animal_subtype` | character | — | **Yes** | Production target (strict): `dairy` or `meat`. |
| `production_role` | character | — | **Yes** | Functional lifecycle role (strict): `mature`, `replacement`, or `slaughter`. |
| `diet_tag` | character | — | **Yes** | Ration identifier matching `diet_profiles.csv`. |
| `cfi` | character | — | **Yes** | Maintenance coefficient descriptor matching `ipcc_coefficients.csv`. |
| `ca` | character | — | **Yes** | Feeding activity coefficient descriptor matching `ipcc_coefficients.csv`. |
| `c` | character | — | Cattle | Growth coefficient category for cattle (`females`, `castrates`, `bulls`). |
| `a`, `b` | character | — | Sheep/Goat | Growth constants `a` and `b` for sheep and goats matching `ipcc_coefficients.csv`. |
| `milk_yield_kg_year` | numeric | kg/head/yr | Dairy | Annual average fresh milk yield per cow/ewe/doe. |
| `fat_content_pct` | numeric | % | Dairy | Milk fat content percentage (e.g. `3.8`). |
| `protein_content_pct` | numeric | % | Dairy | Milk crude protein percentage (e.g. `3.4`). If left blank or `0`, estimated via IPCC fat regression. |
| `wool_yield_kg_year` | numeric | kg/head/yr | Sheep | Annual greasy wool shorn per head (e.g. `3.0`). |
| `work_hours` | numeric | h/day | Working | Daily hours worked by draft animals (default: `0`). |
| `pr_sheep_goat` | numeric | kids/litter | Sheep/Goats | Prolificacy: average kids or lambs born per litter (e.g. `1.5`). |
| `c_pregnancy_cattle` | character | — | Cattle | Pregnancy coefficient descriptor matching `ipcc_coefficients.csv`. |
| `pregnancy_rate` | numeric | 0.0–1.0 | Cattle | Annual calving or pregnancy rate (e.g. `0.90` for 90%). |
| `replacement_rate` | numeric | 0.0–1.0 | Breeding | Annual replacement or culling fraction (e.g. `0.27` for 27% replacement). Used in demographic closure and adult meat culling. |

------------------------------------------------------------------------

### 4. Monogastric Definitions (`monogastric_definitions.csv`)

Configures swine and poultry cohorts following FEDNA and MAPA
nutritional standards.

| Column | Type | Unit | Required? | Description & Valid Values |
|:---|:--:|:--:|:--:|:---|
| `farm_id`, `animal_tag`, `region`, `subregion` | character | — | **Yes** | Demographic keys matching `livestock_census.csv`. |
| `animal_type` | character | — | **Yes** | Broad species (strict): `swine` or `poultry`. |
| `animal_subtype` | character | — | **Yes** | Subtype (strict): `meat` for `swine`; `meat` or `layer` for `poultry`. |
| `production_role` | character | — | **Yes** | Functional lifecycle role (strict): `mature`, `replacement`, or `slaughter`. |
| `diet_tag` | character | — | **Yes** | Ration identifier matching `diet_profiles.csv`. |
| `cfi_maintenance` | numeric | kcal/kg^alpha | **Yes** | Daily metabolizable energy maintenance requirement (e.g. `104.5`–`206` for swine, `113`–`131` for poultry). |
| `frac_fat_pct` | numeric | fraction | Monogastric | Fraction of live weight gain deposited as fat tissue (e.g. `0.266`). |
| `frac_protein_pct` | numeric | fraction | Monogastric | Fraction of live weight gain deposited as protein tissue (e.g. `0.153`). |
| `eggs_per_year` | numeric | eggs/hen/yr | Poultry | Annual egg production per hen (e.g. `297`–`314` for layers, `211.7` for meat breeders). |
| `egg_weight_g` | numeric | g/egg | Poultry | Average weight of one fresh egg (e.g. `60`–`64`). |
| `fertility_rate` | numeric | 0.0–1.0 | Breeders | Hatching / fertility success rate in breeder flocks (e.g. `0.83`). |
| `alpha` | numeric | — | Swine | Metabolic weight exponent (e.g. `0.60` for growing pigs, `0.75` for adult sows/boars; fixed at `0.75` for poultry). |
| `piglets_born` | numeric | piglets/litter | Swine Sows | Total piglets born per farrowing (e.g. `16.26`). |
| `piglets_suckling` | numeric | piglets/litter | Swine Sows | Piglets weaned per farrowing litter (e.g. `13.05`). |
| `replacement_rate` | numeric | 0.0–1.5 | Breeding | Annual replacement or culling fraction (e.g. `0.25` for sows, `0.714` for layers, `1.2126` for meat breeders). |

------------------------------------------------------------------------

### 5. Diet Profiles (`diet_profiles.csv`)

Specifies the macro-composition of each feeding ration.

| Column | Type | Unit | Required? | Description & Valid Values |
|:---|:--:|:--:|:--:|:---|
| `diet_tag` | character | — | **Yes** | Unique diet identifier (e.g., `diet_dairy_mature`). |
| `forage_share` | numeric | % | **Yes** | Forage proportion of diet dry matter. |
| `concentrate_share` | numeric | % | **Yes** | Concentrate / grain proportion of diet dry matter. |
| `milk_share` | numeric | % | **Yes** | Whole fresh milk proportion (suckling young animals). |
| `milk_replacer_share` | numeric | % | **Yes** | Commercial milk replacer powder proportion. |

> ⚠️ **Sum Rule:** For every `diet_tag`, the four shares must sum to
> exactly **100%**:  
> `forage_share + concentrate_share + milk_share + milk_replacer_share = 100.0`

------------------------------------------------------------------------

### 6. Diet Ingredients (`diet_ingredients.csv`)

Disaggregates each diet macro-category into specific crops and
feedstuffs.

| Column | Type | Unit | Required? | Description & Valid Values |
|:---|:--:|:--:|:--:|:---|
| `diet_tag` | character | — | **Yes** | Diet identifier linking to `diet_profiles.csv`. |
| `ingredient_type` | character | — | **Yes** | Category: `forage`, `concentrate`, `milk`, or `milk_replacer`. |
| `ingredient` | character | — | **Yes** | Name matching `feed_characteristics.csv` exactly (e.g. `corn_silage`). |
| `ingredient_share` | numeric | % | **Yes** | Proportion within that specific `ingredient_type` (must sum to **100%** per category). |
| `country_of_origin` | character | — | No | Cultivation country. If left `NA`, the model traces origin via bilateral FAO trade matrices. |
| `custom_yield_kg_ha` | numeric | kg DM/ha | No | Optional farm-measured yield. Overrides national FAO statistics and tags origin as `"Custom Data"`. |

------------------------------------------------------------------------

### 7. Manure Management (`manure_management.csv`)

Specifies housing, waste storage, and pasture deposition systems.

| Column | Type | Unit | Required? | Description & Valid Values |
|:---|:--:|:--:|:--:|:---|
| `farm_id`, `animal_tag`, `region`, `subregion` | character | — | **Yes** | Demographic keys matching `livestock_census.csv`. |
| `system_base` | character | — | **Yes** | IPCC base system (e.g. `liquid_slurry`, `solid_storage`, `pasture_range_paddock`, `poultry_manure`). |
| `system_variant` | character | — | Varies | System subtype (e.g. `with_natural_crust_cover`, `with_cover`, `with_litter`). |
| `management_months` | character/int | months | Varies | Storage duration in months (`1`, `3`, `4`, `6`, `12` for slurry/pit; `>1` or `<1` for deep bedding). |
| `system_climate` | character | — | **Yes** | Climate temperature: `cool`, `temperate`, or `warm`. |
| `system_subclimate` | character | — | Varies | Sub-climate: `boreal`, `temperate`, or `tropical`. |
| `climate_zone` | character | — | Varies | Moisture zone: `zone_dry`, `zone_moist`, `zone_wet`, or `zone_montane`. |
| `climate_moisture` | character | — | **Yes** | Moisture level: `dry`, `wet`, or `default`. |
| `b_0` | character | — | **Yes** | Maximum Methane Producing Capacity descriptor in `ipcc_coefficients.csv`. |
| `allocation` | numeric | 0.0–1.0 | **Yes** | Fraction of cohort manure handled by this system. Must sum to **1.0** per cohort. |

Check the [Manure Management
Guide](https://juancbm99.github.io/herdr/articles/Manure.md) for all
valid IPCC combinations.

------------------------------------------------------------------------

## II. Output Schema (`generate_impact_assessment()`)

Running
[`generate_impact_assessment()`](https://juancbm99.github.io/herdr/reference/generate_impact_assessment.md)
returns a list with two tables (`results$totals` and
`results$intensities`) and writes them to `output/`:

#### 1. Table 1: `results$totals` (`output/total_emissions_and_land_use.csv`)

Reports the direct physical inventory and per-head footprint of each
cohort:

- **Identification & Role:** `farm_id`, `region`, `subregion`,
  `animal_tag`, `animal_type`, `animal_subtype`, `production_role`,
  `population`.
- **Feed & Protein Intake:** `DMI_kgday` (kg DM/head/day),
  `feed_intake_kg` (kg DM/year), `feed_CP_total_kg` (kg CP/year),
  `feed_CP_cropland_kg` (kg arable cropland CP/year).
- **Native Gas Emissions (Gg/year):** `CH4_enteric_Gg`, `CH4_manure_Gg`,
  `N2O_direct_Gg`, `N2O_vol_Gg`, `N2O_lea_Gg`, `N2O_indirect_Gg`.
- **Scope 1 & Scope 3 Feed Carbon Footprints (Gg CO2e/year):**
  `CO2eq_enteric`, `CO2eq_manure`, `CO2eq_N2O_direct`,
  `CO2eq_N2O_indirect`, `CO2eq_N2O`, `CO2eq_feed_fossil_Gg`,
  `CO2eq_feed_luc_Gg`, `CO2eq_feed_Gg`, `CO2eq_Total_Gg` (Scope 1),
  `CO2eq_cradle_to_farm_Gg` (Scope 1 + Feed).
- **Agroecological Land & Water Footprints:** `Land_cropland_m2`,
  `Land_grassland_convertible_m2`, `Land_grassland_unconvertible_m2`,
  `Land_other_m2`, `Land_m2`, `Water_feed_m3`
  ($`\text{m}^3\text{ depriv.}`$).
- **Per-Head Annual Intensities:** `CO2eq_Total_head` (kg CO2e/head/yr),
  `CO2eq_cradle_to_farm_head` (kg CO2e/head/yr), `Land_head`
  ($`\text{m}^2`$/head/yr), `Water_head`
  ($`\text{m}^3\text{ depriv.}`$/head/yr).

#### 2. Table 2: `results$intensities` (`output/product_intensities.csv`)

Reports physical outputs and enterprise-integrated intensities for each
cohort:

- **Physical Outputs (kg/year):** `milk_FPCM_kg`,
  `meat_carcass_weight_kg`, `egg_fresh_kg`, `wool_kg`,
  `total_protein_kg`.
- **Commercial Product Intensities:**
  - **Milk (per kg FPCM):** `GHG_intensity_milk`,
    `GHG_cradle_to_farm_milk`, `Land_intensity_milk`,
    `Water_intensity_milk`.
  - **Meat (per kg carcass weight):** `GHG_intensity_meat`,
    `GHG_cradle_to_farm_meat`, `Land_intensity_meat`,
    `Water_intensity_meat`.
  - **Eggs (per kg fresh eggs):** `GHG_intensity_egg`,
    `GHG_cradle_to_farm_egg`, `Land_intensity_egg`,
    `Water_intensity_egg`.
  - **Wool (per kg greasy wool):** `GHG_intensity_wool`,
    `GHG_cradle_to_farm_wool`, `Land_intensity_wool`,
    `Water_intensity_wool`.
- **Nutritional Protein Intensities (per kg edible protein):**
  `GHG_intensity_protein`, `GHG_cradle_to_farm_protein`,
  `Land_intensity_protein`, `Water_intensity_protein`.
- **Protein Feed Efficiency (Mottet et al. 2017):** `Protein_FCR_total`,
  `Protein_FCR_cropland`.

------------------------------------------------------------------------

## III. Reference Libraries (Consult Only)

These internal datasets supply standard constants and parameters. **Do
not modify these files** unless adding custom research extensions.

#### 1. `feed_characteristics.csv` — Nutritional Reference Table

Comprehensive nutritional composition library derived from **FEDNA
(2019)** and Feedipedia: \* `ingredient`: Canonical identifier
(lowercase with underscores). \* `ingredient_type`: Macro category
(`forage`, `concentrate`, `milk`, `milk_replacer`). \* `land_type`:
Agroecological land category: `cropland`, `grassland_convertible`,
`grassland_unconvertible`, or `none`. \* `DM_pct`: Dry matter content
percentage (% as-fed). \* `CP_pct`, `NDF_pct`, `ASH_pct`, `EE_pct`:
Crude Protein, Neutral Detergent Fiber, Ash, and Ether Extract (% DM).
\* `DE_pct`: Energy digestibility percentage for ruminants (% DE). \*
`GE_feed_kcal_kg`: Gross energy content (kcal/kg DM). \*
`swine_DE_kcal_kg`, `swine_ME_kcal_kg`: Digestible and metabolizable
energy for pigs (kcal/kg DM). \* `poultry_ME_kcal_kg`: Metabolizable
energy for poultry (kcal/kg DM).

#### 2. `mapping.csv` — Agricultural & LCA Connector

Connects diet ingredients to statutory crop yield statistics and Life
Cycle Assessment databases: \* `ingredient`: Name in
`feed_characteristics.csv`. \* `yield_name`: Item name in FAOSTAT
(`fao_crops.parquet`) or forage database (`forages.parquet`). \*
`agribalyse_name`: Matching process in the INRAE / ADEME ECOALIM v9 and
AGRIBALYSE v3.1 LCA databases. \* `economic_allocation`: Fraction (0.0
to 1.0) of crop cultivation area allocated to this co-product (e.g. 0.80
for cereal grain vs. 0.20 for straw).

#### 3. `ecoalim.parquet` — Feed Life Cycle Inventory Database

Bundled reference database containing environmental characterization
factors from **INRAE / ADEME ECOALIM v9** and **AGRIBALYSE v3.1** under
the European Commission’s **Environmental Footprint (EF 3.1)** standard:
\* `Climate change - Fossil (kg CO2 eq/kg)`: Fossil greenhouse gas
emissions per kg of feed fresh weight. \*
`Climate change - Land use and LU change (kg CO2 eq/kg)`: Direct
land-use change / deforestation greenhouse gas emissions per kg fresh
weight. \* `Water use (m3 depriv.)`: Freshwater deprivation
characterized with the international **AWARE** model
($`m^3\text{ depriv.} = m^3\text{ world-eq} / \text{kg}`$ fresh weight).

#### 4. `forages.parquet` — Curated National Forage Database

Official national forage productivities from the Spanish Ministry of
Agriculture (**MAPA 2024**, Anuario de Estadística Agraria) and BC3
research (reported on a 100% dry matter basis, kg DM/ha).

#### 5. `fao_crops.parquet` — Statutory Crop Yields (Auto-Downloaded)

Auto-downloaded crop productivity database (~24 MB) queried from FAOSTAT
Production statistics across countries and reporting years (kg fresh
weight/ha, automatically converted to dry matter via `DM_pct`).

#### 6. `fao_trade_matrix.parquet` — Dynamic Trade Background (Auto-Downloaded)

Auto-downloaded international bilateral trade matrix (~187 MB) recording
export and import flows. Powers the recursive traceback algorithm to
allocate feed consumption back to primary producing nations under a 70%
self-sufficiency threshold.

#### 7. `ipcc_coefficients.csv` — IPCC Biological & Tier 2 Constants

Statutory biological constants from IPCC (2019 Refinement / 2006
Guidelines) Chapter 10 (Livestock): \* `description`: Canonical
descriptor matched against user inputs in `ruminant_definitions.csv` and
`manure_management.csv`. \* `value`: Numerical Tier 2 constant. \*
`coefficient`: Biological parameter symbol (`cfi`, `ca`, `c`, `a`, `b`,
`c_pregnancy`, `b_0`). \* `animal_type`: Species applicability
(`cattle`, `sheep`, `goat`, `swine`, `poultry`). \* `units`: Measurement
units (`Dimensionless`, `MJ/day/kg`, `m3 CH4/kg VS`).

#### 8. `ipcc_mm.csv` — IPCC Manure Management Reference Matrix

Complete statutory master matrix defining valid combinations of manure
management systems, climate zones, and emission parameters: \*
`system_base`, `system_variant`, `management_months`, `system_climate`,
`system_subclimate`, `climate_zone`, `climate_moisture`. \*
`animal_type` & `animal_subtype`: Target animal species (`cattle`,
`sheep`, `goat`, `swine`, `poultry`) and strict subtype (`dairy`,
`meat`, `layer`). \* `MCF_pct`: Methane Conversion Factor (% of $`B_0`$
realized as $`\text{CH}_4`$). \* `EF3`, `EF4`, `EF5`, `frac_gas`,
`frac_leach`: Direct and indirect $`\text{N}_2\text{O}`$ emission
factors and nitrogen loss fractions.
