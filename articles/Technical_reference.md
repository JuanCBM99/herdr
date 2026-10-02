# Technical Data Dictionary: Input Files & Databases

## Overview & Architecture

`herdr` operates on a structured, file-based data engine. All project
data is separated into two categories:

1.  **User Input Tables (`user_data/`):** CSV templates that you create
    or edit to describe your specific livestock scenario.
2.  **Reference Libraries:** Package-bundled reference databases (`.csv`
    and `.parquet`) providing official IPCC coefficients, nutritional
    tables (FEDNA), crop productivities (FAOSTAT / MAPA), and bilateral
    trade flows.

``` text
┌────────────────────────────────────────────────────────────────────────────────────────┐
│                              PRIMARY DEMOGRAPHIC COHORT KEYS                           │
├─────────────────┬──────────────┬─────────────────┬─────────────────────────────────────┤
│  animal_tag     │  region      │  subregion      │  class_flex                         │
│  (Mandatory)    │  (Optional)  │  (Optional)     │  (Optional)                         │
│  e.g., "dairy"  │  e.g., spain │  e.g., galicia  │  e.g., "lactation_phase"            │
└─────────────────┴──────────────┴─────────────────┴─────────────────────────────────────┘
```

> ⚠️ **Critical Rule on Identifiers:** All table joins rely on exact
> string matching across the four demographic keys. Identifiers are
> **strictly lowercase** and use **underscores** instead of spaces
> (e.g., `mature_dairy_cattle`, NOT `Mature Dairy Cattle`).

------------------------------------------------------------------------

## I. User Input Schema (`user_data/`)

### 1. Herd Census (`livestock_census.csv`)

Declares the standing population count for each animal group.

| Column | Type | Unit | Required? | Description & Valid Values |
|:---|:--:|:--:|:--:|:---|
| `animal_tag` | character | — | **Yes** | Primary cohort identifier (e.g., `mature_dairy_cattle`, `fattening_pigs`). |
| `region` | character | — | No | Broad geographic stratum or country (e.g., `spain`). |
| `subregion` | character | — | No | Administrative province, autonomous community, or farm ID. |
| `class_flex` | character | — | No | Physiological status, life stage, or management variant. |
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
| `animal_tag`, `region`, `subregion`, `class_flex` | character | — | **Yes** | Demographic keys matching `livestock_census.csv`. |
| `animal_type` | character | — | **Yes** | Broad species: `cattle`, `sheep`, or `goat`. |
| `animal_subtype` | character | — | **Yes** | Production target: `dairy` or `meat`. |
| `production_role` | character | — | **Yes** | Functional role: `mature`, `replacement`, or `slaughter`. |
| `diet_tag` | character | — | **Yes** | Ration identifier matching `diet_profiles.csv`. |
| `cfi` | character | — | **Yes** | Maintenance coefficient descriptor matching `ipcc_coefficients.csv`. |
| `ca` | character | — | **Yes** | Feeding activity coefficient descriptor matching `ipcc_coefficients.csv`. |
| `c` | character | — | Ruminant | Growth coefficient category (e.g. `females`, `castrates`, `bulls`). |
| `milk_yield_kg_year` | numeric | kg/head/yr | Dairy | Annual average fresh milk yield per cow/ewe/doe. |
| `fat_content_pct` | numeric | % | Dairy | Milk fat content percentage (e.g. `3.8`). |
| `protein_content_pct` | numeric | % | Dairy | Milk crude protein percentage (e.g. `3.4`). If left blank or `0`, estimated via IPCC fat regression. |
| `wool_yield_kg_year` | numeric | kg/head/yr | Sheep | Annual greasy wool shorn per head (e.g. `3.0`). |
| `work_hours` | numeric | h/day | Working | Daily hours worked by draft animals (default: `0`). |
| `pr_sheep_goat` | numeric | kids/litter | Sheep/Goats | Prolificacy: average kids or lambs born per litter (e.g. `1.5`). |
| `pregnancy_rate` | numeric | 0.0–1.0 | Cattle | Annual calving or pregnancy rate (e.g. `0.90` for 90%). |

------------------------------------------------------------------------

### 4. Monogastric Definitions (`monogastric_definitions.csv`)

Configures swine and poultry cohorts following FEDNA and MAPA
nutritional standards.

| Column | Type | Unit | Required? | Description & Valid Values |
|:---|:--:|:--:|:--:|:---|
| `animal_tag`, `region`, `subregion`, `class_flex` | character | — | **Yes** | Demographic keys matching `livestock_census.csv`. |
| `animal_type` | character | — | **Yes** | Broad species: `swine` or `poultry`. |
| `animal_subtype` | character | — | **Yes** | Subtype: `meat` (swine, broilers) or `layer` (laying hens). |
| `production_role` | character | — | **Yes** | Functional role: `mature`, `replacement`, or `slaughter`. |
| `diet_tag` | character | — | **Yes** | Ration identifier matching `diet_profiles.csv`. |
| `cfi_maintenance` | numeric | kcal/kg^0.75 | **Yes** | Daily net energy maintenance requirement (e.g. `106` for swine). |
| `frac_fat_pct` | numeric | % | Monogastric | Percentage of live weight gain deposited as fat tissue. |
| `frac_protein_pct` | numeric | % | Monogastric | Percentage of live weight gain deposited as protein tissue. |
| `eggs_per_year` | numeric | eggs/hen/yr | Poultry | Annual egg production per hen (e.g. `310`). |
| `egg_weight_g` | numeric | g/egg | Poultry | Average weight of one fresh egg (e.g. `62.5`). |
| `fertility_rate` | numeric | 0.0–1.0 | Breeders | Hatching / fertility success rate in breeder flocks (e.g. `0.85`). |
| `piglets_born` | numeric | piglets/litter | Swine Sows | Total piglets born per farrowing (e.g. `14.5`). |
| `piglets_suckling` | numeric | piglets/litter | Swine Sows | Piglets weaned per farrowing litter (e.g. `12.5`). |

------------------------------------------------------------------------

### 5. Diet Profiles (`diet_profiles.csv`)

Specifies the macro-composition of each feeding ration.

| Column | Type | Unit | Required? | Description & Valid Values |
|:---|:--:|:--:|:--:|:---|
| `diet_tag` | character | — | **Yes** | Unique diet identifier (e.g., `diet_lactating_cows`). |
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
| `animal_tag`, `region`, `subregion`, `class_flex` | character | — | **Yes** | Demographic keys matching `livestock_census.csv`. |
| `system_base` | character | — | **Yes** | IPCC base system (e.g. `liquid_slurry`, `solid_storage`, `pasture_range_paddock`). |
| `system_variant` | character | — | Varies | System subtype (e.g. `with_cover`, `without_cover`, `compacted`). |
| `management_months` | integer | months | Varies | Storage duration in months (`1`, `3`, `4`, `6`, or `12` for slurry). |
| `system_climate` | character | — | **Yes** | Climate temperature: `cool`, `temperate`, or `warm`. |
| `system_subclimate` | character | — | Varies | Sub-climate: `boreal`, `temperate`, or `tropical`. |
| `climate_zone` | character | — | Varies | Moisture zone: `zone_dry`, `zone_moist`, or `zone_wet`. |
| `climate_moisture` | character | — | **Yes** | Moisture level: `dry`, `wet`, or `default`. |
| `b_0` | character | — | **Yes** | Maximum Methane Producing Capacity descriptor in `ipcc_coefficients.csv`. |
| `allocation` | numeric | 0.0–1.0 | **Yes** | Fraction of cohort manure handled by this system. Must sum to **1.0** per cohort. |

Check the [Manure Management
Guide](https://juancbm99.github.io/herdr/articles/Manure.md) for all
valid IPCC combinations.

------------------------------------------------------------------------

### 8. Reproduction Parameters (`reproduction_parameters.csv`)

Defines replacement rates used when running under automatic demographic
closure (`automatic_cycle = TRUE`).

| Column | Type | Unit | Description |
|:---|:--:|:--:|:---|
| `animal_tag` | character | — | Adult breeding female tag (e.g. `mature_dairy_cattle`, `breeder_sows`). |
| `parameter` | character | — | Parameter name: `replacement_rate`. |
| `value` | numeric | 0.0–1.0 | Annual replacement fraction (e.g. `0.27` for 27% replacement). |

------------------------------------------------------------------------

## II. Reference Libraries (Consult Only)

These internal datasets supply standard constants and parameters. **Do
not modify these files** unless adding custom research extensions.

#### 1. `feed_characteristics.csv` — Nutritional Reference Table

Comprehensive nutritional composition library derived from **FEDNA
(2019)** and Feedipedia: \* `ingredient`: Canonical identifier
(lowercase with underscores). \* `land_type`: Agroecological land
category: `cropland`, `grassland_convertible`,
`grassland_unconvertible`, or `none`. \* `DM_pct`: Dry matter content
percentage (% as-fed). \* `CP_pct`, `NDF_pct`, `ASH_pct`, `EE_pct`:
Crude Protein, Neutral Detergent Fiber, Ash, and Ether Extract (% DM).
\* `DE_pct`: Energy digestibility percentage (% DE). \*
`GE_feed_kcal_kg`: Gross energy content (kcal/kg DM). \*
`swine_DE_kcal_kg`, `swine_ME_kcal_kg`: Digestible and metabolizable
energy for pigs. \* `poultry_ME_kcal_kg`: Metabolizable energy for
poultry.

#### 2. `mapping.csv` — Agricultural & LCA Connector

Connects diet ingredients to statutory crop yield statistics and Life
Cycle Assessment databases: \* `ingredient`: Name in
`feed_characteristics.csv`. \* `yield_name`: Item name in FAOSTAT
(`fao_crops.parquet`) or forage database (`forages.parquet`). \*
`agribalyse_name`: Matching process in the Agribalyse v3.1 LCA database.
\* `economic_allocation`: Fraction (0.0 to 1.0) of crop cultivation area
allocated to this co-product (e.g. 0.80 for cereal grain vs. 0.20 for
straw).

#### 3. `forages.parquet` — Curated National Forage Database

Official national forage productivities from the Spanish Ministry of
Agriculture (**MAPA 2024**, Anuario de Estadística Agraria) and BC3
research: \* Contains real dry matter yields (kg DM/ha) for Atlantic,
Mediterranean, and Mountain pastures, annual silages, and hays. \*
Serves as statutory proxy fallback when international studies lack local
pasture productivity data.

#### 4. `fao_crops.parquet` — Statutory Crop Yields (Auto-Downloaded)

Auto-downloaded crop productivity database (~24 MB) queried from FAOSTAT
Production statistics across countries and reporting years (kg fresh
weight/ha, automatically converted to dry matter via `DM_pct`).
Downloaded automatically from GitHub Releases to the central user cache
upon first requirement to keep the core package lightweight.

#### 5. `fao_trade_matrix.parquet` — Dynamic Trade Background (Auto-Downloaded)

Auto-downloaded international bilateral trade matrix (~187 MB) recording
export and import flows. Downloaded automatically from GitHub Releases
to the central user cache the first time an assessment requires trade
resolution. Powers the recursive traceback algorithm to allocate feed
consumption back to primary producing nations under a 70%
self-sufficiency threshold.

#### 6. `ipcc_coefficients.csv` — IPCC Biological & Tier 2 Constants

Statutory biological constants from IPCC (2019 Refinement / 2006
Guidelines) Chapter 10 (Livestock): \* `description`: Canonical
descriptor matched against user inputs in `ruminant_definitions.csv` and
`manure_management.csv` (e.g. `pasture`, `stall`, `cattle/buffalo`,
`dairy cattle_high_productivity`). \* `value`: Numerical Tier 2
constant. \* `coefficient`: Biological parameter symbol: \* `cfi`: Net
energy coefficient for maintenance ($`Cf_i`$, $`\text{MJ/day/kg}`$). \*
`ca`: Feeding situation activity coefficient ($`C_a`$, dimensionless or
$`\text{MJ/day/kg}`$). \* `c`: Growth energy coefficient for cattle
($`C`$). \* `c_pregnancy`: Gestation energy coefficient
($`C_{\text{pregnancy}}`$). \* `b_0`: Maximum methane producing capacity
($`B_0`$, $`\text{m}^3\text{ CH}_4\text{/kg VS}`$). \* `units`:
Measurement units (e.g. `Dimensionless`, `MJ/day/kg`, `m³ CH₄/kg VS`).

#### 7. `ipcc_mm.csv` — IPCC Manure Management Reference Matrix

Complete statutory master matrix defining valid combinations of manure
management systems, climate zones, and emission parameters: \*
`system_base`: Base manure storage/treatment technology
(e.g. `liquid_slurry`, `solid_storage`, `pasture_range_paddock`,
`daily_spread`, `anaerobic_lagoon`, `pit_storage`, `composting`,
`deep_bedding`, `aerobic_treatment`, `incineration`, `poultry_manure`).
\* `system_variant`: Storage variant or crust cover
(e.g. `with_natural_crust_cover`, `without_natural_crust_cover`,
`uncovered`, `covered`, `in_vessel`). \* `management_months`: Storage
duration in months (for liquid slurry systems with crust or cover). \*
`climate_zone` & `climate_moisture`: Agroclimatic classification
(`zone_moist`, `zone_dry`, `zone_montane`, `wet`, `dry`). \*
`animal_type` & `animal_subtype`: Target animal species and category
(`cattle`, `swine`, `sheep`, `goat`, `poultry`, `dairy`, `other`). \*
`MCF_pct`: Methane Conversion Factor (% of $`B_0`$ realized as
$`\text{CH}_4`$). \* `EF3`: Direct $`\text{N}_2\text{O}`$ emission
factor ($`\text{kg N}_2\text{O-N / kg N}`$). \* `frac_gas`: Fraction of
manure N volatilized as $`\text{NH}_3 + \text{NO}_x`$
($`\text{Frac}_{\text{GasMS}}`$). \* `frac_leach`: Fraction of manure N
lost via leaching and runoff ($`\text{Frac}_{\text{LeachMS}}`$). \*
`EF4` & `EF5`: Indirect $`\text{N}_2\text{O}`$ emission factors for
atmospheric deposition (0.010) and aquatic leaching (0.011).

*See the [Manure Management
Guide](https://juancbm99.github.io/herdr/articles/Manure.md) for a
complete visual walkthrough and decision flowchart of every supported
combination.*
