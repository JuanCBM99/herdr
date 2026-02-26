# Package Function Reference

## 🛠️ Configuration Reference Guide

This guide defines the valid **Paths** for your `manure_management.csv`.
For the model to function, your entries must match the IPCC database
logic exactly.

### 1. High-Detail Systems (Storage Duration Dependent)

These systems require specific **Management Months**, **Sub-climates**,
and **Climate Zones**.

#### A. Liquid Slurry & Pit Storage

Factors for these systems change based on exactly how many months the
manure is stored.

| System Base | System Variant | Months | Climate | Sub-climate | Climate Zone |
|:---|:---|:---|:---|:---|:---|
| **liquid_slurry** | `with_natural_crust_cover`, `without_natural_crust_cover`, `with_cover` | **1, 3, 4, 6, 12** | `cool` | `boreal`, `temperate` | `zone_dry`, `zone_wet` |
| **liquid_slurry** | `with_natural_crust_cover`, `without_natural_crust_cover`, `with_cover` | **1, 3, 4, 6, 12** | `warm` | `temperate` | `zone_dry`, `zone_wet` |
| **liquid_slurry** | `with_natural_crust_cover`, `without_natural_crust_cover`, `with_cover` | **1, 3, 4, 6, 12** | `warm` | `tropical` | `zone_dry`, `zone_wet`, `zone_montane`, `zone_moist` |
| **pit_storage** | `below_animal_confinements` | **1, 3, 4, 6, 12** | `cool` | `boreal`, `temperate` | `zone_dry`, `zone_wet` |
| **pit_storage** | `below_animal_confinements` | **1, 3, 4, 6, 12** | `warm` | `temperate` | `zone_dry`, `zone_wet` |
| **pit_storage** | `below_animal_confinements` | **1, 3, 4, 6, 12** | `warm` | `tropical` | `zone_dry`, `zone_wet`, `zone_montane`, `zone_moist` |

#### B. Anaerobic Lagoon

Lagoon factors are based on climate/moisture rather than specific
months.

| System Base | System Variant | Months | Climate | Sub-climate | Climate Zone |
|:---|:---|:---|:---|:---|:---|
| **anaerobic_lagoon** | `below_animal_confinements` | *(leave empty)* | `cool` | `boreal`, `temperate` | `zone_dry`, `zone_wet` |
| **anaerobic_lagoon** | `below_animal_confinements` | *(leave empty)* | `warm` | `temperate` | `zone_dry`, `zone_wet` |
| **anaerobic_lagoon** | `below_animal_confinements` | *(leave empty)* | `warm` | `tropical` | `zone_dry`, `zone_wet`, `zone_montane`, `zone_moist` |

#### C. Deep Bedding

The logic here focuses on mixing and whether the storage cycle is longer
or shorter than one month.

| System Base | System Variant | Months | Climate | Sub-climate | Climate Zone |
|:---|:---|:---|:---|:---|:---|
| **deep_bedding** | `active_mixing`,`no_mixing` | **\>1** | `cool` | `boreal`, `temperate` | `zone_dry`, `zone_wet` |
| **deep_bedding** | `active_mixing`,`no_mixing` | **\>1** | `warm` | `temperate` | `zone_dry`, `zone_wet` |
| **deep_bedding** | `active_mixing`,`no_mixing` | **\>1** | `warm` | `tropical` | `zone_dry`, `zone_wet`, `zone_montane`, `zone_moist` |
| **deep_bedding** | `active_mixing`,`no_mixing` | **\<1** | `cool`, `temperate` OR `warm` | *(leave empty)* | *(leave empty)* |

------------------------------------------------------------------------

### 2. Standard Systems (Climate-Only)

For these systems, sub-climates and zones are not required. Leave those
columns blank in your CSV.

| System Base | Valid Variants | System Climate |
|:---|:---|:---|
| **solid_storage** | `additives`, `bulking_agent_addition`, `covered_compacted` OR *(leave empty)* | `cool`, `temperate` OR `warm` |
| **composting** | `intensive_windrow`, `passive_windrow`, `static_pile` OR `vessel` | `cool`, `temperate` OR `warm` |
| **anaerobic_digester** | `low_leak_high_gastight`, `low_leakage_open_storage`, `low_leak_low_gastight`, `high_leakage_open_storage`, `high_leakage_high_gastight` OR `high_leakage_low_gastight` | `cool`, `temperate` OR `warm` |
| **aerobic_treatment** | `forced_aeration` OR `natural_aeration` | `cool`, `temperate` OR `warm` |
| **pasture_range_paddock** | *(leave empty)* | `cool`, `temperate` OR `warm` |
| **burned_for_fuel** | *(leave empty)* | `cool`, `temperate` OR `warm` |
| **dry_lot** | *(leave empty)* | `cool`, `temperate` OR `warm` |
| **daily_spread** | *(leave empty)* | `cool`, `temperate` OR `warm` |
| **poultry_manure** | `with_litter` OR `without_litter` | `cool`, `temperate` OR `warm` |

------------------------------------------------------------------------

### 💡 Quick Logic Rules

#### The “Warm” Rule

- **Warm + Temperate:** Only supports `zone_dry` and `zone_wet`.
- **Warm + Tropical:** Unlocks all zones: `zone_dry`, `zone_wet`,
  `zone_moist`, and `zone_montane`.

#### Data Entry Tips

1.  **Case Sensitivity:** All entries must be **lowercase** (e.g.,
    `zone_wet`, NOT `Zone_Wet`).
2.  **Empty Values:** Leave cells blank for columns marked as
    `(leave empty)`.
3.  **Allocation:** For each unique animal/region combination, the sum
    of `allocation` must equal **1.0 (100%)**.
