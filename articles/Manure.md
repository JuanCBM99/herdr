# Manure Management Systems: Configuration Guide

## Introduction & Purpose

In `herdr`, manure methane ($`CH_4`$) and direct/indirect nitrous oxide
($`N_2O`$) emissions are governed by the systems declared in
`user_data/manure_management.csv`.

To ensure IPCC Tier 2 compliance, the values entered must match official
IPCC system definitions and climate combinations recorded in
`ipcc_mm.csv`. If an invalid combination is entered, the model cannot
assign emission factors.

This guide provides the complete decision matrix of valid paths for your
`manure_management.csv`. For the mathematical emission equations, see
the [Theoretical
Basis](https://juancbm99.github.io/herdr/articles/Theoretical_basis.html#iv-manure-management-methane--nitrous-oxide).

------------------------------------------------------------------------

## 1. The Two Fundamental Rules

#### Rule 1: Allocation Must Equal Exactly 1.0 per Cohort

For each unique animal cohort (defined by `animal_tag`, `region`,
`subregion`, and `class_flex`), the sum of the `allocation` column
across all assigned manure systems must equal **1.0** (100%):

``` math
\sum \text{allocation} = 1.0
```

*Example:* If dairy cows spend 7 months indoors with liquid slurry and 5
months grazing on pasture, declare two rows for that cohort: \* Row 1:
`system_base = "liquid_slurry"`, `allocation = 0.583` ($`7 / 12`$). \*
Row 2: `system_base = "pasture_range_paddock"`, `allocation = 0.417`
($`5 / 12`$).

#### Rule 2: Strict Lowercase Identifiers

All system descriptors, variants, and climate zones are **strictly
lowercase** (e.g., `zone_dry`, NOT `Zone_Dry`).

------------------------------------------------------------------------

## 2. High-Detail Systems (Storage Duration Dependent)

For slurry, pits, and lagoons, Methane Conversion Factors ($`MCF`$)
depend directly on **how many months** manure is stored and the regional
temperature/moisture profile.

#### A. Liquid Slurry & Pit Storage

[TABLE]

#### B. Anaerobic Lagoon

Anaerobic lagoons are climate-dependent but do not require an explicit
storage duration.

| System Base | System Variant | Months | Climate | Sub-climate | Climate Zone | Moisture |
|:---|:---|:--:|:--:|:---|:---|:--:|
| **`anaerobic_lagoon`** | `uncovered` | *(leave blank)* | `cool` | `boreal`, `temperate` | `zone_dry`, `zone_moist` | `dry` or `wet` |
| **`anaerobic_lagoon`** | `uncovered` | *(leave blank)* | `warm` | `temperate` | `zone_dry`, `zone_moist` | `dry` or `wet` |
| **`anaerobic_lagoon`** | `uncovered` | *(leave blank)* | `warm` | `tropical` | `zone_dry`, `zone_wet`, `zone_montane`, `zone_moist` | `dry` or `wet` |

#### C. Deep Bedding

Deep bedding depends on whether the bedding undergoes active mixing and
whether the cleaning cycle is longer or shorter than one month.

[TABLE]

------------------------------------------------------------------------

## 3. Standard Systems (Climate-Only)

For standard solid, composting, digestion, and grazing systems,
sub-climates, storage months, and climate zones are not required. Leave
those columns blank.

| System Base (`system_base`) | Valid Variants (`system_variant`) | System Climate (`system_climate`) | Moisture (`climate_moisture`) |
|:---|:---|:---|:--:|
| **`solid_storage`** | `additives`, `bulking_agent_addition`, `covered_compacted`, or *(leave blank)* | `cool`, `temperate`, or `warm` | `default` |
| **`composting`** | `intensive_windrow`, `passive_windrow`, `static_pile`, or `vessel` | `cool`, `temperate`, or `warm` | `default` |
| **`anaerobic_digester`** | `low_leakage_open_storage`, `low_leak_high_gastight`, `low_leak_low_gastight`, `high_leakage_open_storage`, `high_leakage_high_gastight`, `high_leakage_low_gastight` | `cool`, `temperate`, or `warm` | `default` |
| **`aerobic_treatment`** | `forced_aeration` or `natural_aeration` | `cool`, `temperate`, or `warm` | `default` |
| **`pasture_range_paddock`** | *(leave blank)* | `cool`, `temperate`, or `warm` | `default` |
| **`daily_spread`** | *(leave blank)* | `cool`, `temperate`, or `warm` | `default` |
| **`dry_lot`** | *(leave blank)* | `cool`, `temperate`, or `warm` | `default` |
| **`burned_for_fuel`** | *(leave blank)* | `cool`, `temperate`, or `warm` | `default` |
| **`poultry_manure`** | `with_litter` or `without_litter` | `cool`, `temperate`, or `warm` | `default` |

------------------------------------------------------------------------

## 4. Climate & Moisture Definitions

#### Temperature Classification (`system_climate`):

- **`cool`:** Mean annual temperature (MAT) $`< 15^\circ\text{C}`$
  (e.g., Northern and Atlantic Spain, Central and Northern Europe).
- **`temperate`:** MAT between $`15^\circ\text{C}`$ and
  $`25^\circ\text{C}`$ (e.g., Mediterranean basin, Southern Spain).
- **`warm`:** MAT $`> 25^\circ\text{C}`$ (e.g., tropical lowlands).

#### Moisture Classification (`climate_moisture`):

- For **High-Detail Systems:** Align with `climate_zone`:
  - If `zone_wet`, `zone_moist`, or `zone_montane` $`\rightarrow`$ use
    **`wet`**.
  - If `zone_dry` $`\rightarrow`$ use **`dry`**.
- For **Standard Systems:** Use **`default`**.

------------------------------------------------------------------------

## Next Steps

- [Technical Data
  Dictionary](https://juancbm99.github.io/herdr/articles/Technical_reference.md)
  — data schema for `manure_management.csv` and `ipcc_mm.csv`.
- [General
  Workflow](https://juancbm99.github.io/herdr/articles/Workflow.md) —
  how manure calculations integrate into the modeling pipeline.
- [Theoretical
  Basis](https://juancbm99.github.io/herdr/articles/Theoretical_basis.html#iv-manure-management-methane--nitrous-oxide)
  — full mathematical equations for $`VS`$, $`B_0`$, $`MCF`$, $`EF_3`$,
  $`EF_4`$, and $`EF_5`$.
