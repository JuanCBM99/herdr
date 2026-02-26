# Theoretical Basis: IPCC Tier 2 Methodology

## Introduction

The **herdr** package performs environmental impact assessments based on
the **2019 Refinement to the 2006 IPCC Guidelines**. The core of the
model is the **Energy Balance**, where Gross Energy (GE) intake is
derived from the animal’s physiological requirements.

------------------------------------------------------------------------

## I. Net Energy for Maintenance ($`NE_m`$) (Eq. 10.3)

$`NE_m`$ is the energy required to maintain the animal’s energy balance
without gain or loss of weight.

**Formula:**
``` math
NE_m = Cf_i \cdot \text{Weight}^{0.75}
```

| Variable | Description | Source in the Package |
|:---|:---|:---|
| $`NE_m`$ | Net energy for maintenance (MJ/day) | Calculated |
| $`Cf_i`$ | Maintenance coefficient based on animal species/status | `ipcc_coefficients.csv` |
| Weight | Live weight of the individual animal (kg) | `weights.csv` |

------------------------------------------------------------------------

## II. Net Energy for Activity ($`NE_a`$)

$`NE_a`$ represents energy used for grazing, walking, or other
activities.

- **Cattle:** $`NE_a = C_a \cdot NE_m`$ (Eq. 10.4)
- **Sheep and Goats:** $`NE_a = C_a \cdot \text{Weight}`$ (Eq. 10.5)

| Variable | Description | Source in the Package |
|:---|:---|:---|
| $`NE_a`$ | Net energy for activity (MJ/day) | Calculated |
| $`C_a`$ | Activity coefficient (varies by feeding situation) | `ipcc_coefficients.csv` |
| $`NE_m`$ | Maintenance energy | Result of Eq. 10.3 |
| Weight | Live weight of the animal (kg) | `weights.csv` |

------------------------------------------------------------------------

## III. Net Energy for Growth ($`NE_g`$)

Energy required for tissue deposition (weight gain).

- **Cattle and Buffalo:** (Eq. 10.6)
  ``` math
  NE_{g} = 22.02 \cdot \left( \frac{BW}{C \cdot MW} \right)^{0.75} \cdot WG^{1.097}
  ```

- **Sheep and Goats:** (Eq. 10.7)
  ``` math
  NE_g = \frac{WG_{lamb/kid} \cdot (a + 0.5b(BW_i + BW_f))}{365}
  ```

| Variable | Description | Source in the Package |
|:---|:---|:---|
| $`BW`$ | Average live body weight of the animal (kg) | `weights.csv` |
| $`MW`$ | Mature body weight of an adult female (kg) | `weights.csv` |
| $`WG`$ | Daily weight gain (kg/day) | `weights.csv` |
| $`C, a, b`$ | IPCC species-specific growth constants | `ipcc_coefficients.csv` |
| $`BW_i / BW_f`$ | Initial and final weight for the period (kg) | `weights.csv` |

------------------------------------------------------------------------

## IV. Net Energy for Lactation ($`NE_l`$)

Energy required for milk production based on volume and fat content.

- **Cattle:**
  $`NE_l = \text{Milk} \cdot (1.47 + 0.40 \cdot \text{Fat})`$ (Eq. 10.8)
- **Sheep and Goats:** $`NE_l = \text{Milk} \cdot EV_{milk}`$ (Eq. 10.9)

| Variable | Description | Source in the Package |
|:---|:---|:---|
| $`NE_l`$ | Net energy for lactation (MJ/day) | Calculated |
| Milk | Average daily milk yield (kg/day) | `livestock_definitions.csv` |
| Fat | Milk fat content percentage (%) | `livestock_definitions.csv` |
| $`EV_{milk}`$ | Energy required to produce 1 kg of milk (MJ/kg) | `ipcc_coefficients.csv` |

------------------------------------------------------------------------

## V. Net Energy for Work, Wool, and Pregnancy

- **Work ($`NE_{work}`$):**
  ``` math
  NE_{work} = 0.10 \cdot NE_m \cdot \text{Hours}
  ```
- **Wool ($`NE_{wool}`$):**
  ``` math
  NE_{wool} = \frac{\text{Pr}_{wool} \cdot EV_{wool}}{365}
  ```
- **Pregnancy ($`NE_p`$):**
  ``` math
  NE_p = C_{pregnancy} \cdot NE_m
  ```

| Variable | Description | Source in the Package |
|:---|:---|:---|
| Hours | Number of hours worked per day (draft animals) | `livestock_definitions.csv` |
| $`\text{Pr}_{wool}`$ | Annual wool production (kg/year) | `livestock_definitions.csv` |
| $`EV_{wool}`$ | Energy per kg of wool (IPCC default 24 MJ/kg) | Constant |
| $`C_{pregnancy}`$ | Pregnancy energy coefficient | `ipcc_coefficients.csv` |

------------------------------------------------------------------------

## VI. Efficiency Ratios ($`REM`$ and $`REG`$)

Based on IPCC Equations 10.14 and 10.15, derived from feed digestibility
($`DE`$):

- **For Maintenance ($`REM`$):**
  ``` math
  REM = \left[ 1.123 - (4.092 \cdot 10^{-3} \cdot DE) + (1.126 \cdot 10^{-5} \cdot (DE)^2) - \left( \frac{25.4}{DE} \right) \right]
  ```
- **For Growth ($`REG`$):**
  ``` math
  REG = \left[ 1.164 - (5.16 \cdot 10^{-3} \cdot DE) + (1.308 \cdot 10^{-5} \cdot (DE)^2) - \left( \frac{37.4}{DE} \right) \right]
  ```

| Variable | Description | Source in the Package |
|:---|:---|:---|
| $`REM`$ | Efficiency of digestible energy for maintenance | Calculated |
| $`REG`$ | Efficiency of digestible energy for growth | Calculated |
| $`DE`$ | Digestibility of feed as % of gross energy | `feed_characteristics.csv` |

------------------------------------------------------------------------

## VII. Gross Energy (GE)

**Gross Energy (GE)** is the total daily energy intake required to
sustain all physiological functions.

**Formula:**
``` math
GE = \frac{\left( \frac{NE_m + NE_a + NE_l + NE_{work} + NE_p}{REM} + \frac{NE_g + NE_{wool}}{REG} \right)}{DE/100}
```

| Variable | Description | Source in the Package |
|:---|:---|:---|
| $`GE`$ | Total daily gross energy requirement (MJ/day) | Calculated |
| $`NE_x`$ | Various net energy requirements calculated above | Calculated Results |
| $`REM/REG`$ | Efficiency ratios | Results of Eq. 10.14/10.15 |
| $`DE`$ | Feed digestibility (%) | `feed_characteristics.csv` |

------------------------------------------------------------------------

## VIII. Methane Module: Enteric Fermentation ($`CH_4`$)

#### Methane Emission Factor ($`EF`$) (Eq. 10.21)

``` math
EF = \frac{GE \cdot \left( \frac{Y_m}{100} \right) \cdot 365}{55.65}
```

| Variable | Description | Source in Package |
|:---|:---|:---|
| $`EF`$ | Methane emission factor (kg $`CH_4`$/head/year) | Calculated |
| $`GE`$ | Daily gross energy intake (MJ/day) | Result of Section VII |
| $`Y_m`$ | Methane conversion factor (%) | `feed_characteristics.csv` |
| 55.65 | Energy content of methane (MJ/kg $`CH_4`$) | Constant |

#### Total Enteric Emissions ($`E_T`$) (Eq. 10.19)

``` math
E_T = \sum_{(P)} EF_{(T,P)} \cdot \left( \frac{N_{(T,P)}}{10^6} \right)
```

| Variable | Description | Source in Package |
|:---|:---|:---|
| $`E_T`$ | Total emissions from category $`T`$ (Gg $`CH_4`$/year) | Calculated |
| $`N_{(T,P)}`$ | Number of animals in category $`T`$ | `livestock_census.csv` |

------------------------------------------------------------------------

## IX. Methane Module: Manure Management ($`CH_{4(mm)}`$)

#### Volatile Solid Excretion Rates ($`VS`$) (Eq. 10.24)

``` math
VS = \left[ GE \cdot \left( 1 - \frac{DE}{100} \right) + (UE \cdot GE) \right] \cdot \left[ \frac{1 - ASH}{18.45} \right]
```

| Variable | Description | Source in Package |
|:---|:---|:---|
| $`VS`$ | Daily volatile solid excretion (kg VS/day) | Calculated |
| $`UE`$ | Urinary energy excretion fraction (default 0.04) | `ipcc_coefficients.csv` |
| $`ASH`$ | Ash content of the feed (fraction) | `feed_characteristics.csv` |
| 18.45 | Energy content of dietary dry matter (MJ/kg) | Constant |

#### Manure Emission Factor ($`EF`$) (Eq. 10.23)

``` math
EF_{(T)} = (VS_{(T)} \cdot 365) \cdot \left[ B_{0(T)} \cdot 0.67 \cdot \sum_{S,k} \frac{MCF_{S,k}}{100} \cdot AWMS_{(T,S,k)} \right]
```

| Variable | Description | Source in Package |
|:---|:---|:---|
| $`B_{0(T)}`$ | Maximum methane producing capacity ($`m^3/kg VS`$) | `ipcc_coefficients.csv` |
| $`MCF_{S,k}`$ | Methane conversion factor for system $`S`$ (%) | `ipcc_mm.csv` |
| $`AWMS`$ | Fraction of manure handled in system $`S`$ | `manure_management.csv` |
| 0.67 | Conversion factor ($`m^3 CH_4`$ to kg $`CH_4`$) | Constant |

------------------------------------------------------------------------

## X. N₂O Module: Nitrogen-Based Emissions

### I. Nitrogen Balance & Excretion

#### 1. Nitrogen Intake ($`N_{intake}`$) (Eq. 10.31/10.32)

``` math
N_{intake} = \left( \frac{GE}{18.45} \right) \cdot \left( \frac{CP/100}{6.25} \right)
```

| Variable | Description | Source in Package |
|:---|:---|:---|
| $`N_{intake}`$ | Total nitrogen ingested (kg N/day) | Calculated |
| $`CP`$ | Crude Protein content of the diet (%) | `feed_characteristics.csv` |
| 6.25 | Conversion factor from N to Protein | Constant |

#### 2. Nitrogen Retention ($`N_{retention}`$)

Reflects the logic used in the `herdr` codebase: \* **Cattle:**
``` math
Milk_{protein} = 1.9 + 0.4 \cdot Fat_{content}
```
``` math
N_{retention} = \left[ \frac{Milk \cdot Milk_{protein}}{6.38} \right] + \left[ \frac{WG \cdot (268 - (7.03 \cdot \frac{NE_g}{WG})) / 1000}{6.25} \right]
```

| Variable | Description | Source in Package |
|:---|:---|:---|
| $`Fat_{content}`$ | Milk fat percentage (%) | `livestock_definitions.csv` |
| $`Milk`$ | Daily milk yield (kg/day) | `livestock_definitions.csv` |
| $`WG`$ | Daily weight gain (kg/day) | `weights.csv` |
| 6.38 / 6.25 | Specific N-to-Protein conversion factors | Constants |

#### 3. Annual Excretion ($`N_{excreted}`$)

- **Sheep/Goats:**
  $`N_{excreted} = (N_{intake} \cdot (1 - 0.1)) \cdot 365`$
- **Cattle:** $`N_{excreted} = (N_{intake} - N_{retention}) \cdot 365`$

### II. Direct $`N_2O`$ Emissions (Eq. 10.25)

``` math
N_2O_{direct} = N_{(T,P)} \cdot N_{excreted} \cdot Allocation \cdot EF_3 \cdot \left( \frac{44}{28} \right)
```

| Variable | Description | Source in Package |
|:---|:---|:---|
| $`Allocation`$ | Fraction of N managed in a specific system | `manure_management.csv` |
| $`EF_3`$ | Direct $`N_2O`$ emission factor for the system | `ipcc_mm.csv` |
| 44/28 | Stoichiometric ratio of $`N_2O`$ to $`N_2`$ | Constant |

------------------------------------------------------------------------

## XI. N₂O Module: Indirect Emissions

### I. Nitrogen Losses from Manure

#### 1. Volatilization ($`N_{volatilization-MMS}`$) (Eq. 10.26)

``` math
N_{vol-MMS} = \sum \left[ (N \cdot Nex \cdot AWMS) \cdot Frac_{GasMS} \right]
```

#### 2. Leaching and Runoff ($`N_{leaching-MMS}`$) (Eq. 10.27)

``` math
N_{leach-MMS} = \sum \left[ (N \cdot Nex \cdot AWMS) \cdot Frac_{LeachMS} \right]
```

| Variable | Description | Source in Package |
|:---|:---|:---|
| $`Frac_{GasMS}`$ | Fraction of N lost as volatilized gas ($`NH_3 + NO_x`$) | `ipcc_mm.csv` |
| $`Frac_{LeachMS}`$ | Fraction of N lost via leaching into water | `ipcc_mm.csv` |

### II. Indirect $`N_2O`$ Calculation (Eq. 10.28/10.29)

- **From Volatilization:**
  $`N_2O_{G(mm)} = (N_{vol-MMS} \cdot EF_4) \cdot \frac{44}{28}`$
- **From Leaching:**
  $`N_2O_{L(mm)} = (N_{leach-MMS} \cdot EF_5) \cdot \frac{44}{28}`$

| Variable | Description | Source in Package |
|:---|:---|:---|
| $`EF_4`$ | Indirect $`N_2O`$ factor for volatilization | `ipcc_coefficients.csv` |
| $`EF_5`$ | Indirect $`N_2O`$ factor for leaching | `ipcc_coefficients.csv` |

------------------------------------------------------------------------

#### 📚 References

- IPCC (2019). *Refinement to the 2006 IPCC Guidelines for National
  Greenhouse Gas Inventories*. Volume 4: AFOLU, Chapter 10.
