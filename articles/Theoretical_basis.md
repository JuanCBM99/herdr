# Theoretical Basis: IPCC Tier 2 Methodology

## Introduction

The **herdr** package performs environmental impact assessments based on
the **2019 Refinement to the 2006 IPCC Guidelines**. The core of the
model is the **Energy Balance**, where Gross Energy (GE) intake is
derived from the animal’s physiological requirements.

------------------------------------------------------------------------

## I. Ruminant Net Energy

### 1. Net Energy for Maintenance ($`NE_m`$) (Eq. 10.3)

$`NE_m`$ is the energy required to maintain the animal’s energy balance
without gain or loss of weight.

**Formula:**
``` math
NE_m = Cf_i \cdot Weight^{0.75}
```

| Variable | Description | Source in the Package |
|:---|:---|:---|
| $`NE_m`$ | Net energy for maintenance (MJ/day) | Calculated |
| $`Cf_i`$ | Maintenance coefficient based on animal species/status | `ipcc_coefficients.csv` |
| $`Weight`$ | Live weight of the individual animal (kg) | `livestock_weights.csv` |

------------------------------------------------------------------------

### 2. Net Energy for Activity ($`NE_a`$)

$`NE_a`$ represents energy used for grazing, walking, or other
activities.

- **Cattle:** $`NE_a = C_a \cdot NE_m`$ (Eq. 10.4)
- **Sheep and Goats:** $`NE_a = C_a \cdot Weight`$ (Eq. 10.5)

| Variable | Description | Source in the Package |
|:---|:---|:---|
| $`NE_a`$ | Net energy for activity (MJ/day) | Calculated |
| $`C_a`$ | Activity coefficient (varies by feeding situation) | `ipcc_coefficients.csv` |
| $`NE_m`$ | Maintenance energy | Result of Eq. 10.3 |
| $`Weight`$ | Live weight of the animal (kg) | `livestock_weights.csv` |

------------------------------------------------------------------------

### 3. Net Energy for Growth ($`NE_g`$)

Energy required for tissue deposition (weight gain).

- **Cattle:** (Eq. 10.6)
  ``` math
  NE_{g} = 22.02 \cdot \left( \frac{BW}{C \cdot MW} \right)^{0.75} \cdot WG^{1.097}
  ```

- **Sheep and Goats:** (Eq. 10.7)
  ``` math
  NE_g = \frac{WG_{lamb/kid} \cdot (a + 0.5b(BW_i + BW_f))}{\text{Days}_{prod}}
  ```

| Variable | Description | Source in the Package |
|:---|:---|:---|
| $`BW`$ | Average live body weight of the animal (kg) | `livestock_weights.csv` |
| $`MW`$ | Mature body weight of an adult (kg) | `livestock_weights.csv` |
| $`WG`$ | Daily weight gain (kg/day) | `livestock_weights.csv` |
| $`C, a, b`$ | IPCC species-specific growth constants | `ipcc_coefficients.csv` |
| $`BW_i / BW_f`$ | Initial and final weight for the period (kg) | `livestock_weights.csv` |

------------------------------------------------------------------------

### 4. Net Energy for Lactation ($`NE_l`$)

Energy required for milk production based on volume and fat content.

- **Cattle:** $`NE_l = Milk \cdot (1.47 + 0.40 \cdot Fat)`$ (Eq. 10.8)
- **Sheep and Goats:** $`NE_l = Milk \cdot EV_{milk}`$ (Eq. 10.9)

| Variable | Description | Source in the Package |
|:---|:---|:---|
| $`NE_l`$ | Net energy for lactation (MJ/day) | Calculated |
| $`Milk`$ | Average daily milk yield (kg/day) | `ruminant_definitions.csv` |
| $`Fat`$ | Milk fat content percentage (%) | `ruminant_definitions.csv` |
| $`EV_{milk}`$ | Energy required to produce 1 kg of milk (MJ/kg) | Internal constant (`4.6` sheep; `3.0` goat; IPCC 2019 Eq. 10.9) |

------------------------------------------------------------------------

### 5. Net Energy for Work, Wool, and Pregnancy

- **Work ($`NE_{work}`$):**
  ``` math
  NE_{work} = 0.10 \cdot NE_m \cdot Hours
  ```
- **Wool ($`NE_{wool}`$):**
  ``` math
  NE_{wool} = \frac{Pr_{wool} \cdot EV_{wool}}{365}
  ```
- **Pregnancy ($`NE_p`$):**
  ``` math
  NE_p = C_{pregnancy} \cdot NE_m \cdot \text{pregnancy\_rate}
  ```

| Variable | Description | Source in the Package |
|:---|:---|:---|
| $`Hours`$ | Number of hours worked per day (draft animals) | `ruminant_definitions.csv` |
| $`Pr_{wool}`$ | Annual wool production (kg/year) | `ruminant_definitions.csv` |
| $`EV_{wool}`$ | Energy per kg of wool (IPCC default 24 MJ/kg) | Constant |
| $`C_{pregnancy}`$ | Pregnancy energy coefficient | `ipcc_coefficients.csv` (cattle); interpolated from `pr_sheep_goat` (sheep/goats; IPCC 2019 Table 10.7) |

------------------------------------------------------------------------

### 6. Efficiency Ratios ($`REM`$ and $`REG`$)

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

### 7. Gross Energy (GE)

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

### 8. Dry Matter Intake (`calculate_DMI`)

Daily Dry Matter Intake ($`DMI`$, kg DM/head/day) is derived directly
from physiological energy demand divided by the weighted energy density
of the assigned diet:

- **Ruminants (cattle, sheep, goats):**
  ``` math
  DMI = \frac{GE_{\text{MJ/day}}}{GE_{\text{feed, kcal/kg}} \times 0.004184}
  ```
- **Swine:**
  ``` math
  DMI = \frac{ME_{\text{total, kcal/day}}}{ME_{\text{swine, kcal/kg}}}
  ```
- **Poultry:**
  ``` math
  DMI = \frac{ME_{\text{total, kcal/day}}}{ME_{\text{poultry, kcal/kg}}}
  ```

------------------------------------------------------------------------

## II. Monogastric Metabolizable Energy (ME)

For swine and poultry, energy requirements are estimated directly as
**metabolizable energy (ME)** following FEDNA recommendations, rather
than through the IPCC net energy system used for ruminants.

### 1. Maintenance

Maintenance requirements are calculated as:

``` math
ME_{maint} = C_{fi} \times W^{\alpha}
```

| Variable | Description | Source in Package |
|:---|:---|:---|
| $`ME_{maint}`$ | Daily metabolizable energy requirement (kcal/day) | Calculated |
| $`C_{fi}`$ | Maintenance coefficient | `monogastric_definitions.csv` |
| $`W`$ | Mean live weight (kg) | `livestock_weights.csv` |
| $`\alpha`$ | Species-specific metabolic exponent. | `monogastric_definitions.csv` |

*Note: Poultry has a fixed alpha of 0.75.*

------------------------------------------------------------------------

### 2. Growth

#### Poultry

``` math
ME_{growth} = 13.4 \times Fat \times WG + 12.0 \times Protein \times WG
```

#### Swine

``` math
ME_{growth} = 12.8 \times Fat \times WG + 12.1 \times Protein \times WG
```

| Variable | Description | Source in Package |
|:---|:---|:---|
| $`WG`$ | Average daily weight gain (g/day) | Calculated |
| $`Fat`$ | Fraction of body fat deposited per kg of body weight gain (kg fat/kg gain) | `monogastric_definitions.csv` |
| $`Protein`$ | Fraction of body protein deposited per kg of body weight gain (kg protein/kg gain) | `monogastric_definitions.csv` |

------------------------------------------------------------------------

### 3. Egg Production (Poultry)

For laying hens:

``` math
ME_{eggs} = 2 \times Egg_{mass}
```

Where $`Egg_{mass}`$ is the daily egg mass produced (g/day), derived
internally from user input parameters in `monogastric_definitions.csv`
(`eggs_per_year` and `egg_weight_g`):

``` math
Egg_{mass} = \frac{\text{eggs\_per\_year} \times \text{egg\_weight\_g}}{365}
```

------------------------------------------------------------------------

### 4. Gestation (Swine)

Conceptus development:

``` math
ME_{conceptus} = \frac{2600 \times BW_{birth} \times Piglets}{Days_{gest}} + 55.175
```

| Variable        | Description               | Source in Package             |
|:----------------|:--------------------------|:------------------------------|
| $`Days_{gest}`$ | Period of gestation (day) | `livestock_weights.csv`       |
| $`BW_{birth}`$  | Body Weight at birth (kg) | `livestock_weights.csv`       |
| $`Piglets`$     | Number of born piglets    | `monogastric_definitions.csv` |

*Note: The constant 55.175 represents the mammary gland weight gain in
kcal/day.*

Metabolizable energy for the reserve gain:

``` math
ME_{reserves} = \frac{4800 \times Reserve_{gain}}{Days_{gest}}
```

| Variable | Description | Source in Package |
|:---|:---|:---|
| $`Reserve_{gain}`$ | Body reserve gain during gestation (kg) | `livestock_weights.csv` |
| $`Days_{gest}`$ | Gestation length (days) | `livestock_weights.csv` |

Total gestation requirement:

``` math
ME_{gestation} = ME_{conceptus} + ME_{reserves}
```

------------------------------------------------------------------------

### 5. Lactation (Swine)

Milk production:

``` math
ME_{milk} = (6.83 \times WG_{piglet} - 125) \times Piglets_{suckling}
```

| Variable | Description | Source in Package |
|:---|:---|:---|
| $`WG_{piglet}`$ | Average daily weight gain of a suckling piglet (g/day) | Calculated |
| $`Piglets_{suckling}`$ | Number of suckling piglets per sow | `monogastric_definitions.csv` |

Mobilization of body reserves:

``` math
ME_{mobilization} = -\frac{28183}{Days_{lactation}}
```

| Variable             | Description             | Source in Package       |
|:---------------------|:------------------------|:------------------------|
| $`Days_{lactation}`$ | Lactation length (days) | `livestock_weights.csv` |

Total lactation requirement:

``` math
ME_{lactation} = ME_{milk} + ME_{mobilization}
```

------------------------------------------------------------------------

### 6. Total Metabolizable Energy

For growing/finishing cohorts and poultry, total daily metabolizable
energy is the direct sum of active components. For breeding sows that
undergo both gestation and lactation within a reproductive cycle
($`\text{CycleDays} = \max(\text{productive\_period\_days},\, Days_{gest} + Days_{lact})`$),
gestation-phase and lactation-phase requirements are weighted by their
fraction of the reproductive cycle
($`w_{gest} = Days_{gest} / \text{CycleDays}`$,
$`w_{lact} = Days_{lact} / \text{CycleDays}`$):

``` math
ME_{total} = ME_{maint} + ME_{growth} + ME_{eggs} + w_{gest} \cdot ME_{gestation} + w_{lact} \cdot ME_{lactation}
```

------------------------------------------------------------------------

## III. Methane Module

### 1. Enteric Fermentation ($`CH_4`$)

#### Methane Emission Factor ($`EF`$) (Eq. 10.21)

``` math
EF = \frac{GE \cdot \left( \frac{Y_m}{100} \right) \cdot 365}{55.65}
```

| Variable | Description | Source in Package |
|:---|:---|:---|
| $`EF`$ | Methane emission factor (kg $`CH_4`$/head/year) | Calculated |
| $`GE`$ | Daily gross energy intake (MJ/day) | Result of Section I |
| $`Y_m`$ | Methane conversion factor (%) | IPCC 2019 Table 10.12 (see below) |

#### Methane Conversion Factor ($`Y_m`$, IPCC 2019 Table 10.12)

| Livestock Category | Diet Condition | $`Y_m`$ (%) |
|:---|:---|---:|
| **Lactating dairy cattle** (`milk_yield_kg_year > 0`) | $`DE \ge 70\%`$ and $`NDF \le 35\%`$ | **5.7** |
| **Lactating dairy cattle** (`milk_yield_kg_year > 0`) | $`DE \ge 70\%`$ and $`NDF > 35\%`$ | **6.0** |
| **Lactating dairy cattle** (`milk_yield_kg_year > 0`) | $`62\% < DE < 70\%`$ and $`NDF \le 37\%`$ | **6.0** |
| **Lactating dairy cattle** (`milk_yield_kg_year > 0`) | $`62\% < DE < 70\%`$ and $`NDF > 37\%`$ | **6.3** |
| **Lactating dairy cattle** (`milk_yield_kg_year > 0`) | $`DE \le 62\%`$ | **6.5** |
| **Non-dairy / non-lactating cattle** | $`DE \ge 75\%`$ | **3.0** |
| **Non-dairy / non-lactating cattle** | $`72\% \le DE < 75\%`$ | **4.0** |
| **Non-dairy / non-lactating cattle** | $`62\% < DE < 72\%`$ | **6.3** |
| **Non-dairy / non-lactating cattle** | $`DE \le 62\%`$ | **7.0** |
| **Sheep** | All diets | **6.7** |
| **Goats** | All diets | **5.5** |

#### Total Enteric Emissions ($`E_T`$) (Eq. 10.19)

``` math
E_T = \sum_{(P)} EF_{(T,P)} \cdot \left( \frac{N_{(T,P)}}{10^6} \right)
```

| Variable | Description | Source in Package |
|:---|:---|:---|
| $`E_T`$ | Total emissions from category $`T`$ (Gg $`CH_4`$/year) | Calculated |
| $`N_{(T,P)}`$ | Number of animals in category $`T`$ | `livestock_census.csv` |

#### Swine Enteric Fermentation (IPCC 2019 Refinement Table 10.10)

For swine, enteric methane emissions are estimated using the IPCC Tier 1
methodology for high-productivity systems (IPCC 2019 Refinement, Vol. 4,
Ch. 10, Table 10.10 and Eq. 10.19):

``` math
EF_{\text{swine}} = 1.5 \; \text{kg } CH_4 / \text{head} / \text{year}
```

For poultry, enteric fermentation emissions are negligible
($`EF_{\text{poultry}} = 0`$).

------------------------------------------------------------------------

### 2. Manure Management ($`CH_{4(mm)}`$)

#### Volatile Solid Excretion Rates ($`VS`$)

##### Ruminants (IPCC 2019, Eq. 10.24)

``` math
VS = \left[ GE \cdot \left(1-\frac{DE}{100}\right) + (UE \cdot GE) \right] \cdot \left[ \frac{1-ASH}{18.45} \right]
```

##### Poultry (FEDNA-based)

``` math
VS = DMI \cdot \left( 1-\frac{ME_{feed}}{GE_{feed}} \right) \cdot (1-ASH)
```

##### Swine (FEDNA-based)

``` math
VS = DMI \cdot \left( 1-\frac{DE_{feed}}{GE_{feed}}+0.02 \right) \cdot (1-ASH)
```

| Variable | Description | Source in Package |
|:---|:---|:---|
| $`VS`$ | Daily volatile solids excretion (kg VS/day) | Calculated |
| $`GE`$ | Gross energy intake (MJ/day) | Calculated |
| $`DE`$ | Digestible energy of the diet (%) | `feed_characteristics.csv` |
| $`UE`$ | Urinary energy fraction (default = 0.04) | Function argument (`urinary_energy`) |
| $`DMI`$ | Dry matter intake (kg/day) | Calculated |
| $`ME_{feed}`$ | Metabolizable energy of poultry diets (kcal/kg DM) | `feed_characteristics.csv` |
| $`DE_{feed}`$ | Digestible energy of swine diets (kcal/kg DM) | `feed_characteristics.csv` |
| $`GE_{feed}`$ | Gross energy of the feed (kcal/kg DM) | `feed_characteristics.csv` |
| $`ASH`$ | Ash content of the diet (fraction of DM) | `feed_characteristics.csv` |

#### Manure Emission Factor ($`EF`$) (Eq. 10.23)

``` math
EF_{(T)} = (VS_{(T)} \cdot 365) \cdot \left[ B_{0(T)} \cdot 0.67 \cdot \sum_{S,k} \frac{MCF_{S,k}}{100} \cdot AWMS_{(T,S,k)} \right]
```

| Variable | Description | Source in Package |
|:---|:---|:---|
| $`B_{0(T)}`$ | Maximum methane producing capacity ($`m^3`$/kg VS) | `ipcc_coefficients.csv` |
| $`MCF_{S,k}`$ | Methane conversion factor for system $`S`$ (%) | `ipcc_mm.csv` |
| $`AWMS`$ | Fraction of manure handled in system $`S`$ | `manure_management.csv` |

------------------------------------------------------------------------

## IV. Nitrous Oxide Module

### 1. Direct Emissions

#### 1.1 Nitrogen Intake ($`N_{intake}`$)

Nitrogen intake is calculated differently for ruminants and monogastric
animals.

For **ruminants**, nitrogen intake is estimated from gross energy intake
and dietary crude protein (IPCC Eq. 10.31/10.32):

``` math
N_{intake} = \left( \frac{GE}{18.45} \right) \cdot \left( \frac{CP/100}{6.25} \right)
```

For **swine and poultry**, nitrogen intake is calculated directly from
dry matter intake:

``` math
N_{intake} = DMI \cdot \left( \frac{CP/100}{6.25} \right)
```

| Variable | Description | Source in Package |
|:---|:---|:---|
| $`N_{intake}`$ | Daily nitrogen intake (kg N/head/day) | Calculated |
| $`GE`$ | Gross energy intake (MJ/day) | Calculated |
| $`DMI`$ | Dry matter intake (kg/day) | Calculated |
| $`CP`$ | Crude protein concentration of the diet (%) | `feed_characteristics.csv` |

------------------------------------------------------------------------

#### 1.2 Nitrogen Retention ($`N_{retention}`$)

Nitrogen retention represents the fraction of nitrogen retained in
animal products (milk, body tissues, eggs or offspring) rather than
being excreted.

##### Cattle

Milk protein concentration is taken from `protein_content_pct` in
`ruminant_definitions.csv`, or estimated via IPCC regression if omitted:

``` math
Milk_{protein} = 1.9 + 0.4 \times Fat_{content}
```

For lactating and growing animals:

``` math
N_{retention} = \frac{Milk \times (Milk_{protein}/100)}{6.38} + \frac{WG \left(268-\frac{7.03 \times NE_g}{WG}\right)/1000}{6.25}
```

##### Sheep and Goats

Following the IPCC Tier 2 methodology, nitrogen retention is assumed to
be a constant fraction of nitrogen intake ($`0.10`$).

##### Poultry

For growing birds:

``` math
N_{retention} = \frac{(BW_{final} - BW_{initial}) \times 0.028}{Days}
```

For laying hens:

``` math
N_{retention} = 0.028 \times WG + \frac{0.0185 \times Egg_{mass}}{1000}
```

##### Swine

For growing pigs:

``` math
N_{retention} = \frac{(BW_{final} - BW_{initial}) \times N_{gain}}{Days}
```

For breeding sows:

``` math
N_{retention} = \frac{N_{gain,sow} + N_{weaned}}{365}
```

with

``` math
N_{gain,sow} = 0.025 \times FR \times S_{kg}
```

and

``` math
N_{weaned} = \frac{0.025 \times Piglets \times FR \times (W_{weaning} - W_{birth})}{0.98}
```

------------------------------------------------------------------------

#### 1.3 Nitrogen Excretion ($`N_{excreted}`$)

For **cattle, swine and poultry**, annual nitrogen excretion is
calculated as:

``` math
N_{excreted} = (N_{intake} - N_{retention}) \times 365
```

For **sheep and goats**, the IPCC assumes a fixed nitrogen retention
fraction of 10%:

``` math
N_{excreted} = N_{intake} \times (1 - 0.1) \times 365
```

------------------------------------------------------------------------

#### 1.4 Direct $`N_2O`$ Emissions

Direct nitrous oxide emissions from manure management are calculated
following IPCC (2019, Eq. 10.25):

``` math
N_2O_{direct} = Population \times N_{excreted} \times Allocation \times EF_3 \times \frac{44}{28}
```

------------------------------------------------------------------------

### 2. Indirect Emissions

#### 2.1 Nitrogen Losses from Manure

**Volatilization ($`N_{volatilization-MMS}`$) (Eq. 10.26)**
``` math
N_{vol-MMS} = \sum \left[ (N \cdot N_{ex} \cdot AWMS) \cdot Frac_{GasMS} \right]
```

**Leaching and Runoff ($`N_{leaching-MMS}`$) (Eq. 10.27)**
``` math
N_{leach-MMS} = \sum \left[ (N \cdot N_{ex} \cdot AWMS) \cdot Frac_{LeachMS} \right]
```

#### 2.2 Indirect $`N_2O`$ Calculation (Eq. 10.28/10.29)

- **From Volatilization:**
  ``` math
  N_2O_{G(mm)} = (N_{vol-MMS} \cdot EF_4) \cdot \frac{44}{28}
  ```
- **From Leaching:**
  ``` math
  N_2O_{L(mm)} = (N_{leach-MMS} \cdot EF_5) \cdot \frac{44}{28}
  ```

------------------------------------------------------------------------

## V. Feed-Related Land Use Footprint & Agroecological Competition

In addition to greenhouse gases, `herdr` estimates the feed-related
agricultural land footprint paired with the human food security
competition framework of **Mottet et al. (2017)**.

### 1. Agroecological Soil Classification

Every feedstuff is classified via the `land_type` column in
`feed_characteristics.csv`:

- **`cropland`:** Cultivated arable soils suitable for direct human food
  production (cereals, pulses, oilseeds, and arable silages).
- **`grassland_convertible`:** Fertile grasslands agronomically
  convertible to arable crop cultivation.
- **`grassland_unconvertible`:** Native permanent pastures, upland
  scrub, and marginal rangelands unsuitable for arable cultivation.
- **`none`:** Inputs with zero direct land footprint (minerals,
  vitamins, synthetic amino acids, post-consumer fats, and marine
  by-products).

### 2. Mathematical Formulation

For each ingredient $`i`$ in diet category $`cat`$:

#### A. Dry Matter Crop Yield ($`Yield_i`$)

- **Arable crops (`fao_crops.parquet`):** Converted from fresh harvest
  yield ($`RawYield_i`$) to 100% dry matter basis using ingredient dry
  matter percentage ($`DM\_pct_i`$):
  ``` math
  Yield_i = RawYield_i \times \left( \frac{DM\_pct_i}{100} \right) \quad (\text{kg DM / ha})
  ```
- **Curated forages (`forages.parquet`) & Custom yields
  (`custom_yield_kg_ha`):** Already reported on a 100% dry matter basis
  ($`Yield_i = RawYield_i`$).

#### B. Economic & Mass Allocation Adjustment for Co-products

Attributing the entire crop area to co-products (e.g. soybean meal
vs. soybean oil, or wheat bran vs. wheat flour) would overstate or
distort impacts. Because FAOSTAT yields ($`Yield_i`$) correspond to the
primary harvested crop (`yield_name`), `herdr` scales the inverse
main-crop yield by the ratio of the co-product’s economic allocation
($`EA_i \in [0, 1]`$, `economic_allocation` in `mapping.csv`) to its
physical mass yield fraction
($`MF_i = \frac{\text{kg\_coproduct\_per\_1ton\_maincrop}_i}{1000}`$):
``` math
AdjustedLandPerKg_i = \left( \frac{1}{Yield_i} \right) \times \left( \frac{EA_i}{MF_i} \right) \quad (\text{ha / kg DM})
```
For primary crops ($`EA_i = 1, MF_i = 1`$), this reduces to
$`1 / Yield_i`$. For non-land inputs (`none`),
$`AdjustedLandPerKg_i = 0`$. The same allocation factor
($`EA_i / MF_i`$) is applied in
[`calculate_feed_emissions()`](https://juancbm99.github.io/herdr/reference/calculate_feed_emissions.md)
whenever a co-product ($`EA_i < 1`$) is mapped to an unallocated parent
crop LCI in `ecoalim.parquet`.

#### C. Annual Cohort Intake & Footprint

Annual consumption of ingredient $`i`$ is derived from dry matter intake
($`DMI`$), ration macro-share ($`Share_{cat}`$), and ingredient share
($`Share_i`$):
``` math
AnnualConsumption_i = (DMI_{\text{kg/day}} \times 365) \times \left(\frac{Share_{cat}}{100}\right) \times \left(\frac{Share_i}{100}\right) \quad (\text{kg DM / head / year})
```

The cohort footprint in square meters
($`1\text{ ha} = 10{,}000\text{ m}^2`$) is:
``` math
LandUsePerAnimal_i \; (\text{m}^2/\text{head}) = AdjustedLandPerKg_i \times AnnualConsumption_i \times 10{,}000
```
``` math
TotalLandUse_i \; (\text{m}^2) = LandUsePerAnimal_i \times AAP
```

------------------------------------------------------------------------

## VI. Animal Productivity & Edible Protein Engine (FAO/GLEAM)

`herdr` computes physical livestock productivity and nutritional protein
output using the **FAO Global Livestock Environmental Assessment Model
(GLEAM v3.0)** framework and International Dairy Federation (IDF)
standards.

### 1. Annual Slaughter Output ($`N_{\text{exit}}`$)

Annual throughput of animals leaving the herd for slaughter
($`N_{\text{exit}}`$) is calculated from the Annual Average Population
($`AAP`$) based on strict functional cohort roles (`production_role`):

- **Replacement stock (`replacement`):** $`N_{\text{exit}} = 0`$
  (animals enter the adult breeding herd to maintain stability).
- **Mature breeding stock (`mature`):**
  ``` math
  N_{\text{exit}} = AAP \times \text{replacement\_rate}
  ```
  *(For layer hens where `replacement_rate` is omitted,
  $`N_{\text{exit}} = AAP \times \frac{365}{\text{productive\_period\_days}}`$).*
- **Commercial slaughter cohorts (`slaughter`):**
  ``` math
  N_{\text{exit}} = AAP \times \left( \frac{365}{\text{productive\_period\_days}} \right)
  ```

### 2. Milk Production & FPCM

Total fresh milk is computed as:
``` math
\text{Milk}_{\text{fresh}} = AAP \times \text{milk\_yield\_kg\_year}
```

To standardize milk across breeds and diets, Fat and Protein Corrected
Milk (FPCM) is calculated following the **IDF (2022)** standard:
``` math
\text{Milk}_{\text{FPCM}} = \text{Milk}_{\text{fresh}} \times \left[ 0.1226 \cdot \text{Fat} (\%) + 0.0776 \cdot \text{Protein} (\%) + 0.2534 \right]
```

### 3. Meat Production & Carcass Metrics

1.  **Live weight at slaughter:**
    $`\text{Meat}_{\text{LW}} = N_{\text{exit}} \times \text{Weight}_{\text{slaughter}}`$
2.  **Carcass weight:**
    $`\text{Meat}_{\text{carcass}} = \text{Meat}_{\text{LW}} \times \left( \frac{DP\%}{100} \right)`$
3.  **Boneless meat:**
    $`\text{Meat}_{\text{boneless}} = \text{Meat}_{\text{carcass}} \times BFM`$
4.  **Meat edible protein:**
    $`\text{Protein}_{\text{meat}} = \text{Meat}_{\text{boneless}} \times \text{MEAT\_prot}`$

| Species | Dressing Percentage ($`DP\%`$) | Boneless Fraction ($`BFM`$) | Protein in Meat ($`\text{MEAT\_prot}`$) | Default Milk Protein ($`\text{MILK\_prot}`$) |
|:---|:--:|:--:|:--:|:--:|
| **Cattle** | 54.0% | 0.75 | 0.2113 | 0.033 |
| **Sheep** | 47.0% | 0.70 | 0.2013 | 0.058 |
| **Goat** | 47.0% | 0.70 | 0.1920 | 0.034 |
| **Swine** | 73.0% | 0.65 | 0.2020 | — |
| **Poultry** | 70.0% | 0.75 | 0.1900 | — |

### 4. Egg Production, Greasy Wool, and Total Edible / Digestible Protein

``` math
\text{Eggs}_{\text{fresh}} = \left( \frac{\text{Egg}_{\text{mass}} \times 365}{1000} \right) \times AAP, \qquad \text{Protein}_{\text{eggs}} = \text{Eggs}_{\text{fresh}} \times 0.1240
```
``` math
\text{Wool}_{\text{greasy}} = AAP \times \text{wool\_yield\_kg\_year}
```
``` math
\text{Protein}_{\text{total}} = \text{Protein}_{\text{milk}} + \text{Protein}_{\text{meat}} + \text{Protein}_{\text{eggs}}
```
``` math
\text{HDP}_{\text{total}} = \sum_{s \in \{\text{milk, meat, eggs}\}} \text{Protein}_s \times \frac{\text{DIAAS}_s}{100}
```

------------------------------------------------------------------------

## VII. Enterprise Attribution, Biophysical Allocation & Feed Efficiency (`generate_impact_assessment`)

The consolidated assessment engine
([`generate_impact_assessment()`](https://juancbm99.github.io/herdr/reference/generate_impact_assessment.md))
produces two tables:

- **Table 1 (`totals`):** Direct cohort-level emissions, feed intake,
  land use, water deprivation, and per-head footprints.
- **Table 2 (`intensities`):** Enterprise-integrated commercial and
  nutritional intensities grouped by
  `(farm_id, region, subregion, animal_type, animal_subtype)`.

### 1. Enterprise Biophysical Co-Product Allocation (IDF 2022 / IPCC 2019 / FAO LEAP)

Within each enterprise
`(farm_id, region, subregion, animal_type, animal_subtype)`, all direct
emissions, feed life cycle impacts, land use, and water deprivation
across all cohorts (`mature`, `replacement`, and `slaughter`) are
integrated into a single enterprise pool
($`\text{Pool}_{\text{enterprise}}`$).

Co-product footprints are partitioned across commercial product streams
using the **universal Net Energy biophysical allocation engine** (IDF
Bulletin 520 / 2022, Section 10.5; IPCC 2019 Refinement; FAO LEAP):

``` math
\text{E}_{\text{total}} = \text{E}_{\text{milk}} + \text{E}_{\text{cull}} + \text{E}_{\text{young}} + \text{E}_{\text{wool}} + \text{E}_{\text{egg}}
```

Where net energy requirements per unit of production are: \* **Milk
($`\text{E}_{\text{milk}}`$):**
``` math
\text{E}_{\text{milk}} = \begin{cases} 3.1 \times \text{Milk}_{\text{FPCM}} & (\text{Cattle \& other bovines; IDF 2022}) \\ 4.6 \times \text{Milk}_{\text{fresh}} & (\text{Sheep, 7\% fat; IPCC 2019}) \\ 3.0 \times \text{Milk}_{\text{fresh}} & (\text{Goat, 3.8\% fat; IPCC 2019}) \end{cases}
```
\* **Culled Mature Meat ($`\text{E}_{\text{cull}}`$):**
$`15.0\text{ MJ/kg live weight}`$ for adult breeding stock (IDF 2022).
\* **Young Slaughter Meat ($`\text{E}_{\text{young}}`$):**
$`11.0\text{ MJ/kg live weight}`$ for fattened calves, lambs, pigs, and
broilers (IDF 2022). \* **Greasy Wool ($`\text{E}_{\text{wool}}`$):**
$`24.0\text{ MJ/kg}`$ for wool weighed after drying before scouring
(IPCC 2019, Eq. 10.12). \* **Fresh Eggs ($`\text{E}_{\text{egg}}`$):**
$`6.0\text{ MJ/kg}`$ (FAO LEAP / GLEAM v3.0).

#### Allocation Factors ($`AF_k`$)

The fraction of the enterprise footprint attributed to each product
stream is:
``` math
\text{AF}_k = \frac{\text{E}_k}{\text{E}_{\text{total}}}
```

- **Milk:**
  $`\text{AF}_{\text{milk}} = \text{E}_{\text{milk}} / \text{E}_{\text{total}}`$
- **Cull Meat:**
  $`\text{AF}_{\text{cull}} = \text{E}_{\text{cull}} / \text{E}_{\text{total}}`$
- **Young Meat:**
  $`\text{AF}_{\text{young}} = \text{E}_{\text{young}} / \text{E}_{\text{total}}`$
- **Wool:**
  $`\text{AF}_{\text{wool}} = \text{E}_{\text{wool}} / \text{E}_{\text{total}}`$
- **Eggs:**
  $`\text{AF}_{\text{egg}} = \text{E}_{\text{egg}} / \text{E}_{\text{total}}`$

For **Human Edible Protein**, non-edible fiber (wool) is excluded:
``` math
\text{AF}_{\text{protein}} = 1 - \text{AF}_{\text{wool}} = \frac{\text{E}_{\text{milk}} + \text{E}_{\text{cull}} + \text{E}_{\text{young}} + \text{E}_{\text{egg}}}{\text{E}_{\text{total}}}
```

#### Attribution by Cohort Role in Table 2 (`intensities`)

- **`mature` Cohorts (Breeding / Milking / Laying stock):**
  - $`\text{Intensity}_{\text{milk}} = (\text{Pool} \times \text{AF}_{\text{milk}}) / \text{Milk}_{\text{FPCM, total}}`$
  - $`\text{Intensity}_{\text{cull meat}} = (\text{Pool} \times \text{AF}_{\text{cull}}) / \text{Meat}_{\text{CW, cull}}`$*(expressed
    per kg carcass weight)*
  - $`\text{Intensity}_{\text{egg}} = (\text{Pool} \times \text{AF}_{\text{egg}}) / \text{Egg}_{\text{fresh, total}}`$
  - $`\text{Intensity}_{\text{wool}} = (\text{Pool} \times \text{AF}_{\text{wool}}) / \text{Wool}_{\text{total}}`$
  - $`\text{Intensity}_{\text{protein}} = (\text{Pool} \times \text{AF}_{\text{protein}}) / \text{Protein}_{\text{total}}`$
- **`slaughter` Cohorts (Fattening / Finishing stock):**
  - $`\text{Intensity}_{\text{young meat}} = (\text{Pool} \times \text{AF}_{\text{young}}) / \text{Meat}_{\text{CW, young}}`$*(expressed
    per kg carcass weight)*
  - $`\text{Intensity}_{\text{wool}} = (\text{Pool} \times \text{AF}_{\text{wool}}) / \text{Wool}_{\text{total}}`$
  - $`\text{Intensity}_{\text{protein}} = (\text{Pool} \times \text{AF}_{\text{protein}}) / \text{Protein}_{\text{total}}`$
- **`replacement` Cohorts (Rearing stock):**
  - In Table 1 (`totals`), replacement cohorts show their full direct
    inventory.
  - In Table 2 (`intensities`), replacement cohorts show `NA` for
    commercial product intensities because their feed, emissions, and
    land overhead are already absorbed into the enterprise pool and
    allocated across commercial outputs, preventing double counting.

------------------------------------------------------------------------

### 2. Protein Feed Conversion, Human Food Competition, and Protein Quality

`herdr` evaluates enterprise-level protein conversion efficiency on
producing cohorts (`mature` and `slaughter`), integrating whole-herd
feed intake (including `replacement` cohorts) allocated to edible
protein by $`AF_{\text{protein}} = 1 - AF_{\text{wool}}`$.

| Metric | Formula | Interpretation | Framework |
|:---|:---|:---|:---|
| **`Protein_FCR_total`** | $`\dfrac{\text{Feed}_{\text{CP, total}} \times AF_{\text{protein}}}{\text{Protein}_{\text{total}}}`$ | Total kg feed crude protein per kg edible animal protein (overall biological N efficiency). | Crude protein balance |
| **`Protein_FCR_human_edible`** | $`\dfrac{\text{Feed}_{\text{CP, human-edible}} \times AF_{\text{protein}}}{\text{Protein}_{\text{total}}}`$ | Competing human-edible feed CP per kg edible animal protein (`< 1.0` = net crude-protein producer). | Wilkinson (2011); Ertl et al. (2016a) |
| **`EPCR_DIAAS`** | $`\dfrac{\text{Feed}_{\text{HDP}} \times AF_{\text{protein}}}{\text{Animal}_{\text{HDP}}} = \dfrac{\text{Protein\_FCR}_{\text{human\_edible}}}{\text{PQR}}`$ | Quality-adjusted human-digestible feed protein per kg human-digestible animal protein (`< 1.0` = net digestible-protein producer). | FAO (2013); Ertl et al. (2016a, 2016b) |
| **`PQR`** | $`\dfrac{\text{DIAAS}_{\text{output}}}{\text{DIAAS}_{\text{input}}}`$ | Protein Quality Ratio: amino-acid quality upgrade from human-edible plant feeds to animal food (`> 1.0` = net quality upgrade). | Ertl et al. (2016a, 2016b) |

#### Key Implementation Conventions

- **Human-edible feed protein
  ($`\text{Feed}_{\text{CP, human-edible}}`$):** Each ingredient’s crude
  protein is weighted by the explicit `human_edible` column (`0`, `1`,
  or any fraction in $`[0, 1]`$) in `mapping.csv`, decoupling food-feed
  competition cleanly from land classification (`land_type`). As an
  editable default convention in `herdr`, arable non-forage concentrates
  with `economic_allocation > 0.33` (primary grains, pulses,
  soybean/linseed meals, protein concentrates/isolates, plus commercial
  `casein` and `skimmed_milk`) default to `1`, whereas all forages
  (including dehydrated alfalfa/forage meals), secondary by-products
  (`economic_allocation <= 0.33`), inedible/waste items (whole
  cottonseed, vetches, acorns, bakery by-products), suckled milk, and
  mineral/synthetic additives default to `0`.
- **DIAAS scores (`0.5–3 yr` child pattern, untruncated):** Feed
  human-digestible protein ($`\text{Feed}_{\text{HDP}}`$) weights each
  human-edible ingredient’s CP by `diaas_pct / 100` from `mapping.csv`
  (backbone: Ertl et al., 2016b, Table 4, supplemented by Ertl et al.,
  2016a, Cervantes-Pahm et al., 2014, Rutherfurd et al., 2015, Nosworthy
  et al., 2018, and Herreman et al., 2020). Animal output
  ($`\text{Animal}_{\text{HDP}}`$) uses species-specific DIAAS values
  from Ertl et al. (2016b, Table 5): cow milk (`115.9%`), sheep milk
  (`109.1%`), goat milk (`123.5%`), beef (`109.3%`), sheep/goat meat
  (`116.8%`), pork (`113.9%`), chicken meat (`108.2%`), and whole egg
  (`116.4%`).
- **Coverage & farm-gate transparency:** Following Ertl et al. (2016b,
  Section 2.6), if $`\ge 80\%`$ (but $`< 100\%`$) of human-edible feed
  CP has a DIAAS entry, the known weighted-average DIAAS is applied to
  total human-edible CP (emitting a warning whenever $`> 5\%`$ lacks
  DIAAS, and returning `NA` if coverage $`< 80\%`$). Because
  `EPCR_DIAAS` evaluates raw feedstuffs at the farm gate without
  modeling cereal–legume amino-acid complementarity in mixed human
  diets, it is always reported alongside unadjusted
  `Protein_FCR_human_edible` and `Protein_FCR_total`.

------------------------------------------------------------------------

#### 📚 References

- Cervantes-Pahm, S. K., Liu, Y., & Stein, H. H. (2014). *Digestible
  indispensable amino acid score and digestible amino acids in eight
  cereal grains*. **British Journal of Nutrition**, 111(9), 1663–1672.
- Ertl, P., Knaus, W., & Zollitsch, W. (2016a). *An approach to
  including protein quality when assessing the net contribution of
  livestock to human food supply*. **Animal**, 10(11), 1883–1889.
- Ertl, P., Steinwidder, A., Schönauer, M., Krimberger, K., Knaus, W., &
  Zollitsch, W. (2016b). *Net food production of different livestock: A
  national analysis for Austria including relative occupation of
  different land categories*. **Die Bodenkultur**, 67(2), 91–103.
- FAO (2013). *Dietary protein quality evaluation in human nutrition:
  Report of an FAO Expert Consultation*. FAO Food and Nutrition Paper
  92, Rome.
- FAO (2018). *Global Livestock Environmental Assessment Model (GLEAM)
  v3.0: Model description*. Food and Agriculture Organization of the
  United Nations, Rome.
- FEDNA (2019). *Tablas FEDNA de composición y valor nutritivo de
  alimentos para la fabricación de piensos compuestos* (4ª ed.).
  Fundación Española para el Desarrollo de la Nutrición Animal, Madrid.
- Herreman, L., Nommensen, P., Pennings, B., & Laus, M. C. (2020).
  *Comprehensive overview of the quality of plant- and animal-sourced
  proteins based on the digestible indispensable amino acid score*.
  **Food Science & Nutrition**, 8(10), 5379–5391.
- IDF (2022). *The C-footprint of dairy: Updated IDF standard for carbon
  footprinting of the dairy sector*. International Dairy Federation
  Bulletin 520/2022.
- IPCC (2019). *Refinement to the 2006 IPCC Guidelines for National
  Greenhouse Gas Inventories*. Volume 4: AFOLU, Chapter 10.
- Mottet, A., de Haan, C., Falcucci, A., Tempio, G., Opio, C., &
  Gerber, P. (2017). *More fuel than food: Global food security and the
  role of livestock feed*. **Global Food Security**, 14, 1–8.
- Nosworthy, M. G., et al. (2018). *Effect of processing on the in vitro
  and in vivo protein quality of red and green lentils*. **Food Science
  & Nutrition**, 6(5), 1196–1205.
- Rutherfurd, S. M., Fanning, A. C., Miller, B. J., & Moughan, P. J.
  (2015). *Protein digestibility-corrected amino acid scores and
  digestible indispensable amino acid scores differentially describe
  protein quality in growing male rats*. **The Journal of Nutrition**,
  145(2), 372–379.
- Wilkinson, J. M. (2011). *Re-defining efficiency of feed use by
  livestock*. **Animal**, 5(7), 1014–1022.
