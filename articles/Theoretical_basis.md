# Theoretical Basis: IPCC Tier 2 Methodology

## Introduction

The **herdr** package performs environmental impact assessments based on
the **2019 Refinement to the 2006 IPCC Guidelines**. The core of the
model is the **Energy Balance**, where Gross Energy (GE) intake is
derived from the animal’s physiological requirements.

------------------------------------------------------------------------

## I. Net Energy for Maintenance ($NE_{m}$) (Eq. 10.3)

$NE_{m}$ is the energy required to maintain the animal’s energy balance
without gain or loss of weight.

**Formula:** $$NE_{m} = Cf_{i} \cdot \text{Weight}^{0.75}$$

| Variable | Description                                            | Source in the Package   |
|:---------|:-------------------------------------------------------|:------------------------|
| $NE_{m}$ | Net energy for maintenance (MJ/day)                    | Calculated              |
| $Cf_{i}$ | Maintenance coefficient based on animal species/status | `ipcc_coefficients.csv` |
| Weight   | Live weight of the individual animal (kg)              | `weights.csv`           |

------------------------------------------------------------------------

## II. Net Energy for Activity ($NE_{a}$)

$NE_{a}$ represents energy used for grazing, walking, or other
activities.

- **Cattle:** $NE_{a} = C_{a} \cdot NE_{m}$ (Eq. 10.4)
- **Sheep and Goats:** $NE_{a} = C_{a} \cdot \text{Weight}$ (Eq. 10.5)

| Variable | Description                                        | Source in the Package   |
|:---------|:---------------------------------------------------|:------------------------|
| $NE_{a}$ | Net energy for activity (MJ/day)                   | Calculated              |
| $C_{a}$  | Activity coefficient (varies by feeding situation) | `ipcc_coefficients.csv` |
| $NE_{m}$ | Maintenance energy                                 | Result of Eq. 10.3      |
| Weight   | Live weight of the animal (kg)                     | `weights.csv`           |

------------------------------------------------------------------------

## III. Net Energy for Growth ($NE_{g}$)

Energy required for tissue deposition (weight gain).

- **Cattle and Buffalo:** (Eq. 10.6)
  $$NE_{g} = 22.02 \cdot \left( \frac{BW}{C \cdot MW} \right)^{0.75} \cdot WG^{1.097}$$

- **Sheep and Goats:** (Eq. 10.7)
  $$NE_{g} = \frac{WG_{lamb/kid} \cdot \left( a + 0.5b\left( BW_{i} + BW_{f} \right) \right)}{365}$$

| Variable        | Description                                  | Source in the Package   |
|:----------------|:---------------------------------------------|:------------------------|
| $BW$            | Average live body weight of the animal (kg)  | `weights.csv`           |
| $MW$            | Mature body weight of an adult female (kg)   | `weights.csv`           |
| $WG$            | Daily weight gain (kg/day)                   | `weights.csv`           |
| $C,a,b$         | IPCC species-specific growth constants       | `ipcc_coefficients.csv` |
| $BW_{i}/BW_{f}$ | Initial and final weight for the period (kg) | `weights.csv`           |

------------------------------------------------------------------------

## IV. Net Energy for Lactation ($NE_{l}$)

Energy required for milk production based on volume and fat content.

- **Cattle:**
  $NE_{l} = \text{Milk} \cdot \left( 1.47 + 0.40 \cdot \text{Fat} \right)$
  (Eq. 10.8)
- **Sheep and Goats:** $NE_{l} = \text{Milk} \cdot EV_{milk}$ (Eq. 10.9)

| Variable    | Description                                     | Source in the Package       |
|:------------|:------------------------------------------------|:----------------------------|
| $NE_{l}$    | Net energy for lactation (MJ/day)               | Calculated                  |
| Milk        | Average daily milk yield (kg/day)               | `livestock_definitions.csv` |
| Fat         | Milk fat content percentage (%)                 | `livestock_definitions.csv` |
| $EV_{milk}$ | Energy required to produce 1 kg of milk (MJ/kg) | `ipcc_coefficients.csv`     |

------------------------------------------------------------------------

## V. Net Energy for Work, Wool, and Pregnancy

- **Work ($NE_{work}$):**
  $$NE_{work} = 0.10 \cdot NE_{m} \cdot \text{Hours}$$
- **Wool ($NE_{wool}$):**
  $$NE_{wool} = \frac{\text{Pr}_{wool} \cdot EV_{wool}}{365}$$
- **Pregnancy ($NE_{p}$):** $$NE_{p} = C_{pregnancy} \cdot NE_{m}$$

| Variable           | Description                                    | Source in the Package       |
|:-------------------|:-----------------------------------------------|:----------------------------|
| Hours              | Number of hours worked per day (draft animals) | `livestock_definitions.csv` |
| $\text{Pr}_{wool}$ | Annual wool production (kg/year)               | `livestock_definitions.csv` |
| $EV_{wool}$        | Energy per kg of wool (IPCC default 24 MJ/kg)  | Constant                    |
| $C_{pregnancy}$    | Pregnancy energy coefficient                   | `ipcc_coefficients.csv`     |

------------------------------------------------------------------------

## VI. Efficiency Ratios ($REM$ and $REG$)

Based on IPCC Equations 10.14 and 10.15, derived from feed digestibility
($DE$):

- **For Maintenance ($REM$):**
  $$REM = \left\lbrack 1.123 - \left( 4.092 \cdot 10^{- 3} \cdot DE \right) + \left( 1.126 \cdot 10^{- 5} \cdot (DE)^{2} \right) - \left( \frac{25.4}{DE} \right) \right\rbrack$$
- **For Growth ($REG$):**
  $$REG = \left\lbrack 1.164 - \left( 5.16 \cdot 10^{- 3} \cdot DE \right) + \left( 1.308 \cdot 10^{- 5} \cdot (DE)^{2} \right) - \left( \frac{37.4}{DE} \right) \right\rbrack$$

| Variable | Description                                     | Source in the Package      |
|:---------|:------------------------------------------------|:---------------------------|
| $REM$    | Efficiency of digestible energy for maintenance | Calculated                 |
| $REG$    | Efficiency of digestible energy for growth      | Calculated                 |
| $DE$     | Digestibility of feed as % of gross energy      | `feed_characteristics.csv` |

------------------------------------------------------------------------

## VII. Gross Energy (GE)

**Gross Energy (GE)** is the total daily energy intake required to
sustain all physiological functions.

**Formula:**
$$GE = \frac{\left( \frac{NE_{m} + NE_{a} + NE_{l} + NE_{work} + NE_{p}}{REM} + \frac{NE_{g} + NE_{wool}}{REG} \right)}{DE/100}$$

| Variable  | Description                                      | Source in the Package      |
|:----------|:-------------------------------------------------|:---------------------------|
| $GE$      | Total daily gross energy requirement (MJ/day)    | Calculated                 |
| $NE_{x}$  | Various net energy requirements calculated above | Calculated Results         |
| $REM/REG$ | Efficiency ratios                                | Results of Eq. 10.14/10.15 |
| $DE$      | Feed digestibility (%)                           | `feed_characteristics.csv` |

------------------------------------------------------------------------

## VIII. Methane Module: Enteric Fermentation ($CH_{4}$)

#### Methane Emission Factor ($EF$) (Eq. 10.21)

$$EF = \frac{GE \cdot \left( \frac{Y_{m}}{100} \right) \cdot 365}{55.65}$$

| Variable | Description                                     | Source in Package          |
|:---------|:------------------------------------------------|:---------------------------|
| $EF$     | Methane emission factor (kg $CH_{4}$/head/year) | Calculated                 |
| $GE$     | Daily gross energy intake (MJ/day)              | Result of Section VII      |
| $Y_{m}$  | Methane conversion factor (%)                   | `feed_characteristics.csv` |
| 55.65    | Energy content of methane (MJ/kg $CH_{4}$)      | Constant                   |

#### Total Enteric Emissions ($E_{T}$) (Eq. 10.19)

$$E_{T} = \sum\limits_{(P)}EF_{(T,P)} \cdot \left( \frac{N_{(T,P)}}{10^{6}} \right)$$

| Variable    | Description                                          | Source in Package      |
|:------------|:-----------------------------------------------------|:-----------------------|
| $E_{T}$     | Total emissions from category $T$ (Gg $CH_{4}$/year) | Calculated             |
| $N_{(T,P)}$ | Number of animals in category $T$                    | `livestock_census.csv` |

------------------------------------------------------------------------

## IX. Methane Module: Manure Management ($CH_{4{(mm)}}$)

#### Volatile Solid Excretion Rates ($VS$) (Eq. 10.24)

$$VS = \left\lbrack GE \cdot \left( 1 - \frac{DE}{100} \right) + (UE \cdot GE) \right\rbrack \cdot \left\lbrack \frac{1 - ASH}{18.45} \right\rbrack$$

| Variable | Description                                      | Source in Package          |
|:---------|:-------------------------------------------------|:---------------------------|
| $VS$     | Daily volatile solid excretion (kg VS/day)       | Calculated                 |
| $UE$     | Urinary energy excretion fraction (default 0.04) | `ipcc_coefficients.csv`    |
| $ASH$    | Ash content of the feed (fraction)               | `feed_characteristics.csv` |
| 18.45    | Energy content of dietary dry matter (MJ/kg)     | Constant                   |

#### Manure Emission Factor ($EF$) (Eq. 10.23)

$$EF_{(T)} = \left( VS_{(T)} \cdot 365 \right) \cdot \left\lbrack B_{0{(T)}} \cdot 0.67 \cdot \sum\limits_{S,k}\frac{MCF_{S,k}}{100} \cdot AWMS_{(T,S,k)} \right\rbrack$$

| Variable     | Description                                       | Source in Package       |
|:-------------|:--------------------------------------------------|:------------------------|
| $B_{0{(T)}}$ | Maximum methane producing capacity ($m^{3}/kgVS$) | `ipcc_coefficients.csv` |
| $MCF_{S,k}$  | Methane conversion factor for system $S$ (%)      | `ipcc_mm.csv`           |
| $AWMS$       | Fraction of manure handled in system $S$          | `manure_management.csv` |
| 0.67         | Conversion factor ($m^{3}CH_{4}$ to kg $CH_{4}$)  | Constant                |

------------------------------------------------------------------------

## X. N₂O Module: Nitrogen-Based Emissions

### I. Nitrogen Balance & Excretion

#### 1. Nitrogen Intake ($N_{intake}$) (Eq. 10.31/10.32)

$$N_{intake} = \left( \frac{GE}{18.45} \right) \cdot \left( \frac{CP/100}{6.25} \right)$$

| Variable     | Description                           | Source in Package          |
|:-------------|:--------------------------------------|:---------------------------|
| $N_{intake}$ | Total nitrogen ingested (kg N/day)    | Calculated                 |
| $CP$         | Crude Protein content of the diet (%) | `feed_characteristics.csv` |
| 6.25         | Conversion factor from N to Protein   | Constant                   |

#### 2. Nitrogen Retention ($N_{retention}$)

Reflects the logic used in the `herdr` codebase: \* **Cattle:**
$$Milk_{protein} = 1.9 + 0.4 \cdot Fat_{content}$$$$N_{retention} = \left\lbrack \frac{Milk \cdot Milk_{protein}}{6.38} \right\rbrack + \left\lbrack \frac{WG \cdot \left( 268 - \left( 7.03 \cdot \frac{NE_{g}}{WG} \right) \right)/1000}{6.25} \right\rbrack$$

| Variable        | Description                              | Source in Package           |
|:----------------|:-----------------------------------------|:----------------------------|
| $Fat_{content}$ | Milk fat percentage (%)                  | `livestock_definitions.csv` |
| $Milk$          | Daily milk yield (kg/day)                | `livestock_definitions.csv` |
| $WG$            | Daily weight gain (kg/day)               | `weights.csv`               |
| 6.38 / 6.25     | Specific N-to-Protein conversion factors | Constants                   |

#### 3. Annual Excretion ($N_{excreted}$)

- **Sheep/Goats:**
  $N_{excreted} = \left( N_{intake} \cdot (1 - 0.1) \right) \cdot 365$
- **Cattle:**
  $N_{excreted} = \left( N_{intake} - N_{retention} \right) \cdot 365$

### II. Direct $N_{2}O$ Emissions (Eq. 10.25)

$$N_{2}O_{direct} = N_{(T,P)} \cdot N_{excreted} \cdot Allocation \cdot EF_{3} \cdot \left( \frac{44}{28} \right)$$

| Variable     | Description                                    | Source in Package       |
|:-------------|:-----------------------------------------------|:------------------------|
| $Allocation$ | Fraction of N managed in a specific system     | `manure_management.csv` |
| $EF_{3}$     | Direct $N_{2}O$ emission factor for the system | `ipcc_mm.csv`           |
| 44/28        | Stoichiometric ratio of $N_{2}O$ to $N_{2}$    | Constant                |

------------------------------------------------------------------------

## XI. N₂O Module: Indirect Emissions

### I. Nitrogen Losses from Manure

#### 1. Volatilization ($N_{volatilization - MMS}$) (Eq. 10.26)

$$N_{vol - MMS} = \sum\left\lbrack (N \cdot Nex \cdot AWMS) \cdot Frac_{GasMS} \right\rbrack$$

#### 2. Leaching and Runoff ($N_{leaching - MMS}$) (Eq. 10.27)

$$N_{leach - MMS} = \sum\left\lbrack (N \cdot Nex \cdot AWMS) \cdot Frac_{LeachMS} \right\rbrack$$

| Variable         | Description                                               | Source in Package |
|:-----------------|:----------------------------------------------------------|:------------------|
| $Frac_{GasMS}$   | Fraction of N lost as volatilized gas ($NH_{3} + NO_{x}$) | `ipcc_mm.csv`     |
| $Frac_{LeachMS}$ | Fraction of N lost via leaching into water                | `ipcc_mm.csv`     |

### II. Indirect $N_{2}O$ Calculation (Eq. 10.28/10.29)

- **From Volatilization:**
  $N_{2}O_{G{(mm)}} = \left( N_{vol - MMS} \cdot EF_{4} \right) \cdot \frac{44}{28}$
- **From Leaching:**
  $N_{2}O_{L{(mm)}} = \left( N_{leach - MMS} \cdot EF_{5} \right) \cdot \frac{44}{28}$

| Variable | Description                                 | Source in Package       |
|:---------|:--------------------------------------------|:------------------------|
| $EF_{4}$ | Indirect $N_{2}O$ factor for volatilization | `ipcc_coefficients.csv` |
| $EF_{5}$ | Indirect $N_{2}O$ factor for leaching       | `ipcc_coefficients.csv` |

------------------------------------------------------------------------

#### 📚 References

- IPCC (2019). *Refinement to the 2006 IPCC Guidelines for National
  Greenhouse Gas Inventories*. Volume 4: AFOLU, Chapter 10.
