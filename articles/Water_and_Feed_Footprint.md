# Feed Environmental Footprint & Water Deprivation (AWARE / EF 3.1)

## 1. Overview & Cradle-to-Farm-Gate Scope

In livestock life cycle assessment (LCA), on-farm direct emissions
(Scope 1: enteric methane, manure management methane, and
direct/indirect nitrous oxide) account for only part of the system’s
total footprint. The production, processing, transport, and cultivation
of animal feedstuffs represent a major **upstream Scope 3
cradle-to-farm-gate** environmental burden.

`herdr` couples animal nutrition—specifically animal Dry Matter Intake
(DMI) and dietary ingredient proportions—with international life cycle
inventories:

- **INRAE / ADEME ECOALIM v9 & AGRIBALYSE v3.1:** Life cycle inventories
  for animal feed raw materials under the European Commission **EF 3.1
  (PEF)** framework. Because these are process LCA datasets, upstream
  economic allocation for co-products (e.g., oilseed meals, cereal
  brans) is already resolved at the processing gate.
- **European Commission Environmental Footprint (EF 3.1):**
  Methodological package adopted under Commission Recommendation (EU)
  2021/2279 for Product Environmental Footprint (PEF).

The model evaluates three primary environmental dimensions for
feedstuffs:

1.  **Carbon Footprint (GHG):** Differentiating between fossil inputs
    ($`\text{CO}_2\text{e}`$ from machinery, synthetic fertilizers,
    electricity) and land-use change / deforestation
    ($`\text{CO}_2\text{e}`$ from LUC, particularly critical in imported
    soybean meal).
2.  **Agricultural Land Footprint:** Partitioned into agroecological
    soil classifications (*cropland*, *convertible grassland*,
    *unconvertible rangeland*) following Mottet et al. (2017).
3.  **Water Deprivation Footprint:** Freshwater deprivation
    characterized with the international **AWARE** model.

------------------------------------------------------------------------

## 2. Calculation Methodology: Step-by-Step

In `herdr`, calculating feed environmental footprints (both Water
Deprivation and Greenhouse Gas emissions) follows a 3-step pipeline:

### 2.1 Annual Ingredient Dry Matter Intake

How much of ingredient $`i`$ an animal consumes over a year depends on
daily dry matter intake ($`DMI_{\text{kg/day}}`$), the broad feed
category share
($`Share_{cat} \in \{\text{forage}, \text{concentrate}, \text{milk}, \text{milk\_replacer}\}`$
from `diet_profiles.csv`), and the specific ingredient’s share within
that category ($`Share_i`$ from `diet_ingredients.csv`):

``` math
AnnualDMI_i \; (\text{kg DM / head / yr}) =
(DMI_{\text{kg/day}} \times 365)
\times
\left(\frac{Share_{cat}}{100}\right)
\times
\left(\frac{Share_i}{100}\right)
```

### 2.2 Conversion to As-Fed Fresh Weight

Because Life Cycle Assessment (LCA) emission factors in **ECOALIM v9 /
AGRIBALYSE v3.1** are expressed per kilogram of fresh feedstuff as fed
(rather than 100% dry matter), `herdr` converts dry matter intake to
as-fed fresh weight using the ingredient’s dry matter percentage
($`DM\_pct_i`$) from `feed_characteristics.csv`:

``` math
AnnualFresh_i \; (\text{kg fresh / head / yr}) =
\frac{AnnualDMI_i}{\left(\frac{DM\_pct_i}{100}\right)}
```

*(If $`DM\_pct_i`$ is missing or non-positive, `herdr` defaults to
100%).*

### 2.3 Individual and Herd Footprints

Environmental impacts are calculated by multiplying the annual fresh
feed consumed by the corresponding characterization factor ($`EF_i`$)
from `ecoalim.parquet`:

#### Water Deprivation Footprint ($`m^3\text{ depriv.}`$)

``` math
WaterPerAnimal_i \; (\text{m}^3\text{ depriv. / head / yr}) = AnnualFresh_i \times EF_{water, i}
```

``` math
TotalWater_i \; (\text{m}^3\text{ depriv. / herd / yr}) = WaterPerAnimal_i \times Population
```

where $`EF_{water, i}`$ is `Water use (m3 depriv.)` based on the AWARE
characterization model.

#### Feed Greenhouse Gas Emissions ($`\text{kg CO}_2\text{e}`$)

``` math
GHGPerAnimal_i \; (\text{kg CO}_2\text{e / head / yr}) = AnnualFresh_i \times EF_{climate, i}
```

``` math
TotalGHG_i \; (\text{kg CO}_2\text{e / herd / yr}) = GHGPerAnimal_i \times Population
```

where $`EF_{climate, i}`$ corresponds to the total
(`Climate change - EF 3.1`), fossil (`Climate change - Fossil`), or
land-use change / deforestation
(`Climate change - Land use and LU change`) characterization factor.

------------------------------------------------------------------------

## 3. Water Deprivation (AWARE Model): Physical Volume vs. Scarcity

A common source of confusion in livestock environmental reporting is
equating the **Water Deprivation Footprint** with physical cubic meters
($`m^3`$) of water pumped, drank, or evaporated on the farm:

- **Scarcity-weighted, not volumetric:** The indicator does **not**
  measure raw physical gallons or $`m^3`$ of water extracted from a
  well. It quantifies **potential freshwater deprivation** relative to
  the world consumption-weighted average ($`m^3\text{ depriv.}`$ or
  $`m^3\text{ world-eq.}`$) using characterization factors from the
  European Commission’s **Environmental Footprint (EF 3.1)** and the
  **AWARE** (*Available WAter REmaining*) model (Boulay et al., 2018;
  UNEP-SETAC WULCA working group).
- For the complete hydrological model and watershed equations, see the
  primary reference: [Boulay et
  al. (2018)](https://doi.org/10.1007/s11367-017-1333-8).

------------------------------------------------------------------------

## 4. Feed Carbon Footprint Breakdown: Fossil vs. LUC

In addition to water, `herdr` evaluates feed greenhouse gas emissions
across two distinct life cycle mechanisms:

1.  **Fossil & Inputs GHG ($`\text{CO}_2\text{e}_{\text{fossil}}`$):**
    - Emissions stemming from mineral fertilizer synthesis (Haber-Bosch
      process for nitrogen), agrochemical manufacturing, tractor diesel
      during tillage and harvesting, natural gas for grain and forage
      dehydration, and grid electricity.
2.  **Land-Use Change / Deforestation GHG
    ($`\text{CO}_2\text{e}_{\text{luc}}`$):**
    - Direct greenhouse gas releases associated with deforestation and
      conversion of native biomes (e.g., Amazon, Cerrado, Chaco) to
      agricultural cropland within a 20-year retrospective horizon.
    - Particularly dominant in feed rations relying on solvent-extracted
      soybean meal imported from South American origins without
      zero-deforestation certification.

------------------------------------------------------------------------

## 5. R Code Workflow

### 5.1 Evaluating Standalone Feed Footprints

The
[`calculate_feed_emissions()`](https://juancbm99.github.io/herdr/reference/calculate_feed_emissions.md)
function can be run directly on any scenario directory containing
`user_data/`:

``` r

library(herdr)

# 1. Run standalone feed footprint calculation
feed_results <- calculate_feed_emissions(
  data_dir = system.file("Examples/Level1_Spain_Dairy_Cattle_2015", package = "herdr"),
  saveoutput = FALSE
)

# Preview columns
head(feed_results[, c(
  "animal_tag", "population",
  "feed_CO2e_fossil_kg_year", "feed_CO2e_luc_kg_year",
  "feed_water_m3_year", "feed_water_per_head_year"
)])
```

### 5.2 Full Cradle-to-Farm-Gate Impact Assessment

The complete pipeline
[`generate_impact_assessment()`](https://juancbm99.github.io/herdr/reference/generate_impact_assessment.md)
merges animal biology, IPCC Tier 2 direct emissions, Mottet land
competition, and feed life cycle footprints:

``` r

# 2. Run consolidated impact assessment
impact_results <- generate_impact_assessment(
  data_dir = system.file("Examples/Level1_Spain_Dairy_Cattle_2015", package = "herdr"),
  saveoutput = FALSE,
  ar = "AR5"
)
```

### 5.3 Visualizing Water and Feed Footprints

Use
[`plot_herdr_results()`](https://juancbm99.github.io/herdr/reference/plot_herdr_results.md)
with standardized selectors:

``` r

# A) Dedicated Water Deprivation Footprint (Total herd, in Mm3 depriv.)
p_water_total <- plot_herdr_results(
  df = impact_results,
  group_cols = "animal_tag",
  impact_category = "water",
  functional_unit = "total"
)
p_water_total

# B) Water Deprivation Intensity per kg of Milk (in m3 depriv. / kg FPCM milk)
p_water_milk <- plot_herdr_results(
  df = impact_results,
  group_cols = "animal_tag",
  impact_category = "water",
  functional_unit = "milk"
)
p_water_milk

# C) Multi-Impact Overview (Carbon, Land, Water in a 3-panel comparison)
p_overview <- plot_herdr_results(
  df = impact_results,
  group_cols = "animal_tag",
  impact_category = "overview"
)
p_overview
```

------------------------------------------------------------------------

## 6. Scientific References

- **Boulay, A.-M., Bare, J., Benini, L., Berger, M., Lathuillière, M.
  J., Manzardo, A., Margni, M., Motoshita, M., Núñez, M., Pastor, A. V.,
  Ridoutt, B., Oki, T., Worbe, S., & Pfister, S. (2018).** The
  WULCA-consensus characterization model for water scarcity footprint:
  assessing impacts of water consumption based on available water
  remaining (AWARE). *The International Journal of Life Cycle
  Assessment*, 23(2), 368–378.
- **INRAE (2023).** *ECOALIM dataset v9: Life Cycle Assessment datasets
  for animal feed ingredients*. Institut National de Recherche pour
  l’Agriculture, l’Alimentation et l’Environnement (INRAE), France.
