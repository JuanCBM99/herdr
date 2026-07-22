# Land Use Methodology

## Land Use Methodology

### Introduction

In addition to greenhouse gas emissions, `herdr` estimates the
**feed-related land use** associated with livestock production.

Land use is calculated as the agricultural area required to produce the
feed consumed by a livestock population. The model combines feed intake,
crop and forage productivity, allocation factors, and international
trade information to estimate the total land requirement of each diet.

Results are expressed in **square metres (m²)** and are calculated on a
dry matter (DM) basis.

------------------------------------------------------------------------

## Calculation methodology

Land use is calculated independently for every feed ingredient using
three sequential steps.

### Step 1. Land requirement per kilogram of feed

The inverse of crop yield represents the land area required to produce
one kilogram of dry matter.

``` math
LandPerKg_i = \frac{1}{Yield_i}
```

where **Yield** is expressed in kilograms of dry matter per hectare (kg
DM/ha).

------------------------------------------------------------------------

### Step 2. Allocation adjustment

Some crops generate multiple products (for example grain and straw). To
avoid assigning the entire land area to a single product, the land
requirement is multiplied by an allocation factor.

``` math
AdjustedLandPerKg_i = \frac{1}{Yield_i} \times Allocation_i
```

The allocation factor ranges between 0 and 1 and is defined in
`mapping.csv`.

------------------------------------------------------------------------

### Step 3. Total land use

Annual feed consumption is multiplied by the adjusted land requirement
and converted from hectares to square metres.

``` math
LandUse_i = AnnualConsumption_i \times \left( \frac{1}{Yield_i} \times Allocation_i \right) \times 10\,000
```

where

- **AnnualConsumption** is annual dry matter intake (kg DM/year),
- **Yield** is crop productivity (kg DM/ha),
- **Allocation** is the economic allocation factor,
- **10,000** converts hectares into square metres.

The total land use reported by `herdr` is obtained by summing the
contribution of every ingredient included in the diet.

------------------------------------------------------------------------

## Yield databases

`herdr` combines two complementary yield databases.

### Crop yields

Crop productivity is obtained from official **FAOSTAT** statistics.

These data include major feed crops such as cereals, oilseeds and
legumes and are expressed as country-level average yields.

### Forage yields

Official global statistics for forage productivity are often
unavailable.

For this reason, `herdr` includes a curated forage yield database
compiled from multiple literature sources and technical reports to
provide broad geographical coverage.

Users performing high-resolution regional studies are encouraged to
replace these values with local data whenever available.

------------------------------------------------------------------------

## Ingredient mapping

Land-use calculations rely on the file `mapping.csv`, which links feed
ingredients with the corresponding yield database.

| ingredient   | yield_name | allocation |
|:-------------|:-----------|-----------:|
| grass_fresh  | Grass      |        1.0 |
| barley_grain | Barley     |       0.80 |

The columns have the following meaning:

- **ingredient** — ingredient name used in `diet_ingredients.csv`;
- **yield_name** — corresponding crop or forage name in the yield
  databases;
- **allocation** — proportion of land attributed to that ingredient.

Every ingredient included in a diet must have a corresponding entry in
`mapping.csv`.

------------------------------------------------------------------------

## Feed origin and dynamic background allocation

Land-use calculations require the user to specify the country where the
farm is located and the assessment year.

``` r

results <- calculate_land_use(
  farm_country = "Spain", 
  year = 2022
)
```

The origin of the feed ingredients dictates which crop yields are
applied. Users can explicitly define the origin of each ingredient in
their input files. However, when the exact origin is unknown (left as
NA), herdr employs a dynamic background allocation engine.

This hybrid engine utilizes FAO production and trade data to calculate
missing feed origins based on a 70% self-sufficiency rule. It estimates
the proportion of the ingredient that is produced domestically versus
the proportion that is imported, automatically distributing the land
footprint across the respective trade partner countries.

⚠️ Important Data Note: To perform these dynamic calculations without
bloating the package size, herdr relies on an external high-resolution
FAO trade matrix. The first time you run a calculation that requires
background origin allocation, the package will automatically download
the fao_trade_matrix.parquet file (approx. 187 MB) to your local
environment.

------------------------------------------------------------------------

## Assumptions and limitations

Several assumptions should be considered when interpreting land-use
estimates.

- All calculations are performed on a **dry matter (DM)** basis.
- Crop yields represent **country-average productivity** rather than
  farm-specific values.
- Forage yields are compiled from multiple published sources and may not
  represent local management conditions.
- Allocation factors should reflect the accounting approach adopted by
  the user.
- The background allocation engine relies on a **70% self-sufficiency
  assumption** together with historical **FAO trade matrices**,
  representing macro-economic trade flows rather than farm-level supply
  chains.
- Results should be interpreted as estimates of the land required to
  produce feed, not as direct measurements of occupied agricultural
  land.

------------------------------------------------------------------------

## Running the calculation

Land use can be estimated independently using the standalone function:

``` r

results <- calculate_land_use(
  farm_country = "Spain",
  year = 2022
)
```

Alternatively, land-use calculations are performed automatically when
running the complete assessment through
[`generate_impact_assessment()`](https://juancbm99.github.io/herdr/reference/generate_impact_assessment.md).
