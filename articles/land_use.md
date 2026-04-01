# Land Use Guide & Disclaimers

## Land Use Methodology and Data Disclaimers

### I. Introduction

The
[`calculate_land_use()`](https://juancbm99.github.io/herdr/reference/calculate_land_use.md)
function in the **herdr** package provides an estimation of the total
land area (in $m^{2}$) required to produce the feed consumed by a given
livestock population.

This assessment is critical for understanding the environmental
footprint and spatial efficiency of different production systems,
allowing users to compare the “land cost” of different dietary
strategies.

------------------------------------------------------------------------

### II. Methodology

The model integrates animal nutritional requirements with crop
productivity through a three-step calculation for each feed ingredient.

#### 1. Inverse of Crop Yield

First, we calculate the land required per unit of feed:
$$LandPerKg_{i} = \frac{1}{Yield_{i}}$$*This expresses hectares required
per kilogram of feed (ha/kg).*

#### 2. Economic Allocation Adjustment

This value is adjusted by an allocation factor to account for the share
of land attributed to co-products (e.g., grain vs. straw):
$$AdjustedLandPerKg_{i} = \frac{1}{Yield_{i}} \times Allocation_{i}$$

#### 3. Total Land Use

Finally, the total land use is obtained by multiplying the annual feed
consumption by the adjusted land requirement and converting hectares to
square meters:
$$LandUse_{i} = AnnualConsumption_{i} \times \left( \frac{1}{Yield_{i}} \times Allocation_{i} \right) \times 10,000$$

**Where:**

- **AnnualConsumption**: Total kg of dry matter (DM) consumed by the
  population per year.
- **Yield**: Productivity of the crop in kg DM per hectare (kg/ha).
- **Allocation**: Economic or physical factor (0 to 1) to account for
  co-products.
- **10,000**: Conversion factor from hectares (ha) to square meters
  ($m^{2}$).

------------------------------------------------------------------------

### III. Country-Specific Yield Assumption

The package uses a comprehensive FAO-based global database. However, for
the calculation, the user must specify a **single country** via the
`crop_yield_country` argument.

> ⚠️ **Important Limitation:** It is not possible to assign different
> countries of origin to individual ingredients within a single run. For
> example, you cannot specify that barley comes from Ukraine while maize
> comes from Spain. All ingredients will use the yields of the single
> selected country.

------------------------------------------------------------------------

### IV. Data Sources and Disclaimers

The accuracy of land use estimation depends heavily on the underlying
yield databases:

1.  **Official Crop Yields (FAOSTAT 2024)**: Yields for major crops
    (barley, maize, soya, etc.) represent standardized, country-level
    average yields reported by national authorities.
2.  **Non-Official Forage Yields**: Forage data (grasses, alfalfa,
    silage) often lacks official global statistics. **herdr** includes
    compiled non-official values to provide broader coverage.

*Users conducting high-precision analyses should cross-reference these
values with local data sources where possible.*

------------------------------------------------------------------------

### V. The Mapping System

The `mapping.csv` file acts as a “translator” between your diets and the
yield databases:

| ingredient   | yield_name | allocation |
|:-------------|:-----------|:-----------|
| grass_fresh  | Grass      | 1.0        |
| barley_grain | Barley     | 0.8        |

- **ingredient**: The name used in your `diet_ingredients.csv`.
- **yield_name**: The exact key used to match the FAO or forage yield
  databases.
- **allocation**: The economic share (0 to 1) of land impact attributed
  to the feed.

------------------------------------------------------------------------

### VI. Technical Implementation

You can call the function directly or as part of the full assessment. To
find valid country names, check the `fao_crop_yields.csv` file in the
package.

``` r
# Example: Calculating land use for a study based on Spanish productivity
results <- calculate_land_use(crop_yield_country = "Spain")
```
