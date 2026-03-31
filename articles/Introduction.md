# Introduction to herdr

**herdr** is an R package designed to calculate **greenhouse gas (GHG)
emissions** and **land use (LU)** associated with livestock production.

Currently supported animals: \* **Cattle** \* **Goat** \* **Sheep**

------------------------------------------------------------------------

## ⬇️ Installation & Loading

Install the package directly from GitHub using **remotes**:

``` r
# Install 'remotes' if necessary
if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
}

# Install the herdr package with vignettes
remotes::install_github("JuanCBM99/herdr", build_vignettes = TRUE)
```

------------------------------------------------------------------------

## *What the Package Calculates*

The package offers specific functions to model the primary sources of
environmental impact from livestock based on a detailed **Energy
Balance** model. It accounts for energy requirements for maintenance,
growth, lactation, pregnancy, work, and wool production.

### Methane (CH$_{4}$) Emissions

- **Enteric fermentation:** Emissions produced during the digestive
  process based on Gross Energy intake and diet digestibility.
- **Manure management:** Emissions derived from Volatile Solids ($VS$)
  decay, adjusting for the specific conditions (MCF) of diverse
  management systems (e.g., liquid lagoons, solid storage, pasture,
  composting).

### Nitrous Oxide (N$_{2}$O) Emissions

- **Direct:** Emissions from manure storage/treatment systems.
- **Indirect:** Emissions via nitrogen volatilization ($NH_{3}$,
  $NO_{x}$) and leaching into water systems.

### Land Use

- Calculates the **land area** ($m^{2}$) required to produce the feed
  based on economic allocation, linking animal intake directly to crop
  yields. Work in progress, check [Land
  use](https://juancbm99.github.io/herdr/articles/land_use.md) for more
  info.

### Multi-Scenario Analysis

- `herdr` allows you to model different farms, regions, or management
  strategies within the same project. You can compare how emissions
  change between a farm in the mountains and one in the valley, or
  between two different countries, all in a single run.

------------------------------------------------------------------------

## 🎯 Calculation Methodology (IPCC 2019)

All emission calculations within `herdr` adhere to the Tier 2 methods
established by the Intergovernmental Panel on Climate Change (IPCC).

Specifically, we use the guidelines from the 2019 Refinement to the 2006
IPCC Guidelines for National Greenhouse Gas Inventories:

> *“Volume 4: Agriculture, Forestry and Other Land Use, Chapter 10:
> EMISSIONS FROM LIVESTOCK AND MANURE MANAGEMENT”*

------------------------------------------------------------------------

## 🥕 The Role of Diet & Validation

Diet composition is the primary driver of the results. The functions use
nutritional data (Energy, Protein, NDF) to determine key parameters like
the Methane Conversion Factor ($Y_{m}$) and Nitrogen Excretion
($N_{ex}$).

**Automatic Validation:** Before calculating, `herdr` automatically
checks the consistency of input data, ensuring that biologically
impossible values are flagged or prevented.

------------------------------------------------------------------------
