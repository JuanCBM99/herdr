# herdr <img src="man/figures/logo.png" align="right" height="139" />

[![Project Status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/JuanCBM99/herdr/actions/workflows/build.yaml/badge.svg)](https://github.com/JuanCBM99/herdr/actions/workflows/build.yaml)
[![docs](https://github.com/JuanCBM99/herdr/actions/workflows/docs.yaml/badge.svg)](https://github.com/JuanCBM99ocs.yaml)
[![codecov](https://codecov.io/gh/JuanCBM99/herdr/branch/dev/graph/badge.svg?token=GMIP6FM869)](https://codecov.io/gh/JuanCBM99/herdr)
[![test-coverage](https://github.com/JuanCBM99/herdr/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/JuanCBM99/herdr/actions/workflows/test-coverage.yaml)
[![Minimal-R](https://img.shields.io/badge/Made%20with-R-276DC3.svg)](https://www.r-project.org/)
---

# 🌟 herdr: GHG & LU Calculator 🌎📊

An R package to calculate Greenhouse Gas emissions (CH₄, N₂O) and Land Use (LU) in livestock systems, based on the IPCC Tier 2 methodology.

---

## 🖥️ Prerequisites: Installing R and RStudio

If you do not have R and RStudio installed, follow these steps:

### 1. Install R

1. Go to the [R Project website](https://www.r-project.org/) and download the latest version for your operating system (Windows, macOS, Linux).  
2. Run the installer and follow the on-screen instructions.

### 2. Install RStudio

1. Go to the [RStudio website](https://www.rstudio.com/products/rstudio/download/) and download the free RStudio Desktop version.  
2. Install RStudio using the downloaded installer.

---

## 🚀 Installation

herdr is a development package and can be installed directly from GitHub using `remotes`:

```r
# Install the remotes package if needed
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")

# Install herdr (dev branch)
remotes::install_github("JuanCBM99/herdr", ref = "dev")
```

## ✨✨ Key Features

IPCC Tier 2 Methodology: Robust calculations for enteric emissions, manure management, and indirect N₂O emissions.

Land Use Assessment: Integrated calculation of land requirements (m²/year) associated with different livestock management systems.

Nutritional Precision: Integrated Dry Matter Intake (DMI) estimation and nutritional alerts for Cattle, Sheep, and Goats.

Modular and Flexible: Allows detailed input of population census, diet profiles, and management systems at the farm or subregion level.

Consolidated Output: Generates a single report summarizing impacts (CH₄, N₂O, and LU).


## 📋 Workflow (Quick Start)

1. Prepare your Data
The package relies on a set of interconnected CSV files located in the user_data/ folder. For the full assessment to work correctly, ensure these files are configured:

Livestock & Census: livestock_definitions.csv and livestock_census.csv.

Weight & Growth: livestock_weights.csv.

Diets & Nutrition: diet_profiles.csv, diet_ingredients.csv, and feed_characteristics.csv.

Manure & Land: manure_management.csv and any land-specific parameters required for LU calculations.

Parameters: reproduction_parameters.csv and ipcc_coefficients.csv.

2. Generate Impact Assessment

```r
# Generate summary of all emissions and land use (m²/year)
impact_assessment <- generate_impact_assessment()

# Results
view(impact_assessment)
```

3. Explore Results

The output includes total CH₄, N₂O emissions, and land use.

## 📚 Documentation & Reference

[Package Documentation Website]: Includes workflow guides, some comprehensive examples and the theoretical basis (IPCC Tier 2).

Website URL: https://juancbm99.github.io/herdr/

## 🤝 Contributions

Contributions are welcome! If you find a bug or have suggestions for improvement, please open an issue or submit a pull request.

