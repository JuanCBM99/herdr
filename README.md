# herdr <img src="man/figures/logo.png" align="right" height="139" />

[![Project Status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/JuanCBM99/herdr/actions/workflows/build.yaml/badge.svg)](https://github.com/JuanCBM99/herdr/actions/workflows/build.yaml)
[![docs](https://github.com/JuanCBM99/herdr/actions/workflows/docs.yaml/badge.svg)](https://github.com/JuanCBM99/herdr/actions/workflows/docs.yaml)
[![codecov](https://codecov.io/gh/JuanCBM99/herdr/branch/dev/graph/badge.svg?token=GMIP6FM869)](https://codecov.io/gh/JuanCBM99/herdr)
[![test-coverage](https://github.com/JuanCBM99/herdr/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/JuanCBM99/herdr/actions/workflows/test-coverage.yaml)
[![Tutorial](https://img.shields.io/badge/Video-Tutorial-red?logo=youtube&logoColor=white)](https://www.youtube.com/watch?v=wmGIQ3g-ZFk)
[![Minimal-R](https://img.shields.io/badge/Made%20with-R-276DC3.svg)](https://www.r-project.org/)

---

# 🌱 herdr: Greenhouse Gas Emissions & Land Use Assessment for Livestock

**herdr** is an R package for estimating **greenhouse gas (GHG) emissions** and **land use** from livestock production systems using **IPCC Tier 2** methodologies.

The package currently supports:

- 🐄 Cattle
- 🐑 Sheep
- 🐐 Goats
- 🐖 Swine
- 🐔 Poultry

It provides estimates of:

- Enteric methane (CH₄)
- Methane from manure management (CH₄)
- Direct and indirect nitrous oxide emissions (N₂O)
- Feed-related land use (m²)

---

# 📺 Video Tutorial

A complete walkthrough covering installation, project setup, input files, and running your first assessment is available on YouTube:

👉 https://youtu.be/wmGIQ3g-ZFk

---

# 💻 Prerequisites

Before installing **herdr**, install both **R** and **RStudio**.

### Install R

Download the latest version from:

https://cran.r-project.org/

### Install RStudio

Download the free RStudio Desktop edition from:

https://posit.co/download/rstudio-desktop/

---

# 🚀 Installation

Install the development version directly from GitHub.

```r
if (!requireNamespace("remotes", quietly = TRUE))
  install.packages("remotes")

remotes::install_github("JuanCBM99/herdr")

library(herdr)
```

---

# ⚡ Quick Start

Initialize a new project:

```r
herdr_init()
```

Copy one of the bundled examples into your `user_data/` folder:

```r
file.copy(
  list.files(
    "Examples/Level1_Spain_Dairy_Cattle_2015",
    full.names = TRUE
  ),
  "user_data",
  overwrite = TRUE
)
```

Run the complete assessment:

```r
results <- generate_impact_assessment(crop_yield_country = "Spain")

results
```

Output files will automatically be written to the `output/` directory.

---

# ✨ Main Features

- **IPCC Tier 2 implementation** following the 2019 Refinement.
- **Support for ruminants and monogastric species.**
- **FEDNA-based energy equations** for swine and poultry.
- **Automatic calculation** of:
  - Gross Energy (GE)
  - Dry Matter Intake (DMI)
  - Volatile Solids (VS)
  - Nitrogen intake, retention and excretion
- **Enteric CH₄** estimation.
- **Manure CH₄** estimation.
- **Direct and indirect N₂O** emissions.
- **Feed-related land use** using FAOSTAT and forage yield databases.
- **Modular workflow**, allowing individual calculations or complete assessments.
- **CSV-based inputs**, making the package transparent and easy to modify.

---

# 📚 Documentation

Complete documentation, theoretical background, workflow guides and examples are available at:

**https://juancbm99.github.io/herdr/**

---

# 📖 Methodology

The package follows the **2019 Refinement to the 2006 IPCC Guidelines for National Greenhouse Gas Inventories**, Volume 4 (AFOLU), Chapter 10: *Emissions from Livestock and Manure Management*.

Energy requirement equations for **swine** and **poultry** are implemented following the FEDNA methodology published by the Spanish Ministry of Agriculture.

---

# 🤝 Contributing

Contributions are welcome.

If you encounter a bug or have suggestions for improvements, please open an issue or submit a pull request.
