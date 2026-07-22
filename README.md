------------------------------------------------------------------------

editor_options: markdown: wrap: 72 ---

# herdr <img src="man/figures/logo.png" align="right" height="139"/>

[![Project Status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active) [![R-CMD-check](https://github.com/JuanCBM99/herdr/actions/workflows/build.yaml/badge.svg)](https://github.com/JuanCBM99/herdr/actions/workflows/build.yaml) [![docs](https://github.com/JuanCBM99/herdr/actions/workflows/docs.yaml/badge.svg)](https://github.com/JuanCBM99/herdr/actions/workflows/docs.yaml) [![codecov](https://codecov.io/gh/JuanCBM99/herdr/branch/dev/graph/badge.svg?token=GMIP6FM869)](https://codecov.io/gh/JuanCBM99/herdr) [![test-coverage](https://github.com/JuanCBM99/herdr/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/JuanCBM99/herdr/actions/workflows/test-coverage.yaml) [![Tutorial](https://img.shields.io/badge/Video-Tutorial-red?logo=youtube&logoColor=white)](https://www.youtube.com/watch?v=wmGIQ3g-ZFk) [![Made with R](https://img.shields.io/badge/Made%20with-R-276DC3.svg)](https://www.r-project.org/)

------------------------------------------------------------------------

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

------------------------------------------------------------------------

# 🌐 Web Interface (No R or installation required!)

You don't need to be a programmer or have R installed to use **herdr**. A fully interactive web application allows users to estimate greenhouse gas emissions, calculate feed-related land use, modify diets, and export results directly from a web browser.

👉 **Launch the herdr Web Interface:**\
[**https://juancbm99.shinyapps.io/herdr/**](https://juancbm99.shinyapps.io/herdr/){.uri}

> **Note:** The web application performs all calculations in the background and does not require any software installation or local datasets.

------------------------------------------------------------------------

# 📺 Video Tutorial

A complete walkthrough covering installation, project setup, input files, and running your first assessment is available on YouTube.

👉 <https://youtu.be/wmGIQ3g-ZFk>

------------------------------------------------------------------------

# 💻 Prerequisites (For Local Installation)

To use **herdr** locally, first install both **R** and **RStudio**.

## Install R

<https://cran.r-project.org/>

## Install RStudio

<https://posit.co/download/rstudio-desktop/>

------------------------------------------------------------------------

# 🚀 Installation

Install the development version directly from GitHub.

``` r
if (!requireNamespace("remotes", quietly = TRUE))
  install.packages("remotes")

remotes::install_github("JuanCBM99/herdr")

library(herdr)
```

------------------------------------------------------------------------

# 🖥️ Launch the Interactive Application

Once the package has been installed, launch the graphical interface directly from R.

``` r
library(herdr)

run_app()
```

------------------------------------------------------------------------

# ⚡ Quick Start (Command Line)

## 1. Initialize a new project

``` r
herdr_init()
```

## 2. Copy one of the bundled examples

``` r
file.copy(
  list.files(
    system.file(
      "Examples",
      "Level1_Spain_Dairy_Cattle_2015",
      package = "herdr"
    ),
    full.names = TRUE
  ),
  "user_data",
  overwrite = TRUE
)
```

## 3. Run the assessment

``` r
results <- generate_impact_assessment(
  farm_country = "Spain",
  year = "2024"
)

results
```

> **Important:** The first time an assessment requires feed origin allocation, **herdr** automatically downloads the required FAO background dataset (\~187 MB). This download occurs only once and is reused in future analyses.

Output files are automatically saved in the `output/` directory.

------------------------------------------------------------------------

# ✨ Main Features

- 🌐 Interactive web interface requiring no programming experience.
- 📖 Full implementation of the **IPCC 2019 Refinement (Tier 2)** methodology.
- 🐄 Support for both ruminant and monogastric livestock.
- 🐖 FEDNA-based energy requirement equations for swine and poultry.
- ⚡ Automatic calculation of:
  - Gross Energy (GE)
  - Dry Matter Intake (DMI)
  - Volatile Solids (VS)
  - Nitrogen intake, retention and excretion
- 🌿 Enteric methane (CH₄) emissions.
- ♻️ Manure methane (CH₄) emissions.
- 🌱 Direct and indirect nitrous oxide (N₂O) emissions.
- 🌾 Feed-related land use using:
  - FAOSTAT crop yields
  - Forage yield databases
  - Dynamic FAO trade allocation matrix
  - 70% domestic self-sufficiency allocation rule
- 🧩 Modular workflow allowing both individual calculations and complete assessments.
- 📄 Transparent CSV-based input files that can easily be modified by users.

------------------------------------------------------------------------

# 📚 Documentation

Complete documentation, theoretical background, workflow guides, and examples are available at:

<https://juancbm99.github.io/herdr/>

------------------------------------------------------------------------

# 📖 Methodology

**herdr** follows the methodologies described in:

> **2019 Refinement to the 2006 IPCC Guidelines for National Greenhouse Gas Inventories**
>
> Volume 4 — Agriculture, Forestry and Other Land Use (AFOLU)
>
> Chapter 10 — Emissions from Livestock and Manure Management.

Energy requirement equations for swine and poultry follow the **FEDNA** methodology published by the Spanish Ministry of Agriculture.

------------------------------------------------------------------------

# 🤝 Contributing

Contributions are welcome.

If you encounter a bug, identify an error, or have suggestions for improvements, please open an issue or submit a pull request.

------------------------------------------------------------------------

# 📄 License

This project is distributed under the **MIT License**.

See the `LICENSE` file for details.
