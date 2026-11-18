# Welcome to herdr: GHG & LU Calculator

**herdr** is an R package designed to calculate **Greenhouse Gas (GHG)
emissions** and **Land Use (LU)** associated with different livestock
systems based on **IPCC Tier 2** methodologies.

------------------------------------------------------------------------

## 🌐 Try it online — no installation required

Don’t want to install R right now? The full model is available as an
interactive web app:

[▶ Launch herdr App](https://juancbm99.shinyapps.io/herdr/)

The app uses the same calculation engine as the R package, so results
are identical whichever way you run it. It’s the fastest way to explore
herdr, edit input tables, and visualize results without writing any
code.

------------------------------------------------------------------------

## 🚀 Quick Start (run in \<1 min)

``` r

# Install
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("JuanCBM99/herdr")
library(herdr)

# Initialize project
herdr_init()

# Load example data
file.copy(list.files("Examples/Level1_Spain_Dairy_Cattle_2015", full.names = TRUE), "user_data", overwrite = TRUE)

# Run model
results <- generate_impact_assessment(automatic_cycle = FALSE, farm_country = "Spain", year = 2015)
```

This runs a complete example of a cattle production system and returns
greenhouse gas emissions and land use results.

------------------------------------------------------------------------

## 🎥 Video Tutorial

------------------------------------------------------------------------

## 📚 Guides and Examples

Below are the main guides and examples for using the package:

### 1. Introduction & Shiny App

Learn what herdr does, which animals are supported, how to install the
package, or launch the local web interface.

- [Go to Introduction
  →](https://juancbm99.github.io/herdr/articles/Introduction.md)
- [Go to Interactive Shiny App Guide
  →](https://juancbm99.github.io/herdr/articles/app.md)

### 2. Technical & Theoretical Reference

Explore the included files, schemas, and full IPCC Tier 2 / GLEAM
mathematical formulations.

- [Go to Technical Reference: Files & Parameters
  →](https://juancbm99.github.io/herdr/articles/Technical_reference.md)
- [Go to Theoretical Basis (IPCC Tier 2 & GLEAM)
  →](https://juancbm99.github.io/herdr/articles/Theoretical_basis.md)

### 3. Methodology Guides

Explore herd demography, manure management systems, land competition,
and feed carbon/water footprints.

- [Go to Herd Demography & Life Cycles
  →](https://juancbm99.github.io/herdr/articles/Herd_Demography.md)
- [Go to Manure System Guide
  →](https://juancbm99.github.io/herdr/articles/Manure.md)
- [Go to Land use
  →](https://juancbm99.github.io/herdr/articles/land_use.md)
- [Go to Feed Water & Carbon Footprint (AWARE)
  →](https://juancbm99.github.io/herdr/articles/Water_and_Feed_Footprint.md)
- [Go to Adding a Custom Feed Ingredient
  →](https://juancbm99.github.io/herdr/articles/Adding_Ingredient.md)

### 4. Workflow

Step-by-step guide on how to perform a general use of the package.

- [Go to Workflow Guide: Step-by-Step
  →](https://juancbm99.github.io/herdr/articles/Workflow.md)

### 5. Examples

Simple guide that explains how to do a concrete example (3 included)

- [Go to Easy Example
  →](https://juancbm99.github.io/herdr/articles/Easy_Example.md)
- [Go to Moderate Example
  →](https://juancbm99.github.io/herdr/articles/Moderate_Example.md)
- [Go to Hard Example
  →](https://juancbm99.github.io/herdr/articles/Difficult_Example.md)
