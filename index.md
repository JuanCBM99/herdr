# Welcome to herdr: GHG & LU Calculator

**herdr** is an R package designed to calculate **Greenhouse Gas (GHG)
emissions** and **Land Use (LU)** associated with different livestock
systems based on **IPCC Tier 2** methodologies.

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
file.copy(list.files("Examples/Level1_Spain_Dairy_Cattle_2015", full.names = TRUE), "user_data")

# Run model
results <- generate_impact_assessment()
```

This runs a complete example of a cattle production system and returns
greenhouse gas emissions and land use results.

------------------------------------------------------------------------

## 🎥 Video Tutorial

------------------------------------------------------------------------

## 📚 Guides and Examples

Below are the main guides and examples for using the package:

### 1. Introduction

Learn what herdr does, which animals are supported, and how to install
the package.

- [Go to Introduction
  →](https://juancbm99.github.io/herdr/articles/Introduction.md)

### 2. Technical reference

Explore the included files and some explanation about them.

- [Go to Technical Reference: Files & Parameters
  →](https://juancbm99.github.io/herdr/articles/Technical_reference.md)

### 3. Manure & Land use Guide

Explore the included manure management systems based on IPCC 2019
tables.

- [Go to Manure System Guide
  →](https://juancbm99.github.io/herdr/articles/Manure.md)
- [Go to Land use
  →](https://juancbm99.github.io/herdr/articles/land_use.md)

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
