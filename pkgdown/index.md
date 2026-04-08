# Welcome to herdr: GHG & LU Calculator 

**herdr** is an R package designed to calculate **Greenhouse Gas
(GHG) emissions** and **Land Use (LU)** associated with different
livestock systems based on **IPCC Tier 2** methodologies.

---

## 🚀 Quick Start (run in <1 min)

```r
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

This runs a complete example of a cattle production system and returns greenhouse gas emissions and land use results.

---

## 🎥 Video Tutorial

<h2>Test video</h2>

<div class="ratio ratio-16x9">
  <iframe src="https://www.youtube.com/embed/wmGIQ3g-ZFk"></iframe>
</div>

---

## 📚 Guides and Examples

Below are the main guides and examples for using the package:

### 1. Introduction

Learn what herdr does, which animals are supported, and how to
install the package.  

- [Go to Introduction &rarr; ](articles/Introduction.html)

### 2. Technical reference

Explore the included files and some explanation about them.

- [Go to Technical Reference: Files & Parameters &rarr; ](articles/Technical_reference.html)

### 3. Manure & Land use Guide

Explore the included manure management systems based on IPCC 2019 tables.

- [Go to Manure System Guide &rarr; ](articles/Manure.html)
- [Go to Land use &rarr; ](articles/land_use.html)

### 4. Workflow

Step-by-step guide on how to perform a general use of the package. 

- [Go to Workflow Guide: Step-by-Step &rarr; ](articles/Workflow.html)

### 5. Examples
Simple guide that explains how to do a concrete example (3 included)

- [Go to Easy Example &rarr; ](articles/Easy_Example.html)
- [Go to Moderate Example &rarr; ](articles/Moderate_Example.html)
- [Go to Hard Example &rarr; ](articles/Difficult_Example.html)
