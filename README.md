# 🌟 AnimalGEILU: GHG & LU Calculator 🌎📊

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

AnimalGEILU is a development package and can be installed directly from GitHub using `remotes`:

```r
# Install the remotes package if needed
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")

# Install AnimalGEILU (dev branch)
remotes::install_github("JuanCBM99/Animal_GEI_LU", ref = "dev")
```

## ✨✨ Key Features

IPCC Tier 2 Methodology: Robust calculations for enteric emissions, manure management, and indirect N₂O emissions.

Modular and Flexible: Allows detailed input of population, diets, and management systems at farm/zone level.

Consolidated Output: Generates a single report summarizing impacts (CH₄, N₂O, and LU).


## 📋 Workflow (Quick Start)

1. Download Templates

If you wish to change the default data:

```r
library(AnimalGEILU)

# Download templates into the 'user_data/' folder of your working directory
download_templates()
```

After this, open the downloaded folder and edit them following the website guide.

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

[Package Documentation Website]: Workflow guides, data dictionary, function references, and theoretical basis (IPCC Tier 2).

Website URL: 

## 🤝 Contributions

Contributions are welcome! If you find a bug or have suggestions for improvement, please open an issue or submit a pull request.

