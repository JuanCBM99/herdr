# 🌟 AnimalGEILU: GHG & LU Calculator 🌎📊

An R package to calculate Greenhouse Gas emissions (CH₄, N₂O) and Land Use (LU) in livestock systems, based on the IPCC Tier 2 methodology.

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

```r
library(AnimalGEILU)

# Download templates into the 'user_data/' folder of your working directory
data_path <- download_templates()

# View downloaded files
list.files(data_path)
```

2. Generate Impact Assessment

```r
# Generate summary of all emissions and land use (m²/year)
impact_assessment <- generate_impact_assessment()

# Results
head(impact_assessment)
```

## 📚 Documentation & Reference

[Package Documentation Website]: Workflow guides, function references, and theoretical basis (IPCC Tier 2).

[Introduction Guide Link]: Step-by-step tutorial on preparing your data.

## 🤝 Contributions

Contributions are welcome! If you find a bug or have suggestions for improvement, please open an issue or submit a pull request.

