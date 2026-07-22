# General Workflow: Step-by-Step Guide

## General Workflow

This vignette explains how to prepare a complete `herdr` project, from
creating a working directory to generating the final greenhouse gas and
land-use assessment.

The recommended workflow is:

1.  Install the package.
2.  Create a new R project.
3.  Initialize the project structure.
4.  Prepare the input files.
5.  Run the assessment.
6.  Review and export the results.

------------------------------------------------------------------------

### 1. Install herdr

Install the latest development version directly from GitHub.

``` r

if (!requireNamespace("remotes", quietly = TRUE))
  install.packages("remotes")

remotes::install_github("JuanCBM99/herdr")

library(herdr)
```

### 2. Create a Project

Create a new RStudio project.

Working inside an `.Rproj` keeps every project self-contained and allows
herdr to automatically locate all required folders and input files.

    My_Project/

### 3. Initialize the Project

Initialize the directory structure by running:

``` r

herdr_init()
```

This creates the following directories:

- `user_data/`: Contains all editable CSV input files.
- `Examples/`: Contains ready-to-use example datasets.
- `output/`: Stores the generated results.

### 4. Choose how to prepare your data

There are two possible workflows to input your data.

#### Option A — Interactive User Interface (Recommended)

Launch the graphical web interface directly from your console:

``` r

run_app()
```

The interface allows you to:

- Edit every input table interactively.
- Validate your datasets in real-time.
- Navigate through the workflow step-by-step.
- Execute the model.
- Download the results.

This option is highly recommended for first-time users.

#### Option B — Edit the CSV files manually

Alternatively, you can edit the CSV templates located inside the
`user_data/` folder using Excel, LibreOffice, or any other spreadsheet
software.

### 5. Population (livestock_census.csv)

This file defines the livestock population included in the assessment.
Each row corresponds to one animal cohort.

The key identifiers are:

- `animal_tag`
- `region`
- `subregion`
- `class_flex`

Together, these columns uniquely identify every production group
throughout the project. The `population` column contains the number of
animals represented by that row.

#### Population modes

Two calculation modes are available:

**Manual population**

Every animal category is entered explicitly. Use
`automatic_cycle = FALSE` when all populations are already known.

**Automatic herd cycle**

Only breeding animals are required. The package automatically estimates
offspring and replacement animals using reproductive parameters. Use
`automatic_cycle = TRUE`. (Note: This option currently supports the
predefined livestock categories included with the package).

### 6. Nutrition

Diet information is divided into two files.

#### Diet profiles (diet_profiles.csv)

Defines the overall macronutrient distribution of the diet. The four
proportions must sum to 100%:

- forage
- concentrate
- milk
- milk_replacer

#### Diet ingredients (diet_ingredients.csv)

Defines the specific ingredients composing each feed category. Within
every category, the ingredient percentage must also sum to 100%.

**Dynamic Feed Origin:**

You can specify the `origin_country` for each ingredient. If the origin
is unknown, leave it as `NA`. herdr features a dynamic background
allocation engine that will automatically estimate the origins of NA
ingredients based on FAO trade matrices and a 70% self-sufficiency rule.

#### Adding a new ingredient

When introducing a new custom ingredient, you must also update:

- `feed_characteristics.csv` (for nutritional values)
- `mapping.csv` (for land-use allocation)
- `forage_yields.csv` or `fao_crop_yields.csv` (if appropriate)

### 7. Animal Definitions

Animal characteristics are specified using:

- `livestock_definitions.csv`
- `monogastric_definitions.csv`
- `livestock_weights.csv`

These files connect each animal group to its physiological parameters,
body weights, production periods, diet, and IPCC coefficients.

**Important:** Every combination of `animal_tag`, `region`, `subregion`,
and `class_flex` must exactly match those defined in the livestock
census.

### 8. Manure Management (manure_management.csv)

This file defines how manure is managed for each livestock group. Each
row specifies the manure management system, climate, storage conditions,
and allocation.

If manure is divided among several systems, duplicate the row and assign
the corresponding allocation to each system. For every livestock cohort,
the allocation values must sum to 1.0.

(Detailed descriptions of every supported system are available in the
Manure Management Guide vignette).

### 9. Run the Assessment

Once every input file has been completed, execute the main function:

``` r

results <- generate_impact_assessment(
  automatic_cycle = FALSE,
  farm_country = "Spain",
  year = 2022,
  saveoutput = TRUE
)
```

The function automatically performs data validation, energy
calculations, methane estimation, nitrogen and nitrous oxide estimation,
land-use calculations, and result aggregation.

> ⚠️ **Important Data Note:** If your diet contains ingredients with
> unknown origins (NA), the first time you run this function, herdr will
> automatically download the required FAO background dataset (approx.
> 187 MB in .parquet format) to your local environment. This is a
> one-time process.

### 10. Results

The returned object is a data frame containing all calculated
indicators. Because we set `saveoutput = TRUE`, the results are also
automatically written as CSV files to the `output/` folder.

### Common Issues

Most execution errors are caused by inconsistencies among input files.
Typical problems include:

- Missing animal definitions or missing diets.
- Duplicated identifiers.
- Allocations not summing to 1 (or percentages not summing to 100).
- Unknown feed ingredients (not mapped properly).
- Missing body weights.

The package performs automatic validation before calculations begin and
reports detected problems in the console to help you fix them.

### Next steps

After becoming familiar with the workflow, consult the remaining
vignettes for more detailed information about individual components of
the package:

- Technical Reference
- Land Use Methodology
- Manure Management Guide
