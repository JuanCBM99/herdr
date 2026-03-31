# General Workflow: Step-by-Step Guide

## 🚀 herdr Workflow: From Raw Data to Impact Assessment

The `herdr` package follows a modular logic where information flows from
population and nutrition to biological metabolism and, finally, to
environmental impact.

------------------------------------------------------------------------

### Step 0: Setting up your Workspace

Before entering your data, you must locate your working directories.
When the package is loaded, `herdr` interacts with specific folders in
your project root:

- **`user_data/`**: This is your active production folder. It contains
  CSV templates with the required headers. **This is where you should
  input your data.**
- **`examples/`**: This folder contains pre-filled datasets for the
  cases shown in the website (Easy, Moderate, Difficult).

> **Tip:** We highly recommend copying the files from one of the
> `examples/` subfolders into `user_data/` for your first run to ensure
> everything is configured correctly.

------------------------------------------------------------------------

### 📂 Folder Structure & File Roles

To keep your project organized, *herdr* separates your project data from
internal scientific libraries. To see which specific files you should
edit and which ones to leave as references, check the [Technical
Reference](https://juancbm99.github.io/herdr/articles/Technical_reference.md).

------------------------------------------------------------------------

### Phase 1: Population and Geography (`livestock_census.csv`)

This file is the starting point of your inventory. Here, you define
**who** is being studied, **where** they are located, and **how many**
animals are present.

#### Main Columns:

- **`animal_tag`**: Unique identifier for the animal group (e.g.,
  `mature_dairy_cattle`).
- **`region` & `subregion`**: Defines the geographic scope (e.g.,
  `spain` & `north`).
- **`class_flex`**: A flexible tag for anything you want to study (e.g.,
  `intensive`, `grazing`, `dry_phase`, `lactation_phase`).
- **`population`**: Total number of heads.

#### Calculation Methodologies:

##### Option A: Manual Census (Static Populations)

Best used when you already have the exact numbers for all animal
categories (including offspring) and want to study a specific “snapshot”
in time.

- **Usage:** Manually enter the population for every category. If you
  wish to add a new category just add a new row, but you must manually
  ensure that every new `animal_tag` exists in all other configuration
  files (definitions, nutrition, manure, etc.), or the model will fail.

##### Option B: Automatic Cycle (Herd Projection)

Best used when you want the model to **project the herd structure** for
you. Based on the number of mature animals (mothers/sires) and
biological rates, the model automatically calculates how many offspring
are born and how many replacement animals are present.

- **Usage:** Triggered by using standard predefined tags (e.g.,
  `mature_dairy_cattle`, `mature_sheep_female_meat`).
- **Note:** Custom tags added by the user do not participate in this
  automatic cycle.

------------------------------------------------------------------------

### Phase 2: Nutritional Strategy

This phase calculates the **Gross Energy (GE)**, which drives methane
and nitrogen results.

#### 1. Macro Diet (`diet_profiles.csv`)

Set the general balance of the diet using a `diet_tag`:

- **Categories**: Forage, Concentrate, Milk, and Milk Replacer.
- **The 1.0 Rule**: These four must sum to **1.0** (e.g., 0.7 Forage +
  0.3 Concentrate).

#### 2. Micro Ingredients (`diet_ingredients.csv`)

List exactly what is inside each diet_tag.

- **The 100% Rule**: Ingredients inside each category (like “Forage”)
  must sum to **1.0**.
- **Example:** If your “Forage” macro is 0.70, your ingredient list for
  Forage (e.g., 0.5 Alfalfa + 0.5 Maize Silage) must sum to 1.0.
- **Source**: Choose names from the existing library
  (`feed_characteristics.csv`) or add your own.

#### 🟢 Adding a New Ingredient

If you add a new ingredient name, you **must** add it to these two
files:

1.  **`feed_characteristics.csv`**: Enter the nutritional data (%
    Protein, % Digestibility, etc.). This tells the model how it affects
    the **animal**.
2.  **`forage_yields.csv`**: If your ingredient is a forage (grass,
    silage, alfalfa), ensure a row exists for your target country with
    its productivity in kg DM/ha.
3.  **`fao_crop_yields.csv`**: This contains official FAOSTAT data for
    crops (grains, pulses). If more recent year is available you could
    download and substitute the one in the package by setting th exact
    name.
4.  **`mapping.csv`**: This is the “bridge”. You must add a row where:

- ingredient: Matches exactly the name used in your diet files.
- yield_name: Matches exactly the name used in the yield databases
  (`forage_yields.csv` or `fao_crop_yields.csv`).
- allocation : Set the factor (0 to 1) to define how much land impact is
  attributed to the feed based on economy.

*The Diet File:* Don’t forget that the new ingredient must also be
present in your diet_ingredients.csv. If you define an ingredient in the
databases but don’t add it to a diet, it won’t be calculated.

*Dry Matter (DM) Basis:* All yields in `forage_yields.csv` and
`fao_crop_yields.csv` must be expressed in Dry Matter, not fresh weight.
If you enter fresh weight, your land use requirements will be
significantly underestimated.

------------------------------------------------------------------------

### Phase 3: Animal Metabolism

This phase connects your animals to their diet and defines their
physical characteristics. The model uses this data to calculate energy
requirements (IPCC Tier 2).

#### 1. Defining Traits (`livestock_definitions.csv`)

For every `animal_tag` defined in your census, you must now assign its
“labels” and its diet.

- **`diet_tag`**: Link the animal to one of the diets you created in
  Phase 2.
- **IPCC Coefficients**: Assign the correct **description** (text) from
  the `ipcc_coefficients.csv` library. The exception occurs with
  sheep/goat where the c_pregnancy is calculated by pr and single/double
  birth fraction.
  - *How it works:* The model sees the description (e.g.,
    `cattle and buffalo`) and automatically fetches the mathematical
    constants ($C_{a}$, $C_{fi}$, etc.) needed for the energy equations.

#### 2. Body Metrics (`livestock_weights.csv`)

You must replicate the exact combinations of `animal_tag`, `region`,
`subregion`, and `class_flex` used in your census and assign their
weights.

\*\* **`adult_weight`**: The mature weight of the adult animal. \*\*
**`inital_weight`**: Live weight of the animal at the start of the
period (kg). \*\* **`final_weight`**: Live weight of the animal at the
end of the period (kg). \*\* **`productive_period`**: Duration of the
productive period (days).

#### 💡 How to Proceed

1.  **Tag Consistency**: Start by copying the `animal_tag`, `region`,
    `subregion`, and `class_flex` columns from your `census.csv`. They
    must be identical.
2.  **Add New Tags**: If you added custom animals in Phase 1, make sure
    they are included here too.
3.  **Assign Diets**: Decide which `diet_tag` applies to each category
    in `livestock_definitions.csv` .
4.  **Check the Library**: Open `ipcc_coefficients.csv`, find the
    description that best fits your animal, and copy that exact text
    into `livestock_definitions.csv`.

------------------------------------------------------------------------

### Phase 4: Manure Management (`manure_management.csv`)

This phase is dedicated to defining the fate of animal excreta. It is
the final link in the chain that connects animal metabolism to
environmental impact. By specifying how manure is handled, the model can
quantify the gases released during storage, treatment, and application.

#### 🎯 Purpose of this Phase

The goal is to characterize the management systems used for each
livestock group. This data allows the model to derive two primary
environmental impacts:

- **Methane ($CH_{4}$):** Driven by the anaerobic conditions of the
  storage (determined by system type, temperature, and duration).
- **Nitrous Oxide ($N_{2}O$):** Driven by the Nitrogen excretion
  ($N_{ex}$) calculated in Phase 3 and the specific emission factors of
  each system.

------------------------------------------------------------------------

#### 🛠️ How to Proceed

Follow these steps to fill your `manure_management.csv` file correctly:

1.  **Consult the Reference Guide:** Before entering data, review the
    **[Manure System
    Guide](https://juancbm99.github.io/herdr/articles/Manure.md)**. You
    must use the exact system names and variants listed there, as they
    are the only ones recognized by the IPCC reference database.
2.  **Sync the Tags:** Copy the `animal_tag`, `region`, `subregion`, and
    `class_flex` from your `livestock_census.csv`. They must match
    exactly.
3.  **Manage Multiple Systems (Row Duplication):** If a single animal
    group uses more than one system (e.g., 60% in “Pasture” and 40% in
    “Solid Storage”), you must **create two identical rows** for that
    group, changing only the system details and the allocation value.
4.  **Assign Systems and Climates:** For each row, select the
    appropriate `system_base`, `system_variant`, and `climate_zone`
    following the logic provided in the Manure System Guide.

------------------------------------------------------------------------

#### ⚖️ The Allocation Rule

The `allocation` column defines what percentage of the manure is handled
by a specific system.

- **The 1.0 Rule:** For every unique livestock group (same tag, region,
  and class), the sum of the `allocation` column must be exactly **1.0
  (100%)**.
- **Example:** If a group uses three different systems, you will have
  **three separate rows** for that group, and the total of their
  allocations (e.g., 0.5 + 0.3 + 0.2) must equal 1.0.

------------------------------------------------------------------------

### Phase 5: Generating Results

This is the final stage where the `herdr` engine processes all your
configuration files to generate a comprehensive environmental impact
report.

#### 🎯 Purpose of this Phase

The goal is to consolidate the data from populations, nutrition,
metabolism, and manure management into final emission and resource-use
metrics. The model will calculate:

- **Total Emissions**: $CH_{4}$ (Enteric & Manure) and $N_{2}O$ (Direct
  & Indirect).
- **Carbon Footprint**: Total $CO_{2}eq$ based on IPCC global warming
  potentials.
- **Land Use**: Total area required based on your nutritional strategy
  and crop yields.

------------------------------------------------------------------------

#### 🛠️ How to Proceed

1.  **Load the Library**: Ensure the package is active in your R
    session.
2.  **Run the Assessment**: Use the
    [`generate_impact_assessment()`](https://juancbm99.github.io/herdr/reference/generate_impact_assessment.md)
    function. This function automatically looks into your `user_data/`
    folder for all the CSV files you prepared in Phases 1 through 4.
3.  **Choose Your Mode**:
    - **`automatic_cycle = TRUE`**: Use this if you want the model to
      calculate offspring and herd dynamics based on your mature animals
      (Phase 1, Option B).
    - **`automatic_cycle = FALSE`**: Use this if you want the model to
      stick strictly to the population numbers you entered manually in
      `livestock_census.csv`.

------------------------------------------------------------------------

#### 💻 Execution Code

``` r
library(herdr)

# Execute the master function
# saveoutput = TRUE creates a CSV file with your results in the output folder
results <- generate_impact_assessment(
  automatic_cycle = FALSE, 
  saveoutput = TRUE
)

# Preview the results
head(results)
```
