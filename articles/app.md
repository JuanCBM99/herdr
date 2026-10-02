# Using the herdr App: A Step-by-Step Interactive Guide

## Introduction

The **`herdr` Shiny application** provides a visual environment for
modeling livestock greenhouse gas (GHG) emissions, energy requirements,
and agricultural land use.

Instead of manually modifying multiple CSV files in a text editor, this
interactive guide will walk you through how to set up, edit, and
evaluate your livestock system using the application’s interface.

------------------------------------------------------------------------

## 1. Initializing Your Workspace

Before building your farm scenario, you need to launch the application
and set up your baseline parameters.

- **Launch the application:** Open the tool via your R console by
  running
  [`herdr::run_herdr_app()`](https://juancbm99.github.io/herdr/reference/run_herdr_app.md).
  Alternatively, access the cloud-hosted version directly from your web
  browser at
  [juancbm99.shinyapps.io/herdr](https://juancbm99.shinyapps.io/herdr/).
- **Establish your baseline:** Navigate to the **Step 1: Data Source**
  panel on the sidebar. Choose to load a predefined package example,
  upload your existing CSV project files, or clear all data to build a
  new assessment entirely from scratch.
- **Configure farm parameters:** In the **Step 2: Configuration**
  section, specify the farm’s country location and the FAO reference
  year for trade data. Decide whether to check the **Use automatic herd
  cycle** box if you want the model to calculate replacement and
  offspring dynamics automatically. See the [Herd Demography &
  Population
  Dynamics](https://juancbm99.github.io/herdr/articles/Herd_Demography.md)
  guide for the complete decision matrix.

------------------------------------------------------------------------

## 2. Building Your Farm Scenario

Navigate left-to-right through the top data tabs. Follow this sequence
to accurately map your livestock system. *Note: The exact combination of
`animal_tag`, `region`, `subregion`, and `class_flex` acts as a unique
identifier for each cohort across all tables.*

- **Declare your herd (`Census`):** Create a new row for each distinct
  animal group.
  - Type a clear `animal_tag` (e.g., ‘dairy_cows’).
  - Optionally add region, subregion and/or class_flex for more specific
    definition of the group.
  - Enter the total head count in the `population` column. The model
    strictly requires this data to calculate aggregated environmental
    impacts.
- **Structure the diets (`Diet Profiles`):** Link your animal cohorts to
  a specific ration by typing a name in the `diet_tag` column (e.g.,
  ‘winter_ration’). Define the nutritional macro-composition by
  allocating percentages to forage, concentrate, and milk/milk
  replacers. **Ensure these fractions sum exactly to 100%**.
- **Specify feed items (`Diet Ingredients`):** Detail the exact crops or
  forages consumed by mapping them to your `diet_tag`.
  - Add a row for each ingredient (e.g., ‘Corn silage’) and specify its
    `ingredient_share` within its ingredient_type (e.g., 100% of the
    forage fraction).
  - Leave both the `custom_yield_kg_ha` and `country_of_origin` columns
    blank to let the model dynamically trace the feed using FAO trade
    data. To override the database, you can either input a specific
    nation in the `country_of_origin` column, or bypass FAO statistics
    entirely by entering an exact farm yield into the
    `custom_yield_kg_ha` column.
  - *Note on background databases:* The bilateral trade database
    (`fao_trade_matrix.parquet`, ~187 MB) is downloaded automatically
    from GitHub Releases the first time an assessment requires trade
    resolution – see [Technical
    Reference](https://juancbm99.github.io/herdr/articles/Technical_reference.html#fao_trade_matrix).
    Similarly, statutory crop yields (`fao_crops.parquet`, ~24 MB) are
    downloaded automatically to the central user cache when required,
    ensuring the core package download remains lightweight.
- **Input productive traits (`Definitions` & `Weights`):** Navigate to
  the ruminant/monogastric definitions and weights tabs to enter
  performance metrics for your cohorts.
  - **Weights:** You must provide starting, ending, and adult mature
    body weights, as well as `productive_period_days` (for adult
    breeding females except laying hens: the inter-parturition interval,
    e.g. 365 days; for laying hens: the commercial laying cycle
    duration; for all other cohorts: their life cycle / days on feed,
    e.g. 110 days for fattening pigs).
  - **Definitions:** Enter species-specific yields (such as milk
    kg/year, wool, or eggs) and select the appropriate IPCC coefficients
    from the dropdown menus to accurately calculate energy requirements.
- **Manage waste (`Manure Management`):** Specify how each cohort’s
  manure is stored and treated.
  - Use the cascading dropdowns to select valid combinations of
    `system_base` (e.g., ‘Liquid/Slurry’) and subsequent columns.
  - Distribute the fraction of manure across different systems. If a
    cohort spends half the year outdoors and half indoors, create two
    rows for the same animal group and set the `allocation` to 0.5 for
    each. **Ensure the total allocation for each animal group equals
    exactly 1.0**.

------------------------------------------------------------------------

## 3. Interactive Spreadsheet Features

While entering data, take advantage of the grid’s interactive features
to prevent errors:

- **Direct Editing:** Click any cell to modify its content, similar to
  Excel. Alternatively, click the **Add Record** button to use a guided
  form with dropdowns and explanatory tooltips.
- **Unsaved Indicators:** Watch for a small dot next to a tab title;
  this indicates unsaved edits are present in that table.
- **Validation Alerts:** Pay attention to real-time warning banners. Red
  banners indicate missing cohorts (an animal declared in the Census
  lacks trait or manure data), while yellow banners flag unrecognized
  tags that do not match your Census.

------------------------------------------------------------------------

## 4. Executing and Analyzing Calculations

Once your farm data is completely filled out and validated, proceed to
calculate the impacts:

- **Run the model:** Go to the **Step 3: Calculate** panel in the
  sidebar. Select either a complete impact assessment
  (`generate_impact_assessment`) or a specific physiological module
  (e.g., Land Use, Enteric CH4). Click the **Run Selected** button.
- **Analyze the output:** The interface will automatically switch to the
  **Results** tab once the calculation finishes:
  - **Dynamic Grouping:** Use the **Group Chart By** selector to
    aggregate visual bars by broad species category (`animal_type`),
    production subtype (`animal_subtype`), individual cohort
    (`animal_tag`), or administrative region (`region`, `subregion`,
    `class_flex`).
  - **Functional Unit Toggle:** When evaluating a full assessment,
    choose your preferred benchmarking metric from the **Functional Unit
    / Metric** dropdown:
    - *Total Emissions & Land:* Absolute inventory (Gg CO2e and ha).
    - *Per kg Edible Protein:* Nutritional efficiency (kg CO2e and m2
      per kg human edible protein).
    - *Per Commercial Product:* Commercial footprint per kg FPCM milk,
      carcass meat, or fresh eggs (applying IDF 2022 Net Energy physical
      allocation for dairy herds).
    - *Per Animal Head:* Annual impact per live head (kg CO2e and m2 per
      animal per year).
    - *Protein Feed Efficiency (Mottet FCR):* Dual-bar comparison of
      Total Biological Protein FCR vs Cropland Protein FCR (with dashed
      red 1.0 threshold line separating net food producers from
      competitors).
  - **Detailed Output Table:** Inspect the interactive preview table
    below the chart. In addition to GHG gas breakdowns, the table
    reports species-level feed protein efficiency and food security
    metrics (`feed_CP_total_kg`, `feed_CP_cropland_kg`,
    `Protein_FCR_total`, `Protein_FCR_cropland`) and complete
    agroecological land use partitioning (`Land_cropland_m2`,
    `Land_grassland_convertible_m2`, `Land_grassland_unconvertible_m2`,
    `Land_other_m2`). Click any column header to sort or select cells to
    copy.

------------------------------------------------------------------------

## 5. Exporting and Saving Your Progress

> ⚠️ **Important Session Note:** On the hosted web instance, all
> modifications reside temporarily in browser memory. Always download
> your files before closing or refreshing your browser tab to avoid
> losing your work.

- **Extract your results:** Use the download buttons in the results
  panel to save your generated charts as publication-ready PNG files and
  export the final numerical assessment as a standard CSV report.
- **Back up your session:** Navigate to **Step 4: Downloads** in the
  sidebar and click **Download Input Data (ZIP)**. This bundles all your
  modified CSV tables into a compressed file. To resume your work, unzip
  this file and upload the CSVs using the upload button in Step 1.

------------------------------------------------------------------------

## 6. Troubleshooting Guide

If you encounter issues during your workflow, consult this quick
reference:

| Issue / Alert | Root Cause | Solution |
|:---|:---|:---|
| 🔴 **Red Banner: Missing Cohorts** | An `animal_tag` in `Census` is absent from definitions, weights, or manure tables. | Add a matching row with identical `animal_tag`, `region`, `subregion`, and `class_flex` keys in the required tab. |
| 🟡 **Yellow Banner: Unrecognized Cohorts** | A row in a definition table has no corresponding entry in `Census`. | Correct spelling typos in the tag or add the cohort to the Census table. |
| **Sum of Shares / Allocations Error** | Diet proportions do not sum to 100%, or manure allocations do not sum to 1.0. | Adjust the percentage values so that their sum equals exactly 100% (diets) or 1.0 (manure). |
| **Ignored CSV Upload** | The uploaded file has a modified filename. | Ensure filenames inside your ZIP match standard templates (e.g., `livestock_census.csv`). |
