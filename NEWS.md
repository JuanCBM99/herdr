# herdr 1.0.4

- **Upstream Feed Environmental Footprints & Water Deprivation (`calculate_feed_emissions`)**:
  - Integrated the bundled **INRAE / ADEME ECOALIM v9** and **AGRIBALYSE v3.1** life cycle inventory database (`ecoalim.parquet`) to quantify cradle-to-farm-gate Scope 3 feed production burdens.
  - Implemented the international **AWARE** (*Available WAter REmaining*, Boulay et al., 2018) characterization model adopted under the European Commission's **Environmental Footprint (EF 3.1)** standard for freshwater deprivation.
  - Standardized water units across all functional scales: absolute herd water footprints in $\text{Mm}^3\text{ depriv.}$ ($10^6\text{ m}^3\text{ depriv.} = 1\text{ hm}^3\text{ depriv.}$), per animal head in $\text{m}^3\text{ depriv.} / \text{head}$, per commercial product in $\text{m}^3\text{ depriv.} / \text{kg product}$, and per edible protein in $\text{m}^3\text{ depriv.} / \text{kg protein}$.
  - Partitioned feed carbon footprints into fossil inputs ($\text{CO}_2\text{e}_{\text{fossil}}$) and land-use change / deforestation ($\text{CO}_2\text{e}_{\text{luc}}$).
  - Added dedicated comprehensive methodology vignette: `Feed Environmental Footprint & Water Deprivation (AWARE / EF 3.1)` (`vignettes/Water_and_Feed_Footprint.Rmd`).
- **Consolidated Impact Assessment & Functional Units (`generate_impact_assessment`)**:
  - Implemented 5 standardized functional units across modeling and graphics (`plot_herdr_results`): `total` (absolute inventory in Gg CO2e, ha, and Mm3 depriv.), `protein` (intensities per kg edible human protein), `product` (intensities per kg commercial product), `head` (per animal head per year), and `fcr` (Protein Feed Conversion Ratio and human food competition).
  - Adopted the updated **IDF Bulletin 520/2022** physical Net Energy allocation method for dairy systems (3.1 MJ/kg FPCM milk vs. 15.0 MJ/kg live weight gain) to partition impacts and feed intake between milk and surplus meat co-products.
  - Implemented the **Mottet et al. (2017)** Protein Feed Efficiency and Human Food Security framework: replaced legacy dry-matter FCRs with `feed_CP_total_kg`, `feed_CP_cropland_kg`, `Protein_FCR_total` (biological nitrogen efficiency), and `Protein_FCR_cropland` (cropland human food competition ratio).
  - Enhanced land use reporting with complete agroecological partitioning: `Land_cropland_m2`, `Land_grassland_convertible_m2`, `Land_grassland_unconvertible_m2`, and `Land_other_m2`.

- **Enhanced Interactive Shiny Application**:
  - Added dynamic grouping selector supporting species (`animal_type`) and production subtype (`animal_subtype`) alongside cohort and regional keys.
  - Symmetrical 2-level impact selection: primary choice of Impact Category (Carbon GHG, Land Use, Water Deprivation, Overview, Mottet FCR) followed by Functional Unit and Commercial Product subselector applied uniformly across Carbon, Land, and Water.
  - Interactive preview table with sorted column previews and CSV/PNG download handlers.
- **User-Defined Milk Protein Percentage (`protein_content_pct`)**: Added support for entering explicit milk crude protein percentage in `ruminant_definitions.csv` alongside `fat_content_pct` (particularly relevant for dairy sheep and goats where fat:protein ratios diverge from cattle), while maintaining automatic fallback to IPCC fat regression when omitted or set to 0.

## Bug Fixes and Improvements
- **Consolidation of `replacement_rate`**: Streamlined user input definitions by integrating `replacement_rate` directly as a parameter column in `ruminant_definitions.csv` and `monogastric_definitions.csv`, completely deprecating the separate `reproduction_parameters.csv` file across all demographic modeling functions, vignettes, example files, and the Shiny application data dictionary.
- **Dairy Feedlot Meat Accounting**: Refined `is_non_milking_dairy` detection in `R/14_summary.R` and `R/plots.R` to check `(animal_subtype == "dairy") & (milk_kg == 0) & (meat_kg == 0)`. Cohorts born from dairy herds that yield meat (e.g. `feedlot_calves_male`, `feedlot_calves_female`) are no longer excluded as non-milking phases, ensuring their meat yields, greenhouse gas and land use intensities, and Mottet FCR are fully accounted for and displayed in summary outputs and plots.
- **Dry Phase Meat Allocation**: True non-milking, non-meat phases (e.g. `dry_phase` where both milk and meat are 0) have `AF_meat = 0`, `AF_milk = 0`, and intensities set to `NA` (rendered as empty bars in charts) to avoid 100% allocation of dry-period emissions to cull meat.
- Resolved factor level duplication error in `plot_herdr_results()` when grouping solely by species (`animal_type`).
- Decoupled dairy cohort detection in aggregation routines to support arbitrary demographic grouping combinations without losing co-product allocation rules.
- Cleaned up forage data lookup in `R/fao_data.R`, deprecating legacy `fao_forages.parquet` in favor of standard `forages.parquet`.
- Fully registered package global variables to ensure 0 notes and clean R CMD check execution.
- Comprehensive test suite expansion to 356 unit tests with 100% pass rate.
- Reorganized and updated all documentation vignettes (`Introduction.Rmd`, `Herd_Demography.Rmd`, `Manure.Rmd`, `Technical_reference.Rmd`, `Theoretical_basis.Rmd`, `Workflow.Rmd`, `app.Rmd`, `land_use.Rmd`) and modernized `_pkgdown.yml`.


# herdr 1.0.3

## New Features
- Full integration of monogastric animals (swine and poultry) using FEDNA energy requirement equations.
- Automatic herd demographic modeling for swine (breeding sows, replacement, fattening) and poultry (broilers, layer hens, breeders).
- Tier 1 enteric fermentation methane calculation with liveweight scaling for swine.
- Land use allocation module with FAO trade matrix tracing and self-sufficiency ratio (SSR) thresholds.
- Interactive Shiny graphical user interface with cascading dropdowns, dynamic validation, and automated report generation.

## Bug Fixes and Improvements
- Isolated test environments and anti-download shields for offline execution.
- Scaled small ruminant nitrogen retention to intake according to GLEAM/IPCC standards.
- Refined input validation for manure management combinations and diet profiles.

