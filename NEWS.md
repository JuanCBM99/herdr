# herdr 1.0.4

## New Features
- **Consolidated Impact Assessment & Functional Units (`generate_impact_assessment`)**:
  - Implemented 5 standardized functional units across modeling and graphics (`plot_herdr_results`): `total` (absolute inventory in Gg CO2e and ha), `protein` (intensities per kg edible human protein), `product` (intensities per kg commercial product), `head` (per animal head per year), and `fcr` (Protein Feed Conversion Ratio and human food competition).
  - Adopted the updated **IDF Bulletin 520/2022** physical Net Energy allocation method for dairy systems (3.1 MJ/kg FPCM milk vs. 15.0 MJ/kg live weight gain) to partition impacts and feed intake between milk and surplus meat co-products.
  - Implemented the **Mottet et al. (2017)** Protein Feed Efficiency and Human Food Security framework: replaced legacy dry-matter FCRs with `feed_CP_total_kg`, `feed_CP_cropland_kg`, `Protein_FCR_total` (biological nitrogen efficiency), and `Protein_FCR_cropland` (cropland human food competition ratio).
  - Enhanced land use reporting with complete agroecological partitioning: `Land_cropland_m2`, `Land_grassland_convertible_m2`, `Land_grassland_unconvertible_m2`, and `Land_other_m2`.

- **Enhanced Interactive Shiny Application**:
  - Added dynamic grouping selector supporting species (`animal_type`) and production subtype (`animal_subtype`) alongside cohort and regional keys.
  - Added interactive Functional Unit dropdown in the Results tab to toggle dynamically between absolute footprint, intensities, and Mottet Protein FCR (with a red dashed reference line at 1.0).
  - Added dynamic conditional dropdown to select dedicated commercial products (milk, meat, wool, egg) when evaluating per commercial product, removing redundant generic intensity columns.
  - Interactive preview table with sorted column previews and CSV/PNG download handlers.
- **User-Defined Milk Protein Percentage (`protein_content_pct`)**: Added support for entering explicit milk crude protein percentage in `ruminant_definitions.csv` alongside `fat_content_pct` (particularly relevant for dairy sheep and goats where fat:protein ratios diverge from cattle), while maintaining automatic fallback to IPCC fat regression when omitted or set to 0.

## Bug Fixes and Improvements
- **Dry Phase Meat Allocation Fix**: Corrected non-milking dairy cohorts (`is_non_milking_dairy`, e.g. `dry_phase` where `milk_FPCM_kg == 0`), setting `AF_meat = 0`, `AF_milk = 0`, and intensities to `NA` (rendered as empty bars in charts) to avoid 100% allocation of dry-period emissions to cull meat.
- Resolved factor level duplication error in `plot_herdr_results()` when grouping solely by species (`animal_type`).
- Decoupled dairy cohort detection in aggregation routines to support arbitrary demographic grouping combinations without losing co-product allocation rules.
- Cleaned up forage data lookup in `R/fao_data.R`, deprecating legacy `fao_forages.parquet` in favor of standard `forages.parquet`.
- Fully registered package global variables to ensure 0 notes and clean R CMD check execution.
- Comprehensive test suite expansion to 359 unit tests with 100% pass rate.
- Reorganized and updated all documentation vignettes (`Introduction.Rmd`, `Manure.Rmd`, `Technical_reference.Rmd`, `Theoretical_basis.Rmd`, `Workflow.Rmd`, `app.Rmd`, `land_use.Rmd`) and modernized `_pkgdown.yml`.

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

