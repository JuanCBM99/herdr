# Package index

## Project Setup & Interactive App

Initialize a standardized project directory and launch the Shiny GUI.

- [`herdr_init()`](https://juancbm99.github.io/herdr/reference/herdr_init.md)
  : Initialize herdr project folders and example data
- [`run_herdr_app()`](https://juancbm99.github.io/herdr/reference/run_herdr_app.md)
  [`run_app()`](https://juancbm99.github.io/herdr/reference/run_herdr_app.md)
  : Launch the herdr Interactive Application

## Integrated Assessment & Visualization

End-to-end cradle-to-farm-gate environmental impact assessment and
publication-grade plots.

- [`generate_impact_assessment()`](https://juancbm99.github.io/herdr/reference/generate_impact_assessment.md)
  : Summarize CH4, N2O Emissions, Land Use, and Farm Product Intensities
- [`plot_herdr_results()`](https://juancbm99.github.io/herdr/reference/plot_herdr_results.md)
  : Plot herdr results dynamically with custom grouping and premium
  aesthetics

## Herd Demography & Production

Average Annual Population (AAP), automatic life-cycle modeling, and
edible animal outputs.

- [`calculate_population()`](https://juancbm99.github.io/herdr/reference/calculate_population.md)
  : Calculate Total Animal Population
- [`calculate_population_cattle()`](https://juancbm99.github.io/herdr/reference/calculate_population_cattle.md)
  : Calculate Cattle Population Structure
- [`calculate_population_small_ruminants()`](https://juancbm99.github.io/herdr/reference/calculate_population_small_ruminants.md)
  : Calculate Small Ruminant Population Structure (Sheep and Goats)
- [`calculate_population_swine()`](https://juancbm99.github.io/herdr/reference/calculate_population_swine.md)
  : Calculate Swine Population Structure
- [`calculate_population_poultry()`](https://juancbm99.github.io/herdr/reference/calculate_population_poultry.md)
  : Calculate Poultry Population Structure
- [`calculate_production()`](https://juancbm99.github.io/herdr/reference/calculate_production.md)
  : Calculate Total Livestock Production

## Nutrition, Energy Balance & Intake

Weighted diet composition, IPCC Tier 2 Net/Gross Energy, FEDNA
monogastric ME, and Dry Matter Intake.

- [`calculate_weighted_variable()`](https://juancbm99.github.io/herdr/reference/calculate_weighted_variable.md)
  : Calculate Weighted Nutritional Variables
- [`calculate_NEm()`](https://juancbm99.github.io/herdr/reference/calculate_NEm.md)
  : Calculate Net Energy for Maintenance (NEm)
- [`calculate_NEa()`](https://juancbm99.github.io/herdr/reference/calculate_NEa.md)
  : Calculate Net Energy for Activity (NEa)
- [`calculate_NEg()`](https://juancbm99.github.io/herdr/reference/calculate_NEg.md)
  : Calculate Net Energy for Growth (NEg)
- [`calculate_NEl()`](https://juancbm99.github.io/herdr/reference/calculate_NEl.md)
  : Calculate Net Energy for Lactation (NEl)
- [`calculate_NE_work()`](https://juancbm99.github.io/herdr/reference/calculate_NE_work.md)
  : Calculate Net Energy for Work (NE_work)
- [`calculate_NE_wool()`](https://juancbm99.github.io/herdr/reference/calculate_NE_wool.md)
  : Calculate Net Energy for Wool (NE_wool)
- [`calculate_NE_pregnancy()`](https://juancbm99.github.io/herdr/reference/calculate_NE_pregnancy.md)
  : Calculate Net Energy for Pregnancy (NE_pregnancy)
- [`calculate_ge()`](https://juancbm99.github.io/herdr/reference/calculate_ge.md)
  : Calculate Gross Energy (GE)
- [`calculate_monogastric_energy()`](https://juancbm99.github.io/herdr/reference/calculate_monogastric_energy.md)
  : Calculate Monogastric Metabolizable Energy Requirements (FEDNA)
- [`calculate_DMI()`](https://juancbm99.github.io/herdr/reference/calculate_DMI.md)
  : Calculate Dry Matter Intake (DMI)

## GHG Emissions, Land Use & Feed LCA

Enteric CH4, manure CH4 and N2O, agroecological land footprint, and
ECOALIM/AWARE feed footprint.

- [`calculate_emissions_enteric()`](https://juancbm99.github.io/herdr/reference/calculate_emissions_enteric.md)
  : Calculate methane emissions from enteric fermentation
- [`calculate_vs()`](https://juancbm99.github.io/herdr/reference/calculate_vs.md)
  : Calculate Volatile Solids (VS) for Animals
- [`calculate_CH4_manure()`](https://juancbm99.github.io/herdr/reference/calculate_CH4_manure.md)
  : Calculate CH4 Emissions from Manure Management
- [`calculate_N2O_direct_manure()`](https://juancbm99.github.io/herdr/reference/calculate_N2O_direct_manure.md)
  : Calculate direct N2O emissions from manure
- [`calculate_N2O_indirect_volatilization()`](https://juancbm99.github.io/herdr/reference/calculate_N2O_indirect_volatilization.md)
  : Calculate indirect N2O emissions from volatilization
- [`calculate_N2O_indirect_leaching()`](https://juancbm99.github.io/herdr/reference/calculate_N2O_indirect_leaching.md)
  : Calculate indirect N2O emissions from leaching
- [`calculate_land_use()`](https://juancbm99.github.io/herdr/reference/calculate_land_use.md)
  : Calculate land use
- [`calculate_feed_emissions()`](https://juancbm99.github.io/herdr/reference/calculate_feed_emissions.md)
  : Calculate Feed Environmental Footprint (GHG Emissions and Water Use)

## Reference Data & Cache Utilities

Manage local caches and locate bundled or downloaded FAO/ECOALIM
reference datasets.

- [`herdr_cache_dir()`](https://juancbm99.github.io/herdr/reference/herdr_cache_dir.md)
  : Get herdr user data cache directory
- [`herdr_get_fao_crops_path()`](https://juancbm99.github.io/herdr/reference/herdr_get_fao_crops_path.md)
  : Locate or download the FAO crops dataset (fao_crops.parquet)
- [`herdr_get_fao_trade_matrix_path()`](https://juancbm99.github.io/herdr/reference/herdr_get_fao_trade_matrix_path.md)
  : Locate or download the FAO trade matrix dataset
  (fao_trade_matrix.parquet)
- [`herdr_get_forages_path()`](https://juancbm99.github.io/herdr/reference/herdr_get_forages_path.md)
  : Locate the forages dataset (forages.parquet)
- [`herdr_get_ecoalim_path()`](https://juancbm99.github.io/herdr/reference/herdr_get_ecoalim_path.md)
  : Locate the ECOALIM feed LCA dataset (ecoalim.parquet)
