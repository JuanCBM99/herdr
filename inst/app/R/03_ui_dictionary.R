# ==============================================================================
# herdr — Data Dictionary & Decision Matrix UI Component
# ==============================================================================

build_dictionary_panel <- function() {
  bslib::nav_panel(
    title = tagList(icon("book"), "Data Dictionary"),
    value = "dictionary_tab",
    div(
      class = "p-4",
      div(
        class = "row",
        div(
          class = "col-lg-10 mx-auto",
          h3("Data Dictionary & Decision Matrix", class = "mb-2", style = "font-family: 'Fraunces', serif; font-weight: 900;"),
          p(class = "text-muted mb-3", "A comprehensive guide to variables, definitions, and CSV file structures used across herdr, organized sequentially by table."),

          # --- QUICK JUMP NAVIGATION BAR ---
          div(
            class = "d-flex flex-wrap gap-2 mb-4 p-2 rounded",
            style = "background: var(--herdr-card); border: 1px solid var(--herdr-border); font-size: 0.85rem;",
            tags$span(class = "text-muted align-self-center me-1 fw-bold", icon("arrow-down"), " Jump to:"),
            tags$a(href = "#dict-census", class = "badge bg-secondary text-decoration-none py-2 px-2", "1. Census"),
            tags$a(href = "#dict-diet_prof", class = "badge bg-secondary text-decoration-none py-2 px-2", "2. Diet Profiles"),
            tags$a(href = "#dict-diet_ingr", class = "badge bg-secondary text-decoration-none py-2 px-2", "3. Ingredients"),
            tags$a(href = "#dict-def", class = "badge bg-secondary text-decoration-none py-2 px-2", "4. Ruminants"),
            tags$a(href = "#dict-mono", class = "badge bg-secondary text-decoration-none py-2 px-2", "5. Monogastrics"),
            tags$a(href = "#dict-weights", class = "badge bg-secondary text-decoration-none py-2 px-2", "6. Weights"),
            tags$a(href = "#dict-manure", class = "badge bg-secondary text-decoration-none py-2 px-2", "7. Manure"),
            tags$a(href = "#dict-repro", class = "badge bg-secondary text-decoration-none py-2 px-2", "8. Reproduction"),
            tags$a(href = "#dict-feed_char", class = "badge bg-secondary text-decoration-none py-2 px-2", "9. Feed Characteristics"),
            tags$a(href = "#dict-ipcc_coef", class = "badge bg-secondary text-decoration-none py-2 px-2", "10. IPCC Coefficients"),
            tags$a(href = "#dict-ipcc_mm", class = "badge bg-secondary text-decoration-none py-2 px-2", "11. IPCC Manure Mgt."),
            tags$a(href = "#dict-mapping", class = "badge bg-secondary text-decoration-none py-2 px-2", "12. Mapping")
          ),

          div(
            style = "background: var(--herdr-card); border-radius: 12px; border: 1px solid var(--herdr-border); padding: 1.5rem; max-height: calc(100vh - 270px); overflow-y: auto;",

            # --- QUICK DECISION MATRIX ALERT ---
            div(
              class = "alert alert-info mb-4 shadow-sm",
              h5(icon("compass"), " 5-Minute Farm Decision Matrix", class = "alert-heading fw-bold mb-2"),
              p(class = "mb-2 small", "Not sure what to enter for your farm? Use this quick reference guide to configure herd cohorts, cycles, and turnover rates:"),
              tags$div(
                class = "table-responsive",
                tags$table(
                  class = "table table-sm table-bordered bg-white text-dark small mb-0",
                  tags$thead(class = "table-light",
                    tags$tr(
                      tags$th("Farm Type"),
                      tags$th("Census Entry (population)"),
                      tags$th("productive_period_days"),
                      tags$th("replacement_rate")
                    )
                  ),
                  tags$tbody(
                    tags$tr(
                      tags$td(tags$strong("Broilers (Meat Poultry)")),
                      tags$td("Average barn capacity (e.g. 20,000)"),
                      tags$td("42 days (fattening duration)"),
                      tags$td("0 (None)")
                    ),
                    tags$tr(
                      tags$td(tags$strong("Layer Hens (Eggs)")),
                      tags$td("Laying flock size (e.g. 10,000)"),
                      tags$td("511 days (laying period)"),
                      tags$td("Auto (365/511 = 71.4%)")
                    ),
                    tags$tr(
                      tags$td(tags$strong("Breeding Sows (Farrow-to-finish)")),
                      tags$td("Breeding sows (e.g. 500)"),
                      tags$td("148.9 days (farrowing interval)"),
                      tags$td("0.25 (25% annual)")
                    ),
                    tags$tr(
                      tags$td(tags$strong("Fattening Pigs (Cebo)")),
                      tags$td("Barn places (e.g. 2,000)"),
                      tags$td("110 days (fattening duration)"),
                      tags$td("0 (None)")
                    ),
                    tags$tr(
                      tags$td(tags$strong("Dairy Cattle")),
                      tags$td("Mature cows (e.g. 100)"),
                      tags$td("365 days"),
                      tags$td("0.27 (27% annual)")
                    ),
                    tags$tr(
                      tags$td(tags$strong("Beef Cattle")),
                      tags$td("Mature cows (e.g. 100)"),
                      tags$td("365 days"),
                      tags$td("0.15 (15% annual)")
                    ),
                    tags$tr(
                      tags$td(tags$strong("Meat / Dairy Sheep")),
                      tags$td("Adult ewes (e.g. 1,000)"),
                      tags$td("365 days (lambing interval 200 d)"),
                      tags$td("0.20 (20% annual)")
                    ),
                    tags$tr(
                      tags$td(tags$strong("Dairy / Meat Goats")),
                      tags$td("Adult does (e.g. 500)"),
                      tags$td("365 days (kidding interval 200 d)"),
                      tags$td("0.20 (20% annual)")
                    ),
                    tags$tr(
                      tags$td(tags$strong("Breeder Meat Hens")),
                      tags$td("Breeding flock (e.g. 10,000)"),
                      tags$td("301 days (breeding cycle)"),
                      tags$td("Auto (365/301 = 121.3%)")
                    )
                  )
                )
              )
            ),

            # ==================================================================
            # 1. CENSUS (livestock_census.csv)
            # ==================================================================
            div(
              id = "dict-census",
              class = "mb-4",
              h5(tagList(icon("clipboard-list"), " 1. Census (livestock_census.csv)"), class = "fw-bold text-success border-bottom pb-2"),
              p(class = "text-muted small mb-3", "Defines the animal cohorts present on the farm and their population counts. Cohorts defined here form the baseline for all subsequent tables."),
              tags$dl(
                class = "row small",
                tags$dt(class = "col-sm-3 text-monospace", tags$code("animal_tag")),
                tags$dd(class = "col-sm-9", "Mandatory: A unique name for this specific cohort/group of animals (e.g., 'mature_dairy_cattle', 'fattening_pigs')."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("region")),
                tags$dd(class = "col-sm-9", "Optional: Country or broad geographic grouping (e.g., 'spain', 'europe'). Used for climatic and crop yield lookups."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("subregion")),
                tags$dd(class = "col-sm-9", "Optional: Province, region, or specific farm unit (e.g., 'euskadi', 'farm_north')."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("class_flex")),
                tags$dd(class = "col-sm-9", "Optional: Flexible category tag to differentiate production stages or breeds (e.g., 'lactating', 'dry', 'angus')."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("population")),
                tags$dd(class = "col-sm-9", "Mandatory: Number of animals (head count or standing barn capacity) in this cohort.")
              )
            ),

            # ==================================================================
            # 2. DIET PROFILES (diet_profiles.csv)
            # ==================================================================
            div(
              id = "dict-diet_prof",
              class = "mb-4",
              h5(tagList(icon("utensils"), " 2. Diet Profiles (diet_profiles.csv)"), class = "fw-bold text-success border-bottom pb-2"),
              p(class = "text-muted small mb-3", "Specifies the macro nutritional proportion of each diet type (forage, concentrate, milk, milk replacer). Shares must sum to 100%."),
              tags$dl(
                class = "row small",
                tags$dt(class = "col-sm-3 text-monospace", tags$code("diet_tag")),
                tags$dd(class = "col-sm-9", "Mandatory: Identifier for the diet profile (e.g., 'diet_dairy_mature', 'diet_broiler_grower'). Links animals to their feed."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("forage_share")),
                tags$dd(class = "col-sm-9", "Percentage of the diet made up of roughage/forages (pasture, silage, hay) on a dry matter basis (0–100%)."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("concentrate_share")),
                tags$dd(class = "col-sm-9", "Percentage of the diet made up of concentrates (grains, protein meals, compound feed) on a dry matter basis (0–100%)."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("milk_share")),
                tags$dd(class = "col-sm-9", "Percentage of the diet consisting of natural maternal milk (young nursing animals) (0–100%)."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("milk_replacer_share")),
                tags$dd(class = "col-sm-9", "Percentage of the diet consisting of artificial milk replacer formula (0–100%).")
              )
            ),

            # ==================================================================
            # 3. INGREDIENTS (diet_ingredients.csv)
            # ==================================================================
            div(
              id = "dict-diet_ingr",
              class = "mb-4",
              h5(tagList(icon("wheat-awn"), " 3. Ingredients (diet_ingredients.csv)"), class = "fw-bold text-success border-bottom pb-2"),
              p(class = "text-muted small mb-3", "Specifies the detailed feed items that make up each diet. Select the ingredient type first to filter matching feed items."),
              tags$dl(
                class = "row small",
                tags$dt(class = "col-sm-3 text-monospace", tags$code("diet_tag")),
                tags$dd(class = "col-sm-9", "Mandatory: Identifier linking to Diet Profiles."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("ingredient_type")),
                tags$dd(class = "col-sm-9", "Category of the ingredient: 'forage', 'concentrate', 'milk', or 'milk_replacer'. Filters available ingredients."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("ingredient")),
                tags$dd(class = "col-sm-9", "Specific feed ingredient from the feed characteristics database (e.g., 'corn_national', 'soybean_meal_44_cp')."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("ingredient_share")),
                tags$dd(class = "col-sm-9", "Percentage of this specific ingredient within its ingredient_type (shares for each type must sum to 100%)."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("country_of_origin")),
                tags$dd(class = "col-sm-9", "Optional: Country where this feed was produced. Used for international trade matrix and deforestation footprints."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("custom_yield_kg_ha")),
                tags$dd(class = "col-sm-9", "Optional: On-farm or local crop yield in kg fresh matter per hectare. If left blank, FAO national averages are used.")
              )
            ),

            # ==================================================================
            # 4. RUMINANTS (ruminant_definitions.csv)
            # ==================================================================
            div(
              id = "dict-def",
              class = "mb-4",
              h5(tagList(icon("id-card"), " 4. Ruminants (ruminant_definitions.csv)"), class = "fw-bold text-primary border-bottom pb-2"),
              p(class = "text-muted small mb-3", "Biological, production, and IPCC Tier 2 parameters for cattle, sheep, and goat cohorts."),
              tags$dl(
                class = "row small",
                tags$dt(class = "col-sm-3 text-monospace", tags$code("animal_tag")),
                tags$dd(class = "col-sm-9", "Mandatory: Unique cohort identifier matching Census."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("region")),
                tags$dd(class = "col-sm-9", "Cohort geographic region matching Census."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("subregion")),
                tags$dd(class = "col-sm-9", "Cohort subregion matching Census."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("class_flex")),
                tags$dd(class = "col-sm-9", "Cohort class_flex matching Census."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("animal_type")),
                tags$dd(class = "col-sm-9", "Broad species: 'cattle', 'sheep', or 'goat'."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("animal_subtype")),
                tags$dd(class = "col-sm-9", "Production purpose: 'dairy', 'beef', or 'meat'."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("production_role")),
                tags$dd(class = "col-sm-9", "Role in production cycle: 'mature' (breeding/milking adults), 'replacement' (growing breeding stock), or 'slaughter' (growing animals for meat)."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("diet_tag")),
                tags$dd(class = "col-sm-9", "Assigned diet profile linking to Diet Profiles."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("cfi")),
                tags$dd(class = "col-sm-9", "IPCC maintenance coefficient descriptor (e.g., 'cattle/buffalo [lactating cows]')."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("ca")),
                tags$dd(class = "col-sm-9", "IPCC feeding activity coefficient descriptor ('stall', 'pasture', 'grazing large areas')."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("c")),
                tags$dd(class = "col-sm-9", "IPCC constant 'C' for cattle growth energy ('females', 'castrates', 'bulls')."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("a")),
                tags$dd(class = "col-sm-9", "IPCC growth constant 'a' for sheep/goats."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("b")),
                tags$dd(class = "col-sm-9", "IPCC growth constant 'b' for sheep/goats."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("milk_yield_kg_year")),
                tags$dd(class = "col-sm-9", "Annual milk yield per head (kg/head/year). Leave 0 for non-milking cohorts."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("fat_content_pct")),
                tags$dd(class = "col-sm-9", "Milk fat content percentage (e.g., 3.7 for 3.7%)."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("wool_yield_kg_year")),
                tags$dd(class = "col-sm-9", "Annual wool production per head (kg/head/year) for sheep."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("work_hours")),
                tags$dd(class = "col-sm-9", "Draft animal work hours per day (typically 0 for modern European systems)."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("pr_sheep_goat")),
                tags$dd(class = "col-sm-9", "Prolificacy rate for sheep and goats: lambs/kids born divided by pregnant ewes/does."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("c_pregnancy_cattle")),
                tags$dd(class = "col-sm-9", "IPCC pregnancy coefficient descriptor for cattle."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("pregnancy_rate")),
                tags$dd(class = "col-sm-9", "Proportion of mature females pregnant in a year (bounded between 0 and 1).")
              )
            ),

            # ==================================================================
            # 5. MONOGASTRICS (monogastric_definitions.csv)
            # ==================================================================
            div(
              id = "dict-mono",
              class = "mb-4",
              h5(tagList(icon("drumstick-bite"), " 5. Monogastrics (monogastric_definitions.csv)"), class = "fw-bold", style = "color: #d97706; border-bottom: 1px solid var(--herdr-border); padding-bottom: 0.5rem;"),
              p(class = "text-muted small mb-3", "Biological, production, and energetic parameters for swine and poultry cohorts."),
              tags$dl(
                class = "row small",
                tags$dt(class = "col-sm-3 text-monospace", tags$code("animal_tag")),
                tags$dd(class = "col-sm-9", "Mandatory: Unique cohort identifier matching Census."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("region")),
                tags$dd(class = "col-sm-9", "Cohort geographic region matching Census."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("subregion")),
                tags$dd(class = "col-sm-9", "Cohort subregion matching Census."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("class_flex")),
                tags$dd(class = "col-sm-9", "Cohort class_flex matching Census."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("animal_type")),
                tags$dd(class = "col-sm-9", "Species category: 'swine' or 'poultry'."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("animal_subtype")),
                tags$dd(class = "col-sm-9", "Production category: 'breeder', 'fattening', 'layer', 'meat', or 'replacement'."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("production_role")),
                tags$dd(class = "col-sm-9", "Role in production cycle: 'mature' (breeding sows / layer hens), 'replacement' (gilts / pullets), or 'slaughter' (fattening pigs / broilers)."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("diet_tag")),
                tags$dd(class = "col-sm-9", "Assigned diet profile linking to Diet Profiles."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("cfi_maintenance")),
                tags$dd(class = "col-sm-9", "Maintenance energy coefficient in kcal/kg body weight^alpha per day."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("frac_fat_pct")),
                tags$dd(class = "col-sm-9", "Fat fraction in daily body weight gain (percentage, 0–100%)."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("frac_protein_pct")),
                tags$dd(class = "col-sm-9", "Protein fraction in daily body weight gain (percentage, 0–100%)."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("egg_mass_g_day")),
                tags$dd(class = "col-sm-9", "Daily egg mass produced per hen (grams/day) for layer hens."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("egg_weight_g")),
                tags$dd(class = "col-sm-9", "Average individual egg weight in grams (e.g., 60 to 64 g)."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("fertility_rate")),
                tags$dd(class = "col-sm-9", "Fertility and hatchability rate (0 to 1) for poultry breeding flocks."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("alpha")),
                tags$dd(class = "col-sm-9", "Metabolic body weight exponent (typically 0.75 for swine/poultry)."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("piglets_born")),
                tags$dd(class = "col-sm-9", "Average number of live piglets born per litter (for breeding sows)."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("piglets_suckling")),
                tags$dd(class = "col-sm-9", "Average number of piglets actively suckled per sow during lactation.")
              )
            ),

            # ==================================================================
            # 6. WEIGHTS (livestock_weights.csv)
            # ==================================================================
            div(
              id = "dict-weights",
              class = "mb-4",
              h5(tagList(icon("weight-hanging"), " 6. Weights (livestock_weights.csv)"), class = "fw-bold text-primary border-bottom pb-2"),
              p(class = "text-muted small mb-3", "Live body weights, growth trajectory, and cycle duration for all animal categories."),
              tags$dl(
                class = "row small",
                tags$dt(class = "col-sm-3 text-monospace", tags$code("animal_tag")),
                tags$dd(class = "col-sm-9", "Cohort identifier matching Census."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("region, subregion, class_flex")),
                tags$dd(class = "col-sm-9", "Cohort identity matching Census."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("adult_weight_kg")),
                tags$dd(class = "col-sm-9", "Average live body weight of a fully grown mature animal (in kg)."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("productive_period_days")),
                tags$dd(class = "col-sm-9", "Production cycle duration in days: fattening period for meat animals, farrowing interval for sows, laying duration for hens, or 365 for adults."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("initial_weight_kg")),
                tags$dd(class = "col-sm-9", "Starting live weight at the beginning of the evaluated production period (in kg)."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("final_weight_kg")),
                tags$dd(class = "col-sm-9", "Ending live weight at the end of the evaluated production period / slaughter (in kg)."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("sows_gestation_days")),
                tags$dd(class = "col-sm-9", "Gestation length in breeding sows (typically 115 days)."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("sows_lactation_days")),
                tags$dd(class = "col-sm-9", "Lactation/nursing duration in breeding sows (typically 21–28 days)."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("piglet_birth_weight_kg")),
                tags$dd(class = "col-sm-9", "Individual piglet weight at birth (typically 1.3–1.5 kg)."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("piglet_weaning_weight_kg")),
                tags$dd(class = "col-sm-9", "Individual piglet weight at weaning (typically 6–7 kg)."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("sow_reserve_gain_kg")),
                tags$dd(class = "col-sm-9", "Body reserve recovery weight gained by the mother sow between cycles (kg).")
              )
            ),

            # ==================================================================
            # 7. MANURE (manure_management.csv)
            # ==================================================================
            div(
              id = "dict-manure",
              class = "mb-4",
              h5(tagList(icon("recycle"), " 7. Manure (manure_management.csv)"), class = "fw-bold text-danger border-bottom pb-2"),
              p(class = "text-muted small mb-3", "Manure storage systems, environmental conditions, and allocation proportions. Allocations for each animal cohort must sum to 1.0 (100%)."),
              tags$dl(
                class = "row small",
                tags$dt(class = "col-sm-3 text-monospace", tags$code("animal_tag")),
                tags$dd(class = "col-sm-9", "Cohort identifier matching Census."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("region, subregion, class_flex")),
                tags$dd(class = "col-sm-9", "Cohort identity matching Census."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("system_base")),
                tags$dd(class = "col-sm-9", "Main manure management system (e.g., 'liquid_slurry', 'pit_storage', 'solid_storage', 'pasture', 'daily_spread')."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("system_variant")),
                tags$dd(class = "col-sm-9", "Storage/treatment variation (e.g., 'with_natural_crust_cover', 'uncovered', 'aerobic_treatment')."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("management_months")),
                tags$dd(class = "col-sm-9", "Duration in months per year manure is retained or managed in this system."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("system_climate")),
                tags$dd(class = "col-sm-9", "Facility temperature regime ('cool', 'temperate', 'warm')."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("system_subclimate")),
                tags$dd(class = "col-sm-9", "Specific secondary climate condition if required by IPCC tables."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("climate_zone")),
                tags$dd(class = "col-sm-9", "Regional climate zone ('zone_moist' or 'zone_dry')."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("climate_moisture")),
                tags$dd(class = "col-sm-9", "Moisture category ('wet' or 'dry')."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("b_0")),
                tags$dd(class = "col-sm-9", "Maximum methane-producing capacity (B0) descriptor for this livestock category."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("allocation")),
                tags$dd(class = "col-sm-9", "Fraction of manure managed in this system (0 to 1). All systems for an animal cohort must sum to 1.0.")
              )
            ),

            # ==================================================================
            # 8. REPRODUCTION (reproduction_parameters.csv)
            # ==================================================================
            div(
              id = "dict-repro",
              class = "mb-4",
              h5(tagList(icon("dna"), " 8. Reproduction (reproduction_parameters.csv)"), class = "fw-bold text-success border-bottom pb-2"),
              p(class = "text-muted small mb-3", "Replacement, pregnancy, and fertility rates used to calculate demography and young-stock replacement burdens."),
              tags$dl(
                class = "row small",
                tags$dt(class = "col-sm-3 text-monospace", tags$code("animal_tag")),
                tags$dd(class = "col-sm-9", "Cohort identifier matching Census."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("parameter")),
                tags$dd(class = "col-sm-9", "Name of the parameter: 'replacement_rate', 'pregnancy_rate', 'pr_sheep_goat', or 'fertility_rate'."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("value")),
                tags$dd(class = "col-sm-9", "Numeric value of the parameter (proportion between 0 and 1, or prolificacy ratio).")
              )
            ),

            # ==================================================================
            # 9. FEED CHARACTERISTICS (feed_characteristics.csv)
            # ==================================================================
            div(
              id = "dict-feed_char",
              class = "mb-4",
              h5(tagList(icon("flask"), " 9. Feed Characteristics (feed_characteristics.csv)"), class = "fw-bold text-info border-bottom pb-2"),
              p(class = "text-muted small mb-3", "Nutritional library containing 370 standardized feeds with chemical compositions, dry matter, gross energy, and species-specific digestibilities."),
              tags$dl(
                class = "row small",
                tags$dt(class = "col-sm-3 text-monospace", tags$code("ingredient_type")),
                tags$dd(class = "col-sm-9", "Macro feed classification: 'concentrate', 'forage', 'milk', or 'milk_replacer'."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("ingredient")),
                tags$dd(class = "col-sm-9", "Standardized name of the feed item."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("land_type")),
                tags$dd(class = "col-sm-9", "Land requirement type: 'cropland', 'grassland', or 'none'."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("DM_pct")),
                tags$dd(class = "col-sm-9", "Dry Matter percentage (%)."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("ASH_pct")),
                tags$dd(class = "col-sm-9", "Ash (mineral) content (% of dry matter)."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("CP_pct")),
                tags$dd(class = "col-sm-9", "Crude Protein content (% of dry matter)."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("EE_pct")),
                tags$dd(class = "col-sm-9", "Ether Extract (crude fat) content (% of dry matter)."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("NDF_pct")),
                tags$dd(class = "col-sm-9", "Neutral Detergent Fiber (% of dry matter)."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("DE_pct")),
                tags$dd(class = "col-sm-9", "Digestible Energy percentage for ruminants (% of Gross Energy)."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("GE_feed_kcal_kg")),
                tags$dd(class = "col-sm-9", "Gross Energy density in kcal per kg dry matter."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("swine_ME_kcal_kg, swine_DE_kcal_kg")),
                tags$dd(class = "col-sm-9", "Metabolizable and Digestible Energy specific for pigs (kcal/kg DM)."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("poultry_ME_kcal_kg")),
                tags$dd(class = "col-sm-9", "Metabolizable Energy specific for poultry (kcal/kg DM)."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("source_DE")),
                tags$dd(class = "col-sm-9", "Scientific reference / nutritional database (Feedipedia, INRAE, NRC).")
              )
            ),

            # ==================================================================
            # 10. IPCC COEFFICIENTS (ipcc_coefficients.csv)
            # ==================================================================
            div(
              id = "dict-ipcc_coef",
              class = "mb-4",
              h5(tagList(icon("square-root-variable"), " 10. IPCC Coefficients (ipcc_coefficients.csv)"), class = "fw-bold text-info border-bottom pb-2"),
              p(class = "text-muted small mb-3", "Standard Tier 2 coefficients defined in 2006 and 2019 Refinement IPCC Guidelines for National Greenhouse Gas Inventories."),
              tags$dl(
                class = "row small",
                tags$dt(class = "col-sm-3 text-monospace", tags$code("description")),
                tags$dd(class = "col-sm-9", "Descriptive name of the animal state or management scenario."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("value")),
                tags$dd(class = "col-sm-9", "Numeric coefficient value used in IPCC net energy equations."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("coefficient")),
                tags$dd(class = "col-sm-9", "Equation parameter code ('cfi', 'ca', 'c', 'a', 'b', 'c_pregnancy', 'b_0')."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("units")),
                tags$dd(class = "col-sm-9", "Measurement units (e.g., MJ/day/kg, dimensionless, m3 CH4/kg VS).")
              )
            ),

            # ==================================================================
            # 11. IPCC MANURE MGT. (ipcc_mm.csv)
            # ==================================================================
            div(
              id = "dict-ipcc_mm",
              class = "mb-4",
              h5(tagList(icon("warehouse"), " 11. IPCC Manure Mgt. (ipcc_mm.csv)"), class = "fw-bold text-info border-bottom pb-2"),
              p(class = "text-muted small mb-3", "Lookup database of emission factors and nitrogen loss fractions across thousands of IPCC manure management configurations."),
              tags$dl(
                class = "row small",
                tags$dt(class = "col-sm-3 text-monospace", tags$code("system_base, system_variant, ...")),
                tags$dd(class = "col-sm-9", "IPCC classification hierarchy matching climate and storage conditions."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("MCF_pct")),
                tags$dd(class = "col-sm-9", "Methane Conversion Factor (%): percentage of manure maximum methane capacity actually converted to CH4."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("EF3")),
                tags$dd(class = "col-sm-9", "Direct N2O emission factor from storage (kg N2O-N per kg N excreted)."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("EF4")),
                tags$dd(class = "col-sm-9", "Indirect N2O emission factor for volatilized nitrogen (kg N2O-N per kg NH3-N + NOx-N)."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("EF5")),
                tags$dd(class = "col-sm-9", "Indirect N2O emission factor for leached nitrogen (kg N2O-N per kg N leached)."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("frac_gas")),
                tags$dd(class = "col-sm-9", "Fraction of nitrogen excreted that volatilizes as ammonia and nitrogen oxides."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("frac_leach")),
                tags$dd(class = "col-sm-9", "Fraction of nitrogen excreted lost through leaching and runoff.")
              )
            ),

            # ==================================================================
            # 12. MAPPING (mapping.csv)
            # ==================================================================
            div(
              id = "dict-mapping",
              class = "mb-4",
              h5(tagList(icon("diagram-project"), " 12. Mapping (mapping.csv)"), class = "fw-bold text-info border-bottom pb-2"),
              p(class = "text-muted small mb-3", "Cross-database links connecting internal feed ingredients with international databases (FAOSTAT and AGRIBALYSE v3.1 LCA)."),
              tags$dl(
                class = "row small",
                tags$dt(class = "col-sm-3 text-monospace", tags$code("ingredient")),
                tags$dd(class = "col-sm-9", "Internal ingredient identifier matching Feed Characteristics and Ingredients."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("yield_name")),
                tags$dd(class = "col-sm-9", "Corresponding FAOSTAT commodity name used for national crop yield lookups."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("agribalyse_name")),
                tags$dd(class = "col-sm-9", "AGRIBALYSE v3.1 LCA database process name for upstream feed carbon footprint."),
                tags$dt(class = "col-sm-3 text-monospace", tags$code("economic_allocation")),
                tags$dd(class = "col-sm-9", "Economic allocation fraction applied to co-products (between 0 and 1).")
              )
            )

          )
        )
      )
    )
  )
}
