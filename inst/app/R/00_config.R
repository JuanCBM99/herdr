# ==============================================================================
# herdr — App Configuration & Metadata
# ==============================================================================

# --- Table Definitions & Display Metadata ---
tables_info <- list(
  census    = list(file = "livestock_census.csv", fixed = 0, icon = "clipboard-list", label = "Census"),
  diet_prof = list(file = "diet_profiles.csv", fixed = 1, icon = "utensils", label = "Diet Profiles"),
  diet_ingr = list(file = "diet_ingredients.csv", fixed = 1, icon = "wheat-awn", label = "Ingredients"),
  def       = list(file = "ruminant_definitions.csv", fixed = 4, icon = "id-card", label = "Ruminants"),
  mono      = list(file = "monogastric_definitions.csv", fixed = 4, icon = "drumstick-bite", label = "Monogastrics"),
  weights   = list(file = "livestock_weights.csv", fixed = 4, icon = "weight-hanging", label = "Weights"),
  manure    = list(file = "manure_management.csv", fixed = 4, icon = "recycle", label = "Manure"),
  repro     = list(file = "reproduction_parameters.csv", fixed = 0, icon = "dna", label = "Reproduction"),
  feed_char = list(file = "feed_characteristics.csv", fixed = 1, icon = "flask", label = "Feed Char."),
  ipcc_coef = list(file = "ipcc_coefficients.csv", fixed = 2, icon = "square-root-variable", label = "IPCC Coefficients"),
  ipcc_mm   = list(file = "ipcc_mm.csv", fixed = 2, icon = "warehouse", label = "IPCC Manure Mgt."),
  mapping   = list(file = "mapping.csv", fixed = 1, icon = "diagram-project", label = "Mapping")
)

# --- Dynamic Cross-Table Dropdown Rules ---
dynamic_dropdowns <- list(
  census = list(
    animal_tag      = list(source_table = "census", extract_column = "animal_tag", type = "dropdown", strict = FALSE)
  ),
  def = list(
    animal_tag      = list(source_table = "census", extract_column = "animal_tag", type = "dropdown", strict = FALSE),
    diet_tag        = list(source_table = "diet_prof", extract_column = "diet_tag", type = "dropdown", strict = FALSE),
    animal_type     = list(choices = c("cattle", "sheep", "goat"), type = "dropdown", strict = FALSE),
    animal_subtype  = list(choices = c("dairy", "beef", "meat"), type = "dropdown", strict = FALSE),
    production_role = list(choices = c("mature", "replacement", "slaughter"), type = "dropdown", strict = FALSE),
    cfi             = list(source_table = "ipcc_coef", filter_column = "coefficient", filter_value = "cfi",         extract_column = "description", type = "dropdown", strict = FALSE),
    ca              = list(source_table = "ipcc_coef", filter_column = "coefficient", filter_value = "ca",          extract_column = "description", type = "dropdown", strict = FALSE),
    c               = list(source_table = "ipcc_coef", filter_column = "coefficient", filter_value = "c",           extract_column = "description", type = "dropdown", strict = FALSE),
    a               = list(source_table = "ipcc_coef", filter_column = "coefficient", filter_value = "a",           extract_column = "description", type = "dropdown", strict = FALSE),
    b               = list(source_table = "ipcc_coef", filter_column = "coefficient", filter_value = "b",           extract_column = "description", type = "dropdown", strict = FALSE),
    c_pregnancy     = list(source_table = "ipcc_coef", filter_column = "coefficient", filter_value = "c_pregnancy", extract_column = "description", type = "dropdown", strict = FALSE),
    c_pregnancy_cattle = list(source_table = "ipcc_coef", filter_column = "coefficient", filter_value = "c_pregnancy", extract_column = "description", type = "dropdown", strict = FALSE)
  ),
  mono = list(
    animal_tag      = list(source_table = "census", extract_column = "animal_tag", type = "dropdown", strict = FALSE),
    diet_tag        = list(source_table = "diet_prof", extract_column = "diet_tag", type = "dropdown", strict = FALSE),
    animal_type     = list(choices = c("swine", "poultry"), type = "dropdown", strict = FALSE),
    animal_subtype  = list(choices = c("breeder", "fattening", "replacement", "layer", "meat"), type = "dropdown", strict = FALSE),
    production_role = list(choices = c("mature", "replacement", "slaughter"), type = "dropdown", strict = FALSE)
  ),
  weights = list(
    animal_tag      = list(source_table = "census", extract_column = "animal_tag", type = "dropdown", strict = FALSE)
  ),
  manure = list(
    animal_tag        = list(source_table = "census", extract_column = "animal_tag", type = "dropdown", strict = FALSE),
    system_base       = list(source_table = "ipcc_mm", extract_column = "system_base"),
    system_variant    = list(source_table = "ipcc_mm", extract_column = "system_variant"),
    management_months = list(source_table = "ipcc_mm", extract_column = "management_months"),
    system_climate    = list(source_table = "ipcc_mm", extract_column = "system_climate"),
    system_subclimate = list(source_table = "ipcc_mm", extract_column = "system_subclimate"),
    climate_zone      = list(source_table = "ipcc_mm", extract_column = "climate_zone"),
    climate_moisture  = list(source_table = "ipcc_mm", extract_column = "climate_moisture"),
    b_0               = list(source_table = "ipcc_coef", filter_column = "coefficient", filter_value = "b_0", extract_column = "description", type = "dropdown", strict = FALSE)
  ),
  repro = list(
    animal_tag        = list(source_table = "census", extract_column = "animal_tag", type = "dropdown", strict = FALSE),
    parameter         = list(choices = c("pregnancy_rate", "replacement_rate", "pr_sheep_goat", "fertility_rate"), type = "dropdown", strict = FALSE)
  ),
  diet_prof = list(
    diet_tag          = list(source_table = "diet_prof", extract_column = "diet_tag", type = "dropdown", strict = FALSE)
  ),
  diet_ingr = list(
    diet_tag          = list(source_table = "diet_prof", extract_column = "diet_tag", type = "dropdown", strict = FALSE),
    ingredient_type   = list(choices = c("forage", "concentrate", "milk", "milk_replacer"), type = "dropdown", strict = FALSE)
  ),
  feed_char = list(
    ingredient_type   = list(choices = c("forage", "concentrate", "milk", "milk_replacer"), type = "dropdown", strict = FALSE),
    land_type         = list(choices = c("arable", "grassland", "none", "no_land"), type = "dropdown", strict = FALSE)
  )
)

MANURE_CASCADE_COLUMNS <- c(
  "system_base", "system_variant", "management_months",
  "system_climate", "system_subclimate", "climate_zone", "climate_moisture"
)

# --- Field Help Tooltips ---
modal_tooltips <- list(
  animal_tag               = "Mandatory: A unique name for this specific group of animals (e.g., 'mature_dairy_cattle').",
  region                   = "Optional: A general location or grouping level (e.g., 'Europe', 'Spain', or 'Farm A').",
  subregion                = "Optional: A subdivision of your Region (e.g., if Region is 'Spain', Subregion could be 'Euskadi').",
  class_flex               = "Optional: A flexible tag to group animals by trait, breed, or phase (e.g., 'lactating', 'dry', 'angus').",
  population               = "Average Annual Population (AAP) per IPCC Tier 2 guidelines: the average standing headcount present on the farm over the year (i.e. occupied barn or pasture capacity). For short-cycle animals (e.g. broilers, fattening pigs), annual slaughter throughput is automatically derived from productive_period_days (365 / days on feed).",
  forage_share             = "Percentage of the diet made up of roughage/forage (e.g., pasture, hay, silage).",
  concentrate_share        = "Percentage of the diet made up of concentrates (e.g., grains, pellets, soy).",
  milk_share               = "Percentage of the diet consisting of natural maternal milk (for young animals).",
  milk_replacer_share      = "Percentage of the diet consisting of artificial milk formula/replacer.",
  diet_tag                 = "The name of the diet. This links the animal group to the food they eat.",
  ingredient               = "The specific food item (e.g., 'Corn silage', 'Soybean meal').",
  ingredient_type          = "Category of the ingredient. Must be: 'forage', 'concentrate', 'milk', or 'milk_replacer'.",
  ingredient_share         = "Percentage of this specific ingredient within the ingredient_type.",
  country_of_origin        = "Optional: Where this feed was grown. Leave blank to use your farm's country.",
  custom_yield_kg_ha       = "Optional: Crop yield. Leave blank if you don't know it (we'll use standard FAO data).",
  DE_pct                   = "Digestibility of the feed (%)",
  system_base              = "The main way manure is stored or managed (e.g., 'Liquid/Slurry', 'Pasture').",
  system_variant           = "A specific variation of the manure system (if applicable).",
  management_months        = "How many months per year the animals use this specific manure system.",
  climate_zone             = "The general climate of your farm (e.g., 'Temperate', 'Warm').",
  cfi                      = "IPCC coefficient for maintenance. Select the description that fits your animal.",
  ca                       = "IPCC coefficient for feeding situation. Select how active the animals are (e.g., confined vs. grazing).",
  c                        = "Only for Cattle: IPCC constant 'C' used to calculate energy for growth.",
  a                        = "Only for Sheep: IPCC constant 'a' used to calculate energy for growth.",
  b                        = "Only for Sheep: IPCC constant 'b' used to calculate energy for growth.",
  c_pregnancy              = "IPCC coefficient used to calculate the extra energy needed during pregnancy.",
  milk_yield_kg_year       = "Total milk produced by one average animal in a full year (in kg).",
  fat_content_pct          = "Percentage of fat in the milk (e.g., type 4.0 for 4%).",
  wool_yield_kg_year       = "Total wool produced by one animal in a year (in kg).",
  work_hours               = "Number of hours per day the animal is used for physical draft/work.",
  animal_type              = "Broad species category (e.g., 'cattle', 'sheep', 'swine').",
  animal_subtype           = "Specific production type (e.g., 'dairy', 'beef').",
  production_role          = "Role in production cycle: 'mature' (breeding/milking/laying adults), 'replacement' (growing breeding stock), or 'slaughter' (growing animals for meat).",
  cfi_maintenance          = "Maintenance coefficient in kcal/kg_day",
  frac_fat_pct             = "Fat fraction in the animal's daily weight gain.",
  frac_protein_pct         = "Protein fraction in the animal's daily weight gain.",
  eggs_per_year            = "Annual egg production per laying hen (in eggs/hen/year, e.g. 310-325 for commercial table egg layers, 210-220 for broiler breeders).",
  egg_weight_g             = "Average weight of a single egg in grams (e.g. 60-64 g). Used to calculate daily egg mass and total egg yield biomass.",
  fertility_rate           = "Fertility and hatchability rate (between 0 and 1) for poultry breeding flocks.",
  alpha                    = "Metabolic weight coefficient",
  piglets_born             = "Average number of piglets born in a single litter (only for breeding sows).",
  piglets_suckling         = "Average number of piglets actively nursing from the mother (only for lactating sows).",
  adult_weight_kg          = "Average weight of a fully grown mature animal (in kg).",
  productive_period_days   = "Cycle duration in days: for adult breeding females (except laying hens), the inter-parturition interval (e.g., 365 days for cows, 149 days for sows); for laying hens, the commercial flock laying cycle duration (e.g., 511 days); for all other cohorts (growing/fattening/replacement), their life cycle / days on feed (e.g., 42 days for broilers, 110 days for pigs).",
  replacement_rate         = "Annual replacement rate for breeding or mature animals (e.g., 0.27 for dairy cows, 0.25 for sows, 1.0 for layer hens per batch). Represents the fraction of adult stock replaced each year.",
  initial_weight_kg        = "Starting weight of the animal at the beginning of the evaluated period (in kg).",
  final_weight_kg          = "Target ending weight of the animal at the end of the period (in kg).",
  sows_gestation_days      = "Number of days a sow is pregnant.",
  sows_lactation_days      = "Number of days a sow nurses her piglets before weaning.",
  piglet_birth_weight_kg   = "Average weight of a single piglet exactly when it is born (in kg).",
  piglet_weaning_weight_kg = "Average weight of a single piglet when it is separated from the mother (in kg).",
  sow_reserve_gain_kg      = "Weight gained by the mother sow to recover body fat/reserves after a pregnancy cycle (in kg).",
  system_climate           = "Specific temperature or condition for this manure system, if required by the main system.",
  system_subclimate        = "Specific temperature or condition for this manure system, if required by the main system.",
  climate_moisture         = "Moisture level of your region's climate (e.g., 'Dry' or 'Wet').",
  b_0                      = "Maximum methane-producing capacity (B0). Select the description that matches your animal type.",
  allocation               = "Fraction of the total manure managed in this specific system (between 0 and 1). All systems for an animal must sum to 1.",
  DM_pct                   = "Dry Matter (%): The portion of the feed that remains after all water is removed.",
  CP_pct                   = "Crude Protein (%): Protein content, essential for muscle growth and milk production.",
  NDF_pct                  = "Neutral Detergent Fiber (%): Total structural fiber that gives bulk to the diet.",
  ASH_pct                  = "Ash (%): The total inorganic mineral content left in the feed.",
  EE_pct                   = "Ether Extract (%): The crude fat content of the feed.",
  GE_feed_kcal_kg          = "Gross Energy: The total energy contained in the feed (in kcal per kg).",
  swine_ME_kcal_kg         = "Metabolizable Energy specific for pigs (in kcal per kg).",
  swine_DE_kcal_kg         = "Digestible Energy specific for pigs (in kcal per kg).",
  poultry_ME_kcal_kg       = "Metabolizable Energy specific for poultry (in kcal per kg).",
  MCF_pct                  = "Methane Conversion Factor (%): The percentage of manure that actually turns into methane gas in this system.",
  EF3                      = "Emission Factor 3: Rate used to calculate direct nitrous oxide (N2O) emissions from the manure.",
  EF4                      = "Emission Factor 4: Rate used to calculate indirect N2O emissions from gases escaping into the air (volatilization).",
  EF5                      = "Emission Factor 5: Rate used to calculate indirect N2O emissions from manure washing into soil and water (leaching).",
  frac_gas                 = "Fraction of nitrogen excreted that is volatilized",
  frac_leach               = "Fraction of nitrogen lost through leaching/runoff.",
  economic_allocation      = "Used to allocate environmental impacts between a main product and its co-products based on economic value.",
  yield_name               = "FAOSTAT name of the ingredient used to link its database with herdr",
  agribalyse_name          = "Agribalyse name of the ingredient used to link its database with herdr",
  pregnancy_rate           = "Pregnancy rate expressed as a proportion (numeric value bounded between 0 and 1).",
  pr_sheep_goat            = "Prolificacy rate for sheep and goats, representing the number of lambs/kids born divided by pregnant ewes.",
  c_pregnancy_cattle       = "Pregnancy coefficient specific to cattle. Populate this parameter only for breeding groups.",
  parameter                = "Name of the reproduction or herd parameter (e.g., replacement_rate, pregnancy_rate, pr_sheep_goat).",
  value                    = "Numerical value for the parameter.",
  coefficient              = "IPCC standard coefficient code.",
  description              = "Descriptive category or condition for the coefficient."
)

standard_ids <- c("census", "diet_prof", "diet_ingr", "def", "mono", "weights", "manure", "repro")
advanced_ids <- c("feed_char", "ipcc_coef", "ipcc_mm", "mapping")

# --- Bootstrap Theme Definition ---
herdr_theme <- bslib::bs_theme(
  version = 5,
  bg = "#F8F7F3",
  fg = "#222521",
  primary = "#2D5A38",
  secondary = "#C79A2E",
  base_font = bslib::font_google("Inter"),
  heading_font = bslib::font_google("Fraunces", wght = c(500, 600, 700, 900)),
  code_font = bslib::font_google("IBM Plex Mono"),
  "border-radius" = "0.75rem"
)

herdr_logo <- shiny::HTML('<svg width="32" height="24" viewBox="0 0 30 22"><rect x="1" y="1" width="26" height="16" rx="4" ry="7" fill="#C79A2E"/><circle cx="7" cy="9" r="2.6" fill="#2D5A38"/><path d="M27 5 L29.5 9 L27 13" fill="#2D5A38"/></svg>')
