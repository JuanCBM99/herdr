
# Define global variables to satisfy R CMD check
#' @importFrom utils globalVariables
NULL
utils::globalVariables(c(
  # Identity & Metadata
  ".", ".env", "animal_tag", "region", "subregion", "class_flex",
  "animal_type", "animal_subtype", "diet_tag", "ingredient",
  "ingredient_type", "parameter", "description", "coefficient", "value",

  # Energy & Physics
  "ge", "de", "eb", "ash", "ndf", "ym", "average_weight", "adult_weight",
  "weight_gain", "milk_yield", "fat_content", "wool_yield", "work_hours",
  "NEm", "NEa", "NEg", "NEl", "NE_work", "NE_pregnancy", "NE_wool",
  "C_val", "A_val", "B_val", "a", "b", "cfi", "cfi_value", "ca", "ca_value",
  "c_pregnancy", "c_value", "pr", "C_preg_factor", "rem", "reg",
  "de_safe", "de_percent", "ue_factor",

  # Population Logic
  "population", "mature_beef_bull", "mature_beef_cattle", "mature_dairy_cattle",
  "pop_feedlot_calves_male", "pop_feedlot_calves_female", "pop_beef_calves_male",
  "pop_beef_calves_female", "beef_calves_male_replacement", "beef_calves_female_replacement",
  "dairy_calves_female_replacement", "mature_goat_female_dairy", "mature_goat_male_dairy",
  "mature_goat_female_meat", "mature_goat_male_meat", "pop_kid_goat_female_dairy_replacement",
  "pop_kid_goat_male_dairy_replacement", "pop_kid_goat_female_meat_replacement",
  "pop_kid_goat_male_meat_replacement", "mature_sheep_female_dairy", "mature_sheep_male_dairy",
  "mature_sheep_female_meat", "mature_sheep_male_meat", "total_dairy_births",
  "total_meat_births", "pop_lamb_female_dairy_replacement", "pop_lamb_male_dairy_replacement",
  "pop_lamb_female_meat_replacement", "pop_lamb_male_meat_replacement",
  "pop_lamb_dairy_slaughter", "pop_lamb_meat_slaughter",

  # Emissions & Land Use
  "system_climate", "vs", "N_intake", "N_retention", "N_excreted",
  "N2O_emissions", "n2o_l", "n2o_g", "ef_kg_animal_year", "emissions_total",
  "consumption_kg", "ingredient_share", "land_use_m2_per_unit", "Land_use_Total_m2",
  "CH4_enteric_Gg", "CH4_manure_Gg", "N2O_direct_Gg", "N2O_vol_Gg",
  "N2O_lea_Gg", "CO2eq_Total_Gg", "Land_m2",

# Identity & Management
"key", "allocation", "system_base", "management_months", "system_subclimate",
"system_variant", "climate_zone", "climate_moisture",

# Methane & Manure
"mcf", "B0", "ef_ch4_kg_year", "Emissions_CH4_Gg_year", "EF3",

# Nitrous Oxide
"EF4", "EF5", "frac_leach", "frac_gas", "n_leaching_kg_year",
"n_volatilization_kg_year", "cp",

# Land Use & Diet
"dry_matter_yield", "dm_ingested_derived", "share_weight_factor",
"ing_share_num", "yield_num", "forage_share", "concentrate_share",
"milk_share", "milk_replacer_share", "total_diet", "total_ing",
"forage_pct", "high_fvorage",

# Population
"dairy_births_half", "pop_dairy_calves_female_repl", "beef_births_half",
"pop_beef_calves_male_repl", "pop_beef_calves_female_repl",

# Summary / Impact Assessment
"CO2eq_enteric", "CO2eq_manure", "CO2eq_n2o"
))
