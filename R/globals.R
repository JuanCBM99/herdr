# Generated to satisfy R CMD check - herdr global variables
#' @importFrom utils globalVariables
NULL

if(getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    # --- Identifiers and Metadata ---
    ".", ".env", "region", "subregion", "animal_tag", "class_flex",
    "animal_type", "animal_subtype", "diet_tag", "key", "parameter",
    "description", "coefficient", "value",

    # --- Energy and Physical Parameters ---
    "initial_weight_kg", "final_weight_kg", "adult_weight_kg",
    "productive_period_days", "milk_yield_kg_year", "fat_content_pct",
    "wool_yield_kg_year", "work_hours", "a", "b", "C_val", "A_val", "B_val",
    "cfi", "cfi_value", "ca", "ca_value", "c_pregnancy", "c_value", "pr",
    "C_preg_factor", "REM", "REG", "de_safe", "de_percent",

    # --- Nutrition and Intake (DMI) ---
    "GE_MJday", "ED_MJkg", "DE_pct", "CP_pct", "NDF_pct", "ASH_pct",
    "DMI_kgday", "DMI_bw_pct", "forage_share", "concentrate_share",
    "milk_share", "milk_replacer_share", "total_diet", "total_ing",
    "ingredient", "ingredient_share", "ingredient_type", "forage_pct",

    # --- Population and Herd Dynamics ---
    "population", "mature_beef_bull", "mature_beef_cattle", "mature_dairy_cattle",
    "dairy_births_half", "pop_dairy_calves_female_repl", "beef_births_half",
    "pop_beef_calves_male_repl", "pop_beef_calves_female_repl",
    "pop_feedlot_calves_male", "pop_feedlot_calves_female", "pop_beef_calves_male",
    "pop_beef_calves_female", "beef_calves_male_replacement",
    "beef_calves_female_replacement", "dairy_calves_female_replacement",
    "mature_goat_female_dairy", "mature_goat_male_dairy", "mature_goat_female_meat",
    "mature_goat_male_meat", "pop_kid_goat_female_dairy_replacement",
    "pop_kid_goat_male_dairy_replacement", "pop_kid_goat_female_meat_replacement",
    "pop_kid_goat_male_meat_replacement", "mature_sheep_female_dairy",
    "mature_sheep_male_dairy", "mature_sheep_female_meat", "mature_sheep_male_meat",
    "total_dairy_births", "pop_lamb_female_dairy_replacement",
    "pop_lamb_male_dairy_replacement", "total_meat_births",
    "pop_lamb_female_meat_replacement", "pop_lamb_male_meat_replacement",
    "pop_lamb_dairy_slaughter", "pop_lamb_meat_slaughter",

    # --- Net Energy Components ---
    "NEm_MJday", "NEa_MJday", "NEg_MJday", "NEl_MJday", "NEwork_MJday",
    "NEpregnancy_MJday", "NEwool_MJday",

    # --- Manure and Methane (CH4) ---
    "system_base", "management_months", "system_climate", "system_subclimate",
    "system_variant", "climate_zone", "climate_moisture", "allocation",
    "VS_kgday", "MCF_pct", "B0", "EF_kgheadyear", "EF_kgyear", "Ym_pct",
    "total_CH4_enteric_Ggyear", "total_CH4_mm_Ggyear",

    # --- Nitrogen and Nitrous Oxide (N2O) ---
    "EF3", "EF4", "EF5", "frac_leach", "frac_gas", "N_intake_kgheadday",
    "N_retention", "N_excreted_kgheadday", "direct_N2O_kgyear",
    "N_volatilization_kg_year", "N2O_vol_kgyear", "N_leaching_kg_year",
    "N2O_leach_kgyear",

    # --- Land Use and FAO Data ---
    "Area", "Item", "Year", "Value", "yield_name", "agribalyse_name", "avg",
    "dm_yield", "ha_per_kg", "economic_allocation", "share_factor",
    "ha_kg_allocated", "annual_cons_kg", "land_use_m2", "total_land_use_m2",

    # --- Impacts and Summary (CO2eq) ---
    "CH4_enteric_Gg", "CH4_manure_Gg", "N2O_direct_Gg", "N2O_vol_Gg",
    "N2O_lea_Gg", "CO2eq_enteric", "CO2eq_manure", "CO2eq_N2O",
    "CO2eq_Total_Gg", "Land_m2"
  ))
}
