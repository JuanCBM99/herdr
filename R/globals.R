# Generated to satisfy R CMD check - herdr global variables
#' @importFrom utils globalVariables download.file tail
#' @importFrom bslib bs_theme
#' @importFrom htmlwidgets JS
#' @importFrom jsonlite toJSON
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom shinycssloaders withSpinner
#' @importFrom zip zip
NULL

if(getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    # --- Identifiers and Metadata ---
    ".", ".env", "farm_id", "region", "subregion", "animal_tag",
    "animal_type", "animal_subtype", "diet_tag", "key",
    "description", "coefficient", "value",

    # --- Energy and Physical Parameters ---
    "initial_weight_kg", "final_weight_kg", "adult_weight_kg",
    "productive_period_days", "milk_yield_kg_year", "Milk_yield_kg_day", "fat_content_pct", "protein_content_pct",
    "wool_yield_kg_year", "work_hours", "a", "b", "C_val", "A_val", "B_val",
    "cfi", "cfi_value", "ca", "ca_value", "c_value",
    "C_preg_factor", "REM", "REG", "de_safe", "de_percent", "frac_fat_pct",
    "frac_protein_pct", "egg_mass_g_day", "cfi_maintenance", "W_mean",
    "W_metabolic", "alpha", "pr_sheep_goat", "pregnancy_rate", "raw_preg_rate",

    # --- Nutrition and Intake (DMI) ---
    "GE_MJday", "GE_feed_kcal_kg", "DE_pct", "CP_pct", "NDF_pct", "ASH_pct",
    "DMI_kgday", "DMI_bw_pct", "weight_factor", "forage_share", "concentrate_share",
    "milk_share", "milk_replacer_share", "total_ing",
    "ingredient", "ingredient_share", "ingredient_type", "forage_pct",
    "poultry_ME_kcal_kg", "swine_ME_kcal_kg", "swine_DE_kcal_kg",

    # --- Monogastric & Swine Specific Energy ---
    "sows_gestation_days", "sows_lactation_days", "piglet_birth_weight_kg",
    "piglet_weaning_weight_kg", "sow_reserve_gain_kg", "piglets_born",
    "piglets_suckling", "gestation_adg_gday", "piglet_adg_gday", "adg_gday",
    "cycle_days", "gest_weight", "lact_weight",
    "ME_growth_gestation_daily", "ME_conceptus_daily", "ME_maternal_reserves_daily",
    "ME_milk_daily", "ME_mobilization_daily", "ME_maint_kcal_day",
    "ME_standard_growth_day", "ME_eggs_day", "ME_gestation_phase_daily",
    "ME_lactation_phase_daily", "ME_mant_kcal", "ME_crec_kcal", "ME_eggs_kcal",
    "ME_gestation_kcal", "ME_lactation_kcal", "ME_total_kcal_day",

    # --- Population and Herd Dynamics ---
    "population", "mature_beef_bull", "mature_beef_cattle", "mature_dairy_cattle",
    "dairy_births_half", "pop_dairy_calves_female_repl", "beef_births_half",
    "pop_beef_calves_male_repl", "pop_beef_calves_female_repl",
    "pop_feedlot_calves_male", "pop_feedlot_calves_female", "pop_beef_calves_male",
    "pop_beef_calves_female", "feedlot_calves_male", "feedlot_calves_female",
    "beef_calves_male", "beef_calves_female", "beef_calves_male_replacement",
    "beef_calves_female_replacement", "dairy_calves_female_replacement",
    "beef_yearlings_male_replacement", "beef_yearlings_female_replacement",
    "dairy_yearlings_female_replacement",
    "breeder_sows", "boars", "pop_replacement_sows", "pop_fattening_pigs",
    "annual_piglets", "annual_slaughter",
    "breeder_meat_hens", "laying_hens", "free_range_laying_hens",
    "replacement_meat_pullets", "replacement_layer_pullets", "broilers",
    "annual_meat_pullets", "annual_layer_pullets",
    "pop_replacement_meat_pullets", "pop_replacement_layer_pullets", "pop_broilers", ".has_manual_broilers",

    # --- Net Energy Components ---
    "NEm_MJday", "NEa_MJday", "NEg_MJday", "NEl_MJday", "NEwork_MJday",
    "NEpregnancy_MJday", "NEwool_MJday",

    # --- Manure and Methane (CH4) ---
    "system_base", "management_months", "system_climate", "system_subclimate",
    "system_variant", "climate_zone", "climate_moisture", "allocation", "total_alloc",
    "VS_kgday", "MCF_pct", "B0", "EF_kgheadyear", "EF_kgyear", "Ym_pct",
    "total_CH4_enteric_Ggyear", "total_CH4_mm_kgyear", "b_0", "swine_DE_eff",

    # --- Nitrogen and Nitrous Oxide (N2O) ---
    "EF3", "EF4", "EF5", "frac_leach", "frac_gas", "N_intake_kgheadday",
    "N_excreted_kgheadyear", "direct_N2O_kgyear",
    "N_volatilization_kg_year", "N2O_vol_kgyear", "N_leaching_kg_year",
    "N2O_leach_kgyear", "N_retention_kg_day", "daily_milk", "daily_gain",
    "milk_protein", "ipcc_swine_n_gain_factor", "ipcc_swine_fr", "ipcc_swine_Skg",
    "ipcc_swine_N_weaned_piglets", "ipcc_swine_N_gain_sow",

    # --- Land Use and FAO Data ---
    "Area", "Item", "Value", "yield_name", "agribalyse_name",
    "fao_yield", "dm_yield", "ha_per_kg", "economic_allocation", "human_edible", "diaas_pct", "share_factor",
    "he_frac", "he_cp_term", "he_cp_with_diaas", "he_hdp_known", "diaas_coverage", "HDP_feed_pct",
    "ha_kg_allocated", "annual_cons_kg", "land_use_m2", "total_land_use_m2",
    "Element", "Yield", "country_of_origin", "custom_yield_kg_ha",
    "land_type", "Reporter Countries", "Partner Countries", "Total_Import",
    "Production", "Total_Export", "Apparent_Consumption",
    "Calculated_Origin", "Area Code", "DM_pct",
    "fallback_yield", "used_fallback", "raw_yield", "alloc_ref",
    "coproduct_ref", "kg_coproduct_per_1ton_maincrop", "coproduct_mass_fraction", "lci_alloc_factor",
    "spain_forage_yield",

    # --- Production and Edible Protein (GLEAM) ---
    "production_role", "replacement_rate", "has_replacement_rate", "effective_repl_rate", "BFM", "MEAT_prot", "DP_pct",
    "MILK_prot_def", "MILK_diaas_pct", "MEAT_diaas_pct", "milk_fresh_kg", "milk_prot_pct", "N_exit",
    "slaughter_weight_kg", "meat_live_weight_kg", "meat_carcass_weight_kg",
    "meat_boneless_kg", "egg_fresh_kg", "milk_protein_kg", "meat_protein_kg",
    "egg_protein_kg", "milk_FPCM_kg", "wool_kg", "total_protein_kg",
    "milk_HDP_kg", "meat_HDP_kg", "egg_HDP_kg", "total_HDP_kg",

    # --- Impacts and Summary (CO2eq) ---
    "CH4_enteric_Gg", "CH4_manure_Gg", "N2O_direct_Gg", "N2O_vol_Gg",
    "N2O_lea_Gg", "N2O_indirect_Gg", "CO2eq_enteric", "CO2eq_manure", "CO2eq_N2O",
    "CO2eq_N2O_direct", "CO2eq_N2O_indirect", "CO2eq_Total_Gg", "Land_m2",
    "Land_cropland_m2", "Land_grassland_convertible_m2", "Land_grassland_unconvertible_m2",
    "Land_other_m2", ".pop_row_id", ".orig_row_id",

    # --- Plotting and Graphics ---
    "plot_label", "Percentage", "Nutrient",
    "Land_Val", "Protein_Source", "Protein_kg",
    "component", "panel", "component_raw",
    "land_val", "total_val", "label_text",
    "s", "water_val", "total_ghg",

    # --- Functional Units and Intensities ---
    "CO2eq_Total_head", "CO2eq_cradle_to_farm_head", "Land_head", "Water_head",
    "feed_intake_kg", "CP_human_edible_pct", "feed_CP_total_kg", "feed_CP_human_edible_kg", "feed_HDP_kg", "EPCR_DIAAS", "PQR",

    # --- Enfoque A (IDF Bulletin 520 / 2022) Biophysical Allocation ---
    "GHG_intensity_milk", "Land_intensity_milk",
    "GHG_intensity_meat", "Land_intensity_meat",
    "GHG_intensity_wool", "Land_intensity_wool",
    "GHG_intensity_egg", "Land_intensity_egg",

    # --- Protein Feed Efficiency and Human Food Security (Mottet et al. 2017) ---
    "metric_raw", "metric",

    # --- Feed LCA & Cradle-to-Farm Footprints (ECOALIM V9 & EF 3.1) ---
    "CO2eq_feed_Gg", "CO2eq_feed_fossil_Gg", "CO2eq_feed_luc_Gg",
    "Water_feed_m3", "CO2eq_cradle_to_farm_Gg",
    "GHG_cradle_to_farm_milk", "Water_intensity_milk",
    "GHG_cradle_to_farm_meat", "Water_intensity_meat",
    "GHG_cradle_to_farm_wool", "Water_intensity_wool",
    "GHG_cradle_to_farm_egg", "Water_intensity_egg",
    ".p_ent", ".p_man", ".p_ndir", ".p_nind", ".p_fos", ".p_luc",
    ".p_crop", ".p_gcon", ".p_gunc", ".p_loth", ".s1_sum", ".fd_sum", ".ld_sum",
    "share_ent", "share_man", "share_n2o_dir", "share_n2o_ind",
    "share_fossil", "share_luc", "share_crop", "share_g_conv", "share_g_unco", "share_other",

    # --- ECOALIM Feed Calculation Columns ---
    "Feedstuff", "Simapro LCI name", "Types of feedstuffs for livestock",
    "Climate change - EF 3.1 (kg CO2 eq)", "Climate change - Fossil (kg CO2 eq/kg)",
    "Climate change - Land use and LU change (kg CO2 eq/kg)", "Water use (m3 depriv.)",
    "annual_dmi_kg_head", "annual_dmi_kg_herd", "annual_fresh_kg_head", "annual_fresh_kg_herd",
    "ecoalim_category", "ecoalim_feedstuff", "ef_climate_fossil", "ef_climate_luc", "ef_climate_total", "ef_water",
    "feed_CO2e_fossil_herd_kg", "feed_CO2e_fossil_kg_year", "feed_CO2e_herd_kg",
    "feed_CO2e_luc_herd_kg", "feed_CO2e_luc_kg_year", "feed_CO2e_total_kg_year",
    "feed_water_herd_m3", "feed_water_m3_year"
  ))
}
