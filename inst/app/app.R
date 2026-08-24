library(shiny)
library(bslib)
library(herdr)
library(rhandsontable)
library(dplyr)
library(readr)
library(arrow)
library(zip)

# 1. GLOBAL CONFIGURATION
if (!dir.exists("user_data")) herdr::herdr_init()

tables_info <- list(
  census = list(file = "livestock_census.csv", fixed = 0, icon = "clipboard-list", label = "Census"),
  diet_prof = list(file = "diet_profiles.csv", fixed = 4, icon = "utensils", label = "Diet Profiles"),
  diet_ingr = list(file = "diet_ingredients.csv", fixed = 4, icon = "wheat-awn", label = "Ingredients"),
  def = list(file = "ruminant_definitions.csv", fixed = 4, icon = "id-card", label = "Ruminants"),
  mono = list(file = "monogastric_definitions.csv", fixed = 4, icon = "drumstick-bite", label = "Monogastrics"),
  weights = list(file = "livestock_weights.csv", fixed = 4, icon = "weight-hanging", label = "Weights"),
  manure = list(file = "manure_management.csv", fixed = 4, icon = "recycle", label = "Manure"),
  repro = list(file = "reproduction_parameters.csv", fixed = 0, icon = "dna", label = "Reproduction"),
  feed_char = list(file = "feed_characteristics.csv", fixed = 1, icon = "flask", label = "Feed Char."),
  ipcc_coef = list(file = "ipcc_coefficients.csv", fixed = 2, icon = "square-root-variable", label = "IPCC Coefficients"),
  ipcc_mm = list(file = "ipcc_mm.csv", fixed = 2, icon = "warehouse", label = "IPCC Manure Mgt."),
  mapping = list(file = "mapping.csv", fixed = 1, icon = "diagram-project", label = "Mapping")
)

dynamic_dropdowns <- list(
  def = list(
    cfi         = list(source_table = "ipcc_coef", filter_column = "coefficient", filter_value = "cfi",         extract_column = "description"),
    ca          = list(source_table = "ipcc_coef", filter_column = "coefficient", filter_value = "ca",          extract_column = "description"),
    c           = list(source_table = "ipcc_coef", filter_column = "coefficient", filter_value = "c",           extract_column = "description"),
    a           = list(source_table = "ipcc_coef", filter_column = "coefficient", filter_value = "a",           extract_column = "description"),
    b           = list(source_table = "ipcc_coef", filter_column = "coefficient", filter_value = "b",           extract_column = "description"),
    c_pregnancy = list(source_table = "ipcc_coef", filter_column = "coefficient", filter_value = "c_pregnancy", extract_column = "description")
  ),
  manure = list(
    system_base       = list(source_table = "ipcc_mm", extract_column = "system_base"),
    system_variant    = list(source_table = "ipcc_mm", extract_column = "system_variant"),
    management_months = list(source_table = "ipcc_mm", extract_column = "management_months"),
    system_climate    = list(source_table = "ipcc_mm", extract_column = "system_climate"),
    system_subclimate = list(source_table = "ipcc_mm", extract_column = "system_subclimate"),
    climate_zone      = list(source_table = "ipcc_mm", extract_column = "climate_zone"),
    climate_moisture  = list(source_table = "ipcc_mm", extract_column = "climate_moisture"),
    b_0               = list(source_table = "ipcc_coef", filter_column = "coefficient", filter_value = "b_0", extract_column = "description")
  )
)

MANURE_CASCADE_COLUMNS <- c(
  "system_base", "system_variant", "management_months",
  "system_climate", "system_subclimate", "climate_zone", "climate_moisture"
)

modal_tooltips <- list(
  animal_tag         = "Mandatory: A unique name for this specific group of animals (e.g., 'mature_dairy_cattle').",
  region             = "Optional: A general location or grouping level (e.g., 'Europe', 'Spain', or 'Farm A').",
  subregion          = "Optional: A subdivision of your Region (e.g., if Region is 'Spain', Subregion could be 'Euskadi').",
  class_flex         = "Optional: A flexible tag to group animals by trait, breed, or phase (e.g., 'lactating', 'dry', 'angus').",
  population         = "Total number of animals in this exact combination of animal_tag, region, subregion and class_flex.",
  weight_kg          = "The average live weight of a single animal in kilograms.",
  forage_share       = "Percentage of the diet made up of roughage/forage (e.g., pasture, hay, silage).",
  concentrate_share  = "Percentage of the diet made up of concentrates (e.g., grains, pellets, soy).",
  milk_share         = "Percentage of the diet consisting of natural maternal milk (for young animals).",
  milk_replacer_share= "Percentage of the diet consisting of artificial milk formula/replacer.",
  diet_tag           = "The name of the diet. This links the animal group to the food they eat.",
  ingredient         = "The specific food item (e.g., 'Corn silage', 'Soybean meal').",
  ingredient_type    = "Category of the ingredient. Must be: 'forage', 'concentrate', 'milk', or 'milk_replacer'.",
  ingredient_share   = "Percentage of this specific ingredient within the ingredient_type.",
  country_of_origin  = "Optional: Where this feed was grown. Leave blank to use your farm's country.",
  custom_yield_kg_ha = "Optional: Crop yield. Leave blank if you don't know it (we'll use standard FAO data).",
  DE_pct             = "Digestibility of the feed (%)",
  system_base        = "The main way manure is stored or managed (e.g., 'Liquid/Slurry', 'Pasture').",
  system_variant     = "A specific variation of the manure system (if applicable).",
  management_months  = "How many months per year the animals use this specific manure system.",
  climate_zone       = "The general climate of your farm (e.g., 'Temperate', 'Warm').",
  cfi                = "IPCC coefficient for maintenance. Select the description that fits your animal.",
  ca                 = "IPCC coefficient for feeding situation. Select how active the animals are (e.g., confined vs. grazing).",
  c                  = "Only for Cattle: IPCC constant 'C' used to calculate energy for growth.",
  a                  = "Only for Sheep: IPCC constant 'a' used to calculate energy for growth.",
  b                  = "Only for Sheep: IPCC constant 'b' used to calculate energy for growth.",
  c_pregnancy        = "IPCC coefficient used to calculate the extra energy needed during pregnancy.",
  milk_yield_kg_year = "Total milk produced by one average animal in a full year (in kg).",
  fat_content_pct    = "Percentage of fat in the milk (e.g., type 4.0 for 4%).",
  wool_yield_kg_year = "Total wool produced by one animal in a year (in kg).",
  work_hours         = "Number of hours per day the animal is used for physical draft/work.",
  pr                 = "Prolificacy Rate: The average number of lambs born per pregnant ewe.",
  single_birth_fraction  = "Proportion of pregnancies that result in a single baby (value between 0 and 1).",
  double_birth_fraction  = "Proportion of pregnancies that result in twins (value between 0 and 1).",
  animal_type        = "Broad species category (e.g., 'cattle', 'sheep', 'swine').",
  animal_subtype     = "Specific production type (e.g., 'dairy', 'beef').",
  cfi_maintenance    = "Maintenance coefficient in kcal/kg_day",
  frac_fat_pct       = "Fat fraction in the animal's daily weight gain.",
  frac_protein_pct   = "Protein fraction in the animal's daily weight gain.",
  egg_mass_g_day     = "Egg mass produced by laying hens.",
  alpha              = "Metabolic weight coefficient",
  piglets_born       = "Average number of piglets born in a single litter (only for breeding sows).",
  piglets_suckling   = "Average number of piglets actively nursing from the mother (only for lactating sows).",
  adult_weight_kg          = "Average weight of a fully grown mature animal (in kg).",
  productive_period_days   = "Number of days the animal is in its active production phase (e.g., fattening days).",
  initial_weight_kg        = "Starting weight of the animal at the beginning of the evaluated period (in kg).",
  final_weight_kg          = "Target ending weight of the animal at the end of the period (in kg).",
  sows_gestation_days      = "Number of days a sow is pregnant.",
  sows_lactation_days      = "Number of days a sow nurses her piglets before weaning.",
  piglet_birth_weight_kg   = "Average weight of a single piglet exactly when it is born (in kg).",
  piglet_weaning_weight_kg = "Average weight of a single piglet when it is separated from the mother (in kg).",
  sow_reserve_gain_kg      = "Weight gained by the mother sow to recover body fat/reserves after a pregnancy cycle (in kg).",
  system_climate     = "Specific temperature or condition for this manure system, if required by the main system.",
  system_subclimate  = "Specific temperature or condition for this manure system, if required by the main system.",
  climate_moisture   = "Moisture level of your region's climate (e.g., 'Dry' or 'Wet').",
  b_0                = "Maximum methane-producing capacity (B0). Select the description that matches your animal type.",
  allocation         = "Fraction of the total manure managed in this specific system (between 0 and 1). All systems for an animal must sum to 1.",
  DM_pct             = "Dry Matter (%): The portion of the feed that remains after all water is removed.",
  CP_pct             = "Crude Protein (%): Protein content, essential for muscle growth and milk production.",
  NDF_pct            = "Neutral Detergent Fiber (%): Total structural fiber that gives bulk to the diet.",
  ASH_pct            = "Ash (%): The total inorganic mineral content left in the feed.",
  EE_pct             = "Ether Extract (%): The crude fat content of the feed.",
  GE_feed_kcal_kg    = "Gross Energy: The total energy contained in the feed (in kcal per kg).",
  swine_ME_kcal_kg   = "Metabolizable Energy specific for pigs (in kcal per kg).",
  swine_DE_kcal_kg   = "Digestible Energy specific for pigs (in kcal per kg).",
  poultry_ME_kcal_kg = "Metabolizable Energy specific for poultry (in kcal per kg).",
  MCF_pct            = "Methane Conversion Factor (%): The percentage of manure that actually turns into methane gas in this system.",
  EF3                = "Emission Factor 3: Rate used to calculate direct nitrous oxide (N2O) emissions from the manure.",
  EF4                = "Emission Factor 4: Rate used to calculate indirect N2O emissions from gases escaping into the air (volatilization).",
  EF5                = "Emission Factor 5: Rate used to calculate indirect N2O emissions from manure washing into soil and water (leaching).",
  frac_gas           = "Fraction of nitrogen excreted that is volatilized",
  frac_leach         = "Fraction of nitrogen lost through leaching/runoff.",
  economic_allocation= "Used to allocate environmental impacts between a main product and its co-products (e.g., milk and meat) based on their relative economic value.",
  yield_name         = "FAOSTAT name of the ingredient used to link its database with herdr",
  agribalyse_name    = "Agribalyse name of the ingredient used to link its database with herdr"
)

standard_ids <- c("census", "diet_prof", "diet_ingr", "def", "mono", "weights", "manure", "repro")
advanced_ids <- c("feed_char", "ipcc_coef", "ipcc_mm", "mapping")

# 2. DATA I/O HELPERS

read_clean <- function(path) {
  if (!file.exists(path)) return(data.frame())
  read_csv(path, show_col_types = FALSE, lazy = FALSE) %>%
    mutate(across(where(is.logical), as.character))
}

save_clean <- function(df, path) {
  if (is.null(df) || nrow(df) == 0) return(invisible(NULL))
  key_column <- intersect(c("animal_tag", "diet_tag"), names(df))[1]
  if (!is.na(key_column)) {
    df <- df %>% filter(!is.na(.data[[key_column]]) & .data[[key_column]] != "")
  }
  df %>%
    mutate(across(where(is.logical), ~ NA_character_)) %>%
    mutate(across(where(is.character), ~ na_if(., ""))) %>%
    mutate(across(where(is.character), ~ na_if(., "NA"))) %>%
    write_csv(path, na = "")
}

build_keys <- function(df) {
  if (is.null(df) || nrow(df) == 0 || !"animal_tag" %in% names(df)) return(character(0))
  df <- df[!is.na(df$animal_tag) & df$animal_tag != "", ]
  if (nrow(df) == 0) return(character(0))
  id_columns <- intersect(c("animal_tag", "region", "subregion", "class_flex"), names(df))
  key_frame <- df[, id_columns, drop = FALSE]
  key_frame[is.na(key_frame)] <- ""
  apply(key_frame, 1, paste, collapse = " | ")
}

# 3. HANDSONTABLE HELPERS & JS

apply_dynamic_dropdowns <- function(tbl, id, rv) {
  if (!id %in% names(dynamic_dropdowns)) return(tbl)
  for (target_col in names(dynamic_dropdowns[[id]])) {
    rule <- dynamic_dropdowns[[id]][[target_col]]
    source_df <- rv[[rule$source_table]]
    if (!target_col %in% names(rv[[id]]) || is.null(source_df) || nrow(source_df) == 0) next
    if (!rule$extract_column %in% names(source_df)) next

    options <- if (!is.null(rule$filter_column) && !is.null(rule$filter_value) && rule$filter_column %in% names(source_df)) {
      source_df[[rule$extract_column]][source_df[[rule$filter_column]] == rule$filter_value]
    } else {
      source_df[[rule$extract_column]]
    }
    options <- as.character(unique(na.omit(options)))
    if (length(options) > 0) tbl <- hot_col(tbl, col = target_col, type = "dropdown", source = options)
  }
  tbl
}

CASCADE_AUTOCOMPLETE_JS_TEMPLATE <- "
function(query, process) {
  try {
    var db = %s; var colIndices = %s; var dbColNames = %s;
    var hot = this.instance; var row = this.row;
    if (row === null || row === undefined) {
      var sel = hot.getSelected();
      if (sel && sel.length > 0) row = sel[0][0];
    }
    if (row === null || row === undefined) return process([]);

    var myDbName = '%s'; var myStepIdx = dbColNames.indexOf(myDbName);
    function cln(s) { return (s == null) ? '' : String(s).trim().toLowerCase().replace(/[^a-z0-9]/g, ''); }

    var valid = db;
    for (var i = 0; i < myStepIdx; i++) {
      var dbName = dbColNames[i]; var visualColIdx = colIndices[i];
      if (visualColIdx < 0) continue;
      var val = hot.getDataAtCell(row, visualColIdx);
      var cleanVal = cln(val);
      if (cleanVal !== '' && cleanVal !== 'na') {
        valid = valid.filter(function(r) { return cln(r[dbName]) === cleanVal; });
      }
    }
    var out = [];
    valid.forEach(function(r) {
      var x = r[myDbName];
      if (cln(x) !== '' && cln(x) !== 'na') {
        var strX = String(x).trim();
        if (out.indexOf(strX) === -1) out.push(strX);
      }
    });
    process(out.sort());
  } catch(err) { console.error('Cascade Error:', err); process([]); }
}
"

apply_manure_cascade_dropdowns <- function(tbl, ipcc_mm_df, current_manure_df) {
  if (is.null(ipcc_mm_df) || nrow(ipcc_mm_df) == 0) return(tbl)
  if (!all(MANURE_CASCADE_COLUMNS %in% names(ipcc_mm_df))) return(tbl)

  col_indices <- match(MANURE_CASCADE_COLUMNS, names(current_manure_df)) - 1
  col_indices[is.na(col_indices)] <- -1

  lookup_json  <- jsonlite::toJSON(ipcc_mm_df[, MANURE_CASCADE_COLUMNS, drop = FALSE], dataframe = "rows", auto_unbox = TRUE, na = "null")
  indices_json <- jsonlite::toJSON(col_indices, auto_unbox = TRUE)
  db_names_json <- jsonlite::toJSON(MANURE_CASCADE_COLUMNS, auto_unbox = TRUE)

  for (i in seq_along(MANURE_CASCADE_COLUMNS)) {
    col_name <- MANURE_CASCADE_COLUMNS[i]
    if (col_indices[i] >= 0) {
      js <- sprintf(CASCADE_AUTOCOMPLETE_JS_TEMPLATE, lookup_json, indices_json, db_names_json, col_name)
      tbl <- hot_col(tbl, col = col_name, type = "autocomplete", source = htmlwidgets::JS(js), strict = TRUE, allowInvalid = FALSE)
    }
  }

  reset_js <- sprintf("
    function(el, x) {
      var hot = this.hot; var colIndices = %s;
      hot.addHook('afterChange', function(changes, source) {
        if (!changes || source === 'cascade' || source === 'loadData') return;
        changes.forEach(function(change) {
          var row = change[0], prop = change[1];
          var changedVisualCol = hot.propToCol(prop) ?? prop;
          var stepIdx = colIndices.indexOf(changedVisualCol);
          if (stepIdx < 0) return;
          for (var i = stepIdx + 1; i < colIndices.length; i++) {
            var targetVisualCol = colIndices[i];
            if (targetVisualCol >= 0) hot.setDataAtCell(row, targetVisualCol, '', 'cascade');
          }
        });
      });
    }
  ", indices_json)
  htmlwidgets::onRender(tbl, reset_js)
}

# 4. THEME & CSS

herdr_theme <- bs_theme(
  version = 5, bg = "#F7F5EF", fg = "#262620", primary = "#33593B", secondary = "#C79A2E",
  base_font = font_google("Inter"), heading_font = font_google("Fraunces", wght = c(500, 600, 900)),
  code_font = font_google("IBM Plex Mono"), "border-radius" = "0.65rem"
)

herdr_css <- "
:root{
  --herdr-pasture: #33593B; --herdr-pasture-dark: #223D28; --herdr-pasture-light: #E6EDE3;
  --herdr-wheat: #C79A2E; --herdr-wheat-light: #F3E3B8;
  --herdr-paper: #F7F5EF; --herdr-card: #FFFFFF; --herdr-ink: #262620;
  --herdr-muted: #746F60; --herdr-barn: #A63C30; --herdr-barn-bg: #FBEAE7; --herdr-border: #E7E2D4;
}
body { background: var(--herdr-paper); color: var(--herdr-ink); }
[data-bs-theme='dark']{
  --herdr-pasture: #7CB489; --herdr-pasture-dark: #A9D3B0; --herdr-pasture-light: #24322A;
  --herdr-wheat: #E3BE58; --herdr-wheat-light: #423718;
  --herdr-paper: #1B1F1A; --herdr-card: #242A23; --herdr-ink: #EDEAE0;
  --herdr-muted: #A6A28F; --herdr-barn: #E68979; --herdr-barn-bg: #3A2420; --herdr-border: #38402F;
}
.herdr-brand { display:flex; align-items:center; gap:.65rem; }
.herdr-brand-title { font-family:'Fraunces',serif; font-weight:900; font-size:1.35rem; }
.herdr-brand-sub { font-size:.72rem; opacity:.85; text-transform:uppercase; }
.step-card { background: var(--herdr-card); border-radius: 14px; padding: 1rem; margin-bottom: 1rem; border: 1px solid var(--herdr-border); }
.step-head { display:flex; align-items:center; gap:.6rem; margin-bottom:.5rem; }
.step-tag { background: var(--herdr-pasture); color:#fff; padding: 2px 8px; border-radius: 4px; font-weight:bold; }
.step-card.wheat .step-tag { background: var(--herdr-wheat); }
.step-card.barn .step-tag { background: var(--herdr-barn); }
.btn-herdr { width:100%; border:none; border-radius:10px; font-weight:700; padding:.62rem 1rem; margin-bottom: .5rem; }
.btn-load { background: var(--herdr-pasture-light); color: var(--herdr-pasture-dark); border: 1.5px solid var(--herdr-pasture); }
.btn-clear { background: var(--herdr-card); color: var(--herdr-barn); border: 1.5px dashed var(--herdr-barn); }
.btn-run { background: linear-gradient(135deg, var(--herdr-wheat), #DDB44C); color: #2B2210; }
.btn-download { background: var(--herdr-card); color: var(--herdr-pasture-dark); border: 1.5px dashed var(--herdr-pasture); }
.dirty-dot { display:inline-block; width:8px; height:8px; border-radius:50%; background: var(--herdr-wheat); margin-left:7px; }
.herdr-alert { display: flex; gap: 10px; align-items: center; padding: 0.75rem 1rem; border-radius: 8px; margin-bottom: 0.8rem; font-size: 0.9rem; }
.herdr-alert-danger { background: var(--herdr-barn-bg); color: #7C2417; border: 1px solid #EFC7BE; }
.herdr-alert-warning { background: var(--herdr-wheat-light); color: #5C4716; border: 1px solid #E3BE58; }
.rhandsontable { min-height: 350px !important; }
.handsontable th { background: var(--herdr-pasture) !important; color:#fff !important; }
.handsontable.listbox .wtHolder { overflow-y: auto !important; overscroll-behavior: contain !important; }
.handsontable.listbox { box-shadow: 0 6px 16px rgba(0,0,0,0.18) !important; border-radius: 6px !important; border: 1px solid var(--herdr-pasture) !important; margin-top: 2px !important; }
.handsontable.listbox tr:hover td, .handsontable.listbox tr td.current { background: var(--herdr-pasture-light) !important; color: var(--herdr-pasture-dark) !important; font-weight: 800 !important; }
"

herdr_logo <- HTML('<svg width="30" height="22" viewBox="0 0 30 22"><rect x="1" y="1" width="26" height="16" rx="4" ry="7" fill="#C79A2E"/><circle cx="7" cy="9" r="2.6" fill="#33593B"/><path d="M27 5 L29.5 9 L27 13" fill="#33593B"/></svg>')

BUTTON_LOADING_STATE_JS <- "
Shiny.addCustomMessageHandler('herdr_button_state', function(msg) {
  var btn = document.getElementById(msg.id);
  if (!btn) return;
  if (!btn.dataset.herdrLabel) btn.dataset.herdrLabel = btn.innerHTML;
  if (msg.loading) { btn.innerHTML = '<i class=\"fa fa-circle-notch fa-spin\"></i> ' + msg.text; btn.disabled = true; }
  else { btn.innerHTML = btn.dataset.herdrLabel; btn.disabled = false; }
});
"

SCROLL_CHAINING_GUARD_JS <- "
function stopScrollChaining(e) {
  var isListbox = e.target.closest('.handsontable.listbox');
  if (isListbox) { e.stopPropagation(); return; }
  var isTable = e.target.closest('.handsontable');
  if (isTable) {
    var holder = e.target.closest('.wtHolder');
    if (holder && holder.scrollHeight > holder.clientHeight) {
      var atTop = holder.scrollTop <= 0 && e.deltaY < 0;
      var atBottom = (holder.scrollHeight - holder.clientHeight - holder.scrollTop) <= 2 && e.deltaY > 0;
      if (atTop || atBottom) e.preventDefault();
    }
  }
}
document.addEventListener('wheel', stopScrollChaining, { passive: false, capture: true });
"

# 5. UI DEFINITION

build_standard_tab <- function(id) {
  info <- tables_info[[id]]
  nav_panel(
    title = tagList(icon(info$icon), info$label, uiOutput(paste0("dirty_badge_", id), inline = TRUE)),
    value = id,
    div(class = "p-3",
        uiOutput(paste0("validation_alert_", id)),
        div(class = "d-flex justify-content-between align-items-center mb-2",
            h5(info$label, class = "m-0 text-muted"),
            actionButton(paste0("open_modal_", id), "Add Record", icon = icon("plus"), class = "btn btn-sm btn-outline-success")
        ),
        div(style = "background: var(--herdr-card); border-radius: 8px; border: 1px solid var(--herdr-border); margin-bottom: 1rem;",
            rHandsontableOutput(paste0("table_", id), height = "calc(100vh - 360px)")
        )
    )
  )
}

build_advanced_tab <- function(id) {
  info <- tables_info[[id]]
  nav_panel(
    title = tagList(icon(info$icon), info$label, uiOutput(paste0("dirty_badge_", id), inline = TRUE)),
    value = id,
    div(class = "p-3",
        div(class = "d-flex justify-content-between align-items-center mb-2",
            h5(info$label, class = "m-0 text-muted"),
            actionButton(paste0("open_modal_", id), "Add Record", icon = icon("plus"), class = "btn btn-sm btn-outline-success")
        ),
        div(style = "background: var(--herdr-card); border-radius: 8px; border: 1px solid var(--herdr-border);",
            rHandsontableOutput(paste0("table_", id), height = "calc(100vh - 360px)")
        )
    )
  )
}

ui <- page_sidebar(
  fillable = TRUE, theme = herdr_theme, window_title = "herdr \u2014 Livestock Emissions",
  tags$head(tags$style(HTML(herdr_css))),
  tags$head(tags$script(HTML(paste(BUTTON_LOADING_STATE_JS, SCROLL_CHAINING_GUARD_JS, sep = "\n")))),

  title = tagList(
    div(class = "d-flex align-items-center justify-content-between w-100",
        div(class = "herdr-brand", herdr_logo,
            tags$span(tags$span("herdr", class = "herdr-brand-title"), tags$span("Livestock Emissions", class = "herdr-brand-sub"))),
        div(class = "d-flex align-items-center gap-3",
            input_dark_mode(id = "dark_mode", mode = "light")
        )
    )
  ),

  sidebar = sidebar(
    width = 340, class = "herdr-sidebar", open = "desktop",
    div(class = "step-card",
        div(class = "step-head", span("1", class = "step-tag"), h5("Data Source", class = "step-title m-0")),
        selectInput("data_source", "Load Package Example:", choices = c("Loading..." = "")),
        actionButton("load_data", "Load Example", icon = icon("folder-open"), class = "btn-herdr btn-load"),
        fileInput("upload_csvs", "Or Upload CSV Files:", multiple = TRUE, accept = ".csv", buttonLabel = "Browse..."),
        actionButton("reset_data", "Clear All Data", icon = icon("trash-can"), class = "btn-herdr btn-clear")
    ),
    div(class = "step-card wheat",
        div(class = "step-head", span("2", class = "step-tag"), h5("Configuration", class = "step-title m-0")),
        checkboxInput("auto_cycle", "Use automatic herd cycle", value = FALSE),
        hr(),
        selectInput("farm_country", "Farm Country / Area:", choices = c("Loading..." = "")),
        numericInput("year", "FAO Reference Year:", value = 2022, step = 1)
    ),
    div(class = "step-card wheat",
        div(class = "step-head", span("3", class = "step-tag"), h5("Calculate", class = "step-title m-0")),
        selectInput("function_choice", "Function to Run:", choices = c(
          "Full Assessment (All)" = "generate_impact_assessment",
          "Population" = "calculate_population",
          "Weighted Feed Characteristics" = "calculate_weighted_variable",
          "Net Energy for pregnancy" = "calculate_NE_pregnancy",
          "Net Energy for wool production" = "calculate_NE_wool",
          "Net Energy for work" = "calculate_NE_work",
          "Net Energy for activity" = "calculate_NEa",
          "Net Energy for growth" = "calculate_NEg",
          "Net Energy for lactation" = "calculate_NEl",
          "Net Energy for maintenance" = "calculate_NEm",
          "Gross energy" = "calculate_ge",
          "Metabolizable energy for monogastrics" = "calculate_monogastric_energy",
          "Dry Matter Intake" = "calculate_DMI",
          "CH4 from enteric fermentation" = "calculate_emissions_enteric",
          "Volatile solids" = "calculate_vs",
          "CH4 from Manure" = "calculate_CH4_manure",
          "Direct N2O from manure" = "calculate_N2O_direct_manure",
          "Indirect N2O from leaching" = "calculate_N2O_indirect_leaching",
          "Indirect N2O from volatilization" = "calculate_N2O_indirect_volatilization",
          "Land use" = "calculate_land_use"
        )),
        actionButton("calculate", "Run Selected", icon = icon("play"), class = "btn-herdr btn-run")
    ),
    div(class = "step-card barn",
        div(class = "step-head", span("4", class = "step-tag"), h5("Downloads", class = "step-title m-0")),

        div(class = "mb-3 p-2 rounded", style = "background-color: #FBEAE7; border: 1px solid #EFC7BE; font-size: 0.85rem; color: #7C2417;",
            tags$strong(icon("triangle-exclamation"), " Important:"),
            tags$br(),
            "Data is not saved online. To resume work later, please download your results and input data before closing this page."
        ),

        downloadButton("download", "1. Download Results", class = "btn-herdr btn-download"),
        downloadButton("download_inputs", "2. Download Input Data (ZIP)", class = "btn-herdr btn-download", style = "margin-top: 5px;")
    )
  ),

  navset_card_tab(
    id = "main_tabs", full_screen = TRUE,
    !!!lapply(standard_ids, build_standard_tab),
    nav_panel(
      title = tagList(icon("sliders"), "Advanced (IPCC)"), value = "advanced_tab",
      div(class = "p-3", navset_card_tab(id = "advanced_inner_tabs", !!!lapply(advanced_ids, build_advanced_tab)))
    ),
    nav_panel(
      title = tagList(icon("book"), "Data Dictionary"), value = "dictionary_tab",
      div(class = "p-4",
          div(class = "row",
              div(class = "col-lg-10 mx-auto",
                  h3("Data Dictionary", class = "mb-3", style = "font-family: 'Fraunces', serif; font-weight: 900;"),
                  p(class = "text-muted mb-4", "A quick guide to understanding the variables and abbreviations used in herdr."),
                  div(style = "background: var(--herdr-card); border-radius: 12px; border: 1px solid var(--herdr-border); padding: 1.5rem; max-height: calc(100vh - 230px); overflow-y: auto;",
                      h5(icon("tags"), "Identification & Grouping", class = "mb-3 mt-2 text-success"),
                      tags$dl(class = "row",
                              tags$dt(class = "col-sm-4", "animal_tag"), tags$dd(class = "col-sm-8", "Mandatory: A unique name for this specific group of animals (e.g., 'mature_dairy_cattle')."),
                              tags$dt(class = "col-sm-4", "region / subregion"), tags$dd(class = "col-sm-8", "Optional: General location or subdivision (e.g., 'Spain' / 'Euskadi')."),
                              tags$dt(class = "col-sm-4", "class_flex"), tags$dd(class = "col-sm-8", "Optional: A flexible tag to group animals by trait, breed, or phase."),
                              tags$dt(class = "col-sm-4", "population"), tags$dd(class = "col-sm-8", "Total number of animals in this exact combination of tags."),
                              tags$dt(class = "col-sm-4", "weight_kg"), tags$dd(class = "col-sm-8", "The average live weight of a single animal in kilograms.")
                      ),
                      hr(class = "my-4"),
                      h5(icon("wheat-awn"), "Diet & Feed Characteristics", class = "mb-3 text-warning"),
                      tags$dl(class = "row",
                              tags$dt(class = "col-sm-4", "diet_tag / ingredient"), tags$dd(class = "col-sm-8", "The name of the diet and the specific food item (e.g., 'Corn silage')."),
                              tags$dt(class = "col-sm-4", "ingredient_type"), tags$dd(class = "col-sm-8", "Category: 'forage', 'concentrate', 'milk', or 'milk_replacer'."),
                              tags$dt(class = "col-sm-4", "share (forage, concentrate...)"), tags$dd(class = "col-sm-8", "Percentage of the diet made up of this specific category or ingredient."),
                              tags$dt(class = "col-sm-4", "DM_pct / CP_pct"), tags$dd(class = "col-sm-8", "Dry Matter (%) and Crude Protein (%)."),
                              tags$dt(class = "col-sm-4", "NDF_pct / ASH_pct / EE_pct"), tags$dd(class = "col-sm-8", "Neutral Detergent Fiber (%), Ash (%), and Ether Extract (%)."),
                              tags$dt(class = "col-sm-4", "GE / DE / ME"), tags$dd(class = "col-sm-8", "Gross, Digestible, and Metabolizable Energy (kcal/kg or %)."),
                              tags$dt(class = "col-sm-4", "yield_name / agribalyse_name"), tags$dd(class = "col-sm-8", "Names used to link ingredients with FAOSTAT or Agribalyse databases.")
                      ),
                      hr(class = "my-4"),
                      h5(icon("clipboard-list"), "IPCC Coefficients & Production", class = "mb-3 text-info"),
                      tags$dl(class = "row",
                              tags$dt(class = "col-sm-4", "cfi / ca"), tags$dd(class = "col-sm-8", "IPCC coefficients for maintenance and feeding situation (activity)."),
                              tags$dt(class = "col-sm-4", "c / a / b / c_pregnancy"), tags$dd(class = "col-sm-8", "IPCC constants used to calculate energy for growth and pregnancy."),
                              tags$dt(class = "col-sm-4", "milk_yield / wool_yield"), tags$dd(class = "col-sm-8", "Total milk or wool produced by one animal in a year (in kg)."),
                              tags$dt(class = "col-sm-4", "work_hours"), tags$dd(class = "col-sm-8", "Number of hours per day the animal is used for physical draft/work."),
                              tags$dt(class = "col-sm-4", "pr"), tags$dd(class = "col-sm-8", "Prolificacy Rate: The average number of lambs born per pregnant ewe."),
                              tags$dt(class = "col-sm-4", "single/double_birth_fraction"), tags$dd(class = "col-sm-8", "Proportion of pregnancies that result in a single baby or twins (0 to 1).")
                      ),
                      hr(class = "my-4"),
                      h5(icon("drumstick-bite"), "Monogastrics (Swine & Poultry)", class = "mb-3", style = "color: #d97706;"),
                      tags$dl(class = "row",
                              tags$dt(class = "col-sm-4", "initial / final / adult_weight"), tags$dd(class = "col-sm-8", "Starting, ending, and mature weights of the animal (in kg)."),
                              tags$dt(class = "col-sm-4", "productive_period_days"), tags$dd(class = "col-sm-8", "Number of days the animal is in its active production phase."),
                              tags$dt(class = "col-sm-4", "sows_gestation / lactation"), tags$dd(class = "col-sm-8", "Days a sow is pregnant or nursing her piglets."),
                              tags$dt(class = "col-sm-4", "piglets_born / suckling"), tags$dd(class = "col-sm-8", "Average number of piglets born in a litter or actively nursing."),
                              tags$dt(class = "col-sm-4", "piglet_birth / weaning_weight"), tags$dd(class = "col-sm-8", "Average weight of a piglet at birth or at weaning (in kg)."),
                              tags$dt(class = "col-sm-4", "egg_mass_g_day"), tags$dd(class = "col-sm-8", "Egg mass produced by laying hens.")
                      ),
                      hr(class = "my-4"),
                      h5(icon("recycle"), "Manure Management & LCA", class = "mb-3 text-danger"),
                      tags$dl(class = "row",
                              tags$dt(class = "col-sm-4", "system_base / variant"), tags$dd(class = "col-sm-8", "The main way manure is stored or managed (e.g., 'Liquid/Slurry', 'Pasture')."),
                              tags$dt(class = "col-sm-4", "climate / moisture"), tags$dd(class = "col-sm-8", "Climate zone, subclimate, and moisture level of the region."),
                              tags$dt(class = "col-sm-4", "allocation"), tags$dd(class = "col-sm-8", "Fraction of total manure managed in this system (0 to 1). Must sum to 1."),
                              tags$dt(class = "col-sm-4", "MCF_pct / b_0"), tags$dd(class = "col-sm-8", "Methane Conversion Factor (%) and Maximum methane-producing capacity."),
                              tags$dt(class = "col-sm-4", "EF3 / EF4 / EF5"), tags$dd(class = "col-sm-8", "Emission Factors for calculating direct (EF3) and indirect (EF4, EF5) N2O emissions."),
                              tags$dt(class = "col-sm-4", "frac_gas / frac_leach"), tags$dd(class = "col-sm-8", "Fraction of nitrogen lost through volatilization or leaching/runoff."),
                              tags$dt(class = "col-sm-4", "economic_allocation"), tags$dd(class = "col-sm-8", "Used to allocate impacts between a main product and co-products based on economic value.")
                      )
                  )
              )
          )
      )
    ),
    nav_panel(
      title = tagList(icon("chart-column"), "Results"), value = "results_tab",
      div(class = "p-3",
          uiOutput("results_placeholder"),
          div(class = "results-card",
              div(class = "row mb-3 bg-light p-2 rounded align-items-center",
                  div(class = "col-md-8",
                      selectizeInput("plot_groups", "Group Chart By:",
                                     choices = c("animal_tag", "region", "subregion", "class_flex"),
                                     selected = c("animal_tag", "region", "subregion", "class_flex"),
                                     multiple = TRUE,
                                     options = list(plugins = list('remove_button')))
                  ),
                  div(class = "col-md-4 text-end",
                      downloadButton("download_plot", "Download Chart", class = "btn btn-outline-secondary btn-sm")
                  )
              ),
              plotOutput("main_plot", height = "400px"),
              hr(),
              tableOutput("table_results")
          )
      )
    )
  )
)

# 6. SERVER LOGIC

server <- function(input, output, session) {

  # Session isolation
  session_dir <- tempfile(pattern = "herdr_session_")
  dir.create(session_dir)
  user_data_tmp <- file.path(session_dir, "user_data")
  dir.create(user_data_tmp)

  session$onSessionEnded(function() {
    unlink(session_dir, recursive = TRUE)
  })

  if (dir.exists("user_data")) {
    file.copy(list.files("user_data", full.names = TRUE), user_data_tmp, overwrite = TRUE)
  }

  example_paths <- system.file("Examples", package = "herdr")
  updateSelectInput(session, "data_source", choices = c("My current data" = "current", if (example_paths != "") list.dirs(example_paths, full.names = FALSE, recursive = FALSE) else c()))

  # Load parquet data
  observe({
    req(ui_trigger() > 0)
    parquet_path <- file.path(user_data_tmp, "fao_crops.parquet")
    fallback_countries <- c("Spain", "France", "Germany", "United States of America")

    if (!file.exists(parquet_path)) {
      updateSelectInput(session, "farm_country", choices = fallback_countries)
      return(invisible(NULL))
    }

    try({
      df_area <- arrow::read_parquet(parquet_path, col_select = c("Area"))
      countries <- sort(unique(na.omit(df_area$Area)))
      selected_country <- if ("Spain" %in% countries) "Spain" else countries[1]

      updateSelectInput(session, "farm_country", choices = countries, selected = selected_country)

      ds <- arrow::open_dataset(parquet_path)
      cols <- names(ds)
      year_cols <- grep("^Y[0-9]{4}$", cols, value = TRUE)

      if (length(year_cols) > 0) {
        years <- as.numeric(gsub("Y", "", year_cols))
        updateNumericInput(session, "year", min = min(years), max = max(years))
      }
    })
  })

  rv <- reactiveValues()
  dirty <- reactiveValues()
  ui_trigger <- reactiveVal(0)
  model_data <- reactiveVal(NULL)
  trigger_count <- 0

  load_all_data <- function(reset = FALSE) {
    for (id in names(tables_info)) {
      path <- file.path(user_data_tmp, tables_info[[id]]$file)
      loaded <- read_clean(path)

      if (id == "diet_ingr" && !"custom_yield_kg_ha" %in% names(loaded)) {
        loaded$custom_yield_kg_ha <- NA_character_
      }
      rv[[id]] <- if (reset) loaded[0, ] else loaded
      dirty[[id]] <- FALSE
    }
    model_data(NULL)
    trigger_count <<- trigger_count + 1
    ui_trigger(trigger_count)
  }

  isolate(load_all_data(reset = FALSE))

  observeEvent(input$load_data, {
    req(input$data_source)
    try({
      if (input$data_source != "current") {
        file.copy(list.files(file.path(example_paths, input$data_source), full.names = TRUE), user_data_tmp, overwrite = TRUE)
      }
      load_all_data(reset = FALSE)
      showNotification(paste("Loaded:", input$data_source), type = "message")
      nav_select("main_tabs", "census")
    })
  })

  observeEvent(input$reset_data, {
    load_all_data(reset = TRUE)
    showNotification("All data cleared.", type = "message")
    nav_select("main_tabs", "census")
  })

  observeEvent(input$upload_csvs, {
    req(input$upload_csvs)
    map <- setNames(names(tables_info), sapply(tables_info, function(x) x$file))
    count <- 0

    for (i in seq_len(nrow(input$upload_csvs))) {
      filename <- input$upload_csvs$name[i]
      if (filename %in% names(map)) {
        id <- map[[filename]]
        dest <- file.path(user_data_tmp, filename)
        file.copy(input$upload_csvs$datapath[i], dest, overwrite = TRUE)

        temp_df <- read_clean(dest)
        if (id == "diet_ingr" && !"custom_yield_kg_ha" %in% names(temp_df)) {
          temp_df$custom_yield_kg_ha <- NA_character_
        }

        rv[[id]] <- temp_df
        dirty[[id]] <- FALSE
        count <- count + 1
      }
    }
    if (count > 0) {
      trigger_count <<- trigger_count + 1
      ui_trigger(trigger_count)
      showNotification(paste(count, "files updated!"), type = "message")
      nav_select("main_tabs", "census")
    }
  })

  observe({
    if (isTRUE(input$auto_cycle)) nav_show("main_tabs", "repro") else nav_hide("main_tabs", "repro")
  })

  render_validation_alert <- function(id) {
    req(rv$census, rv[[id]])
    if (id == "census" || !"animal_tag" %in% names(rv[[id]])) return(NULL)

    census_keys <- unique(build_keys(rv$census))
    table_keys  <- unique(build_keys(rv[[id]]))
    if (length(census_keys) == 0) return(NULL)

    alerts <- list()
    missing <- if (id %in% c("def", "mono")) {
      setdiff(census_keys, union(unique(build_keys(rv$def)), unique(build_keys(rv$mono))))
    } else {
      setdiff(census_keys, table_keys)
    }

    if (length(missing) > 0) {
      alerts[[length(alerts)+1]] <- div(class = "herdr-alert herdr-alert-danger", icon("circle-xmark"), HTML(paste("<b>Missing:</b>", paste(missing, collapse = ", "))))
    }

    extra <- setdiff(table_keys, census_keys)
    if (length(extra) > 0) {
      alerts[[length(alerts)+1]] <- div(class = "herdr-alert herdr-alert-warning", icon("triangle-exclamation"), HTML(paste("<b>Unrecognized:</b>", paste(extra, collapse = ", "))))
    }

    if (length(alerts) > 0) do.call(tagList, alerts) else NULL
  }

  lapply(names(tables_info), function(id) {

    output[[paste0("table_", id)]] <- renderRHandsontable({
      req(ui_trigger())
      isolate({
        df <- rv[[id]]
        if (is.null(df) || ncol(df) == 0) return(NULL)

        tbl <- rhandsontable(df, rowHeaders = NULL, width = "100%", stretchH = "all",
                             minSpareRows = 1,
                             allowInsertColumn = FALSE, allowRemoveColumn = FALSE,
                             allowInsertRow = TRUE, allowRemoveRow = TRUE)

        tbl <- hot_cols(tbl, columnSorting = TRUE, fixedColumnsLeft = if(tables_info[[id]]$fixed > 0) tables_info[[id]]$fixed else NULL)
        tbl <- apply_dynamic_dropdowns(tbl, id, rv)
        if (id == "manure") tbl <- apply_manure_cascade_dropdowns(tbl, rv$ipcc_mm, df)

        if (id == "diet_ingr" && "custom_yield_kg_ha" %in% names(df)) {
          tbl <- hot_col(tbl, col = "custom_yield_kg_ha", type = "numeric", format = "0.0", allowInvalid = FALSE)
        }
        tbl
      })
    })

    observeEvent(input[[paste0("table_", id)]], {
      try({
        df <- suppressWarnings(hot_to_r(input[[paste0("table_", id)]]))
        if (is.data.frame(df)) { rv[[id]] <- df; dirty[[id]] <- TRUE }
      }, silent = TRUE)
    })

    observeEvent(input[[paste0("open_modal_", id)]], {
      df <- rv[[id]]
      req(df)

      get_dropdown_choices <- function(col_name) {
        if (id == "manure" && col_name %in% MANURE_CASCADE_COLUMNS) {
          if (col_name == MANURE_CASCADE_COLUMNS[1]) {
            return(c("", sort(as.character(unique(na.omit(rv$ipcc_mm[[col_name]]))))))
          } else { return(c("")) }
        }

        if (id %in% names(dynamic_dropdowns) && col_name %in% names(dynamic_dropdowns[[id]])) {
          rule <- dynamic_dropdowns[[id]][[col_name]]
          source_df <- rv[[rule$source_table]]
          if (is.null(source_df)) return(NULL)

          options <- if (!is.null(rule$filter_column) && !is.null(rule$filter_value)) {
            source_df[[rule$extract_column]][source_df[[rule$filter_column]] == rule$filter_value]
          } else { source_df[[rule$extract_column]] }
          return(c("", sort(as.character(unique(na.omit(options))))))
        }
        return(NULL)
      }

      input_fields <- lapply(names(df), function(col_name) {
        info_text <- if (col_name %in% names(modal_tooltips)) modal_tooltips[[col_name]] else ""
        label_html <- if (info_text != "") {
          tagList(col_name, tags$i(class = "fa-solid fa-circle-info ms-1 text-muted", title = info_text, style = "cursor:help;"))
        } else { col_name }

        choices <- get_dropdown_choices(col_name)
        if (!is.null(choices)) {
          selectInput(paste0("modal_input_", id, "_", col_name), label = label_html, choices = choices, selected = "")
        } else if (is.numeric(df[[col_name]])) {
          numericInput(paste0("modal_input_", id, "_", col_name), label = label_html, value = NA)
        } else {
          textInput(paste0("modal_input_", id, "_", col_name), label = label_html, value = "")
        }
      })

      showModal(modalDialog(
        title = tagList(icon("plus-circle"), paste("Add Record to", tables_info[[id]]$label)),
        div(class = "row", lapply(input_fields, function(f) div(class = "col-md-6", f))),
        footer = tagList(modalButton("Cancel"), actionButton(paste0("save_modal_", id), "Save Record", class = "btn btn-success")),
        size = "l"
      ))
    })

    observeEvent(input[[paste0("save_modal_", id)]], {
      df <- rv[[id]]
      col_key <- names(df)[1]
      df_clean <- if (nrow(df) > 0) df[!is.na(df[[col_key]]) & trimws(as.character(df[[col_key]])) != "", ] else df
      template_row <- if (nrow(df_clean) > 0) df_clean[1, ] else df[1, ]
      new_row <- template_row

      for (col_name in names(df)) {
        val <- input[[paste0("modal_input_", id, "_", col_name)]]
        if (is.numeric(df[[col_name]])) {
          new_row[[col_name]] <- if (is.null(val) || is.na(val) || val == "") NA_real_ else as.numeric(val)
        } else {
          new_row[[col_name]] <- if (is.null(val) || is.na(val) || val == "") NA_character_ else as.character(val)
        }
      }

      rv[[id]] <- dplyr::bind_rows(df_clean, new_row)
      dirty[[id]] <- TRUE
      trigger_count <<- trigger_count + 1
      ui_trigger(trigger_count)
      removeModal()
      showNotification("Record added successfully", type = "message")
    })

    output[[paste0("dirty_badge_", id)]] <- renderUI({ if (isTRUE(dirty[[id]])) tags$span(class = "dirty-dot", title = "Unsaved") else NULL })
    output[[paste0("validation_alert_", id)]] <- renderUI(render_validation_alert(id))
  })

  lapply(seq_along(MANURE_CASCADE_COLUMNS)[-length(MANURE_CASCADE_COLUMNS)], function(i) {
    col_current <- MANURE_CASCADE_COLUMNS[i]
    observeEvent(input[[paste0("modal_input_manure_", col_current)]], {
      req(rv$ipcc_mm)
      valid_df <- rv$ipcc_mm

      for (j in 1:i) {
        col_j <- MANURE_CASCADE_COLUMNS[j]
        val_j <- input[[paste0("modal_input_manure_", col_j)]]
        available_choices <- sort(as.character(unique(na.omit(valid_df[[col_j]]))))

        if (length(available_choices) > 0) {
          if (is.null(val_j) || val_j == "") {
            for (k in (j + 1):length(MANURE_CASCADE_COLUMNS)) {
              updateSelectInput(session, paste0("modal_input_manure_", MANURE_CASCADE_COLUMNS[k]), choices = c(""), selected = "")
            }
            return()
          }
          valid_df <- valid_df[which(as.character(valid_df[[col_j]]) == as.character(val_j)), , drop = FALSE]
        }
      }

      for (k in (i + 1):length(MANURE_CASCADE_COLUMNS)) {
        col_k <- MANURE_CASCADE_COLUMNS[k]
        choices_k <- sort(as.character(unique(na.omit(valid_df[[col_k]]))))

        if (length(choices_k) > 0) {
          updateSelectInput(session, paste0("modal_input_manure_", col_k), choices = c("", choices_k), selected = "")
          if (k + 1 <= length(MANURE_CASCADE_COLUMNS)) {
            for (m in (k + 1):length(MANURE_CASCADE_COLUMNS)) {
              updateSelectInput(session, paste0("modal_input_manure_", MANURE_CASCADE_COLUMNS[m]), choices = c(""), selected = "")
            }
          }
          break
        } else {
          updateSelectInput(session, paste0("modal_input_manure_", col_k), choices = c(""), selected = "")
        }
      }
    }, ignoreInit = TRUE)
  })

  observeEvent(input$calculate, {
    nav_select("main_tabs", "results_tab")
    session$sendCustomMessage("herdr_button_state", list(id = "calculate", loading = TRUE, text = "Running..."))
    on.exit(session$sendCustomMessage("herdr_button_state", list(id = "calculate", loading = FALSE, text = "")), add = TRUE)

    withProgress(message = "Running selected function...", value = 0, {
      incProgress(0.2, detail = "Saving tables...")

      for (id in names(tables_info)) {
        save_clean(rv[[id]], file.path(user_data_tmp, tables_info[[id]]$file))
        dirty[[id]] <- FALSE
      }

      original_wd <- getwd()
      setwd(session_dir)
      on.exit(setwd(original_wd), add = TRUE)

      incProgress(0.4, detail = "Calculating...")
      shown_warnings <- c()

      res <- tryCatch({
        withCallingHandlers(
          expr = {
            func_name <- input$function_choice
            func <- match.fun(func_name)
            func_args <- names(formals(func))

            args_to_pass <- list()
            if ("saveoutput" %in% func_args) args_to_pass$saveoutput <- FALSE
            if ("automatic_cycle" %in% func_args) args_to_pass$automatic_cycle <- input$auto_cycle
            if ("farm_country" %in% func_args) args_to_pass$farm_country <- input$farm_country
            if ("year" %in% func_args) args_to_pass$year <- input$year

            do.call(func, args_to_pass)
          },
          warning = function(w) {
            if (!(w$message %in% shown_warnings)) {
              showNotification(w$message, type = "warning", duration = 8)
              shown_warnings <<- c(shown_warnings, w$message)
            }
            invokeRestart("muffleWarning")
          }
        )
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
        NULL
      })

      incProgress(0.4, detail = "Done")
      if (!is.null(res)) showNotification(paste(input$function_choice, "executed!"), type = "message")
      model_data(res)
    })
  })

  output$results_placeholder <- renderUI({
    if (is.null(model_data())) div(class = "empty-state", h4("No results yet"), p("Run the model to see output.")) else NULL
  })

  current_plot <- reactive({
    req(model_data(), input$plot_groups)
    herdr::plot_herdr_results(df = model_data(), group_cols = input$plot_groups, func_name = input$function_choice)
  })

  output$main_plot <- renderPlot({
    p <- current_plot()
    if (is.null(p)) return(NULL)
    return(p)
  }, bg = "transparent")

  output$download_plot <- downloadHandler(
    filename = function() paste0("herdr_chart_", input$function_choice, "_", Sys.Date(), ".png"),
    content = function(file) {
      p <- current_plot()
      req(p)
      ggplot2::ggsave(filename = file, plot = p, device = "png", width = 10, height = 6, dpi = 300, bg = "white")
    }
  )

  output$table_results <- renderTable({ req(model_data()); model_data() }, digits = 6)

  output$download <- downloadHandler(
    filename = function() paste0("herdr_results_", Sys.Date(), ".csv"),
    content = function(file) write_csv(model_data(), file)
  )

  # Download ZIP
  output$download_inputs <- downloadHandler(
    filename = function() paste0("herdr_inputs_", Sys.Date(), ".zip"),
    content = function(file) {
      temp_zip_dir <- tempfile("zip_dir_")
      dir.create(temp_zip_dir)

      on.exit(unlink(temp_zip_dir, recursive = TRUE), add = TRUE)

      for (id in names(tables_info)) {
        save_clean(rv[[id]], file.path(temp_zip_dir, tables_info[[id]]$file))
      }

      archivos_a_comprimir <- list.files(temp_zip_dir, full.names = TRUE)
      zip::zip(zipfile = file, files = archivos_a_comprimir, mode = "cherry-pick")
    },
    contentType = "application/zip"
  )
}

shinyApp(ui = ui, server = server)
