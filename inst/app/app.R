library(shiny)
library(bslib)
library(herdr)
library(rhandsontable)
library(dplyr)
library(readr)

# ============================================================================
# 1. CONFIGURATION
# ============================================================================

tables_info <- list(
  census = list(
    file  = "livestock_census.csv", fixed = 0, icon = "clipboard-list",
    label = "Census",
    help  = "Enter the total head count for each animal category in your herd."
  ),
  diet_prof = list(
    file  = "diet_profiles.csv", fixed = 4, icon = "utensils",
    label = "Diet Profiles",
    help  = "Percentage of forage, concentrate, milk and/milk replacer making up the diet"
  ),
  diet_ingr = list(
    file  = "diet_ingredients.csv", fixed = 4, icon = "wheat-awn",
    label = "Ingredients",
    help  = "Percentage of each ingredient making up the diet for different animal tags."
  ),
  def = list(
    file  = "livestock_definitions.csv", fixed = 4, icon = "id-card",
    label = "Definitions",
    help  = "Define physiological traits for ruminants using IPCC coefficients and available data."
  ),
  mono = list(
    file  = "monogastric_definitions.csv", fixed = 4, icon = "drumstick-bite",
    label = "Monogastrics",
    help  = "Specific parameters and performance metrics for poultry and swine."
  ),
  weights = list(
    file  = "livestock_weights.csv", fixed = 4, icon = "weight-hanging",
    label = "Weights",
    help  = "Specify the initial and final weights for growing and finishing phases."
  ),
  manure = list(
    file  = "manure_management.csv", fixed = 4, icon = "recycle",
    label = "Manure",
    help  = "Percentage of manure managed in different systems. Must sum 1 per animal_tag."
  ),
  repro = list(
    file  = "reproduction_parameters.csv", fixed = 0, icon = "dna",
    label = "Reproduction",
    help  = "Reproductive rates, herd replacement rates, and mortality percentages."
  ),
  feed_char = list(
    file  = "feed_characteristics.csv", fixed = 1, icon = "flask",
    label = "Feed Char.",
    help  = "Nutritional characteristics of each feed ingredient (e.g., dry matter, crude protein)."
  ),
  forage = list(
    file  = "forage_yields.csv", fixed = 1, icon = "seedling",
    label = "Forage Yields",
    help  = "Default crop and forage yields based on the selected region."
  ),
  ipcc_coef = list(
    file  = "ipcc_coefficients.csv", fixed = 2, icon = "square-root-variable",
    label = "IPCC Coefficients",
    help  = "IPCC constants for enteric fermentation and manure equations."
  ),
  ipcc_mm = list(
    file  = "ipcc_mm.csv", fixed = 2, icon = "warehouse",
    label = "IPCC Manure Mgt.",
    help  = "Methane conversion factors (MCF), emission factors (EF) and fractions for various manure management systems."
  ),
  mapping = list(
    file  = "mapping.csv", fixed = 1, icon = "diagram-project",
    label = "Mapping",
    help  = "Internal mapping."
  )
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

standard_ids <- c("census", "diet_prof", "diet_ingr", "def", "mono", "weights", "manure", "repro")
advanced_ids <- c("feed_char", "forage", "ipcc_coef", "ipcc_mm", "mapping")

KPI_LABELS <- c(
  "total_emissions" = "Total Emissions (Gg CO2e)",
  "ch4_enteric"     = "CH4 Enteric Emissions (Gg)",
  "ch4_manure"      = "CH4 Manure Emissions (Gg)",
  "n2o_manure"      = "N2O Manure Emissions (Gg)"
)
KPI_PRIORITY_KEYWORDS <- c("total", "ghg", "co2", "ch4", "n2o", "emission")

# ============================================================================
# 2. DATA HELPERS
# ============================================================================

read_clean <- function(path) {
  if (!file.exists(path)) return(data.frame())
  read_csv(path, show_col_types = FALSE) %>% mutate(across(where(is.logical), as.character))
}

save_clean <- function(df, path) {
  if (is.null(df) || nrow(df) == 0) return(invisible(NULL))
  key_column <- intersect(c("animal_tag", "diet_tag"), names(df))[1]
  if (!is.na(key_column)) df <- df %>% filter(!is.na(.data[[key_column]]) & .data[[key_column]] != "")

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

compute_kpis <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
  numeric_columns <- names(df)[vapply(df, is.numeric, logical(1))]
  if (length(numeric_columns) == 0) return(NULL)

  is_priority <- vapply(tolower(numeric_columns), function(c) any(vapply(KPI_PRIORITY_KEYWORDS, grepl, logical(1), x = c)), logical(1))
  chosen_columns <- head(unique(c(numeric_columns[is_priority], numeric_columns)), 4)

  lapply(chosen_columns, function(col) {
    display_name <- if (col %in% names(KPI_LABELS)) KPI_LABELS[[col]] else gsub("_", " ", col)
    list(name = display_name, value = sum(df[[col]], na.rm = TRUE))
  })
}

# ============================================================================
# 3. HANDSONTABLE DROPDOWN HELPERS
# ============================================================================

apply_dynamic_dropdowns <- function(tbl, id, rv) {
  if (!id %in% names(dynamic_dropdowns)) return(tbl)
  for (target_col in names(dynamic_dropdowns[[id]])) {
    rule <- dynamic_dropdowns[[id]][[target_col]]
    source_df <- rv[[rule$source_table]]

    if (!target_col %in% names(rv[[id]])) next
    if (is.null(source_df) || nrow(source_df) == 0) next
    if (!rule$extract_column %in% names(source_df)) next
    if (!is.null(rule$filter_column) && !rule$filter_column %in% names(source_df)) next

    options <- if (!is.null(rule$filter_column) && !is.null(rule$filter_value)) {
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
      if (cleanVal !== '' && cleanVal !== 'na') valid = valid.filter(function(r) { return cln(r[dbName]) === cleanVal; });
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

  lookup_json <- jsonlite::toJSON(ipcc_mm_df[, MANURE_CASCADE_COLUMNS, drop = FALSE], dataframe = "rows", auto_unbox = TRUE, na = "null")
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
          var row = change[0]; var prop = change[1];
          var changedVisualCol = hot.propToCol(prop);
          if (changedVisualCol == null) changedVisualCol = prop;
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

# ============================================================================
# 4. THEME & CSS
# ============================================================================

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

# ============================================================================
# 5. UI BUILDERS
# ============================================================================

build_standard_tab <- function(id) {
  info <- tables_info[[id]]
  nav_panel(
    title = tagList(icon(info$icon), info$label, uiOutput(paste0("dirty_badge_", id), inline = TRUE)),
    value = id,
    div(class = "p-3",
        uiOutput(paste0("validation_alert_", id)),
        div(style = "background: var(--herdr-card); border-radius: 8px; border: 1px solid var(--herdr-border); margin-bottom: 1rem;",
            rHandsontableOutput(paste0("table_", id), height = "calc(100vh - 320px)")
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
        input_dark_mode(id = "dark_mode", mode = "light")
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
        numericInput("year", "FAO Reference Year:", value = 2022, min = 1961, max = 2026, step = 1)
    ),
    div(class = "step-card wheat",
        div(class = "step-head", span("3", class = "step-tag"), h5("Calculate", class = "step-title m-0")),
        actionButton("calculate", "Run herdr Model", icon = icon("play"), class = "btn-herdr btn-run")
    ),
    div(class = "step-card barn",
        div(class = "step-head", span("4", class = "step-tag"), h5("Downloads", class = "step-title m-0")),
        downloadButton("download", "Download Results", class = "btn-herdr btn-download")
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
      title = tagList(icon("chart-column"), "Results"), value = "results_tab",
      div(class = "p-3", uiOutput("results_placeholder"), uiOutput("kpi_cards"), div(class = "results-card", tableOutput("table_results")))
    )
  )
)

# ============================================================================
# 6. SERVER
# ============================================================================

server <- function(input, output, session) {

  if (!dir.exists("user_data")) herdr::herdr_init()

  example_paths <- system.file("Examples", package = "herdr")
  updateSelectInput(session, "data_source", choices = c("My current data" = "current", if (example_paths != "") list.dirs(example_paths, full.names = FALSE, recursive = FALSE) else c()))

  observe({
    yields_path <- "user_data/fao_crop_yields.csv"
    if (!file.exists(yields_path)) {
      updateSelectInput(session, "farm_country", choices = c("Spain", "France", "Germany", "United States of America"))
      return()
    }
    try({
      df <- readr::read_csv(yields_path, col_select = Area, show_col_types = FALSE)
      countries <- sort(unique(na.omit(df$Area)))
      updateSelectInput(session, "farm_country", choices = countries, selected = if ("Spain" %in% countries) "Spain" else countries[1])
    })
  })

  rv <- reactiveValues()
  dirty <- reactiveValues()
  ui_trigger <- reactiveVal(0)
  model_data <- reactiveVal(NULL)

  trigger_count <- 0

  load_all_data <- function(reset = FALSE) {
    for (id in names(tables_info)) {
      path <- file.path("user_data", tables_info[[id]]$file)
      loaded <- read_clean(path)
      rv[[id]] <- if (reset) loaded[0, ] else loaded
      dirty[[id]] <- FALSE
    }
    model_data(NULL)
    trigger_count <<- trigger_count + 1
    ui_trigger(trigger_count)
  }

  load_all_data(reset = FALSE)

  observeEvent(input$load_data, {
    req(input$data_source)
    try({
      if (input$data_source != "current") file.copy(list.files(file.path(example_paths, input$data_source), full.names = TRUE), "user_data", overwrite = TRUE)
      load_all_data(reset = FALSE)
      showNotification(paste("\u2705 Loaded:", input$data_source), type = "message")
      nav_select("main_tabs", "census")
    })
  })

  observeEvent(input$reset_data, {
    load_all_data(reset = TRUE)
    showNotification("\u2705 All data cleared.", type = "message")
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
        dest <- file.path("user_data", filename)
        file.copy(input$upload_csvs$datapath[i], dest, overwrite = TRUE)
        rv[[id]] <- read_clean(dest)
        dirty[[id]] <- FALSE
        count <- count + 1
      }
    }
    if (count > 0) {
      trigger_count <<- trigger_count + 1
      ui_trigger(trigger_count)
      showNotification(paste("\u2705", count, "files updated!"), type = "message")
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
    missing <- if (id %in% c("def", "mono")) setdiff(census_keys, union(unique(build_keys(rv$def)), unique(build_keys(rv$mono)))) else setdiff(census_keys, table_keys)
    if (length(missing) > 0) alerts[[length(alerts)+1]] <- div(class = "herdr-alert herdr-alert-danger", icon("circle-xmark"), HTML(paste("<b>Missing:</b>", paste(missing, collapse = ", "))))

    extra <- setdiff(table_keys, census_keys)
    if (length(extra) > 0) alerts[[length(alerts)+1]] <- div(class = "herdr-alert herdr-alert-warning", icon("triangle-exclamation"), HTML(paste("<b>Unrecognized:</b>", paste(extra, collapse = ", "))))

    if (length(alerts) > 0) do.call(tagList, alerts) else NULL
  }

  lapply(names(tables_info), function(id) {
    output[[paste0("table_", id)]] <- renderRHandsontable({
      req(ui_trigger())
      isolate({
        df <- rv[[id]]
        if (is.null(df) || ncol(df) == 0) return(NULL)
        tbl <- rhandsontable(df, rowHeaders = NULL, width = "100%", stretchH = "all", minSpareRows = 1, allowInsertColumn = FALSE, allowRemoveColumn = FALSE)
        tbl <- hot_cols(tbl, columnSorting = TRUE, fixedColumnsLeft = if(tables_info[[id]]$fixed > 0) tables_info[[id]]$fixed else NULL)
        tbl <- apply_dynamic_dropdowns(tbl, id, rv)
        if (id == "manure") tbl <- apply_manure_cascade_dropdowns(tbl, rv$ipcc_mm, df)
        tbl
      })
    })

    observeEvent(input[[paste0("table_", id)]], {
      try({
        df <- suppressWarnings(hot_to_r(input[[paste0("table_", id)]]))
        if (is.data.frame(df)) { rv[[id]] <- df; dirty[[id]] <- TRUE }
      }, silent = TRUE)
    })

    output[[paste0("dirty_badge_", id)]] <- renderUI({ if (isTRUE(dirty[[id]])) tags$span(class = "dirty-dot", title = "Unsaved") else NULL })
    output[[paste0("validation_alert_", id)]] <- renderUI(render_validation_alert(id))
  })

  # 🔥 CÁLCULO DE RESULTADOS ARREGLADO (CATCH WARNINGS CORRECTAMENTE) 🔥
  observeEvent(input$calculate, {
    nav_select("main_tabs", "results_tab")
    session$sendCustomMessage("herdr_button_state", list(id = "calculate", loading = TRUE, text = "Running..."))
    on.exit(session$sendCustomMessage("herdr_button_state", list(id = "calculate", loading = FALSE, text = "")), add = TRUE)

    withProgress(message = "Running herdr model", value = 0, {
      incProgress(0.2, detail = "Saving tables...")
      for (id in names(tables_info)) { save_clean(rv[[id]], file.path("user_data", tables_info[[id]]$file)); dirty[[id]] <- FALSE }

      incProgress(0.4, detail = "Calculating...")
      shown_warnings <- c()

      res <- tryCatch({
        # withCallingHandlers intercepta el warning, hace su magia, y DEJA QUE LA FUNCIÓN SIGA.
        withCallingHandlers(
          expr = generate_impact_assessment(
            automatic_cycle = input$auto_cycle,
            saveoutput = FALSE,
            farm_country = input$farm_country,
            year = input$year
          ),
          warning = function(w) {
            if (!(w$message %in% shown_warnings)) {
              showNotification(paste("\u26A0", w$message), type = "warning", duration = 8)
              shown_warnings <<- c(shown_warnings, w$message)
            }
            invokeRestart("muffleWarning") # Esto silencia la consola, pero ya no rompe nada
          }
        )
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
        NULL
      })

      incProgress(0.4, detail = "Done")
      if (!is.null(res)) showNotification("\u2705 Results ready!", type = "message")
      model_data(res)
    })
  })

  output$table_results <- renderTable({ req(model_data()); model_data() }, digits = 6)
  output$results_placeholder <- renderUI({ if (is.null(model_data())) div(class = "empty-state", h4("No results yet"), p("Run the model to see output.")) else NULL })

  output$kpi_cards <- renderUI({
    kpis <- compute_kpis(model_data())
    if (is.null(kpis)) return(NULL)
    boxes <- lapply(seq_along(kpis), function(i) {
      value_box(title = kpis[[i]]$name, value = format(round(kpis[[i]]$value, 2), big.mark = ","), showcase = icon(if (i==1) "leaf" else "chart-simple"), theme = if (i%%2==1) "primary" else "warning")
    })
    div(class = "kpi-row", do.call(layout_column_wrap, c(list(width = "250px"), boxes)))
  })

  output$download <- downloadHandler(filename = function() paste0("herdr_results_", Sys.Date(), ".csv"), content = function(file) write_csv(model_data(), file))
}

shinyApp(ui = ui, server = server)
