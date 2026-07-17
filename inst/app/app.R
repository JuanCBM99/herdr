library(shiny)
library(bslib)
library(herdr)
library(rhandsontable)
library(dplyr)
library(readr)

# ==========================================
# 1. HELPER FUNCTIONS & CONFIGURATION
# ==========================================

tables_info <- list(
  census    = list(file = "livestock_census.csv",         fixed = 0, icon = "clipboard-list", label = "Census", help = "Enter the total head count for each animal category in your herd."),
  diet_prof = list(file = "diet_profiles.csv",             fixed = 4, icon = "utensils",       label = "Diet Profiles", help = "Percentage of forage, concentrate, milk and/milk replacer making up the diet"),
  diet_ingr = list(file = "diet_ingredients.csv",          fixed = 4, icon = "wheat-awn",      label = "Ingredients", help = "Percentage of each ingredient making up the diet for different animal tags."),
  def       = list(file = "livestock_definitions.csv",     fixed = 4, icon = "id-card",        label = "Definitions", help = "Define physiological traits for ruminants using IPCC coefficients and available data."),
  mono      = list(file = "monogastric_definitions.csv",   fixed = 4, icon = "drumstick-bite", label = "Monogastrics", help = "Specific parameters and performance metrics for poultry and swine."),
  weights   = list(file = "livestock_weights.csv",         fixed = 4, icon = "weight-hanging", label = "Weights", help = "Specify the initial and final weights for growing and finishing phases."),
  manure    = list(file = "manure_management.csv",         fixed = 4, icon = "recycle",        label = "Manure", help = "Percentage of manure managed in different systems. Must sum 1 per animal_tag."),
  repro     = list(file = "reproduction_parameters.csv",   fixed = 0, icon = "dna",            label = "Reproduction", help = "Reproductive rates, herd replacement rates, and mortality percentages."),
  feed_char = list(file = "feed_characteristics.csv",      fixed = 1, icon = "flask",          label = "Feed Char.", help = "Nutritional characteristics of each feed ingredient (e.g., dry matter, crude protein)."),
  forage    = list(file = "forage_yields.csv",             fixed = 1, icon = "seedling",       label = "Forage Yields", help = "Default crop and forage yields based on the selected region."),
  ipcc_coef = list(file = "ipcc_coefficients.csv",         fixed = 2, icon = "square-root-variable", label = "IPCC Coefficients", help = "IPCC constants for enteric fermentation and manure equations."),
  ipcc_mm   = list(file = "ipcc_mm.csv",                    fixed = 2, icon = "warehouse",      label = "IPCC Manure Mgt.", help = "Methane conversion factors (MCF), emission factors (EF) and fractions for various manure management systems."),
  mapping   = list(file = "mapping.csv",                    fixed = 1, icon = "diagram-project",label = "Mapping", help = "Internal mapping.")
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
    b_0 = list(source_table = "ipcc_coef", filter_column = "coefficient", filter_value = "b_0", extract_column = "description")
  )
)

standard_ids <- c("census", "diet_prof", "diet_ingr", "def", "mono", "weights", "manure", "repro")
advanced_ids <- c("feed_char", "forage", "ipcc_coef", "ipcc_mm", "mapping")

read_clean <- function(path) {
  if (!file.exists(path)) return(data.frame())
  read_csv(path, show_col_types = FALSE) %>% mutate(across(where(is.logical), as.character))
}

save_clean <- function(df, path) {
  if (is.null(df) || nrow(df) == 0) return()
  if ("animal_tag" %in% names(df)) df <- df %>% filter(!is.na(animal_tag) & animal_tag != "")
  else if ("diet_tag" %in% names(df)) df <- df %>% filter(!is.na(diet_tag) & diet_tag != "")

  df %>%
    mutate(across(where(is.logical), ~ NA_character_)) %>%
    mutate(across(where(is.character), ~ na_if(., ""))) %>%
    mutate(across(where(is.character), ~ na_if(., "NA"))) %>%
    write_csv(path, na = "")
}

compute_kpis <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
  num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  if (length(num_cols) == 0) return(NULL)

  keywords <- c("total", "ghg", "co2", "ch4", "n2o", "emission")
  is_priority <- vapply(tolower(num_cols), function(x) any(vapply(keywords, grepl, logical(1), x = x)), logical(1))
  chosen <- head(unique(c(num_cols[is_priority], num_cols)), 4)

  nombres_personalizados <- c(
    "total_emissions" = "Total Emissions (Gg CO2e)",
    "ch4_enteric"     = "CH4 Enteric Emissions (Gg)",
    "ch4_manure"      = "CH4 Manure Emissions (Gg)",
    "n2o_manure"      = "N2O Manure Emissions (Gg)"
  )

  lapply(chosen, function(col) {
    texto_mostrar <- if (col %in% names(nombres_personalizados)) nombres_personalizados[[col]] else gsub("_", " ", col)
    list(name = texto_mostrar, value = sum(df[[col]], na.rm = TRUE))
  })
}

# ==========================================
# 2. THEME & CSS
# ==========================================

herdr_theme <- bs_theme(
  version = 5, bg = "#F7F5EF", fg = "#262620", primary = "#33593B", secondary = "#C79A2E",
  success = "#33593B", warning = "#C79A2E", danger = "#A63C30",
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

/* --- Layout & Components --- */
.navbar { box-shadow: 0 2px 10px rgba(34,61,40,0.12); }
.navbar .navbar-brand { display:flex; align-items:center; width:100%; }
.herdr-navbar-inner { display:flex; align-items:center; justify-content:space-between; width:100%; gap:1rem; }
.herdr-brand { display:flex; align-items:center; gap:.65rem; }
.herdr-brand-title { font-family:'Fraunces',serif; font-weight:900; font-size:1.35rem; letter-spacing:.2px; }
.herdr-brand-sub { display:block; font-family:'Inter',sans-serif; font-weight:400; font-size:.72rem; opacity:.85; letter-spacing:.4px; text-transform:uppercase; }

.herdr-sidebar { background: var(--herdr-paper); padding-top:.25rem; }
.step-card { background: var(--herdr-card); border-radius: 14px; padding: 1.05rem 1.15rem 1.15rem; margin-bottom: 1.1rem; box-shadow: 0 1px 4px rgba(38,38,32,0.07); border: 1px solid var(--herdr-border); }
.step-head { display:flex; align-items:center; gap:.6rem; margin-bottom:.5rem; }
.step-tag { position:relative; min-width:36px; height:24px; background: var(--herdr-pasture); color:#fff; border-radius: 4px 10px 10px 4px; display:flex; align-items:center; justify-content:center; font-family:'IBM Plex Mono',monospace; font-weight:700; font-size:.78rem; padding-left:6px; }
.step-tag::before{ content:''; position:absolute; left:5px; top:50%; transform:translateY(-50%); width:5px; height:5px; border-radius:50%; background: var(--herdr-card); }
.step-card.wheat .step-tag { background: var(--herdr-wheat); }
.step-card.barn .step-tag { background: var(--herdr-barn); }
.step-title { font-family:'Fraunces',serif; font-weight:600; font-size:1.02rem; margin:0; }
.step-hint { color: var(--herdr-muted); font-size:.83rem; margin: 0 0 .7rem 0; line-height:1.35; }
.herdr-sidebar label { font-weight:600; font-size:.85rem; display:flex; align-items:center; gap:5px; margin-bottom: 4px; }

/* --- Buttons --- */
.btn-herdr { width:100%; border:none; border-radius:10px; font-weight:700; padding:.62rem 1rem; font-size:.92rem; transition: all .2s ease; margin-bottom: .5rem; }
.btn-load { background: var(--herdr-pasture-light); color: var(--herdr-pasture-dark); border: 1.5px solid var(--herdr-pasture); }
.btn-load:hover { background: var(--herdr-pasture); color:#fff; transform: translateY(-1px); }
.btn-clear { background: var(--herdr-card); color: var(--herdr-barn); border: 1.5px dashed var(--herdr-barn); font-size: .85rem; padding: .4rem 1rem; }
.btn-clear:hover { background: var(--herdr-barn-bg); }
.btn-run { background: linear-gradient(135deg, var(--herdr-wheat), #DDB44C); color: #2B2210; font-size:1rem; padding:.85rem 1rem; box-shadow: 0 5px 16px rgba(199,154,46,0.38); margin-top: .5rem; }
.btn-run:hover:not(:disabled) { transform: translateY(-2px); box-shadow: 0 8px 20px rgba(199,154,46,0.48); color:#2B2210; }
.btn-download { background: var(--herdr-card); color: var(--herdr-pasture-dark); border: 1.5px dashed var(--herdr-pasture); }
.btn-download:hover { background: var(--herdr-pasture-light); }
.btn-next-step { background: var(--herdr-card); border: 1.5px solid var(--herdr-pasture); color: var(--herdr-pasture); border-radius: 8px; font-weight: 600; padding: 0.5rem 1.2rem; transition: all 0.2s; box-shadow: 0 2px 6px rgba(51, 89, 59, 0.15); }
.btn-next-step:hover { background: var(--herdr-pasture); color: #fff; transform: translateY(-1px); }

/* --- Tabs & Notifications --- */
.nav-tabs .nav-link { font-weight:600; font-size:.88rem; color: var(--herdr-muted); border:none; padding:.85rem 1.1rem; }
.nav-tabs .nav-link.active { color: var(--herdr-pasture-dark); background: var(--herdr-card); border-bottom: 3px solid var(--herdr-wheat); }
.dirty-dot { display:inline-block; width:8px; height:8px; border-radius:50%; background: var(--herdr-wheat); margin-left:7px; vertical-align:middle; box-shadow: 0 0 0 2px var(--herdr-card); }
.advanced-warning{ display:flex; gap:14px; background: var(--herdr-barn-bg); border:1px solid #EFC7BE; border-left: 6px solid var(--herdr-barn); border-radius:12px; padding: 1rem 1.25rem; margin-bottom: 1.2rem; }
.empty-state { text-align:center; padding: 3.2rem 1.5rem; color: var(--herdr-muted); background: var(--herdr-card); border:1.5px dashed var(--herdr-border); border-radius:14px; }
.shiny-notification { border-radius: 12px !important; box-shadow: 0 8px 22px rgba(0,0,0,0.14) !important; border:none !important; }

/* ⚡ BANNERS DE VALIDACIÓN CRUZADA ⚡ */
.herdr-alert { display: flex; gap: 10px; align-items: center; padding: 0.75rem 1rem; border-radius: 8px; font-size: 0.9rem; font-weight: 500; margin-bottom: 0.8rem; flex-shrink: 0; }
.herdr-alert i { font-size: 1.2rem; }
.herdr-alert b { font-family: 'IBM Plex Mono', monospace; background: rgba(255,255,255,0.4); padding: 0 4px; border-radius: 4px; }
.herdr-alert-danger { background: var(--herdr-barn-bg); color: #7C2417; border: 1px solid #EFC7BE; }
.herdr-alert-warning { background: var(--herdr-wheat-light); color: #5C4716; border: 1px solid #E3BE58; }

/* --- Handsontable Fixes --- */
.handsontable th { background: var(--herdr-pasture) !important; color:#fff !important; font-family:'Inter',sans-serif; font-weight:600 !important; }
.handsontable td { font-family:'IBM Plex Mono',monospace; font-size:.83rem; }
.rhandsontable { min-height: 350px !important; }

[data-bs-theme='dark'] .handsontable td { background: var(--herdr-card) !important; color: var(--herdr-ink) !important; }
.handsontable tr:nth-child(even) td { background: #FAF8F2 !important; }
[data-bs-theme='dark'] .handsontable tr:nth-child(even) td { background: #2A3128 !important; }

/* REFUERZOS CSS PARA FRENAR EL SCROLL DE LOS DESPLEGABLES */
.handsontable.listbox .wtHolder { overflow-y: auto !important; overscroll-behavior: contain !important; }

/* --- Dropdown Listbox Styles --- */
.handsontable.listbox { box-shadow: 0 6px 16px rgba(0,0,0,0.18) !important; border-radius: 6px !important; border: 1px solid var(--herdr-pasture) !important; margin-top: 2px !important; }
.handsontable.listbox table { background: #ffffff !important; border: none !important; }
.handsontable.listbox td { font-family: 'Inter', sans-serif !important; font-size: 0.9rem !important; font-weight: 600 !important; color: #111 !important; padding: 6px 12px !important; border-bottom: 1px solid #f0f0f0 !important; }
.handsontable.listbox tr:hover td, .handsontable.listbox tr td.current { background: var(--herdr-pasture-light) !important; color: var(--herdr-pasture-dark) !important; font-weight: 800 !important; }
[data-bs-theme='dark'] .handsontable.listbox { border: 1px solid var(--herdr-pasture-dark) !important; box-shadow: 0 6px 16px rgba(0,0,0,0.4) !important; }
[data-bs-theme='dark'] .handsontable.listbox table { background: var(--herdr-card) !important; }
[data-bs-theme='dark'] .handsontable.listbox td { color: #ffffff !important; border-bottom: 1px solid var(--herdr-border) !important; }
[data-bs-theme='dark'] .handsontable.listbox tr:hover td, [data-bs-theme='dark'] .handsontable.listbox tr td.current { background: var(--herdr-pasture-dark) !important; color: #ffffff !important; }

.shiny-notification .progress { height: 10px; border-radius: 6px; background: var(--herdr-pasture-light); margin-top: 6px; overflow: hidden; }
.shiny-notification .progress-bar { background: linear-gradient(90deg, var(--herdr-pasture), var(--herdr-wheat)) !important; transition: width .3s ease; }
#shiny-notification-panel .progress-message { font-weight:700; font-family:'Inter',sans-serif; color: var(--herdr-ink); }
#shiny-notification-panel .progress-detail { color: var(--herdr-muted); font-size:.82rem; }
"

herdr_logo <- HTML('<svg width=\"30\" height=\"22\" viewBox=\"0 0 30 22\"><rect x=\"1\" y=\"1\" width=\"26\" height=\"16\" rx=\"4\" ry=\"7\" fill=\"#C79A2E\"/><circle cx=\"7\" cy=\"9\" r=\"2.6\" fill=\"#33593B\"/><path d=\"M27 5 L29.5 9 L27 13\" fill=\"#33593B\"/></svg>')

spin <- function(x) { if (requireNamespace("shinycssloaders", quietly = TRUE)) shinycssloaders::withSpinner(x, color = "#33593B", type = 6, size = 0.6) else x }

# ==========================================
# 3. USER INTERFACE (UI)
# ==========================================

data_tab <- function(id) {
  info <- tables_info[[id]]
  nav_panel(
    title = tagList(icon(info$icon), info$label, tooltip(icon("circle-info", class="ms-1 text-muted"), info$help), uiOutput(paste0("dirty_badge_", id), inline = TRUE)),
    value = id,
    div(class = "p-3",
        uiOutput(paste0("validation_alert_", id)),

        div(style = "background: var(--herdr-card); border-radius: 8px; border: 1px solid var(--herdr-border); margin-bottom: 1rem;",
            spin(rHandsontableOutput(paste0("table_", id), height = "calc(100vh - 320px)"))
        ),

        uiOutput(paste0("wizard_footer_", id))
    )
  )
}

ui_standard_tabs <- lapply(standard_ids, data_tab)
ui_advanced_tabs <- lapply(advanced_ids, function(id) {
  nav_panel(
    title = tagList(icon(tables_info[[id]]$icon), tables_info[[id]]$label, uiOutput(paste0("dirty_badge_", id), inline = TRUE)), value = id,
    div(class = "p-3",
        div(style = "background: var(--herdr-card); border-radius: 8px; border: 1px solid var(--herdr-border);",
            spin(rHandsontableOutput(paste0("table_", id), height = "calc(100vh - 360px)"))
        )
    )
  )
})

ui <- page_sidebar(
  fillable = TRUE, theme = herdr_theme, window_title = "herdr \u2014 Livestock Emissions Calculator",
  tags$head(tags$style(HTML(herdr_css))),
  tags$head(tags$script(HTML("
    Shiny.addCustomMessageHandler('herdr_button_state', function(msg) {
      var btn = document.getElementById(msg.id);
      if (!btn) return;
      if (!btn.dataset.herdrLabel) btn.dataset.herdrLabel = btn.innerHTML;
      if (msg.loading) { btn.innerHTML = '<i class=\"fa fa-circle-notch fa-spin\"></i> ' + msg.text; btn.disabled = true; btn.classList.add('btn-loading'); }
      else { btn.innerHTML = btn.dataset.herdrLabel; btn.disabled = false; btn.classList.remove('btn-loading'); }
    });

    /* \u26A1 EL PORTERO DE DISCOTECA: BLOQUEADOR DE SCROLL INFALIBLE \u26A1 */
    function stopScrollChaining(e) {
      // 1. \u00bfEstamos encima de la lista del desplegable?
      var isListbox = e.target.closest('.handsontable.listbox');
      if (isListbox) {
        // CORTAMOS LA PROPAGACI\u00d3N. La tabla principal nunca se enterar\u00e1 del scroll.
        e.stopPropagation();
        return;
      }

      // 2. \u00bfEstamos encima de la tabla principal?
      var isTable = e.target.closest('.handsontable');
      if (isTable) {
        var holder = e.target.closest('.wtHolder');
        if (holder && holder.scrollHeight > holder.clientHeight) {
          var atTop = holder.scrollTop <= 0 && e.deltaY < 0;
          var atBottom = (holder.scrollHeight - holder.clientHeight - holder.scrollTop) <= 2 && e.deltaY > 0;

          if (atTop || atBottom) {
            e.preventDefault(); // Evita que baje la web entera
          }
        }
      }
    }

    // El 'capture: true' es el truco de magia. Atrapa el evento ANTES de que Handsontable lo procese.
    document.addEventListener('wheel', stopScrollChaining, { passive: false, capture: true });
    document.addEventListener('mousewheel', stopScrollChaining, { passive: false, capture: true });
  "))),

  title = tagList(
    div(class = "herdr-navbar-inner",
        div(class = "herdr-brand", herdr_logo, tags$span(tags$span("herdr", class = "herdr-brand-title"), tags$span("Livestock Emissions", class = "herdr-brand-sub"))),
        input_dark_mode(id = "dark_mode", mode = "light")
    )
  ),

  sidebar = sidebar(
    width = 340, class = "herdr-sidebar", open = "desktop",
    div(class = "step-card",
        div(class = "step-head", span("1", class = "step-tag"), h5("Data Source", class = "step-title")),
        selectInput("data_source", "Load Package Example:", choices = c("Loading..." = "")),
        actionButton("load_data", "Load Example", icon = icon("folder-open"), class = "btn-herdr btn-load"),
        fileInput("upload_csvs", "Or Upload CSV Files:", multiple = TRUE, accept = ".csv", buttonLabel = "Browse..."),
        actionButton("reset_data", "Clear All Data", icon = icon("trash-can"), class = "btn-herdr btn-clear")
    ),
    div(class = "step-card wheat",
        div(class = "step-head", span("2", class = "step-tag"), h5("Configuration", class = "step-title")),
        tooltip(checkboxInput("auto_cycle", tagList("Use automatic herd cycle", icon("circle-info", class = "ms-1 text-muted")), value = FALSE), "Unlocks the Reproduction table if unchecked."),

        hr(style = "margin: 0.8rem 0; border-color: var(--herdr-border);"),

        # Country Selector con el nuevo Tooltip de ayuda
        tooltip(
          selectInput("farm_country", tagList("Farm Country / Area:", icon("circle-info", class = "ms-1 text-muted")), choices = c("Loading countries..." = "")),
          "If a feed's origin is empty (NA) in your Ingredients table, herdr automatically calculates its true origin using FAO trade/production data for this country."
        ),

        numericInput("year", "FAO Reference Year:", value = 2022, min = 1961, max = 2026, step = 1)
    ),
    div(class = "step-card wheat",
        div(class = "step-head", span("3", class = "step-tag"), h5("Calculate", class = "step-title")),
        actionButton("calculate", "Run herdr Model", icon = icon("play"), class = "btn-herdr btn-run")
    ),
    div(class = "step-card barn",
        div(class = "step-head", span("4", class = "step-tag"), h5("Downloads", class = "step-title")),
        downloadButton("download", "Download Results", class = "btn-herdr btn-download")
    )
  ),

  navset_card_tab(
    id = "main_tabs", full_screen = TRUE,
    !!!ui_standard_tabs,
    nav_panel(title = tagList(icon("sliders"), "Advanced (IPCC)"), value = "advanced_tab",
              div(class = "p-3",
                  div(class = "advanced-warning", icon("triangle-exclamation"), div(h5("Expert territory"), p("IPCC default values. Change only with specific regional data."))),
                  navset_card_tab(id = "advanced_inner_tabs", !!!ui_advanced_tabs)
              )
    ),
    nav_panel(title = tagList(icon("chart-column"), "Results"), value = "results_tab",
              div(class = "p-3",
                  uiOutput("results_placeholder"), uiOutput("kpi_cards"),
                  div(class = "results-card", tableOutput("table_results"))
              )
    )
  )
)

# ==========================================
# 4. SERVER LOGIC
# ==========================================

server <- function(input, output, session) {

  if (!dir.exists("user_data")) herdr::herdr_init()

  example_paths <- system.file("Examples", package = "herdr")
  updateSelectInput(session, "data_source", choices = c("My current data" = "current", if (example_paths != "") list.dirs(example_paths, full.names = FALSE, recursive = FALSE) else c()))

  # Load the unique countries dynamically from fao_crop_yields.csv
  observe({
    path_yields <- "user_data/fao_crop_yields.csv"
    if (file.exists(path_yields)) {
      tryCatch({
        df_yields <- readr::read_csv(path_yields, col_select = Area, show_col_types = FALSE)
        paises <- sort(unique(na.omit(df_yields$Area)))

        # We pre-select "Spain" if it exists in the list, otherwise default to first element
        selected_country <- if ("Spain" %in% paises) "Spain" else paises[1]

        updateSelectInput(session, "farm_country", choices = paises, selected = selected_country)
      }, error = function(e) {
        updateSelectInput(session, "farm_country", choices = c("Spain", "France", "Germany", "United States of America"))
      })
    } else {
      # Fallback defaults in case the file hasn't been created yet
      updateSelectInput(session, "farm_country", choices = c("Spain", "France", "Germany", "United States of America"))
    }
  })

  rv <- reactiveValues()
  dirty <- reactiveValues()
  model_data <- reactiveVal(NULL)

  refresh_data_state <- function(reset = FALSE) {
    lapply(names(tables_info), function(id) {
      if (reset) {
        plantilla <- read_clean(file.path("user_data", tables_info[[id]]$file))
        rv[[id]] <- plantilla[0, ]
      } else {
        rv[[id]] <- read_clean(file.path("user_data", tables_info[[id]]$file))
      }
      dirty[[id]] <- FALSE
    })
    model_data(NULL)
  }

  refresh_data_state(reset = FALSE)

  observeEvent(input$load_data, {
    req(input$data_source)
    tryCatch({
      if (input$data_source != "current") file.copy(list.files(file.path(example_paths, input$data_source), full.names = TRUE), "user_data", overwrite = TRUE)
      refresh_data_state(reset = FALSE)
      showNotification(paste("\u2705 Data loaded from:", input$data_source), type = "message")
      nav_select("main_tabs", "census")
    }, error = function(e) showNotification("Error loading files.", type = "error"))
  })

  observeEvent(input$reset_data, {
    refresh_data_state(reset = TRUE)
    showNotification("\u2705 All tables cleared. Ready for new input.", type = "message")
    nav_select("main_tabs", "census")
  })

  observeEvent(input$upload_csvs, {
    req(input$upload_csvs)
    file_map <- setNames(names(tables_info), sapply(tables_info, function(x) x$file))
    matched <- 0
    for (i in seq_len(nrow(input$upload_csvs))) {
      fname <- input$upload_csvs$name[i]
      if (fname %in% names(file_map)) {
        id <- file_map[[fname]]
        file.copy(input$upload_csvs$datapath[i], file.path("user_data", fname), overwrite = TRUE)
        rv[[id]] <- read_clean(file.path("user_data", fname))
        dirty[[id]] <- FALSE
        matched <- matched + 1
      }
    }
    if (matched > 0) {
      showNotification(paste("\u2705", matched, "CSV file(s) updated!"), type = "message")
      model_data(NULL)
      nav_select("main_tabs", "census")
    } else showNotification("\u26A0 No matching herdr CSV files found.", type = "warning")
  })

  active_standard_ids <- reactive({ if (isTRUE(input$auto_cycle)) standard_ids else setdiff(standard_ids, "repro") })
  observe({ if (isTRUE(input$auto_cycle)) nav_show("main_tabs", "repro") else nav_hide("main_tabs", "repro") })

  build_keys <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(character(0))
    if (!"animal_tag" %in% names(df)) return(character(0))
    df <- df[!is.na(df$animal_tag) & df$animal_tag != "", ]
    if (nrow(df) == 0) return(character(0))
    id_cols <- c("animal_tag", "region", "subregion", "class_flex")
    present_cols <- intersect(id_cols, names(df))
    df_keys <- df[, present_cols, drop = FALSE]
    df_keys[is.na(df_keys)] <- ""
    apply(df_keys, 1, paste, collapse = " | ")
  }

  lapply(standard_ids, function(id) {

    output[[paste0("validation_alert_", id)]] <- renderUI({
      req(rv$census, rv[[id]])
      if (id == "census" || !("animal_tag" %in% names(rv[[id]]))) return(NULL)

      c_keys <- unique(build_keys(rv$census))
      t_keys <- unique(build_keys(rv[[id]]))
      if (length(c_keys) == 0) return(NULL)

      alerts <- list()
      missing_keys <- character(0)
      extra_keys <- setdiff(t_keys, c_keys)

      if (id %in% c("def", "mono")) {
        def_keys <- unique(build_keys(rv$def))
        mono_keys <- unique(build_keys(rv$mono))
        defined_keys <- union(def_keys, mono_keys)
        missing_keys <- setdiff(c_keys, defined_keys)
        if (length(missing_keys) > 0) {
          alerts[[length(alerts) + 1]] <- div(class = "herdr-alert herdr-alert-danger",
                                              icon("circle-xmark"),
                                              HTML(paste0("<b>Missing Cohorts:</b> The following are in your Census but not defined in Definitions OR Monogastrics:<br> <b>", paste(missing_keys, collapse = "</b>, <b>"), "</b>"))
          )
        }
      } else {
        missing_keys <- setdiff(c_keys, t_keys)
        if (length(missing_keys) > 0) {
          alerts[[length(alerts) + 1]] <- div(class = "herdr-alert herdr-alert-danger",
                                              icon("circle-xmark"),
                                              HTML(paste0("<b>Missing Cohorts:</b> You declared these in Census but forgot them here:<br> <b>", paste(missing_keys, collapse = "</b>, <b>"), "</b>"))
          )
        }
      }

      if (length(extra_keys) > 0) {
        alerts[[length(alerts) + 1]] <- div(class = "herdr-alert herdr-alert-warning",
                                            icon("triangle-exclamation"),
                                            HTML(paste0("<b>Unrecognized Cohorts:</b> These rows don't match exactly with any entry in your Census:<br> <b>", paste(extra_keys, collapse = "</b>, <b>"), "</b>"))
        )
      }

      if (length(alerts) > 0) do.call(tagList, alerts) else NULL
    })

    output[[paste0("wizard_footer_", id)]] <- renderUI({
      actives <- active_standard_ids()
      if (!(id %in% actives)) return(NULL)
      idx <- which(actives == id)
      if (idx < length(actives)) {
        div(class = "d-flex justify-content-end", actionButton(paste0("btn_next_", id), paste("Next Step:", tables_info[[actives[idx + 1]]]$label, "\u2794"), class = "btn-next-step"))
      } else {
        div(class = "d-flex justify-content-end", actionButton(paste0("btn_next_", id), "Ready? Click 'Run herdr Model' \u2794", class = "btn-next-step", onclick = "document.getElementById('calculate').click();"))
      }
    })
    observeEvent(input[[paste0("btn_next_", id)]], {
      actives <- active_standard_ids()
      idx <- which(actives == id)
      if (length(idx) > 0 && idx < length(actives)) nav_select("main_tabs", actives[idx + 1])
    })
  })

  lapply(names(tables_info), function(id) {
    output[[paste0("table_", id)]] <- renderRHandsontable({
      req(rv[[id]])
      if (ncol(rv[[id]]) == 0) return(NULL)
      tbl <- rhandsontable(rv[[id]], rowHeaders = NULL, width = "100%", stretchH = "all", minSpareRows = 1, allowInsertColumn = FALSE, allowRemoveColumn = FALSE)
      if (tables_info[[id]]$fixed > 0) tbl <- hot_cols(tbl, columnSorting = TRUE, fixedColumnsLeft = tables_info[[id]]$fixed) else tbl <- hot_cols(tbl, columnSorting = TRUE)

      if (id %in% names(dynamic_dropdowns)) {
        for (target_col in names(dynamic_dropdowns[[id]])) {
          if (target_col %in% names(rv[[id]])) {
            rule <- dynamic_dropdowns[[id]][[target_col]]
            src_df <- rv[[rule$source_table]]
            if (!is.null(src_df) && nrow(src_df) > 0) {
              opciones <- if (!is.null(rule$filter_column) && !is.null(rule$filter_value)) src_df[[rule$extract_column]][ src_df[[rule$filter_column]] == rule$filter_value ] else src_df[[rule$extract_column]]
              opciones <- as.character(unique(na.omit(opciones)))
              if (length(opciones) > 0) tbl <- hot_col(tbl, col = target_col, type = "dropdown", source = opciones)
            }
          }
        }
      }
      tbl
    })

    observeEvent(input[[paste0("table_", id)]], {
      tryCatch({
        nuevo_df <- suppressWarnings(hot_to_r(input[[paste0("table_", id)]]))
        if (is.data.frame(nuevo_df)) { rv[[id]] <- nuevo_df; dirty[[id]] <- TRUE }
      }, error = function(e) {})
    })
    output[[paste0("dirty_badge_", id)]] <- renderUI({ if (isTRUE(dirty[[id]])) tags$span(class = "dirty-dot", title = "Unsaved changes") else NULL })
  })

  observeEvent(input$calculate, {
    nav_select("main_tabs", "results_tab")
    session$sendCustomMessage("herdr_button_state", list(id = "calculate", loading = TRUE, text = "Running..."))
    on.exit({ session$sendCustomMessage("herdr_button_state", list(id = "calculate", loading = FALSE, text = "")) }, add = TRUE)

    withProgress(message = "Running herdr model", value = 0, {
      incProgress(0.15, detail = "Saving your tables...")
      lapply(names(tables_info), function(id) { save_clean(rv[[id]], file.path("user_data", tables_info[[id]]$file)); dirty[[id]] <- FALSE })
      incProgress(0.35, detail = "Calculating emissions...")

      mensajes_mostrados <- c()
      result <- tryCatch({
        withCallingHandlers(
          expr = generate_impact_assessment(
            automatic_cycle = input$auto_cycle,
            saveoutput = FALSE,
            farm_country = input$farm_country,
            year = input$year
          ),
          warning = function(w) {
            if (!(w$message %in% mensajes_mostrados)) { showNotification(paste("\u26A0", w$message), type = "warning", duration = 12); mensajes_mostrados <<- c(mensajes_mostrados, w$message) }
          }
        )
      }, error = function(e) { showNotification(paste("Model error:", e$message), type = "error", duration = NULL); return(NULL) })

      incProgress(0.5, detail = "Done")
      if (!is.null(result)) showNotification("\u2705 Results ready!", type = "message", duration = 4)
      model_data(result)
    })
  })

  output$table_results <- renderTable({ req(model_data()); model_data() }, digits = 6)
  output$results_placeholder <- renderUI({ if (is.null(model_data())) div(class = "empty-state", icon("chart-column"), h4("No results yet"), p("Configure your data and press \u201cRun herdr Model\u201d.")) else NULL })

  output$kpi_cards <- renderUI({
    kpis <- compute_kpis(model_data())
    if (is.null(kpis)) return(NULL)
    boxes <- lapply(seq_along(kpis), function(i) value_box(title = kpis[[i]]$name, value = format(round(kpis[[i]]$value, 2), big.mark = ",", scientific = FALSE), showcase = icon(if (i == 1) "leaf" else "chart-simple"), theme = if (i %% 2 == 1) "primary" else "warning"))
    div(class = "kpi-row", do.call(layout_column_wrap, c(list(width = "250px"), boxes)))
  })

  output$download <- downloadHandler(filename = function() paste0("herdr_impact_assessment_", Sys.Date(), ".csv"), content = function(file) write_csv(model_data(), file))
}

shinyApp(ui = ui, server = server)
