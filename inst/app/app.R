# ==============================================================================
# herdr — Livestock Emissions Interactive Application
# ==============================================================================

library(shiny)
library(bslib)
library(herdr)
library(rhandsontable)
library(dplyr)
library(readr)
if (requireNamespace("arrow", quietly = TRUE)) library(arrow)
library(zip)

# --- 1. Locate and Source Modular Components ---
find_app_dir <- function() {
  if (exists(".HERDR_APP_DIR", envir = .GlobalEnv) &&
      file.exists(file.path(get(".HERDR_APP_DIR", envir = .GlobalEnv), "R", "00_config.R"))) {
    return(get(".HERDR_APP_DIR", envir = .GlobalEnv))
  }
  calls <- sys.calls()
  for (i in rev(seq_along(calls))) {
    cl <- calls[[i]]
    fn_name <- tryCatch(as.character(cl[[1]])[1], error = function(e) "")
    if (fn_name == "sys.source") {
      matched <- match.call(definition = base::sys.source, call = cl)
      f <- tryCatch(eval(matched$file, envir = parent.frame(sys.nframe() - i)), error = function(e) NULL)
      if (!is.null(f) && is.character(f) && file.exists(f)) {
        cand <- dirname(f)
        if (file.exists(file.path(cand, "R", "00_config.R"))) return(normalizePath(cand, winslash = "/"))
      }
    }
  }
  if (file.exists(file.path("R", "00_config.R"))) return(".")
  if (file.exists(file.path("inst", "app", "R", "00_config.R"))) return(file.path("inst", "app"))
  if (file.exists(file.path("..", "..", "inst", "app", "R", "00_config.R"))) {
    return(normalizePath(file.path("..", "..", "inst", "app"), winslash = "/", mustWork = FALSE))
  }
  for (i in rev(seq_len(sys.nframe()))) {
    ofile <- sys.frame(i)$ofile
    if (!is.null(ofile) && file.exists(ofile)) {
      cand <- dirname(ofile)
      if (file.exists(file.path(cand, "R", "00_config.R"))) return(cand)
    }
  }
  pkg_dir <- system.file("app", package = "herdr")
  if (pkg_dir != "" && file.exists(file.path(pkg_dir, "R", "00_config.R"))) return(pkg_dir)
  "."
}

app_dir <- find_app_dir()

# Source all modular subcomponents into current environment
source(file.path(app_dir, "R", "00_config.R"), local = TRUE)
source(file.path(app_dir, "R", "01_helpers.R"), local = TRUE)
source(file.path(app_dir, "R", "02_ui_sidebar.R"), local = TRUE)
source(file.path(app_dir, "R", "03_ui_dictionary.R"), local = TRUE)
source(file.path(app_dir, "R", "04_ui_results.R"), local = TRUE)
source(file.path(app_dir, "R", "05_server_logic.R"), local = TRUE)

# Register www assets path for standalone & packaged execution
www_dir <- file.path(app_dir, "www")
if (dir.exists(www_dir)) {
  shiny::addResourcePath("herdr_assets", www_dir)
}

if (!dir.exists("user_data")) herdr::herdr_init()

# --- 2. Table Tab UI Builders ---
build_standard_tab <- function(id) {
  info <- tables_info[[id]]
  bslib::nav_panel(
    title = tagList(icon(info$icon), info$label, uiOutput(paste0("dirty_badge_", id), inline = TRUE)),
    value = id,
    div(
      class = "p-3",
      div(
        class = "d-flex justify-content-between align-items-center mb-2",
        h5(info$label, class = "m-0 text-muted"),
        actionButton(paste0("open_modal_", id), "Add Record", icon = icon("plus"), class = "btn btn-sm btn-outline-success")
      ),
      uiOutput(paste0("tab_health_banner_", id)),
      div(
        style = "background: var(--herdr-card); border-radius: 8px; border: 1px solid var(--herdr-border); margin-bottom: 1rem;",
        rhandsontable::rHandsontableOutput(paste0("table_", id), height = "calc(100vh - 360px)")
      )
    )
  )
}

build_advanced_tab <- function(id) {
  info <- tables_info[[id]]
  bslib::nav_panel(
    title = tagList(icon(info$icon), info$label, uiOutput(paste0("dirty_badge_", id), inline = TRUE)),
    value = id,
    div(
      class = "p-3",
      div(
        class = "d-flex justify-content-between align-items-center mb-2",
        h5(info$label, class = "m-0 text-muted"),
        actionButton(paste0("open_modal_", id), "Add Record", icon = icon("plus"), class = "btn btn-sm btn-outline-success")
      ),
      uiOutput(paste0("tab_health_banner_", id)),
      div(
        style = "background: var(--herdr-card); border-radius: 8px; border: 1px solid var(--herdr-border);",
        rhandsontable::rHandsontableOutput(paste0("table_", id), height = "calc(100vh - 360px)")
      )
    )
  )
}

# --- 3. Assemble UI ---
ui <- bslib::page_sidebar(
  fillable = TRUE,
  theme = herdr_theme,
  window_title = "herdr \u2014 Livestock Emissions",
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "herdr_assets/herdr.css?v=20260921b"),
    tags$script(src = "herdr_assets/herdr.js?v=20260921b"),
    tags$style(HTML("
      /* Disarm Handsontable red cell invalidation */
      html body .handsontable td.htInvalid,
      html body .handsontable th.htInvalid,
      html body .handsontable .htInvalid,
      html body .handsontable td.htInvalid:hover,
      html body .handsontable th.htInvalid:hover,
      html body table.htCore td.htInvalid,
      html body table.htCore th.htInvalid,
      html body table.htCore .htInvalid,
      .handsontable td.htInvalid,
      .handsontable th.htInvalid,
      .handsontable .htInvalid {
        background: transparent !important;
        background-color: transparent !important;
        color: inherit !important;
        box-shadow: none !important;
      }
    "))
  ),
  title = tagList(
    div(
      class = "d-flex align-items-center justify-content-between w-100",
      div(
        class = "herdr-brand",
        herdr_logo,
        tags$span(
          tags$span("herdr", class = "herdr-brand-title"),
          tags$span("Livestock Emissions", class = "herdr-brand-sub")
        )
      ),
      div(
        class = "herdr-header-actions",
        tags$span(class = "herdr-version-pill", "v1.0.3"),
        tags$a(href = "https://juancbm99.github.io/herdr", target = "_blank", class = "btn btn-outline-secondary", icon("book"), "Docs"),
        tags$a(href = "https://github.com/JuanCBM99/herdr", target = "_blank", class = "btn btn-outline-secondary", icon("github"), "GitHub"),
        bslib::input_dark_mode(id = "dark_mode", mode = "light")
      )
    )
  ),
  sidebar = build_herdr_sidebar(),
  bslib::navset_card_tab(
    id = "main_tabs",
    selected = "census",
    full_screen = TRUE,
    build_dictionary_panel(),
    !!!lapply(standard_ids, build_standard_tab),
    bslib::nav_panel(
      title = tagList(icon("sliders"), "Advanced (IPCC)"),
      value = "advanced_tab",
      div(class = "p-3", bslib::navset_card_tab(id = "advanced_inner_tabs", !!!lapply(advanced_ids, build_advanced_tab)))
    ),
    build_results_panel()
  )
)

# --- 4. Server Logic ---
server <- herdr_server

# --- 5. Application Launch ---
shinyApp(ui = ui, server = server)
