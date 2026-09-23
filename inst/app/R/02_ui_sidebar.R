# ==============================================================================
# herdr — Sidebar UI Component
# ==============================================================================

build_herdr_sidebar <- function() {
  bslib::sidebar(
    width = 340,
    class = "herdr-sidebar",
    open = "desktop",

    # --- STEP 1: DATA SOURCE ---
    div(
      class = "step-card",
      div(class = "step-head", span("1", class = "step-tag"), h5("Data Source", class = "step-title m-0")),
      selectInput("data_source", "Load Package Example:", choices = c("Loading..." = "")),
      actionButton("load_data", "Load Example", icon = icon("folder-open"), class = "btn-herdr btn-load"),
      fileInput("upload_csvs", "Or Upload CSV Files:", multiple = TRUE, accept = ".csv", buttonLabel = "Browse..."),
      actionButton("reset_data", "Clear All Data", icon = icon("trash-can"), class = "btn-herdr btn-clear")
    ),

    # --- STEP 2: CONFIGURATION ---
    div(
      class = "step-card wheat",
      div(class = "step-head", span("2", class = "step-tag"), h5("Configuration", class = "step-title m-0")),
      checkboxInput("auto_cycle", "Use automatic herd cycle", value = FALSE),
      tags$small(
        class = "text-muted d-block",
        style = "font-size: 0.78rem; line-height: 1.35; margin-top: -6px; margin-bottom: 10px;",
        "Use this if you only enter adult breeding stock (e.g. mature cows, sows) to auto-model offspring and replacements."
      ),
      hr(),
      selectInput("farm_country", "Farm Country / Area:", choices = c("Loading..." = "")),
      numericInput("year", "FAO Reference Year:", value = 2022, step = 1)
    ),

    # --- STEP 3: CALCULATE ---
    div(
      class = "step-card wheat",
      div(class = "step-head", span("3", class = "step-tag"), h5("Calculate", class = "step-title m-0")),
      selectInput("function_choice", "Function to Run:", choices = list(
        "Overview & Assessment" = c(
          "Full Assessment (All Emissions & Resources)" = "generate_impact_assessment"
        ),
        "Herd Demography & Production" = c(
          "Population (AAP & Demography)" = "calculate_population",
          "Production (Meat, Milk, Eggs, Wool)" = "calculate_production"
        ),
        "Feed, Intake & Energy Balance" = c(
          "Weighted Feed Characteristics" = "calculate_weighted_variable",
          "Dry Matter Intake (DMI)" = "calculate_DMI",
          "Gross Energy (GE)" = "calculate_ge",
          "Net Energy for Maintenance (NEm)" = "calculate_NEm",
          "Net Energy for Growth (NEg)" = "calculate_NEg",
          "Net Energy for Lactation (NEl)" = "calculate_NEl",
          "Net Energy for Pregnancy" = "calculate_NE_pregnancy",
          "Net Energy for Activity (NEa)" = "calculate_NEa",
          "Net Energy for Wool Production" = "calculate_NE_wool",
          "Net Energy for Work" = "calculate_NE_work",
          "Monogastric Metabolizable Energy" = "calculate_monogastric_energy"
        ),
        "GHG Emissions & Land Use" = c(
          "CH4 from Enteric Fermentation" = "calculate_emissions_enteric",
          "Volatile Solids (VS)" = "calculate_vs",
          "CH4 from Manure Management" = "calculate_CH4_manure",
          "Direct N2O from Manure" = "calculate_N2O_direct_manure",
          "Indirect N2O from Volatilization" = "calculate_N2O_indirect_volatilization",
          "Indirect N2O from Leaching" = "calculate_N2O_indirect_leaching",
          "Land Use Requirement" = "calculate_land_use"
        )
      )),
      uiOutput("sidebar_data_health"),
      actionButton("calculate", "Run Selected", icon = icon("play"), class = "btn-herdr btn-run")
    ),

    # --- STEP 4: DOWNLOADS ---
    div(
      class = "step-card barn",
      div(class = "step-head", span("4", class = "step-tag"), h5("Downloads", class = "step-title m-0")),
      div(
        class = "mb-3 p-2 rounded",
        style = "background-color: var(--herdr-barn-light); border: 1px solid var(--herdr-barn); font-size: 0.85rem; color: var(--herdr-barn-dark);",
        tags$strong(icon("triangle-exclamation"), " Important:"),
        tags$br(),
        "Data is not saved online. To resume work later, please download your results and input data before closing this page."
      ),
      downloadButton("download", "1. Download Results", class = "btn-herdr btn-download"),
      downloadButton("download_inputs", "2. Download Input Data (ZIP)", class = "btn-herdr btn-download", style = "margin-top: 5px;")
    )
  )
}
