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
      hr(),
      selectInput("farm_country", "Farm Country / Area:", choices = c("Loading..." = "")),
      numericInput("year", "FAO Reference Year:", value = 2022, step = 1)
    ),

    # --- STEP 3: CALCULATE ---
    div(
      class = "step-card wheat",
      div(class = "step-head", span("3", class = "step-tag"), h5("Calculate", class = "step-title m-0")),
      selectInput("function_choice", "Function to Run:", choices = c(
        "Full Assessment (All)" = "generate_impact_assessment",
        "Population" = "calculate_population",
        "Production (Meat, Milk, Eggs, Wool)" = "calculate_production",
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
