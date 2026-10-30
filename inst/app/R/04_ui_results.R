# ==============================================================================
# herdr — Results Dashboard UI Component
# ==============================================================================

build_results_panel <- function() {
  bslib::nav_panel(
    title = tagList(icon("chart-column"), "Results"),
    value = "results_tab",
    div(
      class = "p-3",
      uiOutput("results_placeholder"),
      div(
        class = "results-card",
        div(
          class = "row mb-3 p-3 rounded align-items-center",
          style = "background: var(--herdr-paper); border: 1px solid var(--herdr-border);",
          div(
            class = "col-md-4",
            selectizeInput(
              "plot_groups",
              "Group Chart By:",
              choices = c("farm_id", "animal_tag", "animal_type", "animal_subtype", "region", "subregion"),
              selected = c("farm_id", "animal_tag", "region", "subregion"),
              multiple = TRUE,
              options = list(plugins = list('remove_button'))
            )
          ),
          # --- Controls for Full Assessment (generate_impact_assessment) ---
          conditionalPanel(
            condition = "input.function_choice == 'generate_impact_assessment'",
            class = "col-md-3",
            selectInput(
              "plot_impact_category",
              "Impact Category:",
              choices = c(
                "Carbon Footprint (GHG)"          = "ghg",
                "Land Footprint (Land Use)"        = "land",
                "Water Deprivation (AWARE / EF 3.1)" = "water",
                "Multi-Impact Overview"            = "overview",
                "Protein Feed Efficiency (Mottet)"  = "fcr"
              ),
              selected = "ghg"
            )
          ),
          conditionalPanel(
            condition = "input.function_choice == 'generate_impact_assessment' && input.plot_impact_category != 'fcr'",
            class = "col-md-3",
            selectInput(
              "plot_functional_unit",
              "Functional Unit / Scale:",
              choices = c(
                "Total Farm / Herd"      = "total",
                "Per Commercial Product" = "product",
                "Per kg Edible Protein"  = "protein",
                "Per Animal Head"        = "head"
              ),
              selected = "total"
            )
          ),
          conditionalPanel(
            condition = "input.function_choice == 'generate_impact_assessment' && input.plot_impact_category != 'fcr' && input.plot_functional_unit == 'product'",
            class = "col-md-2",
            selectInput(
              "plot_product_subchoice",
              "Product:",
              choices = c(
                "Milk (FPCM)"     = "milk",
                "Meat (Carcass)"  = "meat",
                "Eggs (Fresh)"    = "egg",
                "Wool (Greasy)"   = "wool"
              ),
              selected = "milk"
            )
          ),

          # --- Controls for Feed Footprint (calculate_feed_emissions) ---
          conditionalPanel(
            condition = "input.function_choice == 'calculate_feed_emissions'",
            class = "col-md-3",
            selectInput(
              "plot_feed_metric",
              "Feed Impact Metric:",
              choices = c(
                "Feed GHG (Fossil vs LUC)" = "ghg",
                "Feed Water Deprivation"   = "water",
                "Multi-Impact Overview"    = "overview"
              ),
              selected = "ghg"
            )
          ),
          conditionalPanel(
            condition = "input.function_choice == 'calculate_feed_emissions' && input.plot_feed_metric != 'overview'",
            class = "col-md-3",
            selectInput(
              "plot_feed_scale",
              "Scale:",
              choices = c(
                "Total Herd Annual" = "total",
                "Per Animal Head"   = "head"
              ),
              selected = "total"
            )
          ),
          div(
            class = "col text-end align-self-center",
            downloadButton("download_plot", "Download Chart", class = "btn btn-outline-secondary btn-sm")
          )
        ),
        div(
          style = "margin-bottom: 1.5rem;",
          shinycssloaders::withSpinner(
            plotOutput("main_plot", height = "460px"),
            type = 6,
            color = "#2D5A38"
          )
        ),
        div(
          class = "d-flex justify-content-between align-items-center flex-wrap gap-2 mb-2 mt-4",
          div(
            class = "d-flex align-items-center gap-3 flex-wrap",
            h5("Detailed Model Outputs", class = "m-0 text-muted", style = "font-family: 'Fraunces', serif; font-weight: 700;"),
            conditionalPanel(
              condition = "input.function_choice == 'generate_impact_assessment'",
              div(
                class = "ms-2",
                radioButtons(
                  "results_table_choice",
                  label = NULL,
                  choices = c(
                    "Table 1: Total Inventory" = "totals",
                    "Table 2: Product & Protein Intensities" = "intensities"
                  ),
                  selected = "totals",
                  inline = TRUE
                )
              )
            )
          ),
          tags$span(class = "text-muted small", icon("table"), " Interactive preview — click column headers to sort, select cells to copy (Ctrl+C)")
        ),
        div(
          style = "background: var(--herdr-card); border-radius: 8px; border: 1px solid var(--herdr-border); margin-bottom: 1rem; overflow: hidden;",
          shinycssloaders::withSpinner(
            rhandsontable::rHandsontableOutput("table_results_hot", height = "360px"),
            type = 4,
            color = "#2D5A38"
          )
        )
      )
    )
  )
}
