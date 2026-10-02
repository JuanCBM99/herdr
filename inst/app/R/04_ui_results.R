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
              choices = c("animal_tag", "animal_type", "animal_subtype", "region", "subregion", "class_flex"),
              selected = c("animal_tag", "region", "subregion", "class_flex"),
              multiple = TRUE,
              options = list(plugins = list('remove_button'))
            )
          ),
          conditionalPanel(
            condition = "input.function_choice == 'generate_impact_assessment'",
            class = "col-md-3",
            selectInput(
              "plot_functional_unit",
              "Functional Unit / Metric:",
              choices = c(
                "Total Emissions & Land"               = "total",
                "Per kg Edible Protein"                = "protein",
                "Per Commercial Product"               = "product",
                "Per Animal Head"                      = "head",
                "Protein Feed Efficiency (Mottet FCR)" = "fcr"
              ),
              selected = "total"
            )
          ),
          conditionalPanel(
            condition = "input.function_choice == 'generate_impact_assessment' && input.plot_functional_unit == 'product'",
            class = "col-md-3",
            selectInput(
              "plot_product_subchoice",
              "Commercial Product:",
              choices = c(
                "Primary / All Products" = "product",
                "Milk (kg FPCM)"         = "milk",
                "Meat (kg Carcass)"      = "meat",
                "Wool (kg Greasy Wool)"  = "wool",
                "Eggs (kg Fresh Eggs)"   = "egg"
              ),
              selected = "product"
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
          class = "d-flex justify-content-between align-items-center mb-2 mt-4",
          h5("Detailed Model Outputs", class = "m-0 text-muted", style = "font-family: 'Fraunces', serif; font-weight: 700;"),
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
