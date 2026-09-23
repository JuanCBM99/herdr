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
        uiOutput("results_kpis"),
        div(
          class = "row mb-3 p-3 rounded align-items-center",
          style = "background: var(--herdr-paper); border: 1px solid var(--herdr-border);",
          div(
            class = "col-md-8",
            selectizeInput(
              "plot_groups",
              "Group Chart By:",
              choices = c("animal_tag", "region", "subregion", "class_flex"),
              selected = c("animal_tag", "region", "subregion", "class_flex"),
              multiple = TRUE,
              options = list(plugins = list('remove_button'))
            )
          ),
          div(
            class = "col-md-4 text-end",
            downloadButton("download_plot", "Download Chart", class = "btn btn-outline-secondary btn-sm")
          )
        ),
        div(
          style = "margin-bottom: 1.5rem;",
          shinycssloaders::withSpinner(
            plotOutput("main_plot", height = "420px"),
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
