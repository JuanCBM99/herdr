# ==============================================================================
# herdr — Server Logic & Reactive Pipeline
# ==============================================================================

herdr_server <- function(input, output, session) {

  # --- 1. Isolated Session Sandbox ---
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
  updateSelectInput(
    session,
    "data_source",
    choices = c(
      "My current data" = "current",
      if (example_paths != "") list.dirs(example_paths, full.names = FALSE, recursive = FALSE) else c()
    )
  )

  ipcc_mm_json_data <- reactive({
    df <- rv$ipcc_mm
    if (is.null(df) || nrow(df) == 0 || !all(MANURE_CASCADE_COLUMNS %in% names(df))) return(NULL)
    jsonlite::toJSON(df[, MANURE_CASCADE_COLUMNS, drop = FALSE], dataframe = "rows", auto_unbox = TRUE, na = "null")
  })

  observe({
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
    }, silent = TRUE)
  })

  # --- 2. Reactive State ---
  rv <- reactiveValues()
  dirty <- reactiveValues()
  table_render_triggers <- reactiveValues()
  model_data <- reactiveVal(NULL)

  load_all_data <- function(reset = FALSE) {
    for (id in names(tables_info)) {
      path <- file.path(user_data_tmp, tables_info[[id]]$file)
      loaded <- read_clean(path)

      if (id == "diet_ingr" && !"custom_yield_kg_ha" %in% names(loaded)) {
        loaded$custom_yield_kg_ha <- NA_character_
      }
      rv[[id]] <- if (reset) loaded[0, ] else loaded
      dirty[[id]] <- FALSE
      table_render_triggers[[id]] <- if (is.null(table_render_triggers[[id]])) 1 else table_render_triggers[[id]] + 1
    }
    model_data(NULL)
  }

  isolate(load_all_data(reset = FALSE))

  # --- 3. Sidebar Actions (Load / Reset / Upload) ---
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
        table_render_triggers[[id]] <- if (is.null(table_render_triggers[[id]])) 1 else table_render_triggers[[id]] + 1
        count <- count + 1
      }
    }
    if (count > 0) {
      showNotification(paste(count, "files updated!"), type = "message")
      nav_select("main_tabs", "census")
    }
  })

  observe({
    if (isTRUE(input$auto_cycle)) nav_show("main_tabs", "repro") else nav_hide("main_tabs", "repro")
  })

  # --- 4. Pre-Flight Inspection & Data Health UI ---
  project_validation_issues <- reactive({
    for (id in names(tables_info)) {
      force(rv[[id]])
    }
    validate_project_data(rv)
  })

  output$sidebar_data_health <- renderUI({
    issues <- project_validation_issues()
    errors <- Filter(function(x) x$severity == "error", issues)
    warnings <- Filter(function(x) x$severity == "warning", issues)

    if (length(errors) == 0 && length(warnings) == 0) {
      div(
        class = "p-2 mb-3 rounded-3 d-flex align-items-center gap-2",
        style = "background: rgba(45, 90, 56, 0.08); border: 1px solid rgba(45, 90, 56, 0.25); font-size: 0.85rem;",
        tags$i(class = "fa-solid fa-circle-check text-success fs-5"),
        div(
          tags$strong(class = "text-success", "Ready to Calculate"),
          tags$div(class = "text-muted small", "All cohorts & diets consistent.")
        )
      )
    } else if (length(errors) > 0) {
      div(
        class = "p-2 mb-3 rounded-3 d-flex align-items-center justify-content-between",
        style = "background: rgba(217, 83, 79, 0.08); border: 1px solid rgba(217, 83, 79, 0.25); font-size: 0.85rem;",
        div(
          class = "d-flex align-items-center gap-2",
          tags$i(class = "fa-solid fa-circle-exclamation text-danger fs-5"),
          div(
            tags$strong(class = "text-danger", paste(length(errors), "Issue(s) to Fix")),
            tags$div(class = "text-muted small", "Action required before run.")
          )
        ),
        actionButton("open_health_modal", "Review", class = "btn btn-sm btn-outline-danger px-2 py-1", style = "font-size: 0.75rem;")
      )
    } else {
      div(
        class = "p-2 mb-3 rounded-3 d-flex align-items-center justify-content-between",
        style = "background: rgba(199, 154, 46, 0.1); border: 1px solid rgba(199, 154, 46, 0.3); font-size: 0.85rem;",
        div(
          class = "d-flex align-items-center gap-2",
          tags$i(class = "fa-solid fa-triangle-exclamation text-warning fs-5"),
          div(
            tags$strong(style = "color: #b4750e;", paste(length(warnings), "Advisory Notice(s)")),
            tags$div(class = "text-muted small", "Minor notices (can run).")
          )
        ),
        actionButton("open_health_modal", "Review", class = "btn btn-sm btn-outline-warning px-2 py-1", style = "font-size: 0.75rem;")
      )
    }
  })

  show_data_health_modal <- function(issues, is_preflight = FALSE) {
    errors <- Filter(function(x) x$severity == "error", issues)
    warnings <- Filter(function(x) x$severity == "warning", issues)
    has_errors <- length(errors) > 0

    title_html <- if (has_errors) {
      tagList(icon("circle-exclamation", class = "text-danger me-2"), "Action Required Before Calculating")
    } else {
      tagList(icon("triangle-exclamation", class = "text-warning me-2"), "Data Health & Advisory Notices")
    }

    issue_cards <- lapply(seq_along(issues), function(i) {
      it <- issues[[i]]
      is_err <- it$severity == "error"
      border_color <- if (is_err) "#dc3545" else "#ffc107"
      bg_color <- if (is_err) "rgba(220, 53, 69, 0.04)" else "rgba(255, 193, 7, 0.04)"
      badge_bg <- if (is_err) "bg-danger" else "bg-warning text-dark"

      div(
        class = "p-3 mb-2 rounded-3 d-flex align-items-center justify-content-between gap-3",
        style = sprintf("border-left: 4px solid %s; border-top: 1px solid var(--herdr-border); border-right: 1px solid var(--herdr-border); border-bottom: 1px solid var(--herdr-border); background: %s;", border_color, bg_color),
        div(
          div(
            tags$span(class = paste("badge me-2", badge_bg), it$tab_label),
            tags$strong(it$title)
          ),
          div(class = "text-muted small mt-1", it$message)
        ),
        actionButton(
          paste0("health_jump_", it$table_id),
          paste("Go to", it$tab_label),
          class = "btn btn-sm btn-outline-secondary text-nowrap align-self-center",
          icon = icon("arrow-right")
        )
      )
    })

    intro_alert <- if (has_errors) {
      div(
        class = "alert alert-danger mb-3 py-2 small",
        icon("hand"), " ",
        strong("Calculation paused: "),
        "Some required information is missing or inconsistent. Please review the items below and update the indicated tables to proceed."
      )
    } else {
      div(
        class = "alert alert-warning mb-3 py-2 small",
        icon("circle-info"), " ",
        strong("Advisory notice: "),
        "All vital cohorts are configured. We detected minor notices below, but calculations can proceed safely."
      )
    }

    showModal(modalDialog(
      title = title_html,
      intro_alert,
      div(style = "max-height: 55vh; overflow-y: auto;", issue_cards),
      footer = tagList(
        modalButton("Close"),
        if (!has_errors && is_preflight) {
          actionButton("force_calculate", "Proceed to Calculate Anyway", class = "btn btn-success", icon = icon("play"))
        }
      ),
      size = "l",
      easyClose = TRUE
    ))
  }

  observeEvent(input$open_health_modal, {
    issues <- project_validation_issues()
    show_data_health_modal(issues, is_preflight = FALSE)
  })

  # Health Modal Quick Jump Handlers
  lapply(c(standard_ids, advanced_ids), function(tab_id) {
    observeEvent(input[[paste0("health_jump_", tab_id)]], {
      removeModal()
      if (tab_id %in% standard_ids) {
        nav_select("main_tabs", tab_id)
      } else if (tab_id %in% advanced_ids) {
        nav_select("main_tabs", "advanced_tab")
        nav_select("advanced_inner_tabs", tab_id)
      }
    }, ignoreInit = TRUE)
  })

  # --- 5. RHandsontable & Modal Form Handlers ---
  lapply(names(tables_info), function(id) {

    output[[paste0("table_", id)]] <- renderRHandsontable({
      table_render_triggers[[id]]

      # Re-render when this table's tab is activated so cross-table dropdown choices stay updated
      if (!is.null(input$main_tabs)) {
        if (id %in% standard_ids) {
          req(input$main_tabs == id)
        } else if (id %in% advanced_ids) {
          req(input$main_tabs == "advanced_tab")
          if (!is.null(input$advanced_inner_tabs)) {
            req(input$advanced_inner_tabs == id)
          }
        }
      }

      isolate({
        df <- rv[[id]]
        if (is.null(df) || ncol(df) == 0) return(NULL)

        tbl <- rhandsontable(df, rowHeaders = NULL, width = "100%", stretchH = "all",
                             minSpareRows = 1,
                             allowInsertColumn = FALSE, allowRemoveColumn = FALSE,
                             allowInsertRow = TRUE, allowRemoveRow = TRUE)

        tbl <- hot_cols(
          tbl,
          columnSorting = TRUE,
          fixedColumnsLeft = if (tables_info[[id]]$fixed > 0) tables_info[[id]]$fixed else NULL,
          allowInvalid = TRUE,
          validator = htmlwidgets::JS("function(value, callback) { callback(true); }")
        )
        tbl <- apply_dynamic_dropdowns(tbl, id, rv)

        if (id == "manure") {
          tbl <- apply_manure_cascade_dropdowns_fast(tbl, df, ipcc_mm_json_data())
        }

        if (id == "diet_ingr") {
          tbl <- apply_diet_ingredient_cascade(tbl, df, rv$feed_char)
          if ("custom_yield_kg_ha" %in% names(df)) {
            tbl <- hot_col(
              tbl,
              col = "custom_yield_kg_ha",
              type = "numeric",
              format = "0.0",
              allowInvalid = TRUE,
              validator = htmlwidgets::JS("function(value, callback) { callback(true); }")
            )
          }
        }
        tbl
      })
    })

    # Render dynamic table-level health banner
    output[[paste0("tab_health_banner_", id)]] <- renderUI({
      issues <- project_validation_issues()
      tab_issues <- Filter(function(x) x$table_id == id, issues)
      if (length(tab_issues) == 0) return(NULL)

      errors <- Filter(function(x) x$severity == "error", tab_issues)
      is_err <- length(errors) > 0

      alert_class <- if (is_err) "alert alert-danger" else "alert alert-warning"
      icon_name <- if (is_err) "circle-exclamation" else "triangle-exclamation"
      border_col <- if (is_err) "#dc3545" else "#ffc107"

      items_ui <- lapply(tab_issues, function(iss) {
        div(
          class = "mb-1",
          tags$strong(paste0(iss$title, ": ")),
          tags$span(iss$message)
        )
      })

      div(
        class = paste(alert_class, "d-flex align-items-start gap-2 mb-2 p-2 px-3 rounded-3 shadow-sm"),
        style = sprintf("border-left: 4px solid %s; font-size: 0.88rem;", border_col),
        icon(icon_name, class = if (is_err) "text-danger mt-1 fs-6" else "text-warning mt-1 fs-6"),
        div(class = "flex-grow-1", items_ui)
      )
    })

    # Render tab header status badge
    output[[paste0("dirty_badge_", id)]] <- renderUI({
      issues <- project_validation_issues()
      tab_issues <- Filter(function(x) x$table_id == id, issues)
      err_count <- sum(sapply(tab_issues, function(x) x$severity == "error"))
      warn_count <- sum(sapply(tab_issues, function(x) x$severity == "warning"))

      if (err_count > 0) {
        tags$span(class = "badge bg-danger rounded-pill ms-1", title = paste(err_count, "issue(s) in this table"), err_count)
      } else if (warn_count > 0) {
        tags$span(class = "badge bg-warning text-dark rounded-pill ms-1", title = paste(warn_count, "advisory in this table"), warn_count)
      } else if (isTRUE(dirty[[id]])) {
        tags$span(class = "badge bg-secondary rounded-pill ms-1", style = "font-size: 0.65rem;", "unsaved")
      } else {
        NULL
      }
    })

    observeEvent(input[[paste0("table_", id)]], {
      try({
        df <- suppressWarnings(hot_to_r(input[[paste0("table_", id)]]))
        if (is.data.frame(df)) {
          rv[[id]] <- df
          dirty[[id]] <- TRUE
        }
      }, silent = TRUE)
    }, ignoreInit = TRUE)

    observeEvent(input[[paste0("open_modal_", id)]], {
      df <- rv[[id]]
      req(df)

      get_dropdown_choices <- function(col_name) {
        opts <- get_dropdown_options(id, col_name, rv)
        if (length(opts) > 0) return(c("", opts))
        return(NULL)
      }

      input_fields <- lapply(names(df), function(col_name) {
        info_text <- if (col_name %in% names(modal_tooltips)) modal_tooltips[[col_name]] else ""
        label_html <- if (info_text != "") {
          tagList(col_name, tags$i(class = "fa-solid fa-circle-info ms-1 text-muted", title = info_text, style = "cursor:help;"))
        } else { col_name }

        choices <- get_dropdown_choices(col_name)
        if (!is.null(choices)) {
          rule <- if (id %in% names(dynamic_dropdowns)) dynamic_dropdowns[[id]][[col_name]] else NULL
          is_strict <- if (!is.null(rule) && !is.null(rule$strict)) rule$strict else TRUE
          if (!is_strict) {
            selectizeInput(
              paste0("modal_input_", id, "_", col_name),
              label = label_html,
              choices = choices,
              selected = "",
              options = list(create = TRUE, placeholder = "Select or type custom...")
            )
          } else {
            selectInput(paste0("modal_input_", id, "_", col_name), label = label_html, choices = choices, selected = "")
          }
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
      table_render_triggers[[id]] <- if (is.null(table_render_triggers[[id]])) 1 else table_render_triggers[[id]] + 1
      removeModal()
      showNotification("Record added successfully", type = "message")
    })

    output[[paste0("dirty_badge_", id)]] <- renderUI({
      badges <- list()
      if (isTRUE(dirty[[id]])) {
        badges[[length(badges) + 1]] <- tags$span(class = "dirty-dot", title = "Unsaved changes")
      }
      issues <- validate_project_data(rv)
      tbl_issues <- Filter(function(x) x$table_id == id, issues)
      if (length(tbl_issues) > 0) {
        has_err <- any(sapply(tbl_issues, function(x) x$severity == "error"))
        badge_class <- if (has_err) "badge bg-danger rounded-pill ms-1" else "badge bg-warning text-dark rounded-pill ms-1"
        badges[[length(badges) + 1]] <- tags$span(
          class = badge_class,
          style = "font-size: 0.65rem; padding: 0.2em 0.5em;",
          title = paste(sapply(tbl_issues, function(x) x$title), collapse = "\n"),
          length(tbl_issues)
        )
      }
      if (length(badges) > 0) tagList(badges) else NULL
    })
  })

  # --- 6. Manure Modal Cascades ---
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

  # --- 6b. Diet Ingredient Modal Cascades ---
  observeEvent(input$modal_input_diet_ingr_ingredient_type, {
    selected_type <- input$modal_input_diet_ingr_ingredient_type
    fc <- rv$feed_char
    if (!is.null(fc) && nrow(fc) > 0 && "ingredient_type" %in% names(fc) && "ingredient" %in% names(fc)) {
      if (!is.null(selected_type) && selected_type != "") {
        valid_ingrs <- sort(unique(na.omit(fc$ingredient[fc$ingredient_type == selected_type])))
      } else {
        valid_ingrs <- sort(unique(na.omit(fc$ingredient)))
      }
      valid_ingrs <- valid_ingrs[trimws(valid_ingrs) != ""]
      updateSelectizeInput(session, "modal_input_diet_ingr_ingredient", choices = c("", valid_ingrs), selected = "")
    }
  }, ignoreInit = TRUE)

  # --- 7. Execution Pipeline (Safe & Sandbox-Isolated) ---
  execute_calculation_pipeline <- function() {
    nav_select("main_tabs", "results_tab")
    session$sendCustomMessage("herdr_button_state", list(id = "calculate", loading = TRUE, text = "Running..."))
    on.exit(session$sendCustomMessage("herdr_button_state", list(id = "calculate", loading = FALSE, text = "")), add = TRUE)

    withProgress(message = "Running selected function...", value = 0, {
      incProgress(0.2, detail = "Saving tables...")

      for (id in names(tables_info)) {
        save_clean(rv[[id]], file.path(user_data_tmp, tables_info[[id]]$file))
        dirty[[id]] <- FALSE
      }

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
            if ("data_dir" %in% func_args) args_to_pass$data_dir <- user_data_tmp

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
  }

  observeEvent(input$calculate, {
    issues <- project_validation_issues()
    errors <- Filter(function(x) x$severity == "error", issues)
    if (length(errors) > 0) {
      show_data_health_modal(issues, is_preflight = TRUE)
      showNotification("Please resolve the highlighted items before calculating.", type = "warning")
      return()
    }
    execute_calculation_pipeline()
  })

  observeEvent(input$force_calculate, {
    removeModal()
    execute_calculation_pipeline()
  })

  # --- 8. Dynamic Results & Visualizations ---

  output$results_placeholder <- renderUI({
    if (is.null(model_data())) {
      div(
        class = "empty-state",
        div(class = "empty-state-icon", icon("chart-pie")),
        h4("No Results Yet", style = "font-family: 'Fraunces', serif; font-weight: 700;"),
        p(class = "text-muted mb-0", "Configure your inputs and click 'Run Selected' in the sidebar to view emissions analysis.")
      )
    } else {
      NULL
    }
  })

  # Dynamic KPI Summary Cards
  output$results_kpis <- renderUI({
    df <- model_data()
    if (is.null(df) || nrow(df) == 0) return(NULL)

    cards <- list()

    if ("CO2eq_Total_Gg" %in% names(df)) {
      total_co2 <- sum(df$CO2eq_Total_Gg, na.rm = TRUE)
      cards[[length(cards) + 1]] <- div(
        class = "kpi-card pasture",
        div(class = "kpi-icon-wrap", icon("cloud")),
        div(
          class = "kpi-data",
          span(class = "kpi-value", format(round(total_co2, 3), big.mark = ",")),
          span(class = "kpi-label", "Total GHG (Gg CO2eq)")
        )
      )
    }

    if ("population" %in% names(df)) {
      total_pop <- sum(df$population, na.rm = TRUE)
      cards[[length(cards) + 1]] <- div(
        class = "kpi-card wheat",
        div(class = "kpi-icon-wrap", icon("cow")),
        div(
          class = "kpi-data",
          span(class = "kpi-value", format(round(total_pop, 0), big.mark = ",")),
          span(class = "kpi-label", "Evaluated Animals (head)")
        )
      )
    }

    if ("total_land_use_m2" %in% names(df)) {
      total_land <- sum(df$total_land_use_m2, na.rm = TRUE)
      cards[[length(cards) + 1]] <- div(
        class = "kpi-card barn",
        div(class = "kpi-icon-wrap", icon("earth-americas")),
        div(
          class = "kpi-data",
          span(class = "kpi-value", format(round(total_land / 10000, 1), big.mark = ",")),
          span(class = "kpi-label", "Total Land Use (ha)")
        )
      )
    } else if ("Land_m2" %in% names(df)) {
      total_land <- sum(df$Land_m2, na.rm = TRUE)
      cards[[length(cards) + 1]] <- div(
        class = "kpi-card barn",
        div(class = "kpi-icon-wrap", icon("earth-americas")),
        div(
          class = "kpi-data",
          span(class = "kpi-value", format(round(total_land / 10000, 1), big.mark = ",")),
          span(class = "kpi-label", "Total Land Use (ha)")
        )
      )
    }

    if (length(cards) > 0) {
      div(class = "kpi-container", cards)
    } else {
      NULL
    }
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
