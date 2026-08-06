# Visual editor server domain -----------------------------------------------
#
# The browser editor keeps short-lived graph drafts for interaction, but the
# specification supplied by current_spec remains the sole model authority.

ctgui_visual_draft_set <- function(spec) {
  spec <- ctgui_visual_ensure(spec)
  list(
    state_space = ctgui_visual_graph(spec, "state_space"),
    initial_state = ctgui_visual_graph(spec, "initial_state"),
    tipred_effects = ctgui_visual_graph(spec, "tipred_effects")
  )
}

ctgui_visual_server <- function(input, output, session, current_spec,
    current_data, commit_current_spec, sync_matrix_inputs_from_spec,
    fit_status_value, matrix_status, send_message = NULL, notify = NULL) {
  stopifnot(
    is.function(current_spec), is.function(current_data),
    is.function(commit_current_spec), is.function(sync_matrix_inputs_from_spec),
    is.function(fit_status_value), is.function(matrix_status)
  )
  if (is.null(send_message)) {
    send_message <- function(type, message) session$sendCustomMessage(type, message)
  }
  if (is.null(notify)) notify <- shiny::showNotification

  drafts <- shiny::reactiveVal(list())
  status <- shiny::reactiveVal("Visual editor is loading the current matrices.")

  graph_for_view <- function(view = input$visual_view %||% "state_space") {
    graph <- drafts()[[view]]
    if (is.null(graph)) graph <- ctgui_visual_graph(current_spec(), view)
    graph
  }

  data_columns <- function() {
    data <- shiny::isolate(current_data())
    ctgui_data_columns(data)
  }

  send <- function(view = input$visual_view %||% "state_space") {
    send_message("ctgui-visual-load", list(
      id = "visual_spec_canvas",
      graph = graph_for_view(view),
      data_columns = data_columns(),
      data_roles = list(
        id = current_spec()$id,
        time = current_spec()$time,
        manifest = current_spec()$manifest_names,
        tdpred = current_spec()$tdpred_names,
        tipred = current_spec()$tipred_names
      )
    ))
  }

  refresh <- function(spec = current_spec(),
      view = input$visual_view %||% "state_space", send_graph = TRUE) {
    drafts(ctgui_visual_draft_set(spec))
    if (isTRUE(send_graph)) send(view)
    invisible(drafts())
  }

  reset <- function(message = "Reloaded visual editor from matrices.") {
    spec <- ctgui_visual_ensure(current_spec())
    commit_current_spec(
      spec, reason = "visual_reset", refresh_visual = FALSE,
      refresh_widgets = FALSE
    )
    refresh(spec)
    status(message)
    invisible(spec)
  }

  selected_edge <- shiny::reactive({
    selected <- input$visual_spec_canvas_selection
    if (is.null(selected) || is.null(selected$id)) return(NULL)
    graph <- graph_for_view(
      selected$view %||% input$visual_view %||% "state_space"
    )
    if (isTRUE(selected$parameter_node)) {
      spec <- current_spec()
      matrix <- as.character(selected$matrix)
      mat <- spec$matrices[[matrix]]
      if (is.null(mat)) return(NULL)
      row <- match(as.character(selected$row), rownames(mat))
      col <- match(as.character(selected$col), colnames(mat))
      if (is.na(row) || is.na(col)) return(NULL)
      style <- ctgui_visual_edge_style(
        spec, matrix, rownames(mat)[row], colnames(mat)[col], mat[row, col]
      )
      return(c(list(
        id = NULL, matrix = matrix, row = rownames(mat)[row],
        col = colnames(mat)[col], source = NULL, target = NULL,
        directed = TRUE, edge_kind = "parameter"
      ), style))
    }
    edges <- graph$edges %||% list()
    index <- which(vapply(edges, function(edge) {
      identical(as.character(edge$id), as.character(selected$id))
    }, logical(1L)))
    if (!length(index)) return(NULL)
    edge <- edges[[index[1L]]]
    if (isTRUE(edge$visual_only) ||
        identical(edge$edge_kind, "noise_input")) return(NULL)
    edge
  })

  output$visual_path_inspector <- shiny::renderUI({
    edge <- selected_edge()
    if (is.null(edge)) {
      return(shiny::div(
        class = "matrix-cell-inspector",
        shiny::tags$p("Select a path to edit its parameter settings.")
      ))
    }
    expression <- ctgui_parameter_is_expression(edge$value %||% "", current_spec()$latent_names)
    shiny::div(
      class = "matrix-cell-inspector visual-path-inspector",
      shiny::tags$h5(ctgui_matrix_cell_coordinate(
        current_spec(), edge$matrix, edge$row, edge$col
      )),
      shiny::tags$p(class = "matrix-note", ctgui_expression_metadata_guidance()),
      shiny::div(
        class = "control-grid",
        shiny::textInput(
          "visual_path_value", "Value / parameter label / expression",
          value = if (identical(edge$value, "__free__")) {
            ctgui_auto_label(edge$matrix, edge$row, edge$col)
          } else edge$value %||% ""
        ),
        if (!expression) shiny::tagList(
          shiny::checkboxInput(
            "visual_path_random", "RandomEffects",
            value = isTRUE(edge$indvarying)
          ),
          shiny::textInput(
            "visual_path_transform", "Transform",
            value = ctgui_display_transform(edge$transform)
          ),
          shiny::numericInput(
            "visual_path_sdscale", "RandomEffectsScale",
            value = suppressWarnings(as.numeric(edge$sdscale %||% 1)), step = 0.1
          ),
          if (length(current_spec()$tipred_names)) {
            shiny::selectizeInput(
              "visual_path_tipreds", "Time Independent Predictors",
              choices = current_spec()$tipred_names,
              selected = edge$tipred_effects %||% character(), multiple = TRUE
            )
          }
        ),
        shiny::textInput(
          "visual_path_extra_pars", "PARS (free parameters in expression)",
          value = edge$extra_pars %||% "",
          placeholder = "e.g. nonlinear_a, nonlinear_b"
        )
      )
    )
  })

  output$visual_pars_details <- shiny::renderUI({
    edge <- selected_edge()
    if (is.null(edge)) return(NULL)
    used <- ctgui_split_pars(edge$extra_pars)
    if (!length(used)) return(NULL)
    spec <- current_spec()
    pars <- spec$matrices[["PARS"]]
    if (is.null(pars)) return(NULL)
    rows <- which(as.character(pars[, 1L, drop = TRUE]) %in% used)
    if (!length(rows)) return(NULL)
    cards <- lapply(rows, function(row) {
      meta <- ctgui_matrix_metadata_row(
        spec, "PARS", rownames(pars)[row], colnames(pars)[1L]
      )
      if (is.null(meta)) return(NULL)
      tipreds <- spec$tipred_names[vapply(spec$tipred_names, function(tipred) {
        field <- paste0(tipred, "_effect")
        field %in% names(meta) && isTRUE(meta[[field]][1L])
      }, logical(1L))]
      scale <- suppressWarnings(as.numeric(meta$sdscale[1L]))
      if (is.na(scale)) scale <- 1
      prefix <- paste0("visual_pars_", row)
      shiny::div(
        class = "matrix-cell-inspector visual-path-inspector",
        shiny::tags$h5(as.character(pars[row, 1L])),
        shiny::div(
          class = "control-grid",
          shiny::checkboxInput(
            paste0(prefix, "_indvarying"), "RandomEffects",
            value = isTRUE(meta$indvarying[1L])
          ),
          shiny::textInput(
            paste0(prefix, "_transform"), "Transform",
            value = ctgui_display_transform(meta$transform[1L])
          ),
          shiny::numericInput(
            paste0(prefix, "_sdscale"), "RandomEffectsScale",
            value = scale, step = 0.1
          ),
          if (length(spec$tipred_names)) {
            shiny::selectizeInput(
              paste0(prefix, "_tipreds"), "Time Independent Predictors",
              choices = spec$tipred_names, selected = tipreds, multiple = TRUE
            )
          }
        )
      )
    })
    shiny::div(
      class = "matrix-pars-details",
      shiny::tags$h5("PARS parameter metadata"), cards
    )
  })

  shiny::observeEvent(input$visual_spec_canvas_graph, {
    graph <- input$visual_spec_canvas_graph
    if (is.null(graph$view)) return()
    if (isTRUE(graph$layout_only)) {
      updated <- ctgui_visual_save_layout(current_spec(), graph)
      next_drafts <- drafts()
      next_drafts[[graph$view]] <- graph
      drafts(next_drafts)
      commit_current_spec(
        updated, reason = "visual_layout", refresh_visual = FALSE
      )
      status("Visual layout saved.")
      return()
    }
    updated <- tryCatch(
      ctgui_visual_apply_graph(current_spec(), graph),
      error = function(e) e
    )
    if (inherits(updated, "error")) {
      notify(conditionMessage(updated), type = "error")
      return()
    }
    refresh(updated, view = graph$view, send_graph = FALSE)
    commit_current_spec(updated, reason = "visual_graph")
    sync_matrix_inputs_from_spec(updated)
    fit_status_value("Visual model changed. Refit when ready.")
    matrix_status("Visual change applied to model matrices.")
    status("Visual changes are applied directly to the current model.")
    send(graph$view)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$visual_spec_canvas_reset_layout, {
    view <- as.character(input$visual_spec_canvas_reset_layout$view %||%
      input$visual_view %||% "state_space")
    updated <- ctgui_visual_reset_layout(current_spec(), view)
    refresh(updated, view = view)
    commit_current_spec(updated, reason = "visual_layout_reset")
    status("Visual layout reset to its default positions.")
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$visual_view, {
    if (!length(drafts())) reset() else send(input$visual_view)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$model_tabs, {
    if (identical(input$model_tabs, "Visual Specification")) {
      reset("Loaded visual editor from the current model specification.")
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(current_data(), {
    if (length(drafts())) {
      send(input$visual_view %||% "state_space")
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$visual_spec_canvas_toggle_random_effect, {
    selected <- input$visual_spec_canvas_toggle_random_effect
    if (!is.list(selected) || !identical(selected$view, "tipred_effects")) return()
    matrix <- as.character(selected$matrix %||% "")
    row <- as.character(selected$row %||% "")
    col <- as.character(selected$col %||% "")
    meta <- ctgui_matrix_metadata_row(current_spec(), matrix, row, col)
    if (is.null(meta)) return()
    value <- current_spec()$matrices[[matrix]][row, col]
    if (!identical(matrix, "PARS") &&
        ctgui_parameter_is_expression(value, current_spec()$latent_names)) {
      notify("Random effects for an expression are set on its PARS parameters.", type = "warning")
      return()
    }
    enabled <- !isTRUE(meta$indvarying[1L])
    updated <- ctgui_set_parameter_metadata(
      current_spec(), matrix, row, col, indvarying = enabled
    )
    refresh(updated, view = "tipred_effects")
    commit_current_spec(updated, reason = "visual_random_effect")
    sync_matrix_inputs_from_spec(updated)
    fit_status_value("Visual model changed. Refit when ready.")
    matrix_status("Random effect updated in model matrices.")
    status(if (enabled) "Random effect enabled for the selected parameter." else "Random effect disabled for the selected parameter.")
  }, ignoreInit = TRUE)

  update_path <- function() {
    selected <- input$visual_spec_canvas_selection
    edge <- selected_edge()
    if (is.null(selected) || is.null(edge)) return(invisible(NULL))
    committed_input <- function(id) {
      payload <- input$visual_path_commit
      if (is.list(payload) && id %in% names(payload)) {
        payload[[id]]
      } else {
        input[[id]]
      }
    }
    previous_pars <- current_spec()$matrices[["PARS"]]
    previous_par_values <- if (is.null(previous_pars)) {
      character()
    } else {
      as.character(previous_pars[, 1L, drop = TRUE])
    }
    next_drafts <- drafts()
    view <- selected$view %||% input$visual_view %||% "state_space"
    graph <- next_drafts[[view]]
    index <- which(vapply(graph$edges, function(item) {
      identical(as.character(item$id), as.character(edge$id))
    }, logical(1L)))[1L]
    value <- trimws(committed_input("visual_path_value") %||% "")
    if (!nzchar(value)) value <- "__free__"
    item <- if (is.na(index)) edge else graph$edges[[index]]
    item$value <- value
    item$label <- if (identical(value, "__free__")) "free" else value
    item$fixed <- !is.na(suppressWarnings(
      as.numeric(strsplit(value, "|", fixed = TRUE)[[1L]][1L])
    ))
    item$custom <- nzchar(committed_input("visual_path_extra_pars") %||% "")
    item$indvarying <- isTRUE(committed_input("visual_path_random"))
    item$transform <- committed_input("visual_path_transform") %||% ""
    item$sdscale <- committed_input("visual_path_sdscale") %||% 1
    item$tipred_effects <- committed_input("visual_path_tipreds") %||% character()
    item$extra_pars <- committed_input("visual_path_extra_pars") %||% ""
    if (!is.na(index)) {
      graph$edges[[index]] <- item
      next_drafts[[view]] <- graph
      drafts(next_drafts)
    }
    updated <- tryCatch(
      ctgui_visual_update_edge(current_spec(), item), error = function(e) e
    )
    if (inherits(updated, "error")) {
      notify(conditionMessage(updated), type = "error")
      return(invisible(NULL))
    }
    pars <- updated$matrices[["PARS"]]
    used <- ctgui_split_pars(item$extra_pars)
    if (!is.null(pars) && length(used)) {
      for (row in which(as.character(pars[, 1L, drop = TRUE]) %in% used)) {
        parameter_name <- as.character(pars[row, 1L])
        if (!(parameter_name %in% previous_par_values)) next
        prefix <- paste0("visual_pars_", row)
        updated <- ctgui_set_parameter_metadata(
          updated, "PARS", rownames(pars)[row], colnames(pars)[1L],
          transform = committed_input(paste0(prefix, "_transform")) %||% NULL,
          indvarying =
            committed_input(paste0(prefix, "_indvarying")) %||% NULL,
          sdscale = committed_input(paste0(prefix, "_sdscale")) %||% NULL,
          tipred_effects =
            committed_input(paste0(prefix, "_tipreds")) %||% NULL
        )
      }
    }
    refresh(updated, view = view, send_graph = FALSE)
    commit_current_spec(updated, reason = "visual_path")
    sync_matrix_inputs_from_spec(updated)
    fit_status_value("Visual model changed. Refit when ready.")
    matrix_status("Visual path updated in model matrices.")
    status("Visual changes are applied directly to the current model.")
    if (identical(item$matrix, "T0MEANS")) {
      send("initial_state")
    } else {
      canonical_edge <- Filter(function(edge) identical(edge$id, item$id),
        graph_for_view(view)$edges %||% list())
      send_message("ctgui-visual-update-edge", list(
        id = "visual_spec_canvas",
        edge = if (length(canonical_edge)) canonical_edge[[1L]] else item
      ))
    }
    invisible(NULL)
  }

  shiny::observeEvent(input$visual_path_commit, update_path(), ignoreInit = TRUE)
  output$visual_status <- shiny::renderText(status())
  session$onFlushed(function() shiny::isolate(reset()), once = TRUE)

  list(
    reset = reset,
    refresh = refresh,
    send = send,
    drafts = drafts,
    status = status,
    selected_edge = selected_edge,
    update_path = update_path
  )
}
