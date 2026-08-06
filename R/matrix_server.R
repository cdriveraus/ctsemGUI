# Matrix editor server -------------------------------------------------------

# The matrix editor deliberately has one server entry point.  Pure helpers
# below accept explicit values so matrix semantics can be tested without a
# Shiny session.

ctgui_sync_matrix_inputs_from_spec <- function(
    session, input, spec, spec_inputs_suspended) {
  spec_inputs_suspended(TRUE)
  for (matrix_name in ctgui_matrix_names(spec)) {
    mat <- spec$matrices[[matrix_name]]
    if (!is.matrix(mat)) next
    for (row in seq_len(nrow(mat))) for (col in seq_len(ncol(mat))) {
      shiny::updateTextInput(
        session, ctgui_matrix_cell_id(matrix_name, row, col),
        value = as.character(mat[row, col])
      )
    }
  }
  shiny::updateSelectInput(
    session, "model_visual_matrix", choices = ctgui_matrix_names(spec),
    selected = input$model_visual_matrix %||% "DRIFT"
  )
  shiny::updateTextInput(
    session, "latent_names", value = paste(spec$latent_names, collapse = ", ")
  )
  shiny::updateTextInput(
    session, "manifest_names", value = paste(spec$manifest_names, collapse = ", ")
  )
  shiny::updateSelectizeInput(
    session, "tdpred_names", selected = spec$tdpred_names
  )
  shiny::updateSelectizeInput(
    session, "tipred_names", selected = spec$tipred_names
  )
  shiny::updateCheckboxInput(
    session, "tipredDefault", value = isTRUE(spec$tipredDefault)
  )
  session$onFlushed(function() spec_inputs_suspended(FALSE), once = TRUE)
  invisible(spec)
}

ctgui_matrix_network_id <- function(matrix_name, field) {
  paste0(
    "matrix_network_", ctgui_matrix_id_part(matrix_name), "_", field
  )
}

ctgui_network_matrix_cell_active <- function(value) {
  value <- trimws(as.character(value %||% ""))
  if (!nzchar(value) ||
      grepl("\\|\\|\\s*FALSE\\s*$", value, ignore.case = TRUE)) {
    return(FALSE)
  }
  numeric_value <- suppressWarnings(as.numeric(value))
  if (!is.na(numeric_value)) return(!isTRUE(all.equal(numeric_value, 0)))
  TRUE
}

ctgui_draw_matrix_network <- function(spec, matrix_name, options) {
  mat <- ctgui_matrix(spec, matrix_name)
  covariance <- matrix_name %in% c(
    "T0VAR", "DIFFUSION", "MANIFESTVAR", "TDPREDVAR"
  )
  edges <- data.frame(
    from = character(), to = character(), label = character(),
    loop = logical(), fixed = logical(), stringsAsFactors = FALSE
  )
  for (r in seq_len(nrow(mat))) for (c in seq_len(ncol(mat))) {
    value <- as.character(mat[r, c])
    if (!ctgui_network_matrix_cell_active(value) ||
        (covariance && c > r)) next
    from <- if (matrix_name == "LAMBDA") {
      colnames(mat)[c]
    } else if (matrix_name %in%
        c("CINT", "T0MEANS", "MANIFESTMEANS", "TDPREDMEANS")) {
      "constant"
    } else {
      colnames(mat)[c]
    }
    to <- rownames(mat)[r]
    loop <- identical(from, to)
    if (loop && !isTRUE(options$self_effects)) next
    fixed <- !is.na(suppressWarnings(as.numeric(trimws(value))))
    edges <- rbind(
      edges,
      data.frame(
        from = from, to = to,
        label = strsplit(value, "|", fixed = TRUE)[[1L]][1L],
        loop = loop, fixed = fixed, stringsAsFactors = FALSE
      )
    )
  }
  if (!nrow(edges)) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate(
          "text", x = 0, y = 0, label = "No active paths in this matrix"
        ) +
        ggplot2::theme_void()
    )
  }
  pair_key <- vapply(
    seq_len(nrow(edges)),
    function(i) paste(sort(c(edges$from[i], edges$to[i])), collapse = "\r"),
    character(1L)
  )
  edges$label_pos <- 0.5
  for (key in unique(pair_key[!edges$loop])) {
    index <- which(pair_key == key & !edges$loop)
    if (length(index) > 1L) {
      edges$label_pos[index] <- seq(0.32, 0.68, length.out = length(index))
    }
  }
  nodes <- unique(c(edges$from, edges$to))
  sources <- unique(edges$from)
  targets <- unique(edges$to)
  bipartite <- length(setdiff(sources, targets)) > 0L &&
    length(setdiff(targets, sources)) > 0L
  if (bipartite) {
    coords <- rbind(
      data.frame(
        name = sources, x = rep(-options$spread, length(sources)),
        y = seq(1, -1, length.out = length(sources)) * options$spread
      ),
      data.frame(
        name = targets, x = rep(options$spread, length(targets)),
        y = seq(1, -1, length.out = length(targets)) * options$spread
      )
    )
    coords <- coords[!duplicated(coords$name), , drop = FALSE]
  } else {
    radius <- options$spread * (1 + 0.06 * max(0, length(nodes) - 4L))
    theta <- seq(
      pi / 2, pi / 2 + 2 * pi, length.out = length(nodes) + 1L
    )[-length(nodes) - 1L]
    coords <- data.frame(
      name = nodes, x = radius * cos(theta), y = radius * sin(theta)
    )
  }
  coords$node_type <- if (identical(matrix_name, "LAMBDA")) {
    ifelse(coords$name %in% colnames(mat), "latent", "observed")
  } else {
    "process"
  }
  arrow_end <- if (!covariance) {
    grid::arrow(
      length = grid::unit(5 * options$general_scale, "mm"), type = "closed"
    )
  } else NULL
  arrow_loop <- if (!covariance) {
    grid::arrow(
      length = grid::unit(5 * options$general_scale, "mm"), type = "closed",
      ends = "both"
    )
  } else NULL
  path_strength <- 0.5 + options$label_offset * 2.5
  label_size <- max(2.5, 3.5 * options$text_scale)
  edges$label_size <- label_size
  edges$edge_colour <- ifelse(
    edges$fixed, "#94a3b8", if (covariance) "#7c3aed" else "#2563eb"
  )
  edges$edge_linetype <- ifelse(edges$fixed, "dashed", "solid")
  graph <- igraph::graph_from_data_frame(
    edges[, c(
      "from", "to", "label", "loop", "fixed", "label_pos", "label_size",
      "edge_colour", "edge_linetype"
    )],
    directed = !covariance, vertices = coords
  )
  plot <- ggraph::ggraph(
    graph, layout = "manual", x = coords$x, y = coords$y
  ) +
    ggraph::geom_edge_fan(
      ggplot2::aes(
        label = label, label_pos = label_pos, label_size = label_size,
        edge_colour = edge_colour, edge_linetype = edge_linetype,
        filter = !loop
      ),
      arrow = arrow_end, strength = path_strength,
      label_dodge = grid::unit(options$label_offset, "cm"),
      edge_width = 0.8 * options$general_scale
    ) +
    ggraph::geom_edge_loop(
      ggplot2::aes(
        label = label, label_size = label_size,
        edge_colour = edge_colour, edge_linetype = edge_linetype,
        filter = loop
      ),
      arrow = arrow_loop,
      label_dodge = grid::unit(options$label_offset, "cm"),
      edge_width = 0.8 * options$general_scale
    ) +
    ggraph::scale_edge_colour_identity() +
    ggraph::scale_edge_linetype_identity()
  if (identical(matrix_name, "LAMBDA")) {
    plot <- plot +
      ggraph::geom_node_point(
        ggplot2::aes(filter = node_type == "latent"), shape = 21,
        size = 8 * options$general_scale, fill = "#dbeafe",
        colour = "#1d4ed8", stroke = 0.8
      ) +
      ggraph::geom_node_text(
        ggplot2::aes(label = name, filter = node_type == "latent"),
        size = label_size
      ) +
      ggraph::geom_node_label(
        ggplot2::aes(label = name, filter = node_type == "observed"),
        size = label_size, fill = "#f8fafc", colour = "#334155",
        label.size = 0.5
      )
  } else {
    plot <- plot +
      ggraph::geom_node_point(
        shape = 21, size = 8 * options$general_scale, fill = "#dbeafe",
        colour = "#1d4ed8", stroke = 0.8
      ) +
      ggraph::geom_node_text(ggplot2::aes(label = name), size = label_size)
  }
  plot +
    ggplot2::coord_fixed(clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(16, 32, 16, 32)) +
    ggplot2::ggtitle(
      paste(matrix_name, if (covariance) "covariances" else "paths")
    )
}

ctgui_compose_quick_matrix_value <- function(
    mode, label, value, matrix_name, row, col, tipred_names = character()) {
  mode <- mode %||% "fixed"
  label <- trimws(label %||% "")
  value <- trimws(value %||% "")
  if (identical(mode, "fixed")) {
    return(list(value = suppressWarnings(as.numeric(value)), label = NULL))
  }
  if (!nzchar(label)) {
    label <- ctgui_auto_label(matrix_name, row, col)
  }
  if (identical(mode, "free")) return(list(value = NULL, label = label))
  if (identical(mode, "random")) {
    return(list(value = NULL, label = paste0(label, "||TRUE")))
  }
  if (identical(mode, "ti")) {
    if (!nzchar(value)) value <- paste(tipred_names, collapse = ",")
    return(list(value = NULL, label = paste0(label, "||TRUE||", value)))
  }
  list(value = NULL, label = label)
}

ctgui_apply_matrix_batch <- function(spec, matrix_values, metadata_values) {
  updated <- spec
  changed <- character()
  for (matrix_name in names(matrix_values)) {
    value <- matrix_values[[matrix_name]]
    old <- updated$matrices[[matrix_name]]
    if (is.null(value) && is.null(old)) next
    if (!is.null(value) && !is.null(old) &&
        identical(as.character(old), as.character(value))) next
    updated <- ctgui_set_spec_matrix(updated, matrix_name, value)
    changed <- c(changed, matrix_name)
  }
  for (metadata in metadata_values) {
    before <- updated$parameter_metadata
    updated <- ctgui_set_parameter_metadata(
      updated,
      metadata$matrix, metadata$row, metadata$col,
      transform = metadata$transform,
      indvarying = metadata$indvarying,
      sdscale = metadata$sdscale,
      tipred_effects = metadata$tipred_effects,
      extra_pars = metadata$extra_pars,
      sync = FALSE
    )
    if (!identical(before, updated$parameter_metadata)) {
      changed <- unique(c(changed, "parameter options"))
    }
  }
  list(spec = updated, changed = unique(changed))
}

ctgui_matrix_network_controls <- function(matrix_name) {
  id <- function(field) ctgui_matrix_network_id(matrix_name, field)
  shiny::div(
    class = "matrix-network-controls",
    shiny::sliderInput(
      id("spread"), "Node spacing", min = 0.4, max = 4.5,
      value = 1.5, step = 0.1
    ),
    shiny::sliderInput(
      id("general_scale"), "General scale", min = 0.35, max = 3.5,
      value = 1, step = 0.05
    ),
    shiny::sliderInput(
      id("text_scale"), "Text scale", min = 0.35, max = 3.5,
      value = 1, step = 0.05
    ),
    shiny::sliderInput(
      id("label_offset"), "Path-label offset", min = 0, max = 1.2,
      value = 0.18, step = 0.02
    ),
    shiny::checkboxInput(
      id("self_effects"), "Show self-effect arrows", value = TRUE
    )
  )
}

ctgui_matrix_server <- function(
    input, output, session, current_spec, commit_current_spec,
    matrix_status, fit_status_value, visual_refresh, register_plot_export,
    plot_cache, arg_label) {
  matrix_group_names <- function(spec, group = input$matrix_group) {
    ctgui_matrix_group_names(spec, group)
  }
  matrix_network_options <- function(matrix_name) {
    id <- function(field) ctgui_matrix_network_id(matrix_name, field)
    list(
      spread = input[[id("spread")]] %||% 1.5,
      general_scale = input[[id("general_scale")]] %||% 1,
      text_scale = input[[id("text_scale")]] %||% 1,
      label_offset = input[[id("label_offset")]] %||% 0.18,
      self_effects = isTRUE(input[[id("self_effects")]] %||% TRUE)
    )
  }
  metadata_badges <- function(spec, matrix_name, row_name, col_name) {
    meta <- ctgui_matrix_metadata_row(
      spec, matrix_name, row_name, col_name
    )
    if (is.null(meta)) return(NULL)
    tipreds <- spec$tipred_names[vapply(
      spec$tipred_names,
      function(tipred) {
        field <- paste0(tipred, "_effect")
        field %in% names(meta) && isTRUE(meta[[field]][1L])
      },
      logical(1L)
    )]
    scale <- suppressWarnings(as.numeric(meta$sdscale[1L]))
    if (is.na(scale)) scale <- if (isTRUE(meta$sdscale[1L])) 1 else 0
    shiny::div(
      class = "matrix-cell-metadata",
      shiny::div(
        "Transform: ", ctgui_display_transform(meta$transform[1L])
      ),
      shiny::div(
        "RandomEffects: ",
        if (isTRUE(meta$indvarying[1L])) "TRUE" else "FALSE"
      ),
      shiny::div("RandomEffectsScale: ", format(scale, trim = TRUE)),
      shiny::div(
        "Time Independent Predictors: ",
        if (length(tipreds)) paste(tipreds, collapse = ", ") else "-"
      )
    )
  }
  selected_matrix_cell <- shiny::reactive({
    selected <- input$matrix_selected_cell
    if (is.null(selected) || !is.list(selected)) return(NULL)
    row <- suppressWarnings(as.integer(selected$row))
    col <- suppressWarnings(as.integer(selected$col))
    if (is.null(selected$matrix) || is.na(row) || is.na(col) ||
        row < 1L || col < 1L) return(NULL)
    list(matrix = as.character(selected$matrix), row = row, col = col)
  })
  matrix_cell_inspector <- function(matrix_name) {
    selected <- selected_matrix_cell()
    if (is.null(selected) || !identical(selected$matrix, matrix_name)) {
      return(shiny::div(
        class = "matrix-cell-inspector",
        shiny::helpText(
          "Select a free matrix cell to view and edit its parameter settings."
        )
      ))
    }
    spec <- current_spec()
    mat <- ctgui_matrix(spec, matrix_name)
    if (selected$row > nrow(mat) || selected$col > ncol(mat)) return(NULL)
    value <- input[[
      ctgui_matrix_cell_id(matrix_name, selected$row, selected$col)
    ]] %||% mat[selected$row, selected$col]
    if (!ctgui_active_matrix_cell(value)) {
      return(shiny::div(
        class = "matrix-cell-inspector",
        shiny::helpText(paste(
          "Fixed numeric cells do not have parameter settings. Enter a free",
          "parameter label to edit RandomEffects, Transform,",
          "RandomEffectsScale, or Time Independent Predictors."
        ))
      ))
    }
    row_name <- rownames(mat)[selected$row]
    col_name <- colnames(mat)[selected$col]
    meta <- ctgui_matrix_metadata_row(
      spec, matrix_name, row_name, col_name
    )
    if (is.null(meta)) {
      return(shiny::div(
        class = "matrix-cell-inspector",
        shiny::helpText(
          "Apply the free parameter label before editing its settings."
        )
      ))
    }
    tipreds <- spec$tipred_names[vapply(
      spec$tipred_names,
      function(tipred) {
        field <- paste0(tipred, "_effect")
        field %in% names(meta) && isTRUE(meta[[field]][1L])
      },
      logical(1L)
    )]
    scale <- suppressWarnings(as.numeric(meta$sdscale[1L]))
    if (is.na(scale)) scale <- 1
    expression <- ctgui_parameter_is_expression(value, spec$latent_names)
    id <- function(field) {
      ctgui_matrix_meta_id(
        matrix_name, selected$row, selected$col, field
      )
    }
    shiny::div(
      class = "matrix-cell-inspector",
      shiny::tags$h5("Cell Specific Details"),
      shiny::tags$p(
        class = "help-note",
        paste0(
          "Selected cell: ", ctgui_matrix_cell_coordinate(
            spec, matrix_name, row_name, col_name
          ),
          "]. Settings apply to the free parameter in this cell and are ",
          "saved automatically."
        )
      ),
      shiny::tags$p(class = "matrix-note", ctgui_expression_metadata_guidance()),
      shiny::div(
        class = "control-grid",
        if (!expression) shiny::tagList(
          shiny::checkboxInput(
            id("indvarying"),
            arg_label("RandomEffects", "help_matrix_random_effects"),
            value = isTRUE(meta$indvarying[1L])
          ),
          shiny::textInput(
            id("transform"), arg_label("Transform", "help_matrix_transform"),
            value = ctgui_display_transform(meta$transform[1L])
          ),
          shiny::numericInput(
            id("sdscale"),
            arg_label(
              "RandomEffectsScale", "help_matrix_random_effects_scale"
            ),
            value = scale, step = 0.1
          ),
          if (length(spec$tipred_names)) shiny::selectizeInput(
            id("tipreds"),
            arg_label(
              "Time Independent Predictors",
              "help_matrix_time_independent_predictors"
            ),
            choices = spec$tipred_names, selected = tipreds, multiple = TRUE
          )
        ),
        shiny::textInput(
          id("extra_pars"), "PARS (free parameters in expression)",
          value = meta$extra_pars[1L] %||% "",
          placeholder = "e.g. nonlinear_a, nonlinear_b"
        )
      )
    )
  }
  matrix_pars_details <- function(matrix_name) {
    spec <- current_spec()
    metadata <- spec$parameter_metadata
    if (is.null(metadata) || !nrow(metadata) ||
        !"extra_pars" %in% names(metadata)) return(NULL)
    parent <- metadata[metadata$matrix == matrix_name, , drop = FALSE]
    used <- ctgui_split_pars(parent$extra_pars)
    if (!length(used)) return(NULL)
    pars <- spec$matrices[["PARS"]]
    if (is.null(pars)) return(NULL)
    rows <- which(as.character(pars[, 1L, drop = TRUE]) %in% used)
    if (!length(rows)) return(NULL)
    cards <- lapply(rows, function(row) {
      meta <- ctgui_matrix_metadata_row(
        spec, "PARS", rownames(pars)[row], colnames(pars)[1L]
      )
      if (is.null(meta)) {
        return(shiny::helpText(paste(
          "Parameter", pars[row, 1L],
          "will be available after the next model update."
        )))
      }
      tipreds <- spec$tipred_names[vapply(
        spec$tipred_names,
        function(tipred) {
          field <- paste0(tipred, "_effect")
          field %in% names(meta) && isTRUE(meta[[field]][1L])
        },
        logical(1L)
      )]
      scale <- suppressWarnings(as.numeric(meta$sdscale[1L]))
      if (is.na(scale)) scale <- 1
      id <- function(field) ctgui_matrix_meta_id("PARS", row, 1L, field)
      shiny::div(
        class = "matrix-cell-inspector",
        shiny::tags$h5(as.character(pars[row, 1L])),
        shiny::div(
          class = "control-grid",
          shiny::checkboxInput(
            id("indvarying"), "RandomEffects",
            value = isTRUE(meta$indvarying[1L])
          ),
          shiny::textInput(
            id("transform"), "Transform",
            value = ctgui_display_transform(meta$transform[1L])
          ),
          shiny::numericInput(
            id("sdscale"), "RandomEffectsScale", value = scale, step = 0.1
          ),
          if (length(spec$tipred_names)) shiny::selectizeInput(
            id("tipreds"), "Time Independent Predictors",
            choices = spec$tipred_names, selected = tipreds, multiple = TRUE
          )
        )
      )
    })
    shiny::div(
      class = "matrix-pars-details",
      shiny::tags$h5(paste("PARS parameters used by", matrix_name)),
      shiny::tags$p(
        class = "matrix-note",
        paste(
          "These additional free parameters are included once in the",
          "combined PARS vector. Their settings are saved automatically."
        )
      ),
      cards
    )
  }
  matrix_editor_block <- function(spec, matrix_name) {
    mat <- ctgui_matrix(spec, matrix_name)
    inactive_names <- if (identical(matrix_name, "T0VAR")) {
      ctgui_indvarying_t0means(spec)
    } else character()
    header <- shiny::tags$tr(
      shiny::tags$th(""), lapply(colnames(mat), shiny::tags$th)
    )
    rows <- lapply(seq_len(nrow(mat)), function(row) {
      shiny::tags$tr(
        shiny::tags$th(rownames(mat)[row]),
        lapply(seq_len(ncol(mat)), function(col) {
          inactive <- identical(matrix_name, "T0VAR") &&
            (rownames(mat)[row] %in% inactive_names ||
              colnames(mat)[col] %in% inactive_names)
          shiny::tags$td(
            class = if (inactive) "matrix-inactive" else NULL,
            shiny::div(
              class = "matrix-cell", `data-matrix` = matrix_name,
              `data-row` = row, `data-col` = col,
              shiny::textInput(
                ctgui_matrix_cell_id(matrix_name, row, col),
                label = NULL, value = as.character(mat[row, col]),
                width = "100%"
              ) |>
                shiny::tagAppendAttributes(
                  disabled = if (inactive) "disabled" else NULL
                ),
              if (!inactive && ctgui_active_matrix_cell(mat[row, col])) {
                metadata_badges(
                  spec, matrix_name, rownames(mat)[row], colnames(mat)[col]
                )
              }
            )
          )
        })
      )
    })
    id_part <- ctgui_matrix_id_part(matrix_name)
    shiny::div(
      class = "matrix-block",
      shiny::tags$h4(matrix_name),
      shiny::tags$p(class = "matrix-note", ctgui_matrix_note(matrix_name)),
      if (identical(matrix_name, "T0VAR") && length(inactive_names)) {
        shiny::tags$p(
          class = "matrix-note",
          paste(
            "Inactive cells involve", paste(inactive_names, collapse = ", "),
            paste(
              "because those T0MEANS entries are individual-varying.",
              "ctsem fixes the corresponding T0VAR rows and columns."
            )
          )
        )
      },
      shiny::div(
        class = "matrix-editor",
        shiny::tags$table(
          class = "table table-condensed",
          shiny::tags$thead(header), shiny::tags$tbody(rows)
        )
      ),
      shiny::uiOutput(paste0("matrix_cell_inspector_", id_part)),
      shiny::uiOutput(paste0("matrix_pars_details_", id_part)),
      shiny::div(
        class = "matrix-network",
        shiny::tags$h5("Network diagram"),
        shiny::div(
          class = "matrix-network-layout",
          ctgui_matrix_network_controls(matrix_name),
          shiny::div(
            class = "matrix-network-plot",
            shiny::plotOutput(
              paste0("matrix_network_", id_part), height = 380
            )
          )
        ),
        ctgui_plot_export_controls(paste0("matrix_network_", id_part), 380)
      )
    )
  }
  matrix_section_tabs <- function(group) {
    spec <- current_spec()
    names <- matrix_group_names(spec, group)
    if (!length(names)) {
      if (identical(group, "Predictors")) {
        return(shiny::div(
          class = "matrix-block",
          shiny::helpText(paste(
            "Add time-dependent predictors in Specification to edit",
            "predictor matrices."
          ))
        ))
      }
      return(shiny::div(
        class = "matrix-block",
        shiny::helpText("No matrices are available for this model section.")
      ))
    }
    panels <- lapply(names, function(matrix_name) {
      shiny::tabPanel(
        matrix_name, value = matrix_name,
        matrix_editor_block(spec, matrix_name)
      )
    })
    do.call(
      shiny::tabsetPanel,
      c(
        list(
          id = paste0("matrix_", tolower(group), "_tabs"), type = "pills"
        ),
        panels
      )
    )
  }
  output$matrix_dynamics_editor <- shiny::renderUI(
    matrix_section_tabs("Dynamics")
  )
  output$matrix_measurement_editor <- shiny::renderUI(
    matrix_section_tabs("Measurement")
  )
  output$matrix_initial_editor <- shiny::renderUI(
    matrix_section_tabs("Initial")
  )
  output$matrix_predictor_editor <- shiny::renderUI(
    matrix_section_tabs("Predictors")
  )
  shiny::observe({
    spec <- current_spec()
    for (matrix_name in setdiff(ctgui_matrix_names(spec), "PARS")) local({
      name <- matrix_name
      id_part <- ctgui_matrix_id_part(name)
      output_id <- paste0("matrix_network_", id_part)
      inspector_id <- paste0("matrix_cell_inspector_", id_part)
      pars_details_id <- paste0("matrix_pars_details_", id_part)
      register_plot_export(output_id)
      output[[inspector_id]] <- shiny::renderUI(matrix_cell_inspector(name))
      output[[pars_details_id]] <- shiny::renderUI(matrix_pars_details(name))
      output[[output_id]] <- shiny::renderPlot({
        on.exit({
          plot_cache[[output_id]] <- grDevices::recordPlot()
        }, add = TRUE)
        print(ctgui_draw_matrix_network(
          current_spec(), name, matrix_network_options(name)
        ))
      }, height = 380)
    })
  })
  output$matrix_pars_editor <- shiny::renderUI({
    spec <- current_spec()
    shiny::div(
      class = "matrix-block pars-editor",
      shiny::tags$h4("PARS"),
      shiny::tags$p(
        class = "matrix-note",
        paste(
          "Extra parameter vector for nonlinear or custom expressions."
        )
      ),
      shiny::textAreaInput(
        "pars_vector", "PARS vector",
        value = paste(ctgui_pars_vector(spec), collapse = "\n"),
        width = "100%", height = "180px"
      )
    )
  })
  output$matrix_quick_editor <- shiny::renderUI({
    spec <- current_spec()
    matrix_names <- setdiff(ctgui_matrix_names(spec), "PARS")
    if (!length(matrix_names)) return(NULL)
    matrix_name <- input$quick_matrix %||% matrix_names[1L]
    if (!matrix_name %in% matrix_names) matrix_name <- matrix_names[1L]
    mat <- ctgui_matrix(spec, matrix_name)
    row_choices <- rownames(mat) %||% as.character(seq_len(nrow(mat)))
    col_choices <- colnames(mat) %||% as.character(seq_len(ncol(mat)))
    shiny::div(
      class = "control-grid",
      shiny::selectInput(
        "quick_matrix", "Structured edit matrix",
        choices = matrix_names, selected = matrix_name
      ),
      shiny::selectInput("quick_row", "Row", choices = row_choices),
      shiny::selectInput("quick_col", "Column", choices = col_choices),
      shiny::selectInput(
        "quick_mode", "Cell mode",
        choices = c(
          "Fixed numeric" = "fixed", "Free parameter" = "free",
          "Free + random effects" = "random",
          "Free + TI moderation" = "ti", "Custom expression" = "custom"
        )
      ),
      shiny::textInput("quick_label", "Label / expression", value = ""),
      shiny::textInput(
        "quick_value", "Fixed value / TI predictors", value = "0"
      ),
      shiny::actionButton("quick_apply", "Apply structured edit")
    )
  })
  shiny::observeEvent(input$quick_apply, {
    spec <- current_spec()
    if (is.null(input$quick_matrix) ||
        !input$quick_matrix %in% ctgui_matrix_names(spec)) return()
    new_value <- ctgui_compose_quick_matrix_value(
      input$quick_mode, input$quick_label, input$quick_value,
      input$quick_matrix, input$quick_row, input$quick_col,
      spec$tipred_names
    )
    if (!is.null(new_value$value) &&
        (length(new_value$value) != 1L || is.na(new_value$value))) {
      shiny::showNotification("Fixed value must be numeric", type = "error")
      return()
    }
    updated <- tryCatch(
      ctgui_set_matrix_value(
        spec, input$quick_matrix, input$quick_row, input$quick_col,
        value = new_value$value, label = new_value$label
      ),
      error = function(e) e
    )
    if (inherits(updated, "error")) {
      shiny::showNotification(conditionMessage(updated), type = "error")
      return()
    }
    commit_current_spec(updated, reason = "matrix_metadata")
    matrix_status(paste(
      "Structured edit applied to", input$quick_matrix
    ))
  })

  matrix_input_values <- function(commit = NULL) {
    spec <- current_spec()
    if (identical(input$matrix_group, "PARS")) {
      if (is.null(input$pars_vector)) return(NULL)
      return(list(PARS = ctgui_parse_pars_vector(input$pars_vector)))
    }
    names <- matrix_group_names(spec)
    if (!length(names)) return(list())
    out <- list()
    for (matrix_name in names) {
      mat <- ctgui_matrix(spec, matrix_name)
      values <- matrix(
        "", nrow = nrow(mat), ncol = ncol(mat), dimnames = dimnames(mat)
      )
      for (row in seq_len(nrow(mat))) for (col in seq_len(ncol(mat))) {
        input_id <- ctgui_matrix_cell_id(matrix_name, row, col)
        value <- input[[input_id]]
        if (is.list(commit) && identical(commit$id, input_id)) {
          value <- commit$value
        }
        if (is.null(value)) value <- mat[row, col]
        values[row, col] <- if (!nzchar(value)) "0" else value
      }
      out[[matrix_name]] <- values
    }
    out
  }
  matrix_metadata_values <- function(spec) {
    names <- unique(c(
      matrix_group_names(spec),
      if (!is.null(spec$matrices[["PARS"]])) "PARS" else character()
    ))
    values <- list()
    for (matrix_name in names) {
      mat <- spec$matrices[[matrix_name]]
      if (is.null(mat)) next
      for (row in seq_len(nrow(mat))) for (col in seq_len(ncol(mat))) {
        meta <- ctgui_matrix_metadata_row(
          spec, matrix_name, rownames(mat)[row], colnames(mat)[col]
        )
        if (is.null(meta)) next
        id <- function(field) {
          ctgui_matrix_meta_id(matrix_name, row, col, field)
        }
        transform <- input[[id("transform")]]
        indvarying <- input[[id("indvarying")]]
        sdscale <- input[[id("sdscale")]]
        tipreds <- input[[id("tipreds")]]
        extra_pars <- input[[id("extra_pars")]]
        if (is.null(transform)) transform <- meta$transform
        if (is.null(indvarying)) indvarying <- meta$indvarying
        if (is.null(sdscale)) sdscale <- meta$sdscale
        if (is.null(tipreds)) {
          tipreds <- spec$tipred_names[vapply(
            spec$tipred_names,
            function(tipred) {
              isTRUE(meta[[paste0(tipred, "_effect")]])
            },
            logical(1L)
          )]
        }
        if (is.null(extra_pars)) extra_pars <- meta$extra_pars %||% ""
        values[[length(values) + 1L]] <- list(
          matrix = matrix_name, row = rownames(mat)[row],
          col = colnames(mat)[col], transform = transform,
          indvarying = indvarying, sdscale = sdscale,
          tipred_effects = tipreds, extra_pars = extra_pars
        )
      }
    }
    values
  }
  apply_current_matrix <- function(show_notification = FALSE, commit = NULL) {
    values <- matrix_input_values(commit = commit)
    if (is.null(values)) return(invisible(FALSE))
    result <- tryCatch(
      ctgui_apply_matrix_batch(
        current_spec(), values, matrix_metadata_values(current_spec())
      ),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      matrix_status(conditionMessage(result))
      if (show_notification) {
        shiny::showNotification(conditionMessage(result), type = "error")
      }
      return(invisible(FALSE))
    }
    if (!length(result$changed)) return(invisible(FALSE))
    # Exactly one canonical commit is made for the complete matrix + metadata
    # batch.  Refreshing the visual projections is an explicit downstream
    # effect and never becomes an alternate specification store.
    commit_current_spec(result$spec, reason = "matrix_edit")
    visual_refresh(result$spec, input$visual_view %||% "state_space")
    fit_status_value("Model changed. Refit when ready.")
    matrix_status(paste(
      "Updated", paste(result$changed, collapse = ", "), "at",
      format(Sys.time(), "%H:%M:%S")
    ))
    if (show_notification) {
      shiny::showNotification("Matrix edits applied", type = "message")
    }
    invisible(TRUE)
  }
  shiny::observeEvent(input$matrix_commit_nonce, {
    commit <- input$matrix_commit_nonce
    if (!is.list(commit) || is.null(commit$id)) return()
    apply_current_matrix(show_notification = FALSE, commit = commit)
  })
  shiny::observeEvent(
    input$matrix_metadata_commit,
    apply_current_matrix(show_notification = FALSE)
  )
  output$matrix_status <- shiny::renderText(matrix_status())
  list(
    apply_current_matrix = apply_current_matrix,
    matrix_input_values = matrix_input_values,
    selected_matrix_cell = selected_matrix_cell
  )
}
