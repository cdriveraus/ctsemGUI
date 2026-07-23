ctgui_visual_matrix_names <- c(
  "DRIFT", "CINT", "DIFFUSION", "LAMBDA", "MANIFESTMEANS",
  "MANIFESTVAR", "TDPREDEFFECT", "T0MEANS", "T0VAR"
)

ctgui_visual_state_matrices <- c(
  "DRIFT", "CINT", "DIFFUSION", "LAMBDA", "MANIFESTMEANS",
  "MANIFESTVAR", "TDPREDEFFECT"
)

ctgui_visual_initial_matrices <- c("T0MEANS", "T0VAR")

ctgui_visual_empty_layout <- function() list(
  state_space = list(), initial_state = list(), tipred_effects = list()
)

ctgui_visual_ensure <- function(spec) {
  if (is.null(spec$visual) || !is.list(spec$visual)) {
    spec$visual <- list(version = 1L, layouts = ctgui_visual_empty_layout())
  }
  if (is.null(spec$visual$version)) spec$visual$version <- 1L
  if (is.null(spec$visual$layouts) || !is.list(spec$visual$layouts)) {
    spec$visual$layouts <- ctgui_visual_empty_layout()
  }
  for (view in names(ctgui_visual_empty_layout())) {
    if (is.null(spec$visual$layouts[[view]]) || !is.list(spec$visual$layouts[[view]])) {
      spec$visual$layouts[[view]] <- list()
    }
  }
  if (is.null(spec$visual$tipred_defaults) || !is.list(spec$visual$tipred_defaults)) {
    spec$visual$tipred_defaults <- list()
  }
  spec
}

ctgui_visual_cell_active <- function(value) {
  text <- trimws(as.character(value %||% ""))
  if (!nzchar(text)) return(FALSE)
  number <- suppressWarnings(as.numeric(strsplit(text, "|", fixed = TRUE)[[1L]][1L]))
  is.na(number) || !isTRUE(all.equal(number, 0))
}

ctgui_visual_metadata <- function(spec, matrix, row, col) {
  metadata <- spec$parameter_metadata
  if (is.null(metadata) || !nrow(metadata)) return(NULL)
  index <- which(metadata$matrix == matrix & metadata$row == row & metadata$col == col)
  if (!length(index)) return(NULL)
  metadata[index[1L], , drop = FALSE]
}

ctgui_visual_edge_style <- function(spec, matrix, row, col, value) {
  text <- trimws(as.character(value))
  fixed <- !is.na(suppressWarnings(as.numeric(strsplit(text, "|", fixed = TRUE)[[1L]][1L])))
  meta <- ctgui_visual_metadata(spec, matrix, row, col)
  transform <- if (is.null(meta)) "param" else ctgui_display_transform(meta$transform[1L])
  extra <- if (is.null(meta) || !"extra_pars" %in% names(meta)) "" else meta$extra_pars[1L] %||% ""
  tipreds <- character()
  if (!is.null(meta)) tipreds <- spec$tipred_names[vapply(spec$tipred_names, function(name) {
    field <- paste0(name, "_effect")
    field %in% names(meta) && isTRUE(meta[[field]][1L])
  }, logical(1L))]
  list(
    value = text,
    fixed = fixed,
    # ctsem supplies matrix-specific default transforms for ordinary free
    # labels.  A path is visually custom only when it has extra PARS support.
    custom = !fixed && nzchar(extra),
    indvarying = !is.null(meta) && isTRUE(meta$indvarying[1L]),
    transform = transform,
    sdscale = if (is.null(meta)) 1 else meta$sdscale[1L] %||% 1,
    tipred_effects = tipreds,
    extra_pars = extra
  )
}

ctgui_visual_position <- function(spec, view, id, index, column, kind = NULL) {
  layout <- spec$visual$layouts[[view]][[id]]
  if (is.list(layout) && all(c("x", "y") %in% names(layout))) return(layout)
  if (identical(view, "state_space")) {
    return(switch(kind,
      latent = list(x = 340 + (index - 1L) * 190, y = 255),
      system_noise = list(x = 340 + (index - 1L) * 190, y = 85),
      manifest = list(x = 340 + (index - 1L) * 190, y = 430),
      measurement_noise = list(x = 340 + (index - 1L) * 190, y = 570),
      tdpred = list(x = 85, y = 150 + (index - 1L) * 105),
      tipred = list(x = 85, y = 430 + (index - 1L) * 85),
      constant = list(x = 1080, y = if (grepl("MANIFEST", id)) 430 else 255),
      list(x = column * 200, y = 90 + index * 105)
    ))
  }
  if (identical(view, "initial_state")) {
    return(switch(kind,
      latent = list(x = 360 + (index - 1L) * 190, y = 275),
      initial_noise = list(x = 360 + (index - 1L) * 190, y = 95),
      constant = list(x = 105, y = 275),
      list(x = column * 200, y = 90 + index * 105)
    ))
  }
  list(x = column * 200, y = 90 + index * 105)
}

ctgui_visual_node <- function(spec, view, id, kind, name, index, column, label = name) {
  position <- ctgui_visual_position(spec, view, id, index, column, kind)
  list(id = id, kind = kind, name = name, label = label, original_name = name,
    x = position$x, y = position$y)
}

ctgui_visual_edge <- function(spec, matrix, row, col, source, target, directed = TRUE,
    edge_kind = "path") {
  value <- spec$matrices[[matrix]][row, col]
  style <- ctgui_visual_edge_style(spec, matrix, row, col, value)
  c(list(
    id = paste(matrix, row, col, sep = "\r"), matrix = matrix, row = row, col = col,
    source = source, target = target, directed = directed, edge_kind = edge_kind,
    label = style$value
  ), style)
}

ctgui_visual_tipred_colours <- function(names) {
  palette <- c("#0f766e", "#7c3aed", "#dc2626", "#2563eb", "#c2410c", "#4d7c0f")
  stats::setNames(palette[(seq_along(names) - 1L) %% length(palette) + 1L], names)
}

ctgui_visual_graph <- function(spec, view = c("state_space", "initial_state", "tipred_effects")) {
  ctgui_check_spec(spec)
  spec <- ctgui_visual_ensure(spec)
  view <- match.arg(view)
  nodes <- list(); edges <- list()
  add_node <- function(id, kind, name, index, column, label = name) {
    nodes[[length(nodes) + 1L]] <<- ctgui_visual_node(spec, view, id, kind, name, index, column, label)
  }
  add_edge <- function(...) edges[[length(edges) + 1L]] <<- ctgui_visual_edge(spec, ...)
  add_noise_input <- function(id, source, target) {
    edges[[length(edges) + 1L]] <<- list(
      id = id, source = source, target = target, directed = TRUE,
      edge_kind = "noise_input", value = "1", label = "1", fixed = TRUE,
      visual_only = TRUE, selectable = FALSE
    )
  }
  latent <- spec$latent_names; manifest <- spec$manifest_names; tdpred <- spec$tdpred_names
  if (identical(view, "tipred_effects")) {
    metadata <- spec$parameter_metadata
    tipred_colours <- ctgui_visual_tipred_colours(spec$tipred_names)
    for (i in seq_along(spec$tipred_names)) {
      add_node(paste0("tipred:", spec$tipred_names[i]), "tipred", spec$tipred_names[i], i, 2)
      nodes[[length(nodes)]]$x <- 360 + (i - 1L) * 180
      nodes[[length(nodes)]]$y <- 315
      nodes[[length(nodes)]]$colour <- unname(tipred_colours[[spec$tipred_names[i]]])
      nodes[[length(nodes)]]$tipred_default <- isTRUE(spec$visual$tipred_defaults[[spec$tipred_names[i]]] %||% spec$tipredDefault)
    }
    if (!is.null(metadata) && nrow(metadata)) {
      n <- nrow(metadata)
      radius <- max(250, 52 * n / pi)
      for (i in seq_len(n)) {
        angle <- 2 * pi * (i - 1L) / max(1L, n) - pi / 2
        id <- paste("parameter", metadata$matrix[i], metadata$row[i], metadata$col[i], sep = "\r")
        node <- ctgui_visual_node(spec, view, id, "parameter", metadata$param[i], i, 2,
          paste0(metadata$param[i], "\n", metadata$matrix[i]))
        node$matrix <- metadata$matrix[i]; node$row <- metadata$row[i]; node$col <- metadata$col[i]
        node$x <- 450 + radius * cos(angle); node$y <- 315 + radius * sin(angle)
        nodes[[length(nodes) + 1L]] <- node
        for (tipred in spec$tipred_names) {
          field <- paste0(tipred, "_effect")
          if (field %in% names(metadata) && isTRUE(metadata[[field]][i])) {
            edges[[length(edges) + 1L]] <- list(
              id = paste("tipred_effect", tipred, metadata$matrix[i], metadata$row[i], metadata$col[i], sep = "\r"),
              source = paste0("tipred:", tipred), target = id, directed = TRUE,
              edge_kind = "tipred_effect", tipred = tipred, colour = unname(tipred_colours[[tipred]]),
              matrix = metadata$matrix[i], row = metadata$row[i], col = metadata$col[i],
              value = "1", label = "", fixed = FALSE
            )
          }
        }
      }
    }
  } else if (identical(view, "state_space")) {
    for (i in seq_along(latent)) add_node(paste0("latent:", latent[i]), "latent", latent[i], i, 2)
    for (i in seq_along(manifest)) add_node(paste0("manifest:", manifest[i]), "manifest", manifest[i], i, 3.6)
    for (i in seq_along(tdpred)) add_node(paste0("tdpred:", tdpred[i]), "tdpred", tdpred[i], i, 0.4)
    tipred_colours <- ctgui_visual_tipred_colours(spec$tipred_names)
    for (i in seq_along(spec$tipred_names)) {
      add_node(paste0("tipred:", spec$tipred_names[i]), "tipred", spec$tipred_names[i], i, 0.4)
      nodes[[length(nodes)]]$colour <- unname(tipred_colours[[spec$tipred_names[i]]])
      nodes[[length(nodes)]]$tipred_default <- isTRUE(spec$visual$tipred_defaults[[spec$tipred_names[i]]] %||% spec$tipredDefault)
    }
    add_node("constant:CINT", "constant", "1", 0, 1.5, "1 (CINT)")
    nodes[[length(nodes)]]$matrix_col <- colnames(spec$matrices[["CINT"]])[1L]
    add_node("constant:MANIFESTMEANS", "constant", "1", 0, 3, "1 (MANIFESTMEANS)")
    nodes[[length(nodes)]]$matrix_col <- colnames(spec$matrices[["MANIFESTMEANS"]])[1L]
    for (i in seq_along(latent)) {
      noise_id <- paste0("noise:DIFFUSION:", latent[i])
      add_node(noise_id, "system_noise", latent[i], i, 1, paste0("dW(t)\n", latent[i]))
      add_noise_input(paste0("input:DIFFUSION:", latent[i]), noise_id, paste0("latent:", latent[i]))
    }
    for (i in seq_along(manifest)) {
      noise_id <- paste0("noise:MANIFESTVAR:", manifest[i])
      add_node(noise_id, "measurement_noise", manifest[i], i, 4.4, paste0("\u03b5\n", manifest[i]))
      add_noise_input(paste0("input:MANIFESTVAR:", manifest[i]), noise_id, paste0("manifest:", manifest[i]))
    }
    for (matrix in intersect(c("DRIFT", "LAMBDA", "TDPREDEFFECT"), names(spec$matrices))) {
      mat <- spec$matrices[[matrix]]
      for (r in seq_len(nrow(mat))) for (c in seq_len(ncol(mat))) if (ctgui_visual_cell_active(mat[r, c])) {
        source <- switch(matrix,
          DRIFT = paste0("latent:", colnames(mat)[c]),
          LAMBDA = paste0("latent:", colnames(mat)[c]),
          TDPREDEFFECT = paste0("tdpred:", colnames(mat)[c])
        )
        target <- switch(matrix,
          DRIFT = paste0("latent:", rownames(mat)[r]),
          LAMBDA = paste0("manifest:", rownames(mat)[r]),
          TDPREDEFFECT = paste0("latent:", rownames(mat)[r])
        )
        add_edge(matrix, rownames(mat)[r], colnames(mat)[c], source, target)
      }
    }
    for (matrix in intersect(c("CINT", "MANIFESTMEANS"), names(spec$matrices))) {
      mat <- spec$matrices[[matrix]]
      for (r in seq_len(nrow(mat))) if (ctgui_visual_cell_active(mat[r, 1L])) {
        target <- if (matrix == "CINT") paste0("latent:", rownames(mat)[r]) else paste0("manifest:", rownames(mat)[r])
        add_edge(matrix, rownames(mat)[r], colnames(mat)[1L], paste0("constant:", matrix), target)
      }
    }
    for (matrix in intersect(c("DIFFUSION", "MANIFESTVAR"), names(spec$matrices))) {
      mat <- spec$matrices[[matrix]]
      for (r in seq_len(nrow(mat))) for (c in seq_len(ncol(mat))) {
        # Always expose diagonal disturbance/measurement SD paths: even a
        # fixed-zero cell needs a visible variance handle in the diagram.
        if (c > r || (r != c && !ctgui_visual_cell_active(mat[r, c]))) next
        prefix <- if (matrix == "DIFFUSION") "noise:DIFFUSION:" else "noise:MANIFESTVAR:"
        if (r == c) {
          noise_id <- paste0(prefix, rownames(mat)[r])
          add_edge(matrix, rownames(mat)[r], colnames(mat)[c], noise_id, noise_id, FALSE, "variance")
        } else {
          add_edge(matrix, rownames(mat)[r], colnames(mat)[c], paste0(prefix, rownames(mat)[r]), paste0(prefix, colnames(mat)[c]), FALSE, "correlation")
        }
      }
    }
  } else {
    for (i in seq_along(latent)) add_node(paste0("latent:", latent[i]), "latent", latent[i], i, 2)
    add_node("constant:T0MEANS", "constant", "1", 0, 1.5)
    nodes[[length(nodes)]]$matrix_col <- colnames(spec$matrices[["T0MEANS"]])[1L]
    for (i in seq_along(latent)) {
      noise_id <- paste0("noise:T0VAR:", latent[i])
      add_node(noise_id, "initial_noise", latent[i], i, 1, paste0("noise\n", latent[i]))
      add_noise_input(paste0("input:T0VAR:", latent[i]), noise_id, paste0("latent:", latent[i]))
    }
    if (!is.null(spec$matrices[["T0MEANS"]])) {
      mat <- spec$matrices[["T0MEANS"]]
      for (r in seq_len(nrow(mat))) if (ctgui_visual_cell_active(mat[r, 1L])) {
        add_edge("T0MEANS", rownames(mat)[r], colnames(mat)[1L], "constant:T0MEANS", paste0("latent:", rownames(mat)[r]))
      }
    }
    if (!is.null(spec$matrices[["T0VAR"]])) {
      mat <- spec$matrices[["T0VAR"]]
      inactive <- character()
      meta <- spec$parameter_metadata
      if (!is.null(meta) && nrow(meta)) inactive <- meta$row[meta$matrix == "T0MEANS" & meta$indvarying]
      for (r in seq_len(nrow(mat))) for (c in seq_len(ncol(mat))) {
        if (c > r) next
        suppressed <- rownames(mat)[r] %in% inactive || colnames(mat)[c] %in% inactive
        if (suppressed && r == c) {
          noise_id <- paste0("noise:T0VAR:", rownames(mat)[r])
          edges[[length(edges) + 1L]] <- list(
            id = paste0("inactive:T0VAR:", rownames(mat)[r]),
            matrix = "T0VAR", row = rownames(mat)[r], col = colnames(mat)[c],
            source = noise_id, target = noise_id, directed = FALSE,
            edge_kind = "variance", value = "1e-6", label = "1e-6 (ignored)",
            fixed = TRUE, inactive = TRUE, visual_only = TRUE, selectable = FALSE
          )
          next
        }
        if (suppressed || (r != c && !ctgui_visual_cell_active(mat[r, c]))) next
        if (r == c) {
          noise_id <- paste0("noise:T0VAR:", rownames(mat)[r])
          add_edge("T0VAR", rownames(mat)[r], colnames(mat)[c], noise_id, noise_id, FALSE, "variance")
        } else {
          add_edge("T0VAR", rownames(mat)[r], colnames(mat)[c], paste0("noise:T0VAR:", rownames(mat)[r]), paste0("noise:T0VAR:", colnames(mat)[c]), FALSE, "correlation")
        }
      }
    }
  }
  list(version = 2L, view = view, nodes = nodes, edges = edges,
    matrices = if (view == "state_space") ctgui_visual_state_matrices else if (view == "initial_state") ctgui_visual_initial_matrices else unique(vapply(nodes, function(node) node$matrix %||% "", character(1L))))
}

ctgui_visual_save_layout <- function(spec, graph) {
  spec <- ctgui_visual_ensure(spec)
  view <- graph$view %||% "state_space"
  nodes <- graph$nodes %||% list()
  layout <- list()
  for (node in nodes) {
    if (is.null(node$id) || is.null(node$x) || is.null(node$y)) next
    layout[[as.character(node$id)]] <- list(x = as.numeric(node$x), y = as.numeric(node$y))
  }
  spec$visual$layouts[[view]] <- layout
  spec
}

ctgui_visual_set_edge <- function(spec, edge) {
  required <- c("matrix", "row", "col", "value")
  if (!all(required %in% names(edge))) return(spec)
  matrix <- as.character(edge$matrix); row <- as.character(edge$row); col <- as.character(edge$col)
  if (!matrix %in% names(spec$matrices)) return(spec)
  mat <- spec$matrices[[matrix]]
  r <- match(row, rownames(mat)); c <- match(col, colnames(mat))
  if (is.na(r) || is.na(c)) return(spec)
  value <- trimws(as.character(edge$value %||% "0"))
  if (identical(value, "__free__")) value <- ctgui_auto_label(matrix, row, col)
  if (!nzchar(value)) value <- "0"
  mat[r, c] <- value
  spec$matrices[[matrix]] <- mat
  spec
}

ctgui_visual_update_edge <- function(spec, edge) {
  ctgui_check_spec(spec)
  previous_metadata <- spec$parameter_metadata
  previous_keys <- ctgui_metadata_keys(previous_metadata)
  spec <- ctgui_visual_set_edge(spec, edge)
  spec <- ctgui_refresh_parameter_metadata(spec)
  matrix <- as.character(edge$matrix %||% "")
  row <- as.character(edge$row %||% "")
  col <- as.character(edge$col %||% "")
  key <- ctgui_cell_key(matrix, row, col)
  new_parameter <- !(key %in% previous_keys)
  index <- which(spec$parameter_metadata$matrix == matrix &
    spec$parameter_metadata$row == row & spec$parameter_metadata$col == col)
  if (length(index) && !new_parameter) {
    index <- index[1L]
    if (!is.null(edge$transform)) spec$parameter_metadata$transform[index] <- trimws(as.character(edge$transform))
    if (!is.null(edge$indvarying)) spec$parameter_metadata$indvarying[index] <- isTRUE(edge$indvarying)
    if (!is.null(edge$sdscale)) {
      scale <- suppressWarnings(as.numeric(edge$sdscale)[1L])
      if (is.na(scale)) scale <- 1
      spec$parameter_metadata$sdscale[index] <- scale
    }
    if (!is.null(edge$tipred_effects)) for (tipred in spec$tipred_names) {
      spec$parameter_metadata[[paste0(tipred, "_effect")]][index] <- tipred %in% edge$tipred_effects
    }
    if (!is.null(edge$extra_pars)) {
      spec$parameter_metadata$extra_pars[index] <- paste(ctgui_split_pars(edge$extra_pars), collapse = ", ")
    }
  }
  spec <- ctgui_sync_extra_pars(spec)
  spec <- ctgui_refresh_parameter_metadata(spec)
  new_parameter_keys <- setdiff(ctgui_metadata_keys(spec$parameter_metadata), previous_keys)
  spec$version <- max(3L, as.integer(spec$version %||% 2L))
  ctgui_sync_model_from_matrices(spec, ctsem_default_keys = new_parameter_keys)
}

ctgui_visual_resize_spec <- function(spec, nodes, enforce_model_nodes = TRUE) {
  variables <- Filter(function(node) node$kind %in% c("latent", "manifest", "tdpred", "tipred"), nodes %||% list())
  if (!length(variables)) return(spec)
  names_for <- function(kind) {
    value <- vapply(Filter(function(node) identical(node$kind, kind), variables), function(node) as.character(node$name), character(1L))
    value
  }
  latent <- names_for("latent")
  manifest <- names_for("manifest")
  tdpred <- names_for("tdpred")
  tipred <- names_for("tipred")
  if (anyDuplicated(c(latent, manifest, tdpred, tipred))) {
    stop("Visual variable names must be unique", call. = FALSE)
  }
  if (isTRUE(enforce_model_nodes) && (!length(latent) || !length(manifest))) {
    stop("Visual variables must include at least one unique latent and manifest name", call. = FALSE)
  }
  unchanged <- identical(latent, spec$latent_names) && identical(manifest, spec$manifest_names) &&
    identical(tdpred, spec$tdpred_names) && identical(tipred, spec$tipred_names)
  if (unchanged) return(spec)
  rename <- character()
  for (node in variables) {
    old <- as.character(node$original_name %||% node$name)
    new <- as.character(node$name)
    if (nzchar(old) && nzchar(new)) rename[old] <- new
  }
  inverse_name <- function(name) {
    found <- names(rename)[rename == name]
    if (length(found)) found[1L] else name
  }
  rebuilt <- ctgui_spec(latent_names = latent, manifest_names = manifest, type = spec$type,
    id = spec$id, time = spec$time, Tpoints = spec$Tpoints,
    manifest_type = vapply(manifest, function(name) {
      old <- inverse_name(name); index <- match(old, spec$manifest_names)
      if (is.na(index)) 0L else as.integer(spec$manifest_type[index])
    }, integer(1L)), tdpred_names = tdpred, tipred_names = tipred,
    tipredDefault = spec$tipredDefault)
  rebuilt$matrices[["PARS"]] <- spec$matrices[["PARS"]]
  for (matrix in intersect(names(spec$matrices), names(rebuilt$matrices))) {
    old <- spec$matrices[[matrix]]; target <- rebuilt$matrices[[matrix]]
    if (!is.matrix(old) || !is.matrix(target)) next
    for (r in seq_len(nrow(target))) for (c in seq_len(ncol(target))) {
      old_r <- inverse_name(rownames(target)[r]); old_c <- inverse_name(colnames(target)[c])
      source_r <- match(old_r, rownames(old)); source_c <- match(old_c, colnames(old))
      if (!is.na(source_r) && !is.na(source_c)) target[r, c] <- old[source_r, source_c]
    }
    rebuilt$matrices[[matrix]] <- target
  }
  metadata <- spec$parameter_metadata
  if (!is.null(metadata) && nrow(metadata)) {
    rename_values <- function(values) {
      mapped <- unname(rename[values])
      missing <- is.na(mapped) | !nzchar(mapped)
      mapped[missing] <- values[missing]
      mapped
    }
    metadata$row <- rename_values(metadata$row)
    metadata$col <- rename_values(metadata$col)
    keep <- vapply(seq_len(nrow(metadata)), function(i) {
      mat <- rebuilt$matrices[[metadata$matrix[i]]]
      !is.null(mat) && metadata$row[i] %in% rownames(mat) && metadata$col[i] %in% colnames(mat)
    }, logical(1L))
    rebuilt$parameter_metadata <- metadata[keep, , drop = FALSE]
  }
  rebuilt$matrix_extra_pars <- spec$matrix_extra_pars
  rebuilt$visual <- spec$visual
  rebuilt <- ctgui_refresh_parameter_metadata(rebuilt)
  rebuilt
}

ctgui_visual_resize_tipreds <- function(spec, nodes) {
  tipred_nodes <- Filter(function(node) identical(node$kind, "tipred"), nodes %||% list())
  retained_nodes <- c(
    lapply(spec$latent_names, function(name) list(kind = "latent", name = name, original_name = name)),
    lapply(spec$manifest_names, function(name) list(kind = "manifest", name = name, original_name = name)),
    lapply(spec$tdpred_names, function(name) list(kind = "tdpred", name = name, original_name = name)),
    tipred_nodes
  )
  ctgui_visual_resize_spec(spec, retained_nodes, enforce_model_nodes = FALSE)
}

ctgui_visual_apply_new_tipred_defaults <- function(spec, previous_tipreds, nodes) {
  spec <- ctgui_visual_ensure(spec)
  metadata <- spec$parameter_metadata
  tipred_nodes <- Filter(function(node) identical(node$kind, "tipred"), nodes %||% list())
  for (node in tipred_nodes) {
    name <- as.character(node$name %||% "")
    original <- as.character(node$original_name %||% name)
    apply_default <- isTRUE(node$tipred_apply_default) ||
      (!name %in% previous_tipreds && identical(name, original))
    if (!nzchar(name) || !apply_default) next
    field <- paste0(name, "_effect")
    default <- isTRUE(node$tipred_default)
    spec$visual$tipred_defaults[[name]] <- default
    # ctsem's tipredDefault is global. Per-predictor all/none policies require
    # an opt-in baseline, with the "all" policies written explicitly.
    spec$tipredDefault <- FALSE
    if (!is.null(metadata) && nrow(metadata) && field %in% names(metadata)) metadata[[field]] <- default
  }
  if (is.null(metadata) || !nrow(metadata)) return(spec)
  spec$parameter_metadata <- metadata
  ctgui_sync_extra_pars(spec)
}

ctgui_visual_apply_graph <- function(spec, graph) {
  ctgui_check_spec(spec)
  view <- graph$view %||% "state_space"
  if (identical(view, "tipred_effects")) {
    # TI view is allowed to add, rename, and delete TI predictor nodes.  Use
    # the same resize route as state space so a deletion removes the predictor
    # from the model rather than merely clearing its effects.
    previous_tipreds <- spec$tipred_names
    spec <- ctgui_visual_resize_tipreds(spec, graph$nodes)
    spec <- ctgui_visual_save_layout(spec, graph)
    spec <- ctgui_refresh_parameter_metadata(spec)
    spec <- ctgui_visual_apply_new_tipred_defaults(spec, previous_tipreds, graph$nodes)
    metadata <- spec$parameter_metadata
    if (!is.null(metadata) && nrow(metadata)) {
      # Existing effects are represented by graph edges. Newly added predictors
      # retain the all/none choice made when their node was created.
      for (tipred in intersect(previous_tipreds, spec$tipred_names)) {
        field <- paste0(tipred, "_effect")
        if (field %in% names(metadata)) metadata[[field]] <- FALSE
      }
      for (edge in graph$edges %||% list()) {
        if (!identical(edge$edge_kind, "tipred_effect")) next
        tipred <- as.character(edge$tipred %||% sub("^tipred:", "", edge$source %||% ""))
        field <- paste0(tipred, "_effect")
        index <- which(metadata$matrix == as.character(edge$matrix) &
          metadata$row == as.character(edge$row) & metadata$col == as.character(edge$col))
        if (length(index) && field %in% names(metadata)) metadata[[field]][index[1L]] <- TRUE
      }
      spec$parameter_metadata <- metadata
      spec <- ctgui_sync_extra_pars(spec)
    }
    return(ctgui_sync_model_from_matrices(spec))
  }
  previous_metadata <- spec$parameter_metadata
  previous_parameter_keys <- if (is.null(previous_metadata) || !nrow(previous_metadata)) character() else {
    ctgui_cell_key(previous_metadata$matrix, previous_metadata$row, previous_metadata$col)
  }
  previous_latents <- spec$latent_names
  previous_manifests <- spec$manifest_names
  previous_tipreds <- spec$tipred_names
  genuinely_added <- function(kind, previous) {
    nodes <- Filter(function(node) identical(node$kind, kind), graph$nodes %||% list())
    names <- vapply(nodes, function(node) as.character(node$name), character(1L))
    original <- vapply(nodes, function(node) as.character(node$original_name %||% node$name), character(1L))
    names[!(names %in% previous) & names == original]
  }
  new_latents <- genuinely_added("latent", previous_latents)
  new_manifests <- genuinely_added("manifest", previous_manifests)
  if (identical(view, "state_space")) {
    spec <- ctgui_visual_resize_spec(spec, graph$nodes)
  }
  spec <- ctgui_visual_save_layout(spec, graph)
  matrices <- if (identical(view, "initial_state")) ctgui_visual_initial_matrices else ctgui_visual_state_matrices
  present <- intersect(matrices, names(spec$matrices))
  preserved_t0var <- NULL
  inactive_t0var_names <- character()
  if (identical(view, "initial_state") && "T0VAR" %in% present) {
    preserved_t0var <- spec$matrices[["T0VAR"]]
    metadata <- spec$parameter_metadata
    if (!is.null(metadata) && nrow(metadata)) {
      inactive_t0var_names <- metadata$row[
        metadata$matrix == "T0MEANS" & metadata$indvarying
      ]
    }
  }
  # A graph represents the complete visible layer: omitted edges are fixed zeros.
  for (matrix in present) spec$matrices[[matrix]][, ] <- 0
  if (!is.null(preserved_t0var) && length(inactive_t0var_names)) {
    inactive_cells <- outer(
      rownames(preserved_t0var) %in% inactive_t0var_names,
      colnames(preserved_t0var) %in% inactive_t0var_names,
      `|`
    )
    spec$matrices[["T0VAR"]][inactive_cells] <- preserved_t0var[inactive_cells]
  }
  for (edge in graph$edges %||% list()) {
    if (!isTRUE(edge$visual_only)) spec <- ctgui_visual_set_edge(spec, edge)
  }
  if (length(new_latents) && "DIFFUSION" %in% names(spec$matrices)) {
    diffusion <- spec$matrices$DIFFUSION
    for (name in intersect(new_latents, rownames(diffusion))) {
      diffusion[name, name] <- ctgui_auto_label("DIFFUSION", name, name)
    }
    for (r in seq_len(nrow(diffusion))) for (c in seq_len(r - 1L)) {
      if (rownames(diffusion)[r] %in% new_latents || colnames(diffusion)[c] %in% new_latents) {
        diffusion[r, c] <- ctgui_auto_label("DIFFUSION", rownames(diffusion)[r], colnames(diffusion)[c])
      }
    }
    spec$matrices$DIFFUSION <- diffusion
  }
  if (length(new_latents) && "DRIFT" %in% names(spec$matrices)) {
    drift <- spec$matrices$DRIFT
    for (name in intersect(new_latents, rownames(drift))) {
      drift[name, name] <- ctgui_auto_label("DRIFT", name, name)
    }
    spec$matrices$DRIFT <- drift
  }
  if (length(new_manifests) && "MANIFESTVAR" %in% names(spec$matrices)) {
    manifestvar <- spec$matrices$MANIFESTVAR
    for (name in intersect(new_manifests, rownames(manifestvar))) {
      manifestvar[name, name] <- ctgui_auto_label("MANIFESTVAR", name, name)
    }
    spec$matrices$MANIFESTVAR <- manifestvar
  }
  if (length(new_manifests) && "MANIFESTMEANS" %in% names(spec$matrices)) {
    manifestmeans <- spec$matrices$MANIFESTMEANS
    for (name in intersect(new_manifests, rownames(manifestmeans))) {
      manifestmeans[name, 1L] <- ctgui_auto_label("MANIFESTMEANS", name, colnames(manifestmeans)[1L])
    }
    spec$matrices$MANIFESTMEANS <- manifestmeans
  }
  spec <- ctgui_refresh_parameter_metadata(spec)
  current_parameter_keys <- if (is.null(spec$parameter_metadata) || !nrow(spec$parameter_metadata)) character() else {
    ctgui_cell_key(spec$parameter_metadata$matrix, spec$parameter_metadata$row, spec$parameter_metadata$col)
  }
  new_parameter_keys <- setdiff(current_parameter_keys, previous_parameter_keys)
  if (length(new_manifests) && "MANIFESTMEANS" %in% names(spec$matrices)) {
    for (name in intersect(new_manifests, rownames(spec$matrices$MANIFESTMEANS))) {
      spec <- ctgui_set_parameter_metadata(spec, "MANIFESTMEANS", name,
        colnames(spec$matrices$MANIFESTMEANS)[1L], indvarying = TRUE, sync = FALSE)
    }
  }
  for (edge in graph$edges %||% list()) {
    if (isTRUE(edge$visual_only)) next
    if (!all(c("matrix", "row", "col") %in% names(edge))) next
    if (!is.null(edge$transform) || !is.null(edge$indvarying) || !is.null(edge$sdscale) ||
        !is.null(edge$tipred_effects) || !is.null(edge$extra_pars)) {
      spec <- ctgui_set_parameter_metadata(spec, edge$matrix, edge$row, edge$col,
        transform = edge$transform %||% NULL, indvarying = edge$indvarying %||% NULL,
        sdscale = edge$sdscale %||% NULL, tipred_effects = edge$tipred_effects %||% NULL,
        extra_pars = edge$extra_pars %||% NULL, sync = FALSE)
    }
  }
  spec <- ctgui_visual_apply_new_tipred_defaults(spec, previous_tipreds, graph$nodes)
  spec$version <- max(3L, as.integer(spec$version %||% 2L))
  ctgui_sync_model_from_matrices(spec, ctsem_default_keys = new_parameter_keys)
}
