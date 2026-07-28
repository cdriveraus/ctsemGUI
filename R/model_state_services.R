# Model-state domain services ------------------------------------------------
#
# These helpers contain the non-reactive part of specification, matrix, data
# role, and project handling. Shiny server code should only translate inputs
# into these values and apply the returned commit effects.

ctgui_parse_names <- function(x) {
  if (is.null(x) || !length(x)) return(character())
  values <- unlist(strsplit(as.character(x), ",", fixed = TRUE), use.names = FALSE)
  trimws(values[nzchar(trimws(values))])
}

ctgui_manifest_type_values <- function(manifest_names, input_values,
    fallback = rep(0L, length(manifest_names))) {
  if (!length(manifest_names)) return(integer())
  values <- vapply(seq_along(manifest_names), function(index) {
    value <- input_values[[paste0("manifest_type_", index)]]
    if (is.null(value)) {
      as.numeric(fallback[pmin(index, length(fallback))] %||% 0)
    } else {
      as.numeric(value)
    }
  }, numeric(1L))
  unname(values)
}

ctgui_spec_fields <- function(values, previous) {
  ctgui_check_spec(previous)
  latent_names <- ctgui_parse_names(values$latent_names)
  manifest_names <- ctgui_parse_names(values$manifest_names)
  tdpred_names <- ctgui_parse_names(values$tdpred_names)
  tipred_names <- ctgui_parse_names(values$tipred_names)
  list(
    latent_names = latent_names,
    manifest_names = manifest_names,
    manifest_type = ctgui_manifest_type_values(
      manifest_names, values, previous$manifest_type
    ),
    tdpred_names = tdpred_names,
    tipred_names = tipred_names,
    type = values$type %||% previous$type,
    Tpoints = if (is.null(values$Tpoints) || !length(values$Tpoints) ||
      is.na(values$Tpoints)) NULL else as.integer(values$Tpoints),
    tipredDefault = isTRUE(values$tipredDefault),
    id = values$id %||% previous$id,
    time = values$time %||% previous$time
  )
}

ctgui_spec_fields_changed <- function(spec, fields) {
  ctgui_check_spec(spec)
  compared <- c(
    "latent_names", "manifest_names", "manifest_type", "tdpred_names",
    "tipred_names", "type", "Tpoints", "tipredDefault", "id", "time"
  )
  any(vapply(compared, function(field) {
    !identical(spec[[field]], fields[[field]])
  }, logical(1L)))
}

ctgui_commit_spec_fields <- function(previous, fields, reason = "specification") {
  ctgui_check_spec(previous)
  if (!ctgui_spec_fields_changed(previous, fields)) {
    return(ctgui_commit_spec(previous, previous, reason = reason))
  }
  updated <- ctgui_spec(
    latent_names = fields$latent_names,
    manifest_names = fields$manifest_names,
    type = fields$type,
    id = fields$id,
    time = fields$time,
    Tpoints = fields$Tpoints,
    manifest_type = fields$manifest_type,
    tdpred_names = fields$tdpred_names,
    tipred_names = fields$tipred_names,
    tipredDefault = fields$tipredDefault
  )
  ctgui_commit_spec(previous, updated, reason = reason)
}

ctgui_add_spec_variable <- function(previous, kind, name, measuring = "") {
  ctgui_check_spec(previous)
  kind <- match.arg(kind, c("manifest", "tdpred", "tipred"))
  name <- trimws(as.character(name)[1L])
  measuring <- trimws(as.character(measuring)[1L])
  valid_name <- function(value) grepl("^[A-Za-z._][A-Za-z0-9._]*$", value)
  if (!valid_name(name)) stop("Use a valid R variable name.", call. = FALSE)
  if (kind == "manifest" && !valid_name(measuring)) {
    stop("Choose or type the latent process measured by this manifest variable.", call. = FALSE)
  }
  field <- paste0(kind, "_names")
  if (name %in% previous[[field]]) stop("This variable is already in the specification.", call. = FALSE)
  latent_names <- previous$latent_names
  if (kind == "manifest" && !measuring %in% latent_names) latent_names <- c(latent_names, measuring)
  manifest_names <- previous$manifest_names
  if (kind == "manifest") manifest_names <- c(manifest_names, name)
  manifest_type <- vapply(manifest_names, function(manifest) {
    index <- match(manifest, previous$manifest_names)
    if (is.na(index)) 0L else as.integer(previous$manifest_type[index])
  }, integer(1L))
  fields <- list(
    latent_names = latent_names, manifest_names = manifest_names,
    manifest_type = manifest_type,
    tdpred_names = if (kind == "tdpred") c(previous$tdpred_names, name) else previous$tdpred_names,
    tipred_names = if (kind == "tipred") c(previous$tipred_names, name) else previous$tipred_names,
    type = previous$type, Tpoints = previous$Tpoints,
    tipredDefault = previous$tipredDefault, id = previous$id, time = previous$time
  )
  commit <- ctgui_commit_spec_fields(previous, fields, reason = paste0("add-", kind))
  if (kind != "manifest") return(commit)
  updated <- commit$spec
  loading <- updated$matrices$LAMBDA
  loading[name, ] <- 0
  existing_manifests <- setdiff(rownames(loading), name)
  has_measurement <- length(existing_manifests) && any(vapply(
    loading[existing_manifests, measuring],
    function(value) {
      value <- trimws(as.character(value))
      nzchar(value) && !identical(value, "0")
    },
    logical(1L)
  ))
  loading[name, measuring] <- if (has_measurement) {
    ctgui_auto_label("LAMBDA", name, measuring)
  } else {
    1
  }
  updated$matrices$LAMBDA <- loading
  updated <- ctgui_sync_model_from_matrices(updated)
  ctgui_commit_spec(previous, updated, reason = "add-manifest")
}

ctgui_matrix_group_names <- function(spec, group = "Dynamics") {
  present <- ctgui_matrix_names(spec)
  desired <- switch(group %||% "Dynamics",
    Dynamics = c("DRIFT", "CINT", "DIFFUSION"),
    Measurement = c("LAMBDA", "MANIFESTMEANS", "MANIFESTVAR"),
    Initial = c("T0MEANS", "T0VAR"),
    Predictors = c("TDPREDEFFECT", "TDPREDMEANS", "TDPREDVAR"),
    PARS = "PARS",
    character()
  )
  intersect(desired, present)
}

ctgui_matrix_note <- function(matrix_name) {
  switch(matrix_name,
    DRIFT = "Continuous-time effects among latent processes. Diagonal cells are self-regulation; off-diagonal cells are cross-process effects.",
    CINT = "Latent process intercepts. Fixed numbers or free labels set constant input to each latent process.",
    DIFFUSION = "Lower triangular system-noise matrix. Diagonal entries are system-noise standard deviations; lower off-diagonals set unconstrained noise correlations.",
    LAMBDA = "Measurement loadings. Rows are manifest variables, columns are latent processes.",
    MANIFESTMEANS = "Manifest intercepts. One entry per manifest variable.",
    MANIFESTVAR = "Lower triangular measurement-error matrix. Diagonal entries are error standard deviations; lower off-diagonals set unconstrained residual correlations.",
    T0MEANS = "Initial latent means at the start of each subject series.",
    T0VAR = "Lower triangular initial-state covariance matrix. Diagonal entries are initial standard deviations; lower off-diagonals set unconstrained initial-state correlations.",
    TDPREDEFFECT = "Effects of time-dependent predictors on latent processes. Rows are latent processes, columns are TD predictors.",
    TDPREDMEANS = "Data generation only: means for simulated time-dependent predictors. This matrix is not estimated when fitting a model.",
    TDPREDVAR = "Data generation only: covariance structure for simulated time-dependent predictors. This matrix is not estimated when fitting a model.",
    ""
  )
}

ctgui_fixed_matrix_value <- function(value) {
  value <- trimws(as.character(value %||% ""))
  if (!nzchar(value)) return(TRUE)
  if (grepl("\\|\\|\\s*FALSE\\s*$", value, ignore.case = TRUE)) return(TRUE)
  !is.na(suppressWarnings(as.numeric(value)))
}

ctgui_active_matrix_cell <- function(value) !ctgui_fixed_matrix_value(value)

ctgui_indvarying_t0means <- function(spec) {
  matrix <- spec$matrices[["T0MEANS"]]
  if (is.null(matrix)) return(character())
  values <- as.character(matrix[, 1L, drop = TRUE])
  rownames(matrix)[!vapply(values, ctgui_fixed_matrix_value, logical(1L))]
}

ctgui_matrix_metadata_row <- function(spec, matrix_name, row_name, col_name) {
  metadata <- spec$parameter_metadata
  if (is.null(metadata) || !nrow(metadata)) return(NULL)
  found <- metadata[
    metadata$matrix == matrix_name &
      metadata$row == row_name &
      metadata$col == col_name,
    ,
    drop = FALSE
  ]
  if (!nrow(found)) NULL else found[1L, , drop = FALSE]
}

ctgui_matrix_cell_coordinate <- function(spec, matrix_name, row_name, col_name) {
  vector_matrices <- c(
    "CINT", "MANIFESTMEANS", "T0MEANS", "TDPREDMEANS", "PARS"
  )
  if (matrix_name %in% vector_matrices) {
    return(paste0(matrix_name, "[", row_name, "]"))
  }
  paste0(matrix_name, "[", row_name, ",", col_name, "]")
}

ctgui_matrix_id_part <- function(x) gsub("[^A-Za-z0-9_]", "_", x)

ctgui_matrix_cell_id <- function(matrix_name, row, col) {
  paste0("matrix_cell_", ctgui_matrix_id_part(matrix_name), "_", row, "_", col)
}

ctgui_matrix_meta_id <- function(matrix_name, row, col, field) {
  paste0(
    "matrix_meta_", ctgui_matrix_id_part(matrix_name), "_",
    row, "_", col, "_", field
  )
}

ctgui_pars_vector <- function(spec) {
  pars <- spec$matrices[["PARS"]]
  if (is.null(pars)) return(character())
  as.character(pars[, 1L, drop = TRUE])
}

ctgui_parse_pars_vector <- function(x) {
  if (is.null(x)) return(NULL)
  values <- ctgui_split_pars(x)
  if (!length(values)) return(NULL)
  matrix(
    values, ncol = 1L,
    dimnames = list(paste0("PARS", seq_along(values)), "PARS")
  )
}

ctgui_set_spec_matrix <- function(spec, matrix_name, value) {
  ctgui_check_spec(spec)
  if (!identical(matrix_name, "PARS")) {
    matrix_name <- ctgui_match_matrix_name(spec, matrix_name)
    if (!is.matrix(value)) stop("value must be a matrix", call. = FALSE)
    expected <- ctgui_expected_dims(spec)[[matrix_name]]
    if (!is.null(expected) && !identical(dim(value), expected)) {
      stop(
        matrix_name, " must have dimensions ",
        paste(expected, collapse = " x "), call. = FALSE
      )
    }
    spec$matrices[[matrix_name]] <- ctgui_apply_dimnames_to_one(
      matrix_name, value,
      latent_names = spec$latent_names,
      manifest_names = spec$manifest_names,
      tdpred_names = spec$tdpred_names
    )
    return(spec)
  }
  spec$matrices[["PARS"]] <- value
  spec$matrices <- ctgui_order_matrices(
    spec$matrices[!vapply(spec$matrices, is.null, logical(1L))]
  )
  spec
}

ctgui_apply_matrix_edits <- function(previous, matrices = list(),
    metadata = list(), reason = "matrix_edit") {
  ctgui_check_spec(previous)
  updated <- previous
  for (matrix_name in names(matrices)) {
    updated <- ctgui_set_spec_matrix(updated, matrix_name, matrices[[matrix_name]])
  }
  for (edit in metadata) {
    required <- c("matrix", "row", "col")
    if (!all(required %in% names(edit))) {
      stop("Each metadata edit must identify matrix, row, and col", call. = FALSE)
    }
    args <- edit[setdiff(names(edit), required)]
    args$sync <- FALSE
    updated <- do.call(
      ctgui_set_parameter_metadata,
      c(list(
        spec = updated, matrix = edit$matrix,
        row = edit$row, col = edit$col
      ), args)
    )
  }
  ctgui_commit_spec(previous, updated, reason = reason)
}

ctgui_project_spec <- function(object) {
  loaded <- if (inherits(object, "ctsemgui_spec")) {
    object
  } else if (is.list(object) && !is.null(object$pars) &&
      !is.null(object$latentNames) && !is.null(object$manifestNames)) {
    ctgui_spec_from_model(object)
  } else {
    stop("The RDS does not contain a ctsem model or ctsemGUI project", call. = FALSE)
  }
  ctgui_visual_ensure(loaded)
}

ctgui_data_columns <- function(data) {
  if (is.null(data)) return(character())
  if (is.matrix(data)) return(colnames(data) %||% character())
  names(data)
}

ctgui_data_as_frame <- function(data) {
  if (!is.matrix(data)) return(data)
  out <- as.data.frame(data, stringsAsFactors = FALSE)
  if (!is.null(colnames(data))) names(out) <- colnames(data)
  out
}

ctgui_data_role_selection <- function(data, spec) {
  columns <- ctgui_data_columns(data)
  choices_for <- function(exclude = character(), selected = character()) {
    unique(c(setdiff(columns, exclude[nzchar(exclude)]), selected[nzchar(selected)]))
  }
  list(
    # Kept for callers that need the complete union; UI controls use the
    # role-specific choice vectors below.
    choices = unique(c(
      columns, spec$manifest_names, spec$tdpred_names, spec$tipred_names,
      spec$id, spec$time
    )),
    manifest_choices = choices_for(
      c(spec$manifest_names, spec$id, spec$time), spec$manifest_names
    ),
    tdpred_choices = choices_for(
      c(spec$tdpred_names, spec$id), spec$tdpred_names
    ),
    tipred_choices = choices_for(
      c(spec$tipred_names, spec$id), spec$tipred_names
    ),
    id_choices = choices_for(selected = spec$id),
    time_choices = choices_for(c(spec$id), spec$time),
    manifest_names = spec$manifest_names,
    tdpred_names = spec$tdpred_names,
    tipred_names = spec$tipred_names,
    id = spec$id,
    time = spec$time
  )
}

ctgui_tipred_subject_data <- function(data, spec) {
  data <- ctgui_data_as_frame(data)
  if (is.null(data) || !length(spec$tipred_names) ||
      !spec$id %in% names(data)) return(NULL)
  present <- intersect(spec$tipred_names, names(data))
  if (!length(present)) return(NULL)
  first <- data[
    !duplicated(data[[spec$id]]),
    c(spec$id, present),
    drop = FALSE
  ]
  variation <- vapply(present, function(name) {
    any(vapply(split(data[[name]], data[[spec$id]]), function(x) {
      length(unique(x[!is.na(x)])) > 1L
    }, logical(1L)))
  }, logical(1L))
  list(
    values = first,
    varying = names(variation)[variation],
    missing = setdiff(spec$tipred_names, present)
  )
}

ctgui_data_preview <- function(data, n = 20L) {
  if (is.null(data)) return(data.frame(message = "No data selected"))
  utils::head(ctgui_data_as_frame(data), n)
}

ctgui_data_summary <- function(data) {
  if (is.null(data)) return(data.frame(message = "No data selected"))
  data <- ctgui_data_as_frame(data)
  numeric_names <- names(data)[vapply(data, is.numeric, logical(1L))]
  if (!length(numeric_names)) return(data.frame(message = "No numeric columns"))
  do.call(rbind, lapply(numeric_names, function(name) {
    x <- data[[name]]
    data.frame(
      variable = name, n = sum(!is.na(x)), missing = sum(is.na(x)),
      mean = mean(x, na.rm = TRUE), sd = stats::sd(x, na.rm = TRUE),
      min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE),
      row.names = NULL
    )
  }))
}

ctgui_missingness_summary <- function(data) {
  if (is.null(data)) return(data.frame(message = "No data selected"))
  data <- ctgui_data_as_frame(data)
  data.frame(
    variable = names(data),
    missing = vapply(data, function(x) sum(is.na(x)), integer(1L)),
    percent_missing = round(
      100 * vapply(data, function(x) mean(is.na(x)), numeric(1L)), 2
    ),
    row.names = NULL
  )
}

ctgui_within_between_summary <- function(data, spec) {
  if (is.null(data)) return(data.frame(message = "No data selected"))
  data <- ctgui_data_as_frame(data)
  if (!spec$id %in% names(data)) {
    return(data.frame(message = "ID column not found in active data"))
  }
  numeric_names <- names(data)[vapply(data, is.numeric, logical(1L))]
  if (!length(numeric_names)) return(data.frame(message = "No numeric columns"))
  do.call(rbind, lapply(numeric_names, function(name) {
    groups <- split(data[[name]], data[[spec$id]])
    group_means <- vapply(groups, function(x) mean(x, na.rm = TRUE), numeric(1L))
    group_sds <- vapply(groups, function(x) stats::sd(x, na.rm = TRUE), numeric(1L))
    data.frame(
      variable = name,
      between_sd = stats::sd(group_means, na.rm = TRUE),
      mean_within_sd = mean(group_sds, na.rm = TRUE),
      row.names = NULL
    )
  }))
}
