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
    stop("The RDS does not contain a ctsem model or ctsemgui project", call. = FALSE)
  }
  ctgui_visual_ensure(loaded)
}

ctgui_data_role_selection <- function(data, spec) {
  columns <- if (is.null(data)) character() else names(data)
  selected <- unique(c(
    spec$manifest_names, spec$tdpred_names, spec$tipred_names,
    spec$id, spec$time
  ))
  list(
    choices = unique(c(columns, selected[nzchar(selected)])),
    manifest_names = spec$manifest_names,
    tdpred_names = spec$tdpred_names,
    tipred_names = spec$tipred_names,
    id = spec$id,
    time = spec$time
  )
}

ctgui_tipred_subject_data <- function(data, spec) {
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
  utils::head(data, n)
}

ctgui_data_summary <- function(data) {
  if (is.null(data)) return(data.frame(message = "No data selected"))
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
