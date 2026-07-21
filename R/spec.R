ctgui_required_matrices <- c(
  "LAMBDA", "T0VAR", "T0MEANS", "MANIFESTMEANS",
  "MANIFESTVAR", "DRIFT", "CINT", "DIFFUSION"
)

ctgui_optional_matrices <- c("TDPREDEFFECT", "TDPREDMEANS", "TDPREDVAR", "PARS")

#' Create, edit, validate, and export ctsem GUI specifications
#'
#' @param latent_names Character vector of latent process names.
#' @param manifest_names Character vector of manifest variable names.
#' @param type Model type passed to `ctsem::ctModel()`, currently `"ct"` or
#'   `"dt"`.
#' @param id Subject identifier column name.
#' @param time Time column name.
#' @param Tpoints Optional number of time points.
#' @param manifest_type Manifest variable type vector passed to ctsem.
#' @param tdpred_names Optional time-dependent predictor names.
#' @param tipred_names Optional time-independent predictor names.
#' @param matrices Optional named list of matrix overrides.
#' @param tipredDefault Passed to `ctsem::ctModel()`.
#'
#' @return A `ctsemgui_spec` object.
ctgui_spec <- function(latent_names = "eta1",
    manifest_names = "y1",
    type = c("ct", "dt"),
    id = "id",
    time = "time",
    Tpoints = NULL,
    manifest_type = rep(0, length(manifest_names)),
    tdpred_names = character(),
    tipred_names = character(),
    matrices = NULL,
    tipredDefault = TRUE) {

  type <- match.arg(type)
  latent_names <- ctgui_as_names(latent_names, "latent_names")
  manifest_names <- ctgui_as_names(manifest_names, "manifest_names")
  tdpred_names <- ctgui_as_names(tdpred_names, "tdpred_names", allow_empty = TRUE)
  tipred_names <- ctgui_as_names(tipred_names, "tipred_names", allow_empty = TRUE)

  if (length(manifest_type) != length(manifest_names)) {
    stop("manifest_type must have the same length as manifest_names", call. = FALSE)
  }

  base_matrices <- NULL
  model <- NULL
  pars <- NULL
  parameter_metadata <- NULL
  source <- "fallback"

  if (ctgui_has_ctsem()) {
    model <- ctgui_new_ctsem_model(
      latent_names = latent_names,
      manifest_names = manifest_names,
      type = type,
      id = id,
      time = time,
      Tpoints = Tpoints,
      manifest_type = manifest_type,
      tdpred_names = tdpred_names,
      tipred_names = tipred_names,
      matrices = matrices,
      tipredDefault = tipredDefault,
      silent = TRUE
    )
    base_matrices <- ctgui_prepare_matrices(ctgui_ctsem_matrices(model),
      latent_names = latent_names,
      manifest_names = manifest_names,
      tdpred_names = tdpred_names)
    pars <- model[["pars"]]
    parameter_metadata <- ctgui_parameter_metadata_from_pars(pars, tipred_names, base_matrices)
    source <- "ctsem"
  } else {
    base_matrices <- ctgui_default_matrices(
      latent_names = latent_names,
      manifest_names = manifest_names,
      tdpred_names = tdpred_names,
      Tpoints = Tpoints
    )
    if (!is.null(matrices)) {
      for (matrix_name in names(matrices)) base_matrices[[matrix_name]] <- matrices[[matrix_name]]
    }
    base_matrices <- ctgui_prepare_matrices(base_matrices,
      latent_names = latent_names,
      manifest_names = manifest_names,
      tdpred_names = tdpred_names)
  }

  spec <- list(
    version = 2L,
    type = type,
    id = id,
    time = time,
    Tpoints = Tpoints,
    latent_names = latent_names,
    manifest_names = manifest_names,
    manifest_type = manifest_type,
    tdpred_names = tdpred_names,
    tipred_names = tipred_names,
    tipredDefault = tipredDefault,
    matrices = base_matrices,
    # Parameters entered beside a matrix cell are kept separately from the
    # manually maintained PARS entries.  Their union is written to PARS.
    matrix_extra_pars = character(),
    pars = pars,
    parameter_metadata = parameter_metadata,
    model = model,
    source = source
  )
  class(spec) <- "ctsemgui_spec"
  spec
}

print.ctsemgui_spec <- function(x, ...) {
  cat("<ctsemgui_spec>\n")
  cat("  type: ", x$type, "\n", sep = "")
  cat("  latent: ", paste(x$latent_names, collapse = ", "), "\n", sep = "")
  cat("  manifest: ", paste(x$manifest_names, collapse = ", "), "\n", sep = "")
  cat("  matrices: ", paste(ctgui_matrix_names(x), collapse = ", "), "\n", sep = "")
  cat("  source: ", x$source, "\n", sep = "")
  invisible(x)
}

#' @rdname ctgui_spec
#' @param spec A `ctsemgui_spec` object.
ctgui_validate <- function(spec) {
  ctgui_check_spec(spec)
  messages <- list()

  add_message <- function(severity, field, message) {
    messages[[length(messages) + 1L]] <<- data.frame(
      severity = severity,
      field = field,
      message = message,
      stringsAsFactors = FALSE
    )
  }

  all_names <- c(spec$latent_names, spec$manifest_names, spec$tdpred_names, spec$tipred_names, spec$id, spec$time)
  bad_names <- all_names[grepl("\\W", all_names)]
  if (length(bad_names) > 0) {
    add_message("error", "names", paste("Names contain non-word characters:", paste(unique(bad_names), collapse = ", ")))
  }

  for (name_field in c("latent_names", "manifest_names", "tdpred_names", "tipred_names")) {
    values <- spec[[name_field]]
    if (anyDuplicated(values)) {
      add_message("error", name_field, paste("Duplicate names:", paste(unique(values[duplicated(values)]), collapse = ", ")))
    }
  }

  expected <- ctgui_expected_dims(spec)
  for (matrix_name in names(expected)) {
    mat <- spec$matrices[[matrix_name]]
    if (is.null(mat)) {
      if (!matrix_name %in% c(ctgui_required_matrices, "TDPREDEFFECT")) next
      add_message("error", matrix_name, paste(matrix_name, "is missing"))
      next
    }
    if (!is.matrix(mat)) {
      add_message("error", matrix_name, paste(matrix_name, "must be a matrix"))
      next
    }
    if (!identical(dim(mat), expected[[matrix_name]])) {
      add_message(
        "error",
        matrix_name,
        paste0(
          matrix_name, " has dimensions ", paste(dim(mat), collapse = " x "),
          " but should be ", paste(expected[[matrix_name]], collapse = " x ")
        )
      )
    }
  }

  for (matrix_name in c("T0VAR", "DIFFUSION")) {
    mat <- spec$matrices[[matrix_name]]
    if (!is.null(mat) && is.matrix(mat) && nrow(mat) > 1L) {
      upper <- mat[upper.tri(mat)]
      upper_numeric <- suppressWarnings(as.numeric(upper))
      if (any(is.na(upper_numeric) | upper_numeric != 0)) {
        add_message("warning", matrix_name, paste(matrix_name, "should usually be lower triangular, with fixed zeroes above the diagonal"))
      }
    }
  }

  mat <- spec$matrices[["MANIFESTVAR"]]
  if (!is.null(mat) && is.matrix(mat) && length(mat) > 1L) {
    offdiag <- mat[row(mat) != col(mat)]
    offdiag_numeric <- suppressWarnings(as.numeric(offdiag))
    if (any(is.na(offdiag_numeric) | offdiag_numeric != 0)) {
      add_message("warning", "MANIFESTVAR", "MANIFESTVAR is usually diagonal for first-pass ctsem models")
    }
  }

  lambda <- spec$matrices[["LAMBDA"]]
  if (!is.null(lambda) && is.matrix(lambda)) {
    for (col in seq_len(ncol(lambda))) {
      active <- vapply(lambda[, col], ctgui_cell_active, logical(1L))
      if (!any(active)) {
        add_message("warning", "LAMBDA", paste("Latent process", colnames(lambda)[col], "has no active manifest loading"))
      }
    }
    for (row in seq_len(nrow(lambda))) {
      active <- vapply(lambda[row, ], ctgui_cell_active, logical(1L))
      if (!any(active)) {
        add_message("warning", "LAMBDA", paste("Manifest variable", rownames(lambda)[row], "does not load on any latent process"))
      }
    }
  }

  drift <- spec$matrices[["DRIFT"]]
  if (!is.null(drift) && is.matrix(drift) && identical(spec$type, "ct")) {
    diagonal <- diag(drift)
    diagonal_numeric <- suppressWarnings(as.numeric(diagonal))
    positive <- !is.na(diagonal_numeric) & diagonal_numeric > 0
    if (any(positive)) {
      add_message("warning", "DRIFT", paste("Positive continuous-time auto-effects may imply explosive dynamics:",
        paste(names(diagonal)[positive] %||% which(positive), collapse = ", ")))
    }
  }

  if (length(spec$tipred_names) > 0L && isTRUE(spec$tipredDefault)) {
    add_message("info", "tipredDefault", "TI predictors affect all free parameters by default; beginners often set tipredDefault = FALSE and opt in per parameter")
  }

  pars <- spec$matrices[["PARS"]]
  if (!is.null(pars) && is.matrix(pars)) {
    par_values <- as.character(pars)
    duplicate_pars <- par_values[nzchar(par_values) & duplicated(par_values)]
    if (length(duplicate_pars)) {
      add_message("warning", "PARS", paste("Duplicate PARS entries:", paste(unique(duplicate_pars), collapse = ", ")))
    }
  }

  if (length(messages) == 0L) {
    return(data.frame(
      severity = "ok",
      field = "spec",
      message = "No validation issues found",
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, messages)
}

#' @rdname ctgui_spec
ctgui_matrix_names <- function(spec) {
  ctgui_check_spec(spec)
  names(ctgui_order_matrices(spec$matrices))
}

#' @rdname ctgui_spec
#' @param matrix Matrix name.
ctgui_matrix <- function(spec, matrix) {
  ctgui_check_spec(spec)
  matrix <- ctgui_match_matrix_name(spec, matrix)
  spec$matrices[[matrix]]
}

#' @rdname ctgui_spec
#' @param value A replacement matrix or a scalar matrix cell value.
ctgui_set_matrix <- function(spec, matrix, value) {
  ctgui_check_spec(spec)
  matrix <- ctgui_match_matrix_name(spec, matrix)
  if (!is.matrix(value)) stop("value must be a matrix", call. = FALSE)

  expected <- ctgui_expected_dims(spec)[[matrix]]
  if (!is.null(expected) && !identical(dim(value), expected)) {
    stop(matrix, " must have dimensions ", paste(expected, collapse = " x "), call. = FALSE)
  }

  spec$matrices[[matrix]] <- ctgui_apply_dimnames_to_one(
    matrix, value,
    latent_names = spec$latent_names,
    manifest_names = spec$manifest_names,
    tdpred_names = spec$tdpred_names
  )
  spec$matrices <- ctgui_order_matrices(spec$matrices)
  ctgui_sync_model_from_matrices(spec)
}

#' @rdname ctgui_spec
#' @param row Row index or row name.
#' @param col Column index or column name.
#' @param label Free-parameter label for a matrix cell.
#' @param free Logical; when `TRUE`, `label` or `value` is treated as a
#'   parameter label.
ctgui_set_matrix_value <- function(spec, matrix, row, col = 1, value = NULL,
    label = NULL, free = NULL) {
  ctgui_check_spec(spec)
  matrix <- ctgui_match_matrix_name(spec, matrix)
  mat <- spec$matrices[[matrix]]
  if (is.null(mat)) stop(matrix, " is not present in spec", call. = FALSE)

  row_index <- ctgui_matrix_index(row, rownames(mat), "row")
  col_index <- ctgui_matrix_index(col, colnames(mat), "col")

  if (!is.null(label) && !is.null(value)) {
    stop("Use either label or value, not both", call. = FALSE)
  }
  if (isTRUE(free) && is.null(label)) {
    label <- if (is.null(value)) {
      ctgui_auto_label(matrix, rownames(mat)[row_index], colnames(mat)[col_index])
    } else {
      as.character(value)
    }
  }

  if (!is.null(label)) {
    mat[row_index, col_index] <- as.character(label)
  } else if (!is.null(value)) {
    if (length(value) != 1L || is.na(value)) stop("value must be a single non-missing value", call. = FALSE)
    mat[row_index, col_index] <- value
  } else if (isFALSE(free)) {
    mat[row_index, col_index] <- 0
  } else {
    stop("Provide value, label, or free = TRUE/FALSE", call. = FALSE)
  }

  spec$matrices[[matrix]] <- mat
  spec$matrices <- ctgui_order_matrices(spec$matrices)
  ctgui_sync_model_from_matrices(spec)
}

#' @rdname ctgui_spec
#' @param silent Passed to `ctsem::ctModel()`.
ctgui_to_ctsem_model <- function(spec, silent = TRUE, tipredDefault = spec$tipredDefault) {
  ctgui_check_spec(spec)
  if (!ctgui_has_ctsem()) stop("ctsem must be installed to create a ctsem model", call. = FALSE)

  errors <- ctgui_validate(spec)
  errors <- errors[errors$severity == "error", , drop = FALSE]
  if (nrow(errors) > 0L) {
    stop("Cannot create ctsem model because validation errors are present:\n",
      paste(errors$message, collapse = "\n"),
      call. = FALSE)
  }

  ctgui_new_ctsem_model(
    latent_names = spec$latent_names,
    manifest_names = spec$manifest_names,
    type = spec$type,
    id = spec$id,
    time = spec$time,
    Tpoints = spec$Tpoints,
    manifest_type = spec$manifest_type,
    tdpred_names = spec$tdpred_names,
    tipred_names = spec$tipred_names,
    matrices = ctgui_matrices_with_metadata(spec),
    tipredDefault = tipredDefault,
    silent = silent
  )
}

#' Convert a raw ctsem model object into an editable GUI specification
#'
#' @param model A model created by `ctsem::ctModel()`.
ctgui_spec_from_model <- function(model) {
  if (is.null(model$pars) || is.null(model$latentNames) || is.null(model$manifestNames)) {
    stop("RDS does not contain a ctsem model created by ctModel()", call. = FALSE)
  }
  matrices <- ctgui_ctsem_matrices(model)
  spec <- ctgui_spec(
    latent_names = model$latentNames,
    manifest_names = model$manifestNames,
    type = if (isTRUE(model$continuoustime)) "ct" else "dt",
    id = model$subjectIDname %||% "id",
    time = model$timeName %||% "time",
    manifest_type = model$manifesttype %||% rep(0L, length(model$manifestNames)),
    tdpred_names = model$TDpredNames %||% character(),
    tipred_names = model$TIpredNames %||% character(),
    matrices = matrices,
    tipredDefault = TRUE
  )
  spec$model <- model
  spec$pars <- model$pars
  spec$parameter_metadata <- ctgui_parameter_metadata_from_pars(model$pars, spec$tipred_names, spec$matrices)
  spec$source <- "ctsem-rds"
  spec
}

#' @rdname ctgui_spec
#' @param object_name Name used for the model object in exported code.
ctgui_export_code <- function(spec, object_name = "model") {
  ctgui_check_spec(spec)
  args <- ctgui_ctmodel_args(spec, matrices = ctgui_matrices_with_metadata(spec), silent = FALSE)
  args$silent <- NULL

  lines <- c(
    "library(ctsem)",
    "",
    paste0(object_name, " <- ctsem::ctModel(")
  )

  arg_names <- names(args)
  for (i in seq_along(args)) {
    arg_code <- paste(ctgui_deparse(args[[i]]), collapse = "\n")
    arg_lines <- paste0("  ", arg_names[[i]], " = ", arg_code)
    if (i < length(args)) arg_lines[length(arg_lines)] <- paste0(arg_lines[length(arg_lines)], ",")
    lines <- c(lines, arg_lines)
  }
  lines <- c(lines, ")")
  paste(lines, collapse = "\n")
}

#' @rdname ctgui_spec
#' @param ... Additional arguments passed to the underlying ctsem function.
ctgui_latex <- function(spec, ...) {
  ctgui_check_spec(spec)
  if (!ctgui_has_ctsem()) stop("ctsem must be installed to create model equations", call. = FALSE)
  model <- ctgui_to_ctsem_model(spec, silent = TRUE)
  out <- tryCatch(getExportedValue("ctsem", "ctModelLatex")(
    model,
    compile = FALSE,
    open = FALSE,
    equationonly = TRUE,
    includeNote = FALSE,
    ...
  ), error = function(e) e)
  if (inherits(out, "error")) return(ctgui_latex_fallback(spec, out))
  out
}

ctgui_latex_fallback <- function(spec, error) {
  lines <- c(
    "% ctModelLatex fallback generated by ctsemgui",
    paste0("% ctsem renderer error: ", conditionMessage(error))
  )
  for (matrix_name in ctgui_matrix_names(spec)) {
    mat <- spec$matrices[[matrix_name]]
    if (!is.matrix(mat)) next
    lines <- c(lines, paste0("\\textbf{", matrix_name, "}"), "\\begin{verbatim}",
      paste(utils::capture.output(print(mat, quote = FALSE)), collapse = "\n"),
      "\\end{verbatim}")
  }
  paste(lines, collapse = "\n")
}

ctgui_latex_png <- function(spec, folder = tempdir(), filename = NULL, ...) {
  ctgui_check_spec(spec)
  if (!ctgui_has_ctsem()) stop("ctsem must be installed to render model equations", call. = FALSE)
  if (is.null(filename)) {
    filename <- paste0("ctgui_equations_", Sys.getpid(), "_", as.integer(Sys.time()), "_", sample.int(1e6, 1L))
  }
  model <- ctgui_to_ctsem_model(spec, silent = TRUE)
  getExportedValue("ctsem", "ctModelLatex")(
    model,
    compile = TRUE,
    open = FALSE,
    equationonly = FALSE,
    includeNote = FALSE,
    savepng = TRUE,
    folder = folder,
    filename = filename,
    ...
  )
  png <- file.path(folder, paste0(filename, ".png"))
  if (!file.exists(png)) stop("ctModelLatex did not create a PNG file", call. = FALSE)
  png
}

#' @rdname ctgui_spec
#' @param n.subjects Number of subjects to generate.
#' @param burnin Number of initial generated time points to discard.
#' @param dtmean Mean time interval for generated data.
#' @param logdtsd Log time interval standard deviation.
#' @param wide Logical; passed to `ctsem::ctGenerate()`.
ctgui_generate_data <- function(spec, n.subjects = 100, Tpoints = spec$Tpoints %||% 10,
    burnin = 0, dtmean = 1, logdtsd = 0, wide = FALSE, free_defaults = TRUE) {
  ctgui_check_spec(spec)
  if (!ctgui_has_ctsem()) stop("ctsem must be installed to generate data", call. = FALSE)
  gen_matrices <- if (isTRUE(free_defaults)) ctgui_generation_matrices(spec) else ctgui_matrices_with_metadata(spec)
  ctgui_assert_generation_numeric(gen_matrices, free_defaults = free_defaults)
  model <- ctgui_new_ctsem_model(
    latent_names = spec$latent_names,
    manifest_names = spec$manifest_names,
    type = spec$type,
    id = spec$id,
    time = spec$time,
    Tpoints = Tpoints,
    manifest_type = spec$manifest_type,
    tdpred_names = spec$tdpred_names,
    tipred_names = spec$tipred_names,
    matrices = gen_matrices,
    tipredDefault = spec$tipredDefault,
    silent = TRUE
  )
  generated <- getExportedValue("ctsem", "ctGenerate")(
    ctmodelobj = model,
    n.subjects = n.subjects,
    burnin = burnin,
    dtmean = dtmean,
    logdtsd = logdtsd,
    Tpoints = Tpoints,
    wide = wide
  )
  as.data.frame(generated, stringsAsFactors = FALSE)
}

ctgui_assert_generation_numeric <- function(matrices, free_defaults) {
  bad <- character()
  for (matrix_name in names(matrices)) {
    mat <- matrices[[matrix_name]]
    if (!is.matrix(mat)) next
    numeric <- suppressWarnings(as.numeric(mat))
    if (any(is.na(numeric) & !is.na(as.vector(mat)))) bad <- c(bad, matrix_name)
  }
  if (length(bad)) {
    stop("Data generation requires numeric model matrices. ",
      "Free labels remain in: ", paste(unique(bad), collapse = ", "), ". ",
      if (isTRUE(free_defaults)) {
        "Some entries could not be converted to preview values."
      } else {
        "Enable preview generation to replace free labels with simple numeric values."
      },
      call. = FALSE)
  }
  invisible(TRUE)
}

ctgui_generation_matrices <- function(spec) {
  out <- spec$matrices
  for (matrix_name in names(out)) {
    mat <- out[[matrix_name]]
    if (!is.matrix(mat)) next
    numeric <- suppressWarnings(matrix(as.numeric(mat), nrow = nrow(mat), ncol = ncol(mat), dimnames = dimnames(mat)))
    missing <- is.na(numeric)
    if (any(missing)) numeric[missing] <- ctgui_generation_default(matrix_name, row(numeric)[missing], col(numeric)[missing])
    out[[matrix_name]] <- numeric
  }
  out
}

ctgui_generation_default <- function(matrix_name, row, col) {
  if (matrix_name %in% "DRIFT") return(ifelse(row == col, -0.2, 0))
  if (matrix_name %in% c("DIFFUSION", "T0VAR")) return(ifelse(row == col, 1, 0))
  if (matrix_name %in% "MANIFESTVAR") return(ifelse(row == col, 0.1, 0))
  if (matrix_name %in% "LAMBDA") return(ifelse(row == col, 1, 0))
  rep(0, length(row))
}

ctgui_has_ctsem <- function() {
  requireNamespace("ctsem", quietly = TRUE)
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || is.na(x[1L])) y else x
}

ctgui_new_ctsem_model <- function(latent_names, manifest_names, type, id, time,
    Tpoints, manifest_type, tdpred_names, tipred_names, matrices,
    tipredDefault, silent) {
  args <- ctgui_ctmodel_args_from_values(
    latent_names = latent_names,
    manifest_names = manifest_names,
    type = type,
    id = id,
    time = time,
    Tpoints = Tpoints,
    manifest_type = manifest_type,
    tdpred_names = tdpred_names,
    tipred_names = tipred_names,
    matrices = matrices,
    tipredDefault = tipredDefault,
    silent = silent
  )
  do.call(getExportedValue("ctsem", "ctModel"), args)
}

ctgui_ctmodel_args <- function(spec, matrices = spec$matrices, silent = TRUE) {
  ctgui_ctmodel_args_from_values(
    latent_names = spec$latent_names,
    manifest_names = spec$manifest_names,
    type = spec$type,
    id = spec$id,
    time = spec$time,
    Tpoints = spec$Tpoints,
    manifest_type = spec$manifest_type,
    tdpred_names = spec$tdpred_names,
    tipred_names = spec$tipred_names,
    matrices = matrices,
    tipredDefault = spec$tipredDefault,
    silent = silent
  )
}

ctgui_ctmodel_args_from_values <- function(latent_names, manifest_names, type, id, time,
    Tpoints, manifest_type, tdpred_names, tipred_names, matrices,
    tipredDefault, silent) {
  if (is.null(matrices)) {
    matrices <- list(LAMBDA = ctgui_default_lambda(manifest_names, latent_names))
  }
  args <- list(
    type = type,
    latentNames = latent_names,
    manifestNames = manifest_names,
    manifesttype = manifest_type,
    id = id,
    time = time,
    tipredDefault = tipredDefault,
    silent = silent
  )
  if (!is.null(Tpoints)) args$Tpoints <- Tpoints
  if (length(tdpred_names) > 0L) args$TDpredNames <- tdpred_names
  if (length(tipred_names) > 0L) args$TIpredNames <- tipred_names

  for (matrix_name in names(matrices)) {
    if (!is.null(matrices[[matrix_name]])) args[[matrix_name]] <- matrices[[matrix_name]]
  }
  if (is.null(args$LAMBDA)) args$LAMBDA <- ctgui_default_lambda(manifest_names, latent_names)
  args
}

ctgui_default_matrices <- function(latent_names, manifest_names, tdpred_names, Tpoints) {
  n_latent <- length(latent_names)
  n_manifest <- length(manifest_names)

  matrices <- list(
    LAMBDA = ctgui_default_lambda(manifest_names, latent_names),
    T0VAR = ctgui_lower_label_matrix("T0VAR", latent_names),
    T0MEANS = ctgui_label_matrix("T0MEANS", latent_names, "mean", ncol = 1L),
    MANIFESTMEANS = ctgui_label_matrix("MANIFESTMEANS", manifest_names, "mean", ncol = 1L),
    MANIFESTVAR = ctgui_diag_label_matrix("MANIFESTVAR", manifest_names),
    DRIFT = ctgui_label_matrix("DRIFT", latent_names, latent_names),
    CINT = ctgui_fixed_matrix(0, latent_names, "cint", ncol = 1L),
    DIFFUSION = ctgui_lower_label_matrix("DIFFUSION", latent_names)
  )

  if (length(tdpred_names) > 0L) {
    matrices$TDPREDEFFECT <- ctgui_label_matrix("TDPREDEFFECT", latent_names, tdpred_names)
    if (!is.null(Tpoints)) {
      td_rows <- paste0(rep(tdpred_names, times = Tpoints), "_T", rep(seq_len(Tpoints), each = length(tdpred_names)))
      matrices$TDPREDMEANS <- ctgui_label_matrix("TDPREDMEANS", td_rows, "mean", ncol = 1L)
      matrices$TDPREDVAR <- ctgui_lower_label_matrix("TDPREDVAR", td_rows)
    }
  }

  matrices
}

ctgui_default_lambda <- function(manifest_names, latent_names) {
  mat <- matrix(0, nrow = length(manifest_names), ncol = length(latent_names),
    dimnames = list(manifest_names, latent_names))
  diag_n <- min(nrow(mat), ncol(mat))
  if (diag_n > 0L) mat[cbind(seq_len(diag_n), seq_len(diag_n))] <- 1
  mat
}

ctgui_fixed_matrix <- function(value, row_names, col_names, ncol = length(col_names)) {
  if (ncol == 1L) {
    col_names <- col_names[1L]
  }
  matrix(value, nrow = length(row_names), ncol = ncol, dimnames = list(row_names, col_names))
}

ctgui_label_matrix <- function(prefix, row_names, col_names, ncol = length(col_names)) {
  if (ncol == 1L) {
    mat <- matrix(paste(prefix, row_names, sep = "_"),
      nrow = length(row_names), ncol = 1L,
      dimnames = list(row_names, col_names[1L]))
    return(mat)
  }

  mat <- outer(row_names, col_names, function(r, c) ctgui_auto_label(prefix, r, c))
  dimnames(mat) <- list(row_names, col_names)
  mat
}

ctgui_lower_label_matrix <- function(prefix, names) {
  mat <- matrix(0, nrow = length(names), ncol = length(names), dimnames = list(names, names))
  for (r in seq_along(names)) {
    for (c in seq_len(r)) mat[r, c] <- ctgui_auto_label(prefix, names[r], names[c])
  }
  mat
}

ctgui_diag_label_matrix <- function(prefix, names) {
  mat <- matrix(0, nrow = length(names), ncol = length(names), dimnames = list(names, names))
  for (i in seq_along(names)) mat[i, i] <- ctgui_auto_label(prefix, names[i], names[i])
  mat
}

ctgui_auto_label <- function(matrix, row_name, col_name) {
  clean <- function(x) gsub("\\W", "_", x)
  paste(clean(tolower(matrix)), clean(row_name), clean(col_name), sep = "_")
}

ctgui_expected_dims <- function(spec) {
  n_latent <- length(spec$latent_names)
  n_manifest <- length(spec$manifest_names)
  n_tdpred <- length(spec$tdpred_names)
  dims <- list(
    LAMBDA = c(n_manifest, n_latent),
    T0VAR = c(n_latent, n_latent),
    T0MEANS = c(n_latent, 1L),
    MANIFESTMEANS = c(n_manifest, 1L),
    MANIFESTVAR = c(n_manifest, n_manifest),
    DRIFT = c(n_latent, n_latent),
    CINT = c(n_latent, 1L),
    DIFFUSION = c(n_latent, n_latent)
  )
  if (n_tdpred > 0L) {
    dims$TDPREDEFFECT <- c(n_latent, n_tdpred)
    if (!is.null(spec$Tpoints)) {
      dims$TDPREDMEANS <- c(n_tdpred * spec$Tpoints, 1L)
      dims$TDPREDVAR <- c(n_tdpred * spec$Tpoints, n_tdpred * spec$Tpoints)
    }
  }
  dims
}

ctgui_prepare_matrices <- function(matrices, latent_names, manifest_names, tdpred_names) {
  matrices <- ctgui_order_matrices(matrices)
  for (matrix_name in names(matrices)) {
    matrices[[matrix_name]] <- ctgui_apply_dimnames_to_one(
      matrix_name, matrices[[matrix_name]],
      latent_names = latent_names,
      manifest_names = manifest_names,
      tdpred_names = tdpred_names
    )
  }
  matrices
}

ctgui_order_matrices <- function(matrices) {
  preferred <- c(ctgui_required_matrices, ctgui_optional_matrices)
  ordered <- preferred[preferred %in% names(matrices)]
  extra <- setdiff(names(matrices), ordered)
  matrices[c(ordered, extra)]
}

ctgui_apply_dimnames_to_one <- function(matrix_name, mat, latent_names, manifest_names, tdpred_names) {
  if (!is.matrix(mat)) return(mat)
  dims <- switch(matrix_name,
    LAMBDA = list(manifest_names, latent_names),
    T0VAR = list(latent_names, latent_names),
    T0MEANS = list(latent_names, matrix_name),
    MANIFESTMEANS = list(manifest_names, matrix_name),
    MANIFESTVAR = list(manifest_names, manifest_names),
    DRIFT = list(latent_names, latent_names),
    CINT = list(latent_names, matrix_name),
    DIFFUSION = list(latent_names, latent_names),
    TDPREDEFFECT = list(latent_names, tdpred_names),
    NULL
  )
  if (!is.null(dims) && length(dims[[1L]]) == nrow(mat) && length(dims[[2L]]) == ncol(mat)) {
    dimnames(mat) <- dims
  }
  mat
}

# The matrix labels returned by ctModelMatrices() deliberately omit the
# per-parameter annotations stored in model$pars.  Keep those annotations in
# the GUI specification and reapply them only when calling ctModel().
ctgui_parameter_metadata_from_pars <- function(pars, tipred_names = character(), matrices = NULL) {
  fields <- c("matrix", "row", "col", "param", "transform", "indvarying", "sdscale")
  if (is.null(pars) || !is.data.frame(pars) || !all(fields %in% names(pars))) {
    return(data.frame(matrix = character(), row = character(), col = character(),
      param = character(), transform = character(), indvarying = logical(),
      sdscale = numeric(), extra_pars = character(), stringsAsFactors = FALSE))
  }
  out <- pars[, fields, drop = FALSE]
  out <- out[!is.na(out$param) & nzchar(as.character(out$param)), , drop = FALSE]
  out$matrix <- as.character(out$matrix)
  out$row <- as.character(out$row)
  out$col <- as.character(out$col)
  if (!is.null(matrices)) for (i in seq_len(nrow(out))) {
    mat <- matrices[[out$matrix[i]]]
    if (is.null(mat) || !is.matrix(mat)) next
    row_index <- suppressWarnings(as.integer(out$row[i])); col_index <- suppressWarnings(as.integer(out$col[i]))
    if (!is.na(row_index) && row_index >= 1L && row_index <= nrow(mat) && !is.null(rownames(mat))) out$row[i] <- rownames(mat)[row_index]
    if (!is.na(col_index) && col_index >= 1L && col_index <= ncol(mat) && !is.null(colnames(mat))) out$col[i] <- colnames(mat)[col_index]
  }
  out$param <- as.character(out$param)
  out$transform <- as.character(out$transform %||% "")
  as_flag <- function(x) !is.na(x) & tolower(as.character(x)) %in% c("true", "t", "1")
  as_sdscale <- function(x) {
    values <- suppressWarnings(as.numeric(as.character(x)))
    text <- tolower(trimws(as.character(x)))
    values[is.na(values) & text %in% c("true", "t")] <- 1
    values[is.na(values) & text %in% c("false", "f")] <- 0
    values[is.na(values)] <- 1
    values
  }
  out$indvarying <- as_flag(out$indvarying)
  out$sdscale <- as_sdscale(out$sdscale)
  out$extra_pars <- ""
  for (tipred in tipred_names) {
    field <- paste0(tipred, "_effect")
    out[[field]] <- if (field %in% names(pars)) as.logical(pars[[field]][match(out$param, as.character(pars$param))]) else FALSE
  }
  rownames(out) <- NULL
  out
}

ctgui_cell_key <- function(matrix, row, col) paste(matrix, row, col, sep = "\r")

ctgui_split_pars <- function(x) {
  values <- trimws(unlist(strsplit(paste(x %||% character(), collapse = ","), "[,\r\n]+"), use.names = FALSE))
  unique(values[nzchar(values)])
}

ctgui_merge_extra_metadata <- function(metadata, previous) {
  if (!"extra_pars" %in% names(metadata)) metadata$extra_pars <- ""
  if (is.null(previous) || !nrow(previous) || !"extra_pars" %in% names(previous)) return(metadata)
  index <- match(ctgui_cell_key(metadata$matrix, metadata$row, metadata$col),
    ctgui_cell_key(previous$matrix, previous$row, previous$col))
  keep <- !is.na(index)
  metadata$extra_pars[keep] <- previous$extra_pars[index[keep]] %||% ""
  metadata
}

ctgui_sync_extra_pars <- function(spec) {
  metadata <- spec$parameter_metadata
  extra <- if (!is.null(metadata) && nrow(metadata) && "extra_pars" %in% names(metadata)) {
    ctgui_split_pars(metadata$extra_pars)
  } else character()
  previous <- spec$matrix_extra_pars %||% character()
  current <- spec$matrices[["PARS"]]
  current <- if (is.null(current)) character() else as.character(current[, 1L, drop = TRUE])
  manual <- current[!(current %in% previous)]
  values <- unique(c(ctgui_split_pars(manual), extra))
  spec$matrix_extra_pars <- extra
  if (length(values)) {
    spec$matrices[["PARS"]] <- matrix(values, ncol = 1L,
      dimnames = list(paste0("PARS", seq_along(values)), "PARS"))
  } else {
    spec$matrices[["PARS"]] <- NULL
  }
  spec$matrices <- ctgui_order_matrices(spec$matrices)
  spec
}

ctgui_parse_parameter_cell <- function(value, tipred_names = character()) {
  value <- trimws(as.character(value)[1L])
  parts <- strsplit(value, "|", fixed = TRUE)[[1L]]
  base <- trimws(parts[1L])
  numeric <- suppressWarnings(as.numeric(base))
  free <- is.na(numeric) && nzchar(base)
  out <- list(param = if (free) base else NA_character_, transform = "",
    indvarying = FALSE, sdscale = 1, tipreds = character())
  if (!free || length(parts) == 1L) return(out)
  if (length(parts) >= 2L) out$transform <- trimws(parts[2L])
  if (length(parts) >= 3L) out$indvarying <- identical(tolower(trimws(parts[3L])), "true")
  if (length(parts) >= 4L) {
    sdscale_text <- tolower(trimws(parts[4L]))
    sdscale_value <- suppressWarnings(as.numeric(sdscale_text))
    if (identical(sdscale_text, "true")) sdscale_value <- 1
    if (identical(sdscale_text, "false")) sdscale_value <- 0
    if (!is.na(sdscale_value)) out$sdscale <- sdscale_value else if (length(parts) == 4L) {
      # Older ctsemgui labels omitted the RandomEffectsScale field.
      out$tipreds <- intersect(trimws(unlist(strsplit(parts[4L], ",", fixed = TRUE))), tipred_names)
    }
  }
  if (length(parts) >= 5L) {
    out$tipreds <- intersect(trimws(unlist(strsplit(parts[5L], ",", fixed = TRUE))), tipred_names)
  }
  out
}

ctgui_refresh_parameter_metadata <- function(spec, matrices = spec$matrices) {
  tipred_names <- spec$tipred_names
  old <- spec$parameter_metadata
  if (is.null(old)) old <- ctgui_parameter_metadata_from_pars(spec$pars, tipred_names, matrices)
  rows <- list()
  cleaned <- matrices
  for (matrix_name in names(matrices)) {
    mat <- matrices[[matrix_name]]
    if (!is.matrix(mat)) next
    for (r in seq_len(nrow(mat))) for (c in seq_len(ncol(mat))) {
      parsed <- ctgui_parse_parameter_cell(mat[r, c], tipred_names)
      if (is.na(parsed$param)) next
      cleaned[[matrix_name]][r, c] <- parsed$param
      key <- ctgui_cell_key(matrix_name, rownames(mat)[r], colnames(mat)[c])
      prior <- old[ctgui_cell_key(old$matrix, old$row, old$col) == key, , drop = FALSE]
      row <- data.frame(matrix = matrix_name, row = rownames(mat)[r], col = colnames(mat)[c],
        param = parsed$param, transform = parsed$transform, indvarying = parsed$indvarying,
        sdscale = parsed$sdscale, extra_pars = "", stringsAsFactors = FALSE)
      if (nrow(prior) && !grepl("|", as.character(mat[r, c]), fixed = TRUE)) {
        row$transform <- prior$transform[1L] %||% ""
        row$indvarying <- isTRUE(prior$indvarying[1L])
        row$sdscale <- suppressWarnings(as.numeric(prior$sdscale[1L]))
        if (is.na(row$sdscale)) row$sdscale <- if (isTRUE(prior$sdscale[1L])) 1 else 0
        if ("extra_pars" %in% names(prior)) row$extra_pars <- prior$extra_pars[1L] %||% ""
      }
      for (tipred in tipred_names) {
        field <- paste0(tipred, "_effect")
        row[[field]] <- tipred %in% parsed$tipreds
        if (nrow(prior) && !grepl("|", as.character(mat[r, c]), fixed = TRUE) && field %in% names(prior)) row[[field]] <- isTRUE(prior[[field]][1L])
      }
      rows[[length(rows) + 1L]] <- row
    }
  }
  empty <- ctgui_parameter_metadata_from_pars(NULL, tipred_names)
  spec$parameter_metadata <- if (length(rows)) do.call(rbind, rows) else empty
  spec$matrices <- ctgui_order_matrices(cleaned)
  ctgui_sync_extra_pars(spec)
  spec
}

ctgui_matrices_with_metadata <- function(spec) {
  matrices <- spec$matrices
  metadata <- spec$parameter_metadata
  if (is.null(metadata) || !nrow(metadata)) return(matrices)
  for (i in seq_len(nrow(metadata))) {
    matrix_name <- metadata$matrix[i]
    mat <- matrices[[matrix_name]]
    if (is.null(mat) || !is.matrix(mat)) next
    r <- match(metadata$row[i], rownames(mat))
    c <- match(metadata$col[i], colnames(mat))
    if (is.na(r) || is.na(c)) next
    transform <- metadata$transform[i] %||% ""
    indvarying <- isTRUE(metadata$indvarying[i])
    sdscale <- suppressWarnings(as.numeric(metadata$sdscale[i]))
    if (is.na(sdscale)) sdscale <- if (isTRUE(metadata$sdscale[i])) 1 else 0
    effects <- vapply(spec$tipred_names, function(tipred) {
      field <- paste0(tipred, "_effect")
      field %in% names(metadata) && isTRUE(metadata[[field]][i])
    }, logical(1L))
    needs_annotation <- nzchar(transform) || indvarying || !identical(sdscale, 1) || any(effects)
    if (needs_annotation) {
      suffix <- c(transform, if (indvarying) "TRUE" else "", if (identical(sdscale, 1)) "" else as.character(sdscale))
      if (any(effects)) suffix <- c(suffix, paste(spec$tipred_names[effects], collapse = ","))
      while (length(suffix) && !nzchar(suffix[length(suffix)])) suffix <- suffix[-length(suffix)]
      mat[r, c] <- paste(c(as.character(mat[r, c]), suffix), collapse = "|")
    }
    matrices[[matrix_name]] <- mat
  }
  matrices
}

ctgui_set_parameter_metadata <- function(spec, matrix, row, col, transform = NULL,
    indvarying = NULL, sdscale = NULL, tipred_effects = NULL, extra_pars = NULL) {
  spec <- ctgui_refresh_parameter_metadata(spec)
  index <- which(spec$parameter_metadata$matrix == matrix &
    spec$parameter_metadata$row == row & spec$parameter_metadata$col == col)
  if (!length(index)) return(spec)
  index <- index[1L]
  if (!is.null(transform)) spec$parameter_metadata$transform[index] <- trimws(as.character(transform))
  if (!is.null(indvarying)) spec$parameter_metadata$indvarying[index] <- isTRUE(indvarying)
  if (!is.null(sdscale)) {
    value <- suppressWarnings(as.numeric(sdscale)[1L])
    if (is.na(value)) value <- if (isTRUE(sdscale)) 1 else 0
    spec$parameter_metadata$sdscale[index] <- value
  }
  if (!is.null(tipred_effects)) for (tipred in spec$tipred_names) {
    spec$parameter_metadata[[paste0(tipred, "_effect")]][index] <- tipred %in% tipred_effects
  }
  if (!is.null(extra_pars)) spec$parameter_metadata$extra_pars[index] <- paste(ctgui_split_pars(extra_pars), collapse = ", ")
  spec <- ctgui_sync_extra_pars(spec)
  ctgui_sync_model_from_matrices(spec)
}

ctgui_sync_model_from_matrices <- function(spec) {
  spec <- ctgui_refresh_parameter_metadata(spec)
  if (!is.null(spec$model) && ctgui_has_ctsem()) {
    synced <- tryCatch({
      # Rebuilding through ctModel is more reliable than ctModelMatrices<- for
      # annotated cells: the latter normalises some legacy annotation fields.
      spec$model <- ctgui_new_ctsem_model(
        latent_names = spec$latent_names, manifest_names = spec$manifest_names,
        type = spec$type, id = spec$id, time = spec$time, Tpoints = spec$Tpoints,
        manifest_type = spec$manifest_type, tdpred_names = spec$tdpred_names,
        tipred_names = spec$tipred_names, matrices = ctgui_matrices_with_metadata(spec),
        tipredDefault = spec$tipredDefault, silent = TRUE
      )
      spec$pars <- spec$model[["pars"]]
      spec$parameter_metadata <- ctgui_merge_extra_metadata(
        ctgui_parameter_metadata_from_pars(spec$pars, spec$tipred_names, spec$matrices),
        spec$parameter_metadata
      )
      spec$source <- "ctsem"
      spec
    }, error = function(e) {
      spec$source <- paste0("ctsem-unsynced: ", conditionMessage(e))
      spec
    })
    return(synced)
  }
  spec
}

ctgui_ctsem_matrices <- function(model) {
  getExportedValue("ctsem", "ctModelMatrices")(model)
}

ctgui_match_matrix_name <- function(spec, matrix) {
  if (length(matrix) != 1L) stop("matrix must have length 1", call. = FALSE)
  matched <- match.arg(matrix, choices = names(spec$matrices))
  matched
}

ctgui_matrix_index <- function(index, names, label) {
  if (is.character(index)) {
    matched <- match(index, names)
    if (is.na(matched)) stop(label, " name not found: ", index, call. = FALSE)
    return(matched)
  }
  if (!is.numeric(index) || length(index) != 1L || is.na(index)) {
    stop(label, " must be a single index or name", call. = FALSE)
  }
  index <- as.integer(index)
  if (index < 1L || index > length(names)) stop(label, " index out of bounds", call. = FALSE)
  index
}

ctgui_as_names <- function(x, field, allow_empty = FALSE) {
  if (is.null(x)) x <- character()
  x <- trimws(as.character(x))
  x <- x[nzchar(x)]
  if (!allow_empty && length(x) == 0L) stop(field, " must contain at least one name", call. = FALSE)
  x
}

ctgui_check_spec <- function(spec) {
  if (!inherits(spec, "ctsemgui_spec")) stop("spec must be a ctsemgui_spec object", call. = FALSE)
  invisible(TRUE)
}

ctgui_deparse <- function(x) {
  if (is.matrix(x)) return(ctgui_deparse_matrix(x))
  deparse(x, width.cutoff = 500L, control = "keepNA")
}

ctgui_deparse_matrix <- function(x) {
  value_code <- paste(deparse(as.vector(x), width.cutoff = 500L, control = "keepNA"), collapse = "\n")
  out <- paste0("matrix(", value_code, ", nrow = ", nrow(x), ", ncol = ", ncol(x))
  if (!is.null(dimnames(x))) {
    dimnames_code <- paste(deparse(dimnames(x), width.cutoff = 500L, control = "keepNA"), collapse = "\n")
    out <- paste0(out, ", dimnames = ", dimnames_code)
  }
  paste0(out, ")")
}
