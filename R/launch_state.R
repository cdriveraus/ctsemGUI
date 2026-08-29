# Launch-time session state --------------------------------------------------
#
# A model specified in a script should open in the GUI on exactly the terms a
# model loaded through the interface does.  These helpers translate the objects
# a user already has in their R session -- a ctModel(), a fit, a data frame --
# into the initial reactive state the application starts from, so the launch
# path and the in-app load path converge on the same canonical specification.

ctgui_launch_spec <- function(model) {
  if (is.null(model)) return(NULL)
  if (is.character(model)) model <- ctgui_read_rds_object(model, "model")
  spec <- ctgui_project_spec(model)
  # Committing here is what makes a scripted model behave like one authored in
  # the GUI: matrices are normalised, parameter metadata is refreshed, and the
  # cached ctsem model is rebuilt from the specification the app will edit.
  ctgui_commit_spec(previous = spec, updated = spec, reason = "launch")$spec
}

ctgui_launch_fit <- function(fit) {
  if (is.null(fit)) return(NULL)
  if (is.character(fit)) fit <- ctgui_read_rds_object(fit, "fit")
  if (!ctgui_ctsem_is_fit(fit)) {
    stop("fit must be a fitted ctsem model, as returned by ctsem::ctFit()", call. = FALSE)
  }
  fit
}

ctgui_launch_data <- function(data) {
  if (is.null(data)) return(NULL)
  if (is.character(data)) return(ctgui_read_data_file(data))
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("data must be a long-format data.frame or matrix, or a path to a .csv or .rds file",
      call. = FALSE)
  }
  data
}

ctgui_read_rds_object <- function(path, label) {
  if (length(path) != 1L || !nzchar(path)) {
    stop(label, " must be a single file path", call. = FALSE)
  }
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  readRDS(path)
}

ctgui_read_data_file <- function(path) {
  if (length(path) != 1L || !nzchar(path)) {
    stop("data must be a single file path", call. = FALSE)
  }
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  extension <- tolower(tools::file_ext(path))
  data <- if (identical(extension, "rds")) {
    readRDS(path)
  } else if (identical(extension, "csv")) {
    utils::read.csv(path, stringsAsFactors = FALSE)
  } else {
    stop("data file must be a .csv or .rds file", call. = FALSE)
  }
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("The data file must contain a data.frame or matrix", call. = FALSE)
  }
  data
}

ctgui_launch_data_name <- function(data, label = NULL) {
  if (is.null(data)) return("No data selected")
  if (is.character(label) && length(label) == 1L && nzchar(label)) {
    return(paste0("R data: ", label))
  }
  "Supplied at launch"
}

# Columns the specification expects but the data does not provide.  This is
# reported rather than enforced: a partially matching data set is still worth
# previewing, and the roles can be repointed in the Specification tab.
ctgui_launch_data_gaps <- function(spec, data) {
  if (is.null(spec) || is.null(data)) return(character())
  expected <- c(
    spec$id, spec$time, spec$manifest_names,
    spec$tdpred_names, spec$tipred_names
  )
  setdiff(expected[nzchar(expected)], ctgui_data_columns(data))
}

ctgui_launch_status <- function(spec, fit, data, missing_columns = character()) {
  parts <- character()
  if (!is.null(spec)) {
    parts <- c(parts, sprintf(
      "Opened a %s model with %d latent process%s and %d manifest variable%s.",
      if (identical(spec$type, "ct")) "continuous-time" else "discrete-time",
      length(spec$latent_names), if (length(spec$latent_names) == 1L) "" else "es",
      length(spec$manifest_names), if (length(spec$manifest_names) == 1L) "" else "s"
    ))
  }
  if (!is.null(fit)) parts <- c(parts, "A fitted model was supplied, so diagnostics and output are available.")
  if (!is.null(data) && length(missing_columns)) {
    parts <- c(parts, paste0(
      "These specification columns are not in the supplied data: ",
      paste(missing_columns, collapse = ", "), "."
    ))
  }
  if (!length(parts)) return(NULL)
  paste(parts, collapse = " ")
}

# The whole initial state in one value, so ctgui_create_app() and its tests can
# pass a single object around rather than a growing argument list.
ctgui_launch_state <- function(model = NULL, data = NULL, fit = NULL,
    data_label = NULL) {
  fit <- ctgui_launch_fit(fit)
  # A fit carries the model it was fitted with, so opening a fit alone is
  # enough to see that model's diagram, equations, and diagnostics together.
  if (is.null(model) && !is.null(fit)) model <- ctgui_ctsem_fit_model(fit)
  spec <- ctgui_launch_spec(model)
  data <- ctgui_launch_data(data)
  missing_columns <- ctgui_launch_data_gaps(spec, data)
  empty_spec <- function() {
    ctgui_spec(latent_names = character(), manifest_names = character())
  }
  list(
    spec = if (is.null(spec)) empty_spec() else spec,
    data = data,
    data_name = ctgui_launch_data_name(data, data_label),
    fit = fit,
    fit_status = if (is.null(fit)) "No fit available." else "Using the fit supplied at launch.",
    # An opened model is the reason the app was started, so show it first.
    tab = if (is.null(spec)) "Data" else "Model",
    status = ctgui_launch_status(spec, fit, data, missing_columns)
  )
}
