# ctsem compatibility boundary ----------------------------------------------

ctgui_ctsem_capabilities <- function() {
  required <- c("ctModel", "ctModelMatrices")
  optional <- c("ctFit", "ctModelLatex", "ctGenerate", "ctOptimUncertainty",
    "ctSummaryMatrices", "ctFitCovCheck", "ctFitCovCheckPlot", "ctPredict",
    "ctPredictTIP", "ctPostPredPlots", "ctACFresiduals", "ctDiscretePars", "plotctACF")
  installed <- requireNamespace("ctsem", quietly = TRUE)
  exports <- if (installed) getNamespaceExports("ctsem") else character()
  list(installed = installed,
    version = if (installed) as.character(utils::packageVersion("ctsem")) else NA_character_,
    required = stats::setNames(required %in% exports, required),
    optional = stats::setNames(optional %in% exports, optional))
}

ctgui_has_ctsem <- function() {
  caps <- ctgui_ctsem_capabilities()
  isTRUE(caps$installed) && all(caps$required)
}

ctgui_ctsem_call <- function(name, ..., .args = NULL) {
  caps <- ctgui_ctsem_capabilities()
  available <- isTRUE(caps$installed) && name %in% getNamespaceExports("ctsem")
  if (!available) stop("The loaded ctsem version does not provide ", name, call. = FALSE)
  args <- c(list(...), .args %||% list())
  do.call(getExportedValue("ctsem", name), args)
}

ctgui_ctsem_new_model <- function(args) ctgui_ctsem_call("ctModel", .args = args)

ctgui_ctsem_matrices <- function(model) ctgui_ctsem_call("ctModelMatrices", model)

ctgui_ctsem_fit_field <- function(fit, field, default = NULL) {
  if (is.null(fit) || !is.list(fit) || !field %in% names(fit)) return(default)
  fit[[field]] %||% default
}

ctgui_ctsem_dormant_t0var <- function(spec, build) {
  t0var <- spec$matrices[["T0VAR"]]
  saved <- if (is.null(t0var)) NULL else matrix(as.character(t0var),
    nrow = nrow(t0var), ncol = ncol(t0var), dimnames = dimnames(t0var))
  model <- build()
  if (!is.null(saved)) spec$matrices[["T0VAR"]] <- saved
  list(spec = spec, model = model)
}
