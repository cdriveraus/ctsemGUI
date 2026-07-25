# Canonical matrix catalogue -------------------------------------------------
#
# ctsem accepts a growing set of named matrices.  The GUI only supports a
# deliberate subset, so keep its ordering, shape and visual ownership in one
# place.  This is intentionally internal: callers should edit a spec through
# ctgui_commit_spec() rather than depending on this representation.

ctgui_matrix_schema <- function(spec) {
  ctgui_check_spec(spec)
  latent <- spec$latent_names
  manifest <- spec$manifest_names
  td <- spec$tdpred_names
  td_rows <- if (length(td) && !is.null(spec$Tpoints)) {
    paste0(rep(td, times = spec$Tpoints), "_T", rep(seq_len(spec$Tpoints), each = length(td)))
  } else character()

  entry <- function(order, rows, cols, triangular = "none", visual = NULL,
                    default = NULL) {
    list(order = order, dims = c(length(rows), length(cols)),
      dimnames = list(rows, cols), triangular = triangular,
      visual = visual, default = default)
  }

  list(
    LAMBDA = entry(10L, manifest, latent, visual = "state_space",
      default = function() ctgui_default_lambda(manifest, latent)),
    T0VAR = entry(20L, latent, latent, triangular = "lower",
      visual = "initial_state", default = function() ctgui_lower_label_matrix("T0VAR", latent)),
    T0MEANS = entry(30L, latent, "T0MEANS", visual = "initial_state",
      default = function() ctgui_label_matrix("T0MEANS", latent, "mean", ncol = 1L)),
    MANIFESTMEANS = entry(40L, manifest, "MANIFESTMEANS", visual = "state_space",
      default = function() ctgui_label_matrix("MANIFESTMEANS", manifest, "mean", ncol = 1L)),
    MANIFESTVAR = entry(50L, manifest, manifest, triangular = "diagonal",
      visual = "state_space", default = function() ctgui_diag_label_matrix("MANIFESTVAR", manifest)),
    DRIFT = entry(60L, latent, latent, visual = "state_space",
      default = function() ctgui_label_matrix("DRIFT", latent, latent)),
    CINT = entry(70L, latent, "CINT", visual = "state_space",
      default = function() ctgui_fixed_matrix(0, latent, "cint", ncol = 1L)),
    DIFFUSION = entry(80L, latent, latent, triangular = "lower",
      visual = "state_space", default = function() ctgui_lower_label_matrix("DIFFUSION", latent)),
    TDPREDEFFECT = entry(90L, latent, td, visual = "state_space",
      default = function() ctgui_label_matrix("TDPREDEFFECT", latent, td)),
    TDPREDMEANS = entry(100L, td_rows, "TDPREDMEANS", visual = NULL,
      default = function() ctgui_label_matrix("TDPREDMEANS", td_rows, "mean", ncol = 1L)),
    TDPREDVAR = entry(110L, td_rows, td_rows, triangular = "lower", visual = NULL,
      default = function() ctgui_lower_label_matrix("TDPREDVAR", td_rows)),
    PARS = entry(120L, character(), "PARS", visual = "tipred_effects", default = NULL)
  )
}

ctgui_matrix_schema_entry <- function(spec, matrix_name) {
  schema <- ctgui_matrix_schema(spec)
  schema[[matrix_name]]
}

ctgui_matrix_schema_dims <- function(spec) {
  schema <- ctgui_matrix_schema(spec)
  keep <- c(ctgui_required_matrices)
  if (length(spec$tdpred_names)) keep <- c(keep, "TDPREDEFFECT")
  if (length(spec$tdpred_names) && !is.null(spec$Tpoints)) {
    keep <- c(keep, "TDPREDMEANS", "TDPREDVAR")
  }
  lapply(schema[keep], `[[`, "dims")
}

ctgui_matrix_schema_visual <- function(spec, view = NULL) {
  schema <- ctgui_matrix_schema(spec)
  if (is.null(view)) return(vapply(schema, function(x) x$visual %||% "", character(1L)))
  names(Filter(function(x) identical(x$visual, view), schema))
}

ctgui_normalize_matrices <- function(spec, matrices = spec$matrices) {
  ctgui_check_spec(spec)
  schema <- ctgui_matrix_schema(spec)
  matrices <- matrices[!vapply(matrices, is.null, logical(1L))]
  for (name in intersect(names(matrices), names(schema))) {
    mat <- matrices[[name]]
    entry <- schema[[name]]
    if (!is.matrix(mat) || !identical(dim(mat), entry$dims)) next
    # PARS row labels are stable identifiers for its values, not model names.
    if (identical(name, "PARS")) next
    dimnames(mat) <- entry$dimnames
    matrices[[name]] <- mat
  }
  ctgui_order_matrices(matrices)
}
