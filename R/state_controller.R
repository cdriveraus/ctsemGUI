# Canonical specification commit path ---------------------------------------

#' Commit an internally edited specification
#'
#' This internal boundary is the only place an interactive editing module
#' should normalise matrices, metadata and PARS before invalidating a fit.
#' It returns both the canonical specification and the effects the caller must
#' apply to reactive fit/widget/visual state.
ctgui_commit_spec <- function(previous, updated = previous, reason = "edit",
    sync_model = TRUE, refresh_visual = NULL, refresh_widgets = TRUE) {
  ctgui_check_spec(previous)
  ctgui_check_spec(updated)
  previous_keys <- ctgui_metadata_keys(previous$parameter_metadata)
  previous_semantics <- ctgui_spec_semantic_snapshot(previous)

  updated$matrices <- ctgui_normalize_matrices(updated)
  updated <- ctgui_refresh_parameter_metadata(updated)
  new_keys <- setdiff(ctgui_metadata_keys(updated$parameter_metadata), previous_keys)
  if (isTRUE(sync_model)) {
    updated <- ctgui_sync_model_from_matrices(updated, ctsem_default_keys = new_keys,
      normalize = FALSE)
  }

  changed <- !identical(previous_semantics, ctgui_spec_semantic_snapshot(updated))
  if (is.null(refresh_visual)) refresh_visual <- changed
  structure(list(
    spec = updated,
    effects = list(
      reason = reason,
      changed = changed,
      invalidate_fit = changed,
      refresh_matrices = changed,
      refresh_visual = isTRUE(refresh_visual),
      refresh_widgets = isTRUE(refresh_widgets),
      new_metadata_keys = new_keys
    )
  ), class = "ctgui_spec_commit")
}

# Cached ctsem objects are deliberately excluded: rebuilding a model can alter
# implementation details without changing the model the user authored. Every
# remaining field can change fitting, data mapping, parameter interpretation,
# or a persisted visual specification and therefore participates in commit
# invalidation.
ctgui_spec_semantic_snapshot <- function(spec) {
  ctgui_check_spec(spec)
  fields <- c(
    "version", "type", "id", "time", "Tpoints", "latent_names", "manifest_names",
    "manifest_type", "tdpred_names", "tipred_names", "tipredDefault", "matrices",
    "matrix_extra_pars", "parameter_metadata", "visual"
  )
  spec[intersect(fields, names(spec))]
}

ctgui_commit_result <- function(commit) {
  if (!inherits(commit, "ctgui_spec_commit")) stop("commit must be a ctgui_spec_commit", call. = FALSE)
  commit$spec
}
