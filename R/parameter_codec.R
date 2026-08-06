# Parameter annotation codec -------------------------------------------------
#
# ctsem's compact matrix syntax has five fields.  Keeping the parser and
# serializer together prevents one editing path from silently dropping the
# metadata created by another.

ctgui_parameter_annotation_decode <- function(value, tipred_names = character()) {
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
    scale_text <- tolower(trimws(parts[4L]))
    scale <- suppressWarnings(as.numeric(scale_text))
    if (identical(scale_text, "true")) scale <- 1
    if (identical(scale_text, "false")) scale <- 0
    if (!is.na(scale)) out$sdscale <- scale else if (length(parts) == 4L) {
      # Pre-five-field labels used field four for TI predictors.
      out$tipreds <- intersect(trimws(unlist(strsplit(parts[4L], ",", fixed = TRUE))), tipred_names)
    }
  }
  if (length(parts) >= 5L) {
    out$tipreds <- intersect(trimws(unlist(strsplit(parts[5L], ",", fixed = TRUE))), tipred_names)
  }
  out
}

ctgui_parameter_annotation_base <- function(value) {
  trimws(strsplit(as.character(value)[1L], "|", fixed = TRUE)[[1L]][1L])
}

# ctsem only permits compact `|` annotations for a single free parameter.
# Expressions (including references to a named latent process) must carry
# their own transform and have any individual-difference metadata attached to
# the separate PARS elements instead.
ctgui_parameter_is_expression <- function(value, latent_names = character()) {
  base <- ctgui_parameter_annotation_base(value)
  if (!nzchar(base) || !is.na(suppressWarnings(as.numeric(base)))) return(FALSE)
  !grepl("^[A-Za-z._][A-Za-z0-9._]*$", base) || base %in% latent_names
}

ctgui_spec_has_expressions <- function(spec) {
  matrices <- spec$matrices[setdiff(names(spec$matrices), "PARS")]
  any(vapply(matrices, function(matrix) {
    any(vapply(as.vector(matrix), ctgui_parameter_is_expression,
      logical(1L), latent_names = spec$latent_names))
  }, logical(1L)))
}

ctgui_expression_metadata_guidance <- function() {
  paste(
    "If this value is an expression (for example, involving latent states,",
    "time-dependent predictors, multiple free parameters, or mathematical",
    "operations), the other metadata settings are ignored. Set any needed",
    "metadata on its individual free parameter elements below instead."
  )
}

ctgui_parameter_annotation_encode <- function(param, transform = "",
    indvarying = FALSE, sdscale = 1, tipreds = character()) {
  param <- trimws(as.character(param)[1L])
  if (!nzchar(param) || !is.na(suppressWarnings(as.numeric(param)))) return(param)
  transform <- trimws(as.character(transform %||% "")[1L])
  scale <- suppressWarnings(as.numeric(sdscale)[1L])
  if (is.na(scale)) scale <- if (isTRUE(sdscale)) 1 else 0
  tipreds <- ctgui_split_pars(tipreds)
  # An omitted individual-differences field asks ctsem to use that matrix's
  # default, which can be TRUE.  Keep FALSE explicit so disabling random
  # effects in either editor is preserved when the model is rebuilt.
  suffix <- c(transform, if (isTRUE(indvarying)) "TRUE" else "FALSE", if (identical(scale, 1)) "" else as.character(scale))
  if (length(tipreds)) suffix <- c(suffix, paste(tipreds, collapse = ","))
  while (length(suffix) && !nzchar(suffix[length(suffix)])) suffix <- suffix[-length(suffix)]
  if (!length(suffix)) return(param)
  paste(c(param, suffix), collapse = "|")
}
