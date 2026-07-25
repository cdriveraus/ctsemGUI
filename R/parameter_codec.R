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

ctgui_parameter_annotation_encode <- function(param, transform = "",
    indvarying = FALSE, sdscale = 1, tipreds = character()) {
  param <- trimws(as.character(param)[1L])
  if (!nzchar(param) || !is.na(suppressWarnings(as.numeric(param)))) return(param)
  transform <- trimws(as.character(transform %||% "")[1L])
  scale <- suppressWarnings(as.numeric(sdscale)[1L])
  if (is.na(scale)) scale <- if (isTRUE(sdscale)) 1 else 0
  tipreds <- ctgui_split_pars(tipreds)
  suffix <- c(transform, if (isTRUE(indvarying)) "TRUE" else "", if (identical(scale, 1)) "" else as.character(scale))
  if (length(tipreds)) suffix <- c(suffix, paste(tipreds, collapse = ","))
  while (length(suffix) && !nzchar(suffix[length(suffix)])) suffix <- suffix[-length(suffix)]
  if (!length(suffix)) return(param)
  paste(c(param, suffix), collapse = "|")
}
