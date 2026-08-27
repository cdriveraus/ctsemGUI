# Surviving a crash ------------------------------------------------------------

# A model built by hand over an hour is the one thing in a session that cannot
# be reconstructed from anything else: data can be reloaded and a fit can be
# refitted, but the specification exists only in the running application. If
# the browser is closed or R falls over, it is gone.
#
# So the model, its history and the data it was built against are written to
# the package's cache directory whenever the model changes, and offered back at
# the start of the next session. Fits are deliberately not saved: they are
# large, slow to serialise, and recoverable by refitting.

ctgui_autosave_version <- 1L

# The size above which data is remembered by name rather than by value. A
# recovered model with a note about which data it wants is far more useful
# than an autosave that stalls the interface writing a hundred megabytes on
# every edit.
ctgui_autosave_data_limit <- 20 * 1024^2

ctgui_autosave_dir <- function() {
  tools::R_user_dir("ctsemGUI", which = "cache")
}

ctgui_autosave_path <- function(dir = ctgui_autosave_dir()) {
  file.path(dir, "autosave.rds")
}

ctgui_autosave_data_fits <- function(data) {
  if (is.null(data)) return(FALSE)
  isTRUE(as.numeric(utils::object.size(data)) <= ctgui_autosave_data_limit)
}

#' Write the recoverable part of a session
#'
#' @param spec The current specification.
#' @param history The model history, if any.
#' @param data The active data.
#' @param data_name How the active data was described.
#' @param path Where to write.
#' @return `TRUE` if something was written.
#' @keywords internal
ctgui_autosave_write <- function(spec, history = NULL, data = NULL,
    data_name = NULL, path = ctgui_autosave_path()) {
  if (is.null(spec)) return(FALSE)
  keeps_data <- ctgui_autosave_data_fits(data)
  state <- list(
    version = ctgui_autosave_version,
    saved_at = Sys.time(),
    session = Sys.getpid(),
    spec = spec,
    history = history,
    data = if (keeps_data) data else NULL,
    data_name = data_name,
    data_omitted = !is.null(data) && !keeps_data
  )
  # Autosave is a safety net. A cache directory that cannot be written to --
  # a locked-down profile, a full disk -- must not put a warning in the console
  # on every edit, and must never be the thing that stops a session working.
  written <- suppressWarnings(tryCatch({
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(state, path)
    TRUE
  }, error = function(e) FALSE))
  isTRUE(written)
}

ctgui_autosave_read <- function(path = ctgui_autosave_path()) {
  if (!file.exists(path)) return(NULL)
  state <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!is.list(state) || !identical(state$version, ctgui_autosave_version)) return(NULL)
  if (is.null(state$spec)) return(NULL)
  state
}

ctgui_autosave_clear <- function(path = ctgui_autosave_path()) {
  invisible(tryCatch(unlink(path), error = function(e) NULL))
}

# An autosave of an untouched starting model is not worth offering back: it
# holds nothing the user would miss, and offering it makes every session begin
# with a question.
ctgui_autosave_worth_offering <- function(state) {
  if (is.null(state)) return(FALSE)
  spec <- state$spec
  length(spec$latent_names) > 0L || length(spec$manifest_names) > 0L
}

# The autosave from this same R process is the one this session just wrote, so
# offering it back would be recovering from nothing.
ctgui_autosave_from_other_session <- function(state, pid = Sys.getpid()) {
  !is.null(state) && !identical(state$session, pid)
}

ctgui_autosave_describe <- function(state) {
  if (is.null(state)) return("")
  spec <- state$spec
  parts <- c(
    paste0(length(spec$latent_names), " latent process",
      if (length(spec$latent_names) == 1L) "" else "es"),
    paste0(length(spec$manifest_names), " manifest variable",
      if (length(spec$manifest_names) == 1L) "" else "s")
  )
  data_note <- if (!is.null(state$data)) {
    paste0("Data included: ", state$data_name %||% "unnamed", ".")
  } else if (isTRUE(state$data_omitted)) {
    paste0("The data (", state$data_name %||% "unnamed",
      ") was too large to keep, so it needs loading again.")
  } else {
    "No data was loaded."
  }
  paste0(
    "Saved ", format(state$saved_at, "%Y-%m-%d %H:%M"), ": ",
    paste(parts, collapse = ", "), ". ", data_note
  )
}
