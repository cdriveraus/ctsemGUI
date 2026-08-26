# Fitting in a separate process ------------------------------------------------

# Fitting used to run on the main R thread, which froze the whole interface for
# as long as it took and left no way to stop it.  Running it in a separate
# process fixes both, and gives something the in-process version could not: a
# real message window.  ctsem and Stan write their progress to the console at a
# level R cannot capture from inside its own process, but a child process can
# have its output redirected to a file, which the application tails.
#
# The separate process is also the safe option.  A fit that crashes takes its
# own process down and nothing else, so the session, the model and any fits
# already stored survive it.

ctgui_fit_worker <- function(args) {
  # Runs in the child. Only ctsem is needed here, so the worker does not
  # depend on ctsemGUI being installed or loadable in the child process.
  do.call(ctsem::ctFit, args)
}

#' Start a fit in a background process
#'
#' @param args Arguments for `ctsem::ctFit()`.
#' @param log_path File that receives the fit's console output.
#' @return A `callr` process handle.
#' @keywords internal
ctgui_fit_process_start <- function(args, log_path) {
  if (!requireNamespace("callr", quietly = TRUE)) {
    stop(
      "Background fitting needs the callr package. Install it, or turn off ",
      "'Fit in the background' to fit in this session instead.",
      call. = FALSE
    )
  }
  file.create(log_path)
  callr::r_bg(
    func = ctgui_fit_worker,
    args = list(args = args),
    stdout = log_path,
    stderr = "2>&1",
    # Without this a fit outlives the session that started it and keeps a core
    # busy with results nobody can collect.
    supervise = TRUE,
    package = FALSE
  )
}

ctgui_fit_process_alive <- function(process) {
  !is.null(process) && isTRUE(tryCatch(process$is_alive(), error = function(e) FALSE))
}

# The log is tailed by byte offset rather than re-read whole, so a long fit does
# not re-send its entire output on every poll.
ctgui_fit_log_read <- function(log_path, offset = 0) {
  empty <- list(text = "", offset = offset)
  if (is.null(log_path) || !file.exists(log_path)) return(empty)
  size <- file.info(log_path)$size
  if (is.na(size) || size <= offset) return(empty)

  connection <- file(log_path, open = "rb")
  on.exit(close(connection), add = TRUE)
  if (offset > 0) seek(connection, where = offset, origin = "start")
  raw <- readBin(connection, what = "raw", n = size - offset)
  list(text = rawToChar(raw), offset = size)
}

# ctsem reports optimiser progress by overwriting one line with a carriage
# return. Left as-is that arrives as a single unreadable run of text, so each
# rewritten line becomes its own line here.
ctgui_fit_log_clean <- function(text) {
  if (!length(text) || !nzchar(text)) return("")
  text <- gsub("\r\n", "\n", text, fixed = TRUE)
  gsub("\r", "\n", text, fixed = TRUE)
}

# A long optimisation produces thousands of iteration lines, and the useful
# ones are the most recent. Keeping the tail bounds both the message the user
# reads and the memory the session holds.
ctgui_fit_log_limit <- 400L

ctgui_fit_log_tail <- function(text, limit = ctgui_fit_log_limit) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
  if (length(lines) <= limit) return(text)
  paste(c(
    paste0("... ", length(lines) - limit, " earlier lines omitted ..."),
    utils::tail(lines, limit)
  ), collapse = "\n")
}

#' Collect the outcome of a background fit
#'
#' @param process A handle from `ctgui_fit_process_start()`.
#' @param cancelled Whether the caller stopped the process deliberately.
#' @return A list with `status` (`"value"`, `"error"` or `"cancelled"`) and
#'   either the fitted object or the condition that ended it.
#' @keywords internal
ctgui_fit_process_collect <- function(process, cancelled = FALSE) {
  if (is.null(process)) {
    return(list(status = "error", error = simpleError("No fit was running.")))
  }
  # A failing fit and a cancelled one both exit non-zero, so the exit status
  # cannot tell them apart. Only the caller knows whether it pressed stop, and
  # reporting a real failure as a cancellation would hide the reason from the
  # person who needs it.
  if (isTRUE(cancelled)) {
    return(list(status = "cancelled", exit_status = tryCatch(
      as.integer(process$get_exit_status()), error = function(e) NA_integer_
    )))
  }
  result <- tryCatch(process$get_result(), error = function(e) e)
  if (inherits(result, "condition")) return(list(status = "error", error = result))
  list(status = "value", value = result)
}

ctgui_fit_log_path <- function(directory = tempdir()) {
  file.path(directory, paste0(
    "ctgui-fit-", Sys.getpid(), "-", as.integer(Sys.time()), "-",
    sample.int(1e6, 1L), ".log"
  ))
}
