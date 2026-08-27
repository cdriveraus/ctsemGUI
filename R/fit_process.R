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

ctgui_generate_worker <- function(args) {
  do.call(ctsem::ctGenerateFromFit, args)
}

#' Start a long ctsem call in a background process
#'
#' @param func The worker function to run in the child.
#' @param args Arguments passed to the worker.
#' @param log_path File that receives the child's console output.
#' @return A `callr` process handle.
#' @keywords internal
ctgui_background_start <- function(func, args, log_path) {
  if (!requireNamespace("callr", quietly = TRUE)) {
    stop(
      "Background work needs the callr package. Install it, or turn off ",
      "'Fit in the background' to run in this session instead.",
      call. = FALSE
    )
  }
  file.create(log_path)
  callr::r_bg(
    func = func,
    args = list(args = args),
    stdout = log_path,
    stderr = "2>&1",
    # Without this the child outlives the session that started it and keeps a
    # core busy with results nobody can collect.
    supervise = TRUE,
    package = FALSE
  )
}

ctgui_fit_process_start <- function(args, log_path) {
  ctgui_background_start(ctgui_fit_worker, args, log_path)
}

ctgui_generate_process_start <- function(args, log_path) {
  ctgui_background_start(ctgui_generate_worker, args, log_path)
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
# return, the way a terminal shows a counter ticking in place. Turning each
# rewrite into its own line reproduces exactly the flood the console avoids: a
# single fit rewrites its progress line a few hundred times.
#
# So the carriage returns are applied rather than expanded. A line is rebuilt
# by overwriting from its start, which is what a terminal does, and the reader
# sees one progress line holding its latest value.
ctgui_fit_log_overwrite <- function(line) {
  if (!grepl("\r", line, fixed = TRUE)) return(line)
  out <- ""
  for (segment in strsplit(line, "\r", fixed = TRUE)[[1L]]) {
    out <- if (nchar(segment) >= nchar(out)) {
      segment
    } else {
      paste0(segment, substring(out, nchar(segment) + 1L))
    }
  }
  out
}

ctgui_fit_log_clean <- function(text) {
  if (!length(text) || !nzchar(text)) return("")
  text <- gsub("\r\n", "\n", text, fixed = TRUE)
  lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
  paste(vapply(lines, ctgui_fit_log_overwrite, character(1L), USE.NAMES = FALSE), collapse = "\n")
}

# Cluster workers announce themselves and repeat their startup warnings once
# per worker per pass, which says the same thing many times over.
ctgui_fit_log_collapse <- function(lines) {
  if (length(lines) < 2L) return(lines)
  runs <- rle(lines)
  unlist(Map(function(value, count) {
    if (count > 1L) paste0(value, "   (x", count, ")") else value
  }, runs$values, runs$lengths), use.names = FALSE)
}

# A chunk read from the log can stop anywhere, including part way through a
# line that is still being overwritten. The incomplete tail is carried to the
# next read so the line is rendered once, finished, rather than once per poll.
ctgui_fit_log_state <- function(lines = character(), pending = "") {
  list(lines = lines, pending = pending)
}

ctgui_fit_log_append <- function(state, text, limit = ctgui_fit_log_limit) {
  if (!length(text) || !nzchar(text)) return(state)
  combined <- paste0(state$pending, gsub("\r\n", "\n", text, fixed = TRUE))
  parts <- strsplit(combined, "\n", fixed = TRUE)[[1L]]
  if (!length(parts)) return(state)

  ends_complete <- grepl("\n$", combined)
  pending <- if (ends_complete) "" else parts[length(parts)]
  complete <- if (ends_complete) parts else parts[-length(parts)]

  rendered <- vapply(complete, ctgui_fit_log_overwrite, character(1L), USE.NAMES = FALSE)
  lines <- ctgui_fit_log_collapse(c(state$lines, rendered))
  if (length(lines) > limit) lines <- utils::tail(lines, limit)
  ctgui_fit_log_state(lines, pending)
}

ctgui_fit_log_text <- function(state) {
  paste(c(state$lines, if (nzchar(state$pending)) ctgui_fit_log_overwrite(state$pending)),
    collapse = "\n")
}

# Warnings arrive interleaved with everything else on the child's output, so
# they are pulled out to the panel that exists for them. Leaving them only in
# the message stream means the box a user checks for them stays empty while
# the thing they need is buried in a few hundred lines.
ctgui_fit_log_warnings <- function(lines) {
  if (!length(lines)) return(character())
  collected <- character()
  index <- 1L
  while (index <= length(lines)) {
    line <- lines[index]
    if (grepl("^\\s*Warning in ", line) || grepl("^\\s*Warning:", line)) {
      collected <- c(collected, trimws(line))
    } else if (grepl("^\\s*Warning messages?:", line)) {
      # R prints the header on its own line and the warnings beneath it.
      index <- index + 1L
      while (index <= length(lines) && nzchar(trimws(lines[index]))) {
        collected <- c(collected, trimws(lines[index]))
        index <- index + 1L
      }
      next
    }
    index <- index + 1L
  }
  unique(collected)
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
