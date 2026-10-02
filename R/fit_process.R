# Fitting in a separate process ------------------------------------------------

# Fitting used to run on the main R thread, which froze the whole interface for
# as long as it took and left no way to stop it.  Running it in a separate
# process fixes both, and gives a real message window: the job writes its
# output to a file, which the application tails.
#
# The separate process is also the safe option.  A fit that crashes takes its
# own process down and nothing else, so the session, the model and any fits
# already stored survive it.
#
# One process serves the whole session.  Each job used to start an R process of
# its own, and with it a Julia of its own; a fit's process ended when the fit
# returned, taking the engine and every model shape it had compiled with it, so
# generating from that fit a moment later started Julia again and compiled the
# same shape again.  Jobs now run one after another in a process that lives as
# long as the session.  Stopping a job still ends the process -- R cannot
# interrupt a call running in another process on Windows -- and the next job
# starts a fresh one.

ctgui_fit_worker <- function(args) {
  # Runs in the child. Only ctsem is needed here, so the worker does not
  # depend on ctsemGUI being installed or loadable in the child process.
  do.call(ctsem::ctFit, args)
}

ctgui_generate_worker <- function(args) {
  do.call(ctsem::ctGenerateFromFit, args)
}

# Any other ctsem function, by name, for the calls the session makes on a fit.
ctgui_ctsem_worker <- function(args) {
  do.call(getExportedValue("ctsem", args$name), args$args)
}

# Runs in the child around every job. Messages and warnings are written to the
# job's log as they happen, so the application can show them live; ctsem's
# progress lines keep their carriage returns, which the log renderer applies.
# Warnings are also returned, so the panel for them is filled from the
# conditions themselves rather than from a reading of the log. Self-contained,
# like the workers: the child has ctsem, not ctsemGUI.
ctgui_worker_job <- function(func, args, log_path) {
  connection <- file(log_path, open = "a")
  sink(connection)
  on.exit({
    sink()
    close(connection)
  }, add = TRUE)
  write <- function(...) {
    cat(..., sep = "", file = connection)
    flush(connection)
  }
  # A fit asking for more cores than the session's Julia was started with
  # restarts it at the wider count, which is what a process per fit gave.
  options(ctsem.julia.restart = TRUE)
  warnings <- character()
  value <- withCallingHandlers(
    tryCatch(func(args), error = function(error) error),
    message = function(condition) {
      write(conditionMessage(condition))
      invokeRestart("muffleMessage")
    },
    warning = function(condition) {
      warnings <<- c(warnings, conditionMessage(condition))
      write("Warning: ", conditionMessage(condition), "\n")
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = unique(warnings))
}

#' The session's background R process
#'
#' A holder for the process, created on first use and replaced when it has
#' gone, so a job can find the current one.
#'
#' @return An environment.
#' @keywords internal
ctgui_worker <- function() {
  worker <- new.env(parent = emptyenv())
  worker$session <- NULL
  worker$job <- NULL
  worker
}

ctgui_worker_session <- function(worker) {
  session <- worker$session
  if (is.null(session) || !isTRUE(tryCatch(session$is_alive(), error = function(e) FALSE))) {
    if (!requireNamespace("callr", quietly = TRUE)) {
      stop(
        "Background work needs the callr package. Install it, or turn off ",
        "'Fit in the background' to run in this session instead.",
        call. = FALSE
      )
    }
    # Supervised: without it the process outlives a session that dies, and
    # keeps a core busy with results nobody can collect.
    session <- callr::r_session$new(
      options = callr::r_session_options(supervise = TRUE),
      wait = FALSE
    )
    worker$session <- session
    worker$job <- NULL
  }
  session
}

# Whether the process can take a job now. It starts asynchronously, and its
# start-up message has to be read before the first call.
ctgui_worker_idle <- function(worker) {
  if (!is.null(worker$job)) return(FALSE)
  session <- ctgui_worker_session(worker)
  if (identical(session$get_state(), "starting") &&
      identical(session$poll_process(0), "ready")) {
    session$read()
  }
  identical(session$get_state(), "idle")
}

ctgui_worker_close <- function(worker) {
  session <- worker$session
  worker$session <- NULL
  worker$job <- NULL
  if (!is.null(session)) invisible(tryCatch(session$kill_tree(), error = function(e) NULL))
  invisible(NULL)
}

#' Queue a long ctsem call on the session's background process
#'
#' The job starts as soon as the process is free, which is at once unless
#' another job is running; `ctgui_job_running()` starts it.
#'
#' @param worker From `ctgui_worker()`.
#' @param func The worker function to run in the child.
#' @param args Arguments passed to the worker.
#' @param log_path File that receives the job's output.
#' @return A job: an environment that `ctgui_job_running()` advances.
#' @keywords internal
ctgui_background_start <- function(worker, func, args, log_path) {
  if (!requireNamespace("callr", quietly = TRUE)) {
    stop(
      "Background work needs the callr package. Install it, or turn off ",
      "'Fit in the background' to run in this session instead.",
      call. = FALSE
    )
  }
  file.create(log_path)
  # The function travels to the child without its namespace, which the child
  # may not be able to load; the workers name everything they use.
  environment(func) <- globalenv()
  job <- new.env(parent = emptyenv())
  job$worker <- worker
  job$func <- func
  job$args <- args
  job$log_path <- log_path
  job$state <- "queued"
  job$result <- NULL
  ctgui_job_running(job)
  job
}

ctgui_fit_process_start <- function(worker, args, log_path) {
  ctgui_background_start(worker, ctgui_fit_worker, args, log_path)
}

ctgui_generate_process_start <- function(worker, args, log_path) {
  ctgui_background_start(worker, ctgui_generate_worker, args, log_path)
}

ctgui_job_finish <- function(job, result) {
  job$state <- "done"
  job$result <- result
  if (identical(job$worker$job, job)) job$worker$job <- NULL
  FALSE
}

#' Advance a background job
#'
#' Starts a queued job once the process is free, and collects a running one
#' once it has finished.
#'
#' @param job From `ctgui_background_start()`.
#' @return `TRUE` while the job is queued or running.
#' @keywords internal
ctgui_job_running <- function(job) {
  if (is.null(job) || identical(job$state, "done")) return(FALSE)
  worker <- job$worker

  if (identical(job$state, "queued")) {
    if (!ctgui_worker_idle(worker)) return(TRUE)
    worker$session$call(ctgui_worker_job,
      list(func = job$func, args = job$args, log_path = job$log_path))
    worker$job <- job
    job$session <- worker$session
    job$state <- "running"
    return(TRUE)
  }

  session <- job$session
  if (!isTRUE(tryCatch(session$is_alive(), error = function(e) FALSE))) {
    return(ctgui_job_finish(job, list(status = "error",
      error = simpleError("The background R process ended before the job finished."))))
  }
  if (!identical(session$poll_process(0), "ready")) return(TRUE)
  reply <- session$read()
  if (!is.null(reply$error)) {
    return(ctgui_job_finish(job, list(status = "error", error = reply$error)))
  }
  value <- reply$result$value
  warnings <- reply$result$warnings %||% character()
  if (inherits(value, "error")) {
    return(ctgui_job_finish(job, list(status = "error", error = value, warnings = warnings)))
  }
  ctgui_job_finish(job, list(status = "value", value = value, warnings = warnings))
}

# Stopping a running job ends the process, since there is no other way to stop
# it here, and takes its Julia with it.
ctgui_job_cancel <- function(job) {
  if (is.null(job) || identical(job$state, "done")) return(invisible(FALSE))
  if (identical(job$state, "running")) ctgui_worker_close(job$worker)
  ctgui_job_finish(job, list(status = "cancelled"))
  invisible(TRUE)
}

#' Collect the outcome of a background job
#'
#' @param job From `ctgui_background_start()`.
#' @return A list with `status` (`"value"`, `"error"` or `"cancelled"`), the
#'   returned object or the condition that ended the job, and its warnings.
#' @keywords internal
ctgui_job_collect <- function(job) {
  if (is.null(job)) {
    return(list(status = "error", error = simpleError("No job was running.")))
  }
  ctgui_job_running(job)
  if (!identical(job$state, "done")) {
    return(list(status = "error", error = simpleError("The job has not finished.")))
  }
  job$result
}

#' Run a ctsem function on a fit, in the background process when it is free
#'
#' Blocks until it returns, like a call in this session, but uses the process
#' that fitted the model: its Julia has the model's shape compiled already,
#' where this session's would compile it again. While a job has the process,
#' the call runs here instead rather than wait for the job.
#'
#' @param worker From `ctgui_worker()`, or `NULL` to run here.
#' @param name A ctsem export.
#' @param args Its arguments.
#' @return A list with `value` (or the error), `messages` and `warnings`, as
#'   from `ctgui_run_result()`.
#' @keywords internal
ctgui_worker_run <- function(worker, name, args) {
  here <- function() ctgui_run_result(function() ctgui_ctsem_call(name, .args = args))
  if (is.null(worker) || !requireNamespace("callr", quietly = TRUE)) return(here())
  idle <- tryCatch(ctgui_worker_idle(worker), error = function(e) FALSE)
  if (!idle && is.null(worker$job) && !is.null(worker$session)) {
    # A process still starting is ready within a second or two.
    worker$session$poll_process(5000)
    idle <- tryCatch(ctgui_worker_idle(worker), error = function(e) FALSE)
  }
  if (!idle) return(here())

  log_path <- ctgui_fit_log_path()
  on.exit(unlink(log_path), add = TRUE)
  job <- ctgui_background_start(worker, ctgui_ctsem_worker,
    list(name = name, args = args), log_path)
  while (ctgui_job_running(job)) {
    if (is.null(job$session)) Sys.sleep(0.05) else job$session$poll_process(200)
  }
  outcome <- job$result
  log <- ctgui_fit_log_append(ctgui_fit_log_state(), ctgui_fit_log_read(log_path)$text)
  lines <- ctgui_fit_log_lines(log)
  # Warnings come back as conditions; their copies in the log are for watching.
  lines <- lines[nzchar(trimws(lines)) & !grepl("^Warning: ", lines)]
  list(
    value = if (identical(outcome$status, "value")) outcome$value else outcome$error,
    messages = lines,
    warnings = outcome$warnings %||% character()
  )
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

ctgui_fit_log_lines <- function(state) {
  c(state$lines, if (nzchar(state$pending)) ctgui_fit_log_overwrite(state$pending))
}

ctgui_fit_log_text <- function(state) {
  paste(ctgui_fit_log_lines(state), collapse = "\n")
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

ctgui_fit_log_path <- function(directory = tempdir()) {
  file.path(directory, paste0(
    "ctgui-fit-", Sys.getpid(), "-", as.integer(Sys.time()), "-",
    sample.int(1e6, 1L), ".log"
  ))
}
