ctgui_fit_log_overwrite <- getFromNamespace("ctgui_fit_log_overwrite", "ctsemGUI")
ctgui_fit_log_state <- getFromNamespace("ctgui_fit_log_state", "ctsemGUI")
ctgui_fit_log_append <- getFromNamespace("ctgui_fit_log_append", "ctsemGUI")
ctgui_fit_log_text <- getFromNamespace("ctgui_fit_log_text", "ctsemGUI")
ctgui_fit_log_warnings <- getFromNamespace("ctgui_fit_log_warnings", "ctsemGUI")
ctgui_fit_log_collapse <- getFromNamespace("ctgui_fit_log_collapse", "ctsemGUI")
ctgui_fit_log_read <- getFromNamespace("ctgui_fit_log_read", "ctsemGUI")
ctgui_fit_log_path <- getFromNamespace("ctgui_fit_log_path", "ctsemGUI")
ctgui_worker <- getFromNamespace("ctgui_worker", "ctsemGUI")
ctgui_worker_close <- getFromNamespace("ctgui_worker_close", "ctsemGUI")
ctgui_worker_run <- getFromNamespace("ctgui_worker_run", "ctsemGUI")
ctgui_background_start <- getFromNamespace("ctgui_background_start", "ctsemGUI")
ctgui_job_running <- getFromNamespace("ctgui_job_running", "ctsemGUI")
ctgui_job_collect <- getFromNamespace("ctgui_job_collect", "ctsemGUI")
ctgui_job_cancel <- getFromNamespace("ctgui_job_cancel", "ctsemGUI")
ctgui_fit_log_lines <- getFromNamespace("ctgui_fit_log_lines", "ctsemGUI")

test_that("the log is read forward from an offset rather than re-read whole", {
  # A long fit writes thousands of lines. Re-reading the file on every poll
  # would resend all of them several times a second.
  path <- tempfile(fileext = ".log")
  on.exit(unlink(path), add = TRUE)
  writeLines(c("first", "second"), path)

  opening <- ctgui_fit_log_read(path, 0)
  expect_match(opening$text, "first", fixed = TRUE)
  expect_gt(opening$offset, 0)

  # Nothing new written yet.
  expect_equal(ctgui_fit_log_read(path, opening$offset)$text, "")

  cat("third\n", file = path, append = TRUE)
  continued <- ctgui_fit_log_read(path, opening$offset)
  expect_match(continued$text, "third", fixed = TRUE)
  expect_false(grepl("first", continued$text, fixed = TRUE))
})

test_that("a missing log reads as empty rather than failing", {
  missing <- ctgui_fit_log_read(tempfile(fileext = ".log"), 0)
  expect_equal(missing$text, "")
  expect_equal(ctgui_fit_log_read(NULL, 0)$text, "")
})

test_that("progress rewritten in place stays one line", {
  # ctsem reports optimiser progress by overwriting one line with a carriage
  # return, the way a terminal shows a counter ticking in place. Turning each
  # rewrite into its own line reproduces exactly the flood the console avoids:
  # a single fit rewrites its progress line a few hundred times.
  state <- ctgui_fit_log_append(ctgui_fit_log_state(), "iter 1\riter 2\riter 3\n")
  expect_equal(ctgui_fit_log_text(state), "iter 3")
  state <- ctgui_fit_log_append(ctgui_fit_log_state(), "line\r\nnext\n")
  expect_equal(ctgui_fit_log_text(state), "line\nnext")
  expect_equal(ctgui_fit_log_text(ctgui_fit_log_append(ctgui_fit_log_state(), "")), "")

  # A shorter rewrite leaves the tail of what it overwrote, as a terminal does.
  expect_equal(ctgui_fit_log_overwrite("abcdef\rXY"), "XYcdef")
  expect_equal(ctgui_fit_log_overwrite("plain"), "plain")
})

test_that("a progress line split across reads is rendered once, finished", {
  # A read of the log can stop anywhere, including part way through a line the
  # child is still overwriting.
  state <- ctgui_fit_log_state()
  state <- ctgui_fit_log_append(state, "prog 1\rprog 2")
  expect_equal(ctgui_fit_log_text(state), "prog 2")

  state <- ctgui_fit_log_append(state, "\rprog 3\ndone")
  expect_equal(ctgui_fit_log_text(state), "prog 3\ndone")

  state <- ctgui_fit_log_append(state, " line\nnext\n")
  expect_equal(ctgui_fit_log_text(state), "prog 3\ndone line\nnext")
})

test_that("repeated identical lines are counted rather than repeated", {
  # Cluster workers announce themselves and repeat their startup warnings once
  # per worker per pass, which says the same thing many times over.
  state <- ctgui_fit_log_append(
    ctgui_fit_log_state(), "worker up\nworker up\nworker up\nfitting\n"
  )
  expect_equal(ctgui_fit_log_text(state), "worker up   (x3)\nfitting")
})

test_that("warnings are pulled out of the output into their own panel", {
  # Left only in the message stream, the box a user checks for warnings stays
  # empty while the thing they need is buried in a few hundred lines.
  lines <- c(
    "fitting", "Warning message:", "Hessian required numerical repair", "",
    "more output", "Warning in ctFit(...) : something odd",
    "Warning messages:", "1: first thing", "2: second thing", "", "end"
  )
  warnings <- ctgui_fit_log_warnings(lines)

  expect_true("Hessian required numerical repair" %in% warnings)
  expect_true("Warning in ctFit(...) : something odd" %in% warnings)
  expect_true("1: first thing" %in% warnings)
  expect_false("fitting" %in% warnings)
  expect_false("end" %in% warnings)

  expect_equal(ctgui_fit_log_warnings(character()), character())
  expect_equal(ctgui_fit_log_warnings(c("all", "quiet")), character())
})

test_that("only the recent tail of a long log is kept", {
  text <- paste0(paste(paste0("iteration ", 1:100), collapse = "\n"), "\n")
  lines <- strsplit(ctgui_fit_log_text(
    ctgui_fit_log_append(ctgui_fit_log_state(), text, limit = 10L)), "\n")[[1L]]
  expect_length(lines, 10L)
  expect_equal(lines[10L], "iteration 100")

  short <- paste0(paste(paste0("line ", 1:5), collapse = "\n"), "\n")
  expect_equal(ctgui_fit_log_text(ctgui_fit_log_append(ctgui_fit_log_state(), short, limit = 10L)),
    paste(paste0("line ", 1:5), collapse = "\n"))
})

test_that("log paths do not collide between fits", {
  expect_false(identical(ctgui_fit_log_path(), ctgui_fit_log_path()))
})

test_that("a dead or absent job is reported without erroring", {
  expect_false(ctgui_job_running(NULL))
  expect_equal(ctgui_job_collect(NULL)$status, "error")
})

wait_for <- function(job) {
  deadline <- Sys.time() + 60
  while (ctgui_job_running(job) && Sys.time() < deadline) Sys.sleep(0.05)
  ctgui_job_collect(job)
}

test_that("jobs share one process, so its Julia and compiled shapes survive", {
  skip_if_not_installed("callr")

  # Each job used to start a process of its own, and the fit's process ended
  # when the fit returned, taking every model shape compiled in it along. A
  # generation straight afterwards compiled the same shape again.
  worker <- ctgui_worker()
  on.exit(ctgui_worker_close(worker), add = TRUE)
  first <- ctgui_background_start(worker, function(args) Sys.getpid(), list(), ctgui_fit_log_path())
  # Queued behind the first rather than started beside it.
  second <- ctgui_background_start(worker, function(args) Sys.getpid(), list(), ctgui_fit_log_path())
  expect_equal(second$state, "queued")

  expect_equal(wait_for(first)$status, "value")
  expect_equal(wait_for(second)$value, first$result$value)
  expect_false(identical(first$result$value, Sys.getpid()))
})

test_that("a job's output, progress and warnings reach its log as they happen", {
  skip_if_not_installed("callr")

  worker <- ctgui_worker()
  on.exit(ctgui_worker_close(worker), add = TRUE)
  path <- ctgui_fit_log_path()
  on.exit(unlink(path), add = TRUE)
  job <- ctgui_background_start(worker, function(args) {
    cat("printed\n")
    for (percent in 1:3) message("\r", percent, "%", appendLF = FALSE)
    message("")
    warning("careful")
    "done"
  }, list(), path)
  outcome <- wait_for(job)

  expect_equal(outcome$value, "done")
  expect_equal(outcome$warnings, "careful")
  lines <- strsplit(ctgui_fit_log_text(
    ctgui_fit_log_append(ctgui_fit_log_state(), ctgui_fit_log_read(path)$text)
  ), "\n", fixed = TRUE)[[1L]]
  expect_equal(lines, c("printed", "3%", "Warning: careful"))
  expect_equal(ctgui_fit_log_warnings(lines), "Warning: careful")
})

test_that("a stopped job is not reported as a failure, and the next starts afresh", {
  skip_if_not_installed("callr")

  worker <- ctgui_worker()
  on.exit(ctgui_worker_close(worker), add = TRUE)
  job <- ctgui_background_start(worker, function(args) Sys.sleep(30), list(), ctgui_fit_log_path())
  deadline <- Sys.time() + 30
  while (!identical(job$state, "running") && Sys.time() < deadline) {
    ctgui_job_running(job)
    Sys.sleep(0.05)
  }
  stopped_pid <- job$session$get_pid()

  # A failing fit and a cancelled one both end without a value, so only the
  # caller can tell them apart. Reporting a real failure as a cancellation
  # would hide the reason from the person who needs it.
  ctgui_job_cancel(job)
  expect_equal(ctgui_job_collect(job)$status, "cancelled")

  after <- wait_for(ctgui_background_start(worker, function(args) Sys.getpid(), list(), ctgui_fit_log_path()))
  expect_equal(after$status, "value")
  expect_false(identical(after$value, stopped_pid))
})

test_that("a failure inside the job comes back as an error, not a crash", {
  skip_if_not_installed("callr")

  worker <- ctgui_worker()
  on.exit(ctgui_worker_close(worker), add = TRUE)
  outcome <- wait_for(ctgui_background_start(
    worker, function(args) stop("something went wrong in the fit"), list(), ctgui_fit_log_path()
  ))
  expect_equal(outcome$status, "error")
  expect_match(conditionMessage(outcome$error), "something went wrong in the fit", fixed = TRUE)
})

test_that("a value from the worker comes back intact", {
  skip_if_not_installed("callr")

  worker <- ctgui_worker()
  on.exit(ctgui_worker_close(worker), add = TRUE)
  outcome <- wait_for(ctgui_background_start(
    worker, function(args) list(estimate = args$x * 21, label = "recovered"), list(x = 2),
    ctgui_fit_log_path()
  ))
  expect_equal(outcome$status, "value")
  expect_equal(outcome$value$estimate, 42)
})

test_that("a call on a fit runs in the worker when free, and here when not", {
  skip_if_not_installed("callr")
  skip_if_not_installed("ctsem")

  worker <- ctgui_worker()
  on.exit(ctgui_worker_close(worker), add = TRUE)
  args <- list(type = "ct", LAMBDA = diag(1), Tpoints = 3)
  there <- ctgui_worker_run(worker, "ctModel", args)
  expect_s3_class(there$value, "ctStanModel")
  expect_false(is.null(worker$session))

  # While a job has the process the call does not wait for it.
  busy <- ctgui_background_start(worker, function(args) Sys.sleep(30), list(), ctgui_fit_log_path())
  started <- Sys.time()
  here <- ctgui_worker_run(worker, "ctModel", args)
  expect_lt(as.numeric(difftime(Sys.time(), started, units = "secs")), 20)
  expect_s3_class(here$value, "ctStanModel")
  ctgui_job_cancel(busy)
})

test_that("the workers need only ctsem, not ctsemGUI", {
  # The child process cannot be assumed to have ctsemGUI, so anything a worker
  # reaches for has to be named explicitly. A reference to a ctsemGUI helper
  # would fail only at fit time, in a separate process, which is the worst
  # place to discover it.
  fit_worker <- getFromNamespace("ctgui_fit_worker", "ctsemGUI")
  expect_match(paste(deparse(body(fit_worker)), collapse = "\n"), "ctsem::ctFit", fixed = TRUE)
  for (name in c("ctgui_fit_worker", "ctgui_generate_worker", "ctgui_ctsem_worker", "ctgui_worker_job")) {
    body_text <- paste(deparse(body(getFromNamespace(name, "ctsemGUI"))), collapse = "\n")
    expect_false(grepl("ctgui_", body_text, fixed = TRUE), info = name)
  }
})

test_that("the fit form stays usable while a background fit runs", {
  javascript <- paste(
    readLines(ctgui_test_asset_path("www", "app", "app.js"), warn = FALSE),
    collapse = "\n"
  )

  # Disabling the whole interface during a fit would give up most of the
  # benefit of running it elsewhere.
  expect_false(grepl(
    'app.find("input, select, textarea, button").not("#run_fit").prop("disabled", true)',
    javascript, fixed = TRUE
  ))
  expect_match(javascript, "#cancel_fit", fixed = TRUE)
})
