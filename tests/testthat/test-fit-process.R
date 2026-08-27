ctgui_fit_log_overwrite <- getFromNamespace("ctgui_fit_log_overwrite", "ctsemGUI")
ctgui_fit_log_state <- getFromNamespace("ctgui_fit_log_state", "ctsemGUI")
ctgui_fit_log_append <- getFromNamespace("ctgui_fit_log_append", "ctsemGUI")
ctgui_fit_log_text <- getFromNamespace("ctgui_fit_log_text", "ctsemGUI")
ctgui_fit_log_warnings <- getFromNamespace("ctgui_fit_log_warnings", "ctsemGUI")
ctgui_fit_log_collapse <- getFromNamespace("ctgui_fit_log_collapse", "ctsemGUI")
ctgui_fit_log_read <- getFromNamespace("ctgui_fit_log_read", "ctsemGUI")
ctgui_fit_log_clean <- getFromNamespace("ctgui_fit_log_clean", "ctsemGUI")
ctgui_fit_log_tail <- getFromNamespace("ctgui_fit_log_tail", "ctsemGUI")
ctgui_fit_log_path <- getFromNamespace("ctgui_fit_log_path", "ctsemGUI")
ctgui_fit_process_alive <- getFromNamespace("ctgui_fit_process_alive", "ctsemGUI")
ctgui_fit_process_collect <- getFromNamespace("ctgui_fit_process_collect", "ctsemGUI")
ctgui_fit_process_start <- getFromNamespace("ctgui_fit_process_start", "ctsemGUI")

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
  expect_equal(ctgui_fit_log_clean("iter 1\riter 2\riter 3"), "iter 3")
  expect_equal(ctgui_fit_log_clean("line\r\nnext"), "line\nnext")
  expect_equal(ctgui_fit_log_clean(""), "")

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
  text <- paste(paste0("iteration ", 1:100), collapse = "\n")
  tailed <- ctgui_fit_log_tail(text, limit = 10L)

  expect_match(tailed, "iteration 100", fixed = TRUE)
  expect_false(grepl("iteration 1\n", tailed, fixed = TRUE))
  expect_match(tailed, "earlier lines omitted", fixed = TRUE)

  short <- paste(paste0("line ", 1:5), collapse = "\n")
  expect_equal(ctgui_fit_log_tail(short, limit = 10L), short)
})

test_that("log paths do not collide between fits", {
  expect_false(identical(ctgui_fit_log_path(), ctgui_fit_log_path()))
})

test_that("a dead or absent process is reported without erroring", {
  expect_false(ctgui_fit_process_alive(NULL))
  expect_equal(ctgui_fit_process_collect(NULL)$status, "error")
})

test_that("a stopped fit is not reported as a failure", {
  skip_if_not_installed("callr")

  path <- ctgui_fit_log_path()
  on.exit(unlink(path), add = TRUE)
  process <- callr::r_bg(function() Sys.sleep(30), stdout = path, stderr = "2>&1", supervise = TRUE)
  on.exit(try(process$kill(), silent = TRUE), add = TRUE)

  expect_true(ctgui_fit_process_alive(process))
  process$kill()
  Sys.sleep(0.5)

  # A failing fit and a cancelled one both exit non-zero, so the exit status
  # cannot tell them apart. Reporting a real failure as a cancellation would
  # hide the reason from the person who needs it.
  expect_equal(ctgui_fit_process_collect(process, cancelled = TRUE)$status, "cancelled")
})

test_that("a failure inside the fit comes back as an error, not a crash", {
  skip_if_not_installed("callr")

  path <- ctgui_fit_log_path()
  on.exit(unlink(path), add = TRUE)
  process <- callr::r_bg(
    function() stop("something went wrong in the fit"),
    stdout = path, stderr = "2>&1", supervise = TRUE
  )
  while (ctgui_fit_process_alive(process)) Sys.sleep(0.1)

  outcome <- ctgui_fit_process_collect(process, cancelled = FALSE)
  expect_equal(outcome$status, "error")
  expect_match(conditionMessage(outcome$error), "something went wrong in the fit", fixed = TRUE)
})

test_that("a value from the worker comes back intact", {
  skip_if_not_installed("callr")

  path <- ctgui_fit_log_path()
  on.exit(unlink(path), add = TRUE)
  process <- callr::r_bg(
    function() list(estimate = 42, label = "recovered"),
    stdout = path, stderr = "2>&1", supervise = TRUE
  )
  while (ctgui_fit_process_alive(process)) Sys.sleep(0.1)

  outcome <- ctgui_fit_process_collect(process)
  expect_equal(outcome$status, "value")
  expect_equal(outcome$value$estimate, 42)
})

test_that("the worker needs only ctsem, not ctsemGUI", {
  # The child process is started with package = FALSE, so anything the worker
  # reaches for has to be named explicitly. A reference to a ctsemGUI helper
  # would fail only at fit time, in a separate process, which is the worst
  # place to discover it.
  worker <- getFromNamespace("ctgui_fit_worker", "ctsemGUI")
  body_text <- paste(deparse(body(worker)), collapse = "\n")

  expect_match(body_text, "ctsem::ctFit", fixed = TRUE)
  expect_false(grepl("ctgui_", body_text, fixed = TRUE))
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
