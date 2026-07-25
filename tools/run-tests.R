log_file <- tempfile("ctsemgui-tests-", fileext = ".log")
log_connection <- file(log_file, open = "wt")
sink(log_connection, type = "output")
sink(log_connection, type = "message")

failure <- tryCatch(
  {
    testthat::test_local(
      ".",
      reporter = "summary",
      stop_on_failure = TRUE,
      stop_on_warning = TRUE
    )
    NULL
  },
  error = function(error) error
)

sink(type = "message")
sink(type = "output")
close(log_connection)

if (!is.null(failure)) {
  cat(readLines(log_file, warn = FALSE), sep = "\n")
  unlink(log_file)
  stop(conditionMessage(failure), call. = FALSE)
}

unlink(log_file)
