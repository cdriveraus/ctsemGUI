ctgui_report_document <- getFromNamespace("ctgui_report_document", "ctsemGUI")
ctgui_report_warnings <- getFromNamespace("ctgui_report_warnings", "ctsemGUI")
ctgui_report_readings <- getFromNamespace("ctgui_report_readings", "ctsemGUI")
ctgui_output_snippet <- getFromNamespace("ctgui_output_snippet", "ctsemGUI")

quiet_report <- function(code) suppressWarnings(suppressMessages(force(code)))

report_spec <- function() {
  quiet_report(ctgui_spec(
    latent_names = c("stress", "sleep"),
    manifest_names = c("stress_1", "sleep_1")
  ))
}

report_snippets <- function() {
  list(
    residual_acf = paste(ctgui_output_snippet("residual_acf", list(nboot = 100)), collapse = "\n"),
    fit = paste(ctgui_output_snippet("fit", list(optimize = TRUE, cores = 2)), collapse = "\n"),
    summary = paste(ctgui_output_snippet("summary"), collapse = "\n")
  )
}

# Chunks are extracted the way a renderer would find them, so a malformed
# document shows up here rather than when someone tries to render it.
report_chunks <- function(doc) {
  starts <- which(doc == "```{r}")
  lapply(starts, function(start) {
    offset <- which(doc[seq(start + 1L, length(doc))] == "```")[1L]
    doc[seq(start + 1L, start + offset - 1L)]
  })
}

test_that("the report is a well-formed Quarto document", {
  doc <- ctgui_report_document(report_spec(), snippets = report_snippets())

  expect_equal(doc[[1L]], "---")
  expect_true(any(grepl("^title:", doc)))
  expect_true(any(doc == "format: html"))
  # Front matter has to close before the body starts.
  expect_equal(which(doc == "---")[2L], 7L)

  # Every opened chunk is closed.
  expect_equal(sum(doc == "```{r}") * 2L, sum(doc %in% c("```{r}", "```")))
})

test_that("every code chunk in the report is valid R", {
  doc <- ctgui_report_document(report_spec(), snippets = report_snippets())
  chunks <- report_chunks(doc)

  expect_gt(length(chunks), 0L)
  for (chunk in chunks) {
    expect_no_error(parse(text = paste(chunk, collapse = "\n")))
  }
})

test_that("the model chunk builds a model on its own", {
  skip_if_not_installed("ctsem")

  # The point of the report is that running it reproduces the analysis rather
  # than describing it, so the first chunk has to stand alone.
  doc <- ctgui_report_document(report_spec(), snippets = report_snippets())
  chunk <- report_chunks(doc)[[1L]]

  env <- new.env(parent = globalenv())
  quiet_report(eval(parse(text = paste(chunk, collapse = "\n")), envir = env))
  expect_true(exists("model", envir = env, inherits = FALSE))
  expect_equal(get("model", envir = env)$latentNames, c("stress", "sleep"))
})

test_that("sections are ordered as a write-up, not as they were clicked", {
  doc <- ctgui_report_document(report_spec(), snippets = report_snippets())
  headings <- grep("^## ", doc, value = TRUE)

  fitting <- which(headings == "## Fitting")
  summary <- which(headings == "## Fit summary")
  acf <- which(headings == "## Residual autocorrelation")

  # The snippets were supplied acf-first; the report puts fitting before its
  # summary, and diagnostics after both.
  expect_lt(fitting, summary)
  expect_lt(summary, acf)
})

test_that("a model with nothing run yet reports the specification only", {
  doc <- ctgui_report_document(report_spec())

  expect_match(paste(doc, collapse = "\n"), "No fit or diagnostics have been run", fixed = TRUE)
  expect_false(any(grepl("^## Fitting", doc)))
})

test_that("equations reach the report as math, not as an image", {
  skip_if_not_installed("ctsem")

  spec <- report_spec()
  doc <- ctgui_report_document(
    spec, snippets = report_snippets(), latex = quiet_report(ctgui_latex(spec))
  )

  expect_gt(sum(doc == "$$"), 0L)
  expect_equal(sum(doc == "$$") %% 2L, 0L)
  expect_false(any(grepl("<img", doc, fixed = TRUE)))
})

test_that("warnings are reported with their guidance, and silence is not", {
  hessian <- "Hessian covariance from Hessian required numerical repair: nearPD applied"
  lines <- ctgui_report_warnings(hessian)

  expect_match(paste(lines, collapse = "\n"), "Warnings from the fit", fixed = TRUE)
  expect_match(paste(lines, collapse = "\n"), "usually harmless", fixed = TRUE)

  # A fit that warned about nothing gets no section rather than an empty one.
  expect_equal(ctgui_report_warnings("No warnings."), character())
  expect_equal(ctgui_report_warnings(character()), character())
  expect_equal(ctgui_report_warnings(""), character())
})

test_that("readings appear only when there is a fit to read", {
  notes <- list(
    list(text = "A disturbance to stress halves in 2 units.", kind = "reading"),
    list(text = "The auto-effect is not negative.", kind = "caution")
  )

  lines <- ctgui_report_readings(notes)
  expect_match(paste(lines, collapse = "\n"), "halves in 2 units", fixed = TRUE)
  # A caution has to look different from a reading in the rendered document.
  expect_match(paste(lines, collapse = "\n"), "**Worth checking.**", fixed = TRUE)

  expect_equal(ctgui_report_readings(list()), character())

  without_fit <- ctgui_report_document(report_spec(), snippets = report_snippets(),
    notes = notes, has_fit = FALSE)
  expect_false(any(grepl("^## Reading", without_fit)))
})

test_that("the report says when and with what it was made", {
  doc <- ctgui_report_document(
    report_spec(), snippets = report_snippets(),
    generated = as.POSIXct("2026-08-27 09:30:00", tz = "UTC")
  )
  text <- paste(doc, collapse = "\n")

  expect_match(text, "2026-08-27", fixed = TRUE)
  expect_match(text, "ctsemGUI", fixed = TRUE)
})

test_that("the report is offered as source rather than rendered output", {
  # Rendering would need a Quarto or pandoc installation, which is exactly the
  # dependency the browser-rendered equations removed.
  source <- paste(
    readLines(ctgui_test_source_path("R", "app_server.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(source, "output$download_report", fixed = TRUE)
  expect_match(source, ".qmd", fixed = TRUE)
  expect_false(grepl("quarto::", source, fixed = TRUE))
  expect_false(grepl("rmarkdown::render", source, fixed = TRUE))
})
