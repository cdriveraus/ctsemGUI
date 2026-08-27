ctgui_example <- getFromNamespace("ctgui_example", "ctsemGUI")
ctgui_example_ids <- getFromNamespace("ctgui_example_ids", "ctsemGUI")
ctgui_example_catalog <- getFromNamespace("ctgui_example_catalog", "ctsemGUI")
ctgui_example_spec <- getFromNamespace("ctgui_example_spec", "ctsemGUI")
ctgui_example_data <- getFromNamespace("ctgui_example_data", "ctsemGUI")
ctgui_example_truth_note <- getFromNamespace("ctgui_example_truth_note", "ctsemGUI")
ctgui_example_data_label <- getFromNamespace("ctgui_example_data_label", "ctsemGUI")

quiet_example <- function(code) suppressWarnings(suppressMessages(force(code)))

test_that("every example opens into a model ctsem accepts", {
  skip_if_not_installed("ctsem")

  for (id in ctgui_example_ids()) {
    example <- ctgui_example(id)
    spec <- quiet_example(ctgui_example_spec(example))

    expect_true(length(spec$latent_names) > 0L, info = id)
    expect_true(length(spec$manifest_names) > 0L, info = id)
    expect_no_error(quiet_example(ctgui_to_ctsem_model(spec)))
  }
})

test_that("every example's data carries the columns its model asks for", {
  skip_if_not_installed("ctsem")

  # An example that opens into a model its own data cannot fit is worse than
  # no example: the first thing the reader does is hit an error they did not
  # cause.
  for (id in ctgui_example_ids()) {
    example <- ctgui_example(id)
    spec <- quiet_example(ctgui_example_spec(example))
    data <- quiet_example(ctgui_example_data(example))

    expect_s3_class(data, "data.frame")
    expect_gt(nrow(data), 0L)

    needed <- c(spec$id, spec$time, spec$manifest_names, spec$tdpred_names, spec$tipred_names)
    expect_equal(setdiff(needed, names(data)), character(), info = id)
  }
})

test_that("generated examples are the same every time they are opened", {
  skip_if_not_installed("ctsem")

  # A worked example whose numbers move between sessions cannot be written
  # about or talked about.
  first <- quiet_example(ctgui_example_data(ctgui_example("coupled")))
  second <- quiet_example(ctgui_example_data(ctgui_example("coupled")))
  expect_equal(first, second)
})

test_that("generating an example leaves the caller's random stream alone", {
  skip_if_not_installed("ctsem")

  set.seed(99L)
  expected <- runif(3L)

  set.seed(99L)
  quiet_example(ctgui_example_data(ctgui_example("coupled")))
  expect_equal(runif(3L), expected)
})

test_that("examples that disagree with their data say so", {
  # Two examples hand over a model that is deliberately not the one the data
  # came from. Presenting that as a recovery check would teach the wrong
  # lesson entirely.
  misspecified <- c("no_coupling", "ignored_trend")
  for (id in misspecified) {
    example <- ctgui_example(id)
    expect_false(is.null(example$model))
    expect_match(ctgui_example_truth_note(example), "different model", fixed = TRUE)
  }

  recovering <- ctgui_example("coupled")
  expect_null(recovering$model)
  expect_match(ctgui_example_truth_note(recovering), "should recover them", fixed = TRUE)
})

test_that("values added only to make generation possible are disclosed", {
  # ctGenerate needs an invertible system with non-degenerate noise, which a
  # growth curve and a constant trend do not have. The nudge that works around
  # that is not part of the model and must not read as though it were.
  for (id in c("growth", "ignored_trend")) {
    example <- ctgui_example(id)
    note <- example$generation_note %||% ""
    expect_true(nzchar(note), info = id)
    # The reader has to be told these values are an artefact of simulating, not
    # part of the model, and told it without needing to know what a singular
    # drift matrix is.
    expect_match(note, "only so the data could be simulated", fixed = TRUE)
    expect_match(note, "ignore them", fixed = TRUE)
    expect_true(grepl(note, ctgui_example_truth_note(example), fixed = TRUE), info = id)
  }
})

test_that("the real-data example keeps latent and manifest names apart", {
  skip_if_not_installed("ctsem")

  spec <- quiet_example(ctgui_example_spec(ctgui_example("real_data")))
  expect_equal(intersect(spec$latent_names, spec$manifest_names), character())
  expect_true(all(c("Y1", "Y2") %in% spec$manifest_names))
  expect_equal(spec$tdpred_names, "TD1")
})

test_that("every example describes itself and says what to look at", {
  for (id in ctgui_example_ids()) {
    example <- ctgui_example(id)
    expect_true(nzchar(example$title), info = id)
    expect_true(nzchar(example$brief), info = id)
    expect_true(nzchar(example$detail), info = id)
    # An example without this is a dataset, not a worked example.
    expect_true(nzchar(example$look_for), info = id)
    expect_true(nzchar(ctgui_example_data_label(example)), info = id)
  }
})

test_that("an unknown example is refused by name", {
  expect_error(ctgui_example("not_an_example"), "Unknown example")
})

test_that("the application reaches every example it defines", {
  source <- paste(
    readLines(ctgui_test_source_path("R", "app_ui_helpers.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(source, "ctgui_example_catalog()", fixed = TRUE)
  expect_match(source, "example_load", fixed = TRUE)
})
