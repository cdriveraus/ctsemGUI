ctgui_manifest_type_catalog <- getFromNamespace("ctgui_manifest_type_catalog", "ctsemGUI")
ctgui_manifest_type_entry <- getFromNamespace("ctgui_manifest_type_entry", "ctsemGUI")
ctgui_manifest_type_choices <- getFromNamespace("ctgui_manifest_type_choices", "ctsemGUI")
ctgui_manifest_type_needs <- getFromNamespace("ctgui_manifest_type_needs", "ctsemGUI")
ctgui_normalize_measurement <- getFromNamespace("ctgui_normalize_measurement", "ctsemGUI")
ctgui_measurement_problems <- getFromNamespace("ctgui_measurement_problems", "ctsemGUI")
ctgui_measurement_backends <- getFromNamespace("ctgui_measurement_backends", "ctsemGUI")
ctgui_measurement_requires_julia <- getFromNamespace("ctgui_measurement_requires_julia", "ctsemGUI")
ctgui_measurement_backend_message <- getFromNamespace("ctgui_measurement_backend_message", "ctsemGUI")
ctgui_measurement_model_args <- getFromNamespace("ctgui_measurement_model_args", "ctsemGUI")
ctgui_measurement_summary <- getFromNamespace("ctgui_measurement_summary", "ctsemGUI")
ctgui_measurement_input_values <- getFromNamespace("ctgui_measurement_input_values", "ctsemGUI")
ctgui_respec_preserving <- getFromNamespace("ctgui_respec_preserving", "ctsemGUI")
ctgui_measurement_matrix_note <- getFromNamespace("ctgui_measurement_matrix_note", "ctsemGUI")
ctgui_ctsem_extended_measurement <- getFromNamespace("ctgui_ctsem_extended_measurement", "ctsemGUI")

# Ordinal, count and censored measurement need ctsem 3.12. On 3.11 the types
# cannot be built at all, so these cases have nothing to assert; the refusal
# that replaces them is covered by its own test below.
skip_without_extended_measurement <- function() {
  testthat::skip_if_not(ctgui_ctsem_extended_measurement(),
    "installed ctsem has no ordinal, count or censored measurement")
}

quiet_types <- function(code) suppressWarnings(suppressMessages(force(code)))

ordinal_spec <- function() {
  quiet_types(ctgui_spec(
    latent_names = c("eta1", "eta2"),
    manifest_names = c("mood", "score"),
    manifest_type = c(2L, 4L), ncategories = c(5L, 0L),
    censormin = c(-Inf, 0), censormax = c(Inf, 20)
  ))
}

test_that("the catalog covers every type ctsem accepts", {
  values <- vapply(ctgui_manifest_type_catalog(), function(e) e$value, integer(1L))
  expect_equal(sort(values), 0:4)

  for (entry in ctgui_manifest_type_catalog()) {
    expect_true(nzchar(entry$label))
    expect_true(nzchar(entry$short))
    expect_true(nzchar(entry$detail))
    expect_true(entry$needs %in% c("none", "ncategories", "censor"))
    expect_true(length(entry$backends) > 0L)
  }
  expect_equal(unname(ctgui_manifest_type_choices()), 0:4)
})

test_that("only the types that use an extra argument ask for one", {
  # Asking for a category count on a continuous variable would be noise; not
  # asking on an ordinal one is a fit-time failure.
  expect_equal(ctgui_manifest_type_needs(0L), "none")
  expect_equal(ctgui_manifest_type_needs(1L), "none")
  expect_equal(ctgui_manifest_type_needs(2L), "ncategories")
  expect_equal(ctgui_manifest_type_needs(3L), "none")
  expect_equal(ctgui_manifest_type_needs(4L), "censor")
})

test_that("normalising keeps the vectors aligned and clears what is unused", {
  measurement <- ctgui_normalize_measurement(
    c("a", "b", "c"),
    manifest_type = c(2L, 4L, 0L),
    ncategories = c(5L, 9L, 7L),
    censormin = c(1, 0, 2), censormax = c(9, 20, 8)
  )
  expect_length(measurement$manifest_type, 3L)

  # A count belongs to the type that uses it. Left on a censored or continuous
  # variable it would be written into the model and quietly ignored there.
  expect_equal(measurement$ncategories, c(5L, 0L, 0L))
  expect_equal(measurement$censormin, c(-Inf, 0, -Inf))
  expect_equal(measurement$censormax, c(Inf, 20, Inf))

  # Short, absent or nonsense input still yields full-length vectors.
  short <- ctgui_normalize_measurement(c("a", "b"), manifest_type = 2L)
  expect_equal(short$manifest_type, c(2L, 0L))
  unknown <- ctgui_normalize_measurement("a", manifest_type = 99L)
  expect_equal(unknown$manifest_type, 0L)
  expect_equal(ctgui_normalize_measurement(character())$manifest_type, integer())
})

test_that("a type missing its argument is caught before ctsem sees it", {
  # These are the rules about which type needs which argument, so they hold
  # whatever ctsem is installed. Pinned, or on a ctsem that cannot fit these
  # types every case would also carry the version problem and count twice.
  testthat::local_mocked_bindings(
    ctgui_ctsem_extended_measurement = function() TRUE,
    .package = "ctsemGUI"
  )
  problems <- function(...) ctgui_measurement_problems(c("a", "b"), ...)

  # ctsem needs at least three categories; two is binary, not ordinal.
  expect_length(problems(c(2L, 0L), c(0L, 0L)), 1L)
  expect_length(problems(c(2L, 0L), c(2L, 0L)), 1L)
  expect_length(problems(c(2L, 0L), c(3L, 0L)), 0L)

  # Censored nowhere is just a Gaussian variable.
  expect_length(problems(c(4L, 0L)), 1L)
  expect_length(problems(c(4L, 0L), censormin = c(0, -Inf)), 0L)
  expect_length(problems(c(4L, 0L), censormax = c(9, Inf)), 0L)

  # A floor at or above the ceiling admits no observation at all.
  crossed <- problems(c(4L, 0L), censormin = c(9, -Inf), censormax = c(2, Inf))
  expect_length(crossed, 1L)
  expect_match(crossed[[1L]]$message, "no value", fixed = TRUE)

  expect_length(problems(c(0L, 1L)), 0L)
  expect_length(problems(c(3L, 3L)), 0L)
})

test_that("the engine a model needs follows from its measurement types", {
  # Non-Gaussian measurement is julia-only, with binary also available in stan.
  expect_setequal(ctgui_measurement_backends(c(0L, 1L)), c("julia", "stan"))
  expect_equal(ctgui_measurement_backends(2L), "julia")
  expect_equal(ctgui_measurement_backends(c(0L, 3L)), "julia")

  expect_false(ctgui_measurement_requires_julia(c(0L, 1L)))
  expect_true(ctgui_measurement_requires_julia(c(0L, 4L)))

  # The message names the variables responsible rather than leaving the user
  # to work out which one it means.
  message <- ctgui_measurement_backend_message(c("mood", "score"), c(2L, 0L))
  expect_match(message, "mood", fixed = TRUE)
  expect_false(grepl("score", message, fixed = TRUE))
  expect_equal(ctgui_measurement_backend_message(c("a", "b"), c(0L, 1L)), "")
})

test_that("ctModel is given the extra arguments only when a type uses them", {
  plain <- ctgui_measurement_model_args(ctgui_normalize_measurement(c("a", "b")))
  expect_equal(names(plain), "manifesttype")

  ordinal <- ctgui_measurement_model_args(ctgui_normalize_measurement(
    c("a", "b"), c(2L, 0L), c(4L, 0L)))
  expect_true("ncategories" %in% names(ordinal))
  expect_false("censormin" %in% names(ordinal))

  censored <- ctgui_measurement_model_args(ctgui_normalize_measurement(
    c("a", "b"), c(4L, 0L), censormax = c(9, Inf)))
  expect_true(all(c("censormin", "censormax") %in% names(censored)))
  expect_false("ncategories" %in% names(censored))
})

test_that("a specification carries its measurement model into ctsem", {
  skip_without_extended_measurement()
  skip_if_not_installed("ctsem")
  spec <- ordinal_spec()

  expect_equal(spec$manifest_type, c(2L, 4L))
  expect_equal(spec$ncategories, c(5L, 0L))
  expect_equal(spec$censormax, c(Inf, 20))

  model <- quiet_types(ctgui_to_ctsem_model(spec))
  expect_equal(as.integer(model$manifesttype), c(2L, 4L))
  expect_equal(as.integer(model$ncategories), c(5L, 0L))
  expect_equal(model$censormax, c(Inf, 20))
})

test_that("THRESHOLDS is not carried as an editable matrix", {
  skip_without_extended_measurement()
  skip_if_not_installed("ctsem")
  # ctModelMatrices() reports it, but ctModel() has no such argument and the
  # thresholds are derived from the category count. Carrying it would break
  # every rebuild.
  spec <- ordinal_spec()
  expect_false("THRESHOLDS" %in% names(spec$matrices))
  expect_no_error(quiet_types(ctgui_to_ctsem_model(spec)))
})

test_that("an incomplete choice is a draft rather than a crash", {
  skip_without_extended_measurement()
  skip_if_not_installed("ctsem")
  # Choosing Ordinal before typing a category count is an ordinary in-progress
  # state. Building the ctsem model would throw and take the session with it.
  spec <- quiet_types(ctgui_spec(
    latent_names = "eta1", manifest_names = "item",
    manifest_type = 2L, ncategories = 0L
  ))
  expect_s3_class(spec, "ctsemgui_spec")
  expect_equal(spec$source, "draft")

  validation <- quiet_types(ctgui_validate(spec))
  errors <- validation[validation$field == "manifesttype" & validation$severity == "error", ]
  expect_equal(nrow(errors), 1L)
  expect_match(errors$message[1L], "at least 3", fixed = TRUE)
})

test_that("identification traps ctsem warns about are raised as warnings", {
  skip_without_extended_measurement()
  skip_if_not_installed("ctsem")
  # An ordinal variable's thresholds and its manifest intercept both describe
  # where the categories sit, so they are not separately identified. ctsem
  # warns at model construction; saying so here puts it beside the choice.
  spec <- quiet_types(ctgui_spec(
    latent_names = "eta1", manifest_names = "item",
    manifest_type = 2L, ncategories = 5L
  ))
  validation <- quiet_types(ctgui_validate(spec))
  warnings <- validation[validation$severity == "warning", , drop = FALSE]

  expect_gte(nrow(warnings), 1L)
  combined <- paste(warnings$message, collapse = " ")
  expect_match(combined, "not separately identified", fixed = TRUE)
  # A warning that does not name the cell to change leaves the reader stuck.
  expect_match(combined, "MANIFESTMEANS", fixed = TRUE)

  # None of this applies to an ordinary continuous model.
  plain <- quiet_types(ctgui_spec(latent_names = "eta1", manifest_names = "y1"))
  plain_validation <- quiet_types(ctgui_validate(plain))
  expect_equal(nrow(plain_validation[plain_validation$field == "manifesttype", ]), 0L)
})

test_that("a measurement model survives resizing and renaming", {
  skip_without_extended_measurement()
  skip_if_not_installed("ctsem")
  spec <- ordinal_spec()

  grown <- quiet_types(ctgui_respec_preserving(
    spec, latent_names = spec$latent_names,
    manifest_names = c("mood", "score", "extra")
  ))
  expect_equal(grown$manifest_type, c(2L, 4L, 0L))
  expect_equal(grown$ncategories, c(5L, 0L, 0L))
  expect_equal(grown$censormax, c(Inf, 20, Inf))

  renamed <- quiet_types(ctgui_respec_preserving(
    spec, latent_names = spec$latent_names,
    manifest_names = c("feeling", "score"), rename = c(mood = "feeling")
  ))
  expect_equal(renamed$manifest_type, c(2L, 4L))
  expect_equal(renamed$ncategories, c(5L, 0L))
})

test_that("a loaded model brings its measurement model with it", {
  skip_without_extended_measurement()
  skip_if_not_installed("ctsem")
  model <- quiet_types(ctgui_to_ctsem_model(ordinal_spec()))
  loaded <- quiet_types(ctgui_spec_from_model(model))

  expect_equal(loaded$manifest_type, c(2L, 4L))
  expect_equal(loaded$ncategories, c(5L, 0L))
  expect_equal(loaded$censormax, c(Inf, 20))
})

test_that("exported code reproduces the measurement model", {
  skip_without_extended_measurement()
  skip_if_not_installed("ctsem")
  code <- ctgui_export_code(ordinal_spec())

  for (argument in c("manifesttype", "ncategories", "censormin", "censormax")) {
    expect_match(code, argument, fixed = TRUE)
  }
  env <- new.env(parent = globalenv())
  quiet_types(eval(parse(text = code), envir = env))
  rebuilt <- get("model", envir = env)
  expect_equal(as.integer(rebuilt$manifesttype), c(2L, 4L))
  expect_equal(as.integer(rebuilt$ncategories), c(5L, 0L))
})

test_that("the controls read back into a measurement description", {
  values <- list(
    manifest_type_1 = "2", manifest_ncategories_1 = 6,
    manifest_type_2 = "4", manifest_censormin_2 = 0, manifest_censormax_2 = NA
  )
  measurement <- ctgui_measurement_input_values(c("a", "b"), values)

  expect_equal(measurement$manifest_type, c(2L, 4L))
  expect_equal(measurement$ncategories, c(6L, 0L))
  # A blank limit means "not censored on that side", which is how a one-sided
  # censor is expressed rather than an error.
  expect_equal(measurement$censormin, c(-Inf, 0))
  expect_equal(measurement$censormax, c(Inf, Inf))
})

test_that("a summary describes a variable in the terms of its type", {
  expect_equal(ctgui_measurement_summary("a", 0L), "Continuous")
  expect_match(ctgui_measurement_summary("a", 2L, 5L), "5 categories", fixed = TRUE)
  expect_match(ctgui_measurement_summary("a", 4L, 0L, 0, 10), "from 0 to 10", fixed = TRUE)
  expect_match(ctgui_measurement_summary("a", 4L, 0L, -Inf, 10), "to 10", fixed = TRUE)
})

test_that("the matrix editor explains what is not a matrix", {
  skip_without_extended_measurement()
  skip_if_not_installed("ctsem")
  notes <- ctgui_measurement_matrix_note(ordinal_spec())
  combined <- paste(notes, collapse = " ")

  # Thresholds and censoring limits are consequences of a type, not cells.
  expect_match(combined, "thresholds are estimated automatically", fixed = TRUE)
  expect_match(combined, "not parameters", fixed = TRUE)

  plain <- quiet_types(ctgui_spec(latent_names = "eta1", manifest_names = "y1"))
  expect_null(ctgui_measurement_matrix_note(plain))
})

test_that("the engine sent to ctFit is resolved against the measurement types", {
  skip_without_extended_measurement()
  skip_if_not_installed("shiny")
  ctgui_blueprint_apply <- getFromNamespace("ctgui_blueprint_apply", "ctsemGUI")
  empty <- quiet_types(ctgui_spec(latent_names = character(), manifest_names = character()))

  suppressWarnings(shiny::testServer(ctgui_app_server(empty, ctgui_help_catalog()), {
    julia_status(list(available = TRUE, message = "Julia is available."))
    session$setInputs(fit_backend = "stan")

    ordinal <- quiet_types(ctgui_blueprint_apply(empty, ctgui_blueprint(
      "coupled", c("mood", "energy"), indicator_type = 2L, indicator_ncategories = 5L
    ), "replace"))
    commit_current_spec(ordinal, reason = "blueprint")

    # Stan cannot fit these at all, so the engine is switched rather than left
    # to fail inside ctFit with a message naming a backend nobody chose.
    switched <- fit_backend_for_model(current_spec())
    expect_equal(switched$backend, "julia")
    expect_match(switched$message, "Switched to the Julia engine", fixed = TRUE)

    # A model Stan can fit is left alone.
    plain <- quiet_types(ctgui_blueprint_apply(
      empty, ctgui_blueprint("coupled", c("a", "b")), "replace"
    ))
    commit_current_spec(plain, reason = "blueprint")
    untouched <- fit_backend_for_model(current_spec())
    expect_equal(untouched$backend, "stan")
    expect_equal(untouched$message, "")

    # With no Julia the fit is refused up front, since it cannot succeed.
    julia_status(list(available = FALSE, message = "not set up"))
    commit_current_spec(ordinal, reason = "blueprint")
    refused <- fit_backend_for_model(current_spec())
    expect_true(is.na(refused$backend))
    expect_match(refused$message, "cannot be fitted", fixed = TRUE)
  }))
})

test_that("a type the installed ctsem cannot fit is refused where it was chosen", {
  skip_if_not_installed("ctsem")
  # Simulates a ctsem 3.11 session, where ctModel() has no ncategories
  # argument. Without this the choice fails much later, inside ctModel(), with
  # an unused-argument error naming an argument the user never wrote.
  testthat::local_mocked_bindings(
    ctgui_ctsem_extended_measurement = function() FALSE,
    .package = "ctsemGUI"
  )

  problems <- ctgui_measurement_problems(
    c("mood", "score"), manifest_type = c(2L, 0L), ncategories = c(5L, 0L)
  )
  messages <- vapply(problems, function(p) p$message, character(1L))
  expect_true(any(grepl("mood (ordinal)", messages, fixed = TRUE)))
  expect_true(any(grepl("ctsem 3.12 or later", messages, fixed = TRUE)))
  expect_true(all(vapply(problems, function(p) p$severity, character(1L)) == "error"))

  # Continuous and binary are unaffected.
  expect_length(ctgui_measurement_problems(c("a", "b"), manifest_type = c(0L, 1L)), 0L)

  # And the controls stop offering what cannot be built, while the catalog
  # keeps every label so a spec loaded from elsewhere still reads correctly.
  expect_equal(unname(ctgui_manifest_type_choices(available_only = TRUE)), 0:1)
  expect_equal(unname(ctgui_manifest_type_choices()), 0:4)
})
