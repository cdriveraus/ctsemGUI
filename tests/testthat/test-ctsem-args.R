test_that("argument descriptions come from the function itself", {
  env <- new.env()
  env$.resolve_kind <- function(x) match.arg(as.character(x), env$.kinds)
  env$.kinds <- c("unit", "observed", "shock")
  env$.series <- c("y", "yprior", "ysmooth")
  f <- function(kind = "unit", flag = FALSE, mode = c("fast", "exact"), sel = c("y", "yprior"),
      check, nboot = 100, verbose = 0, picks = "y") {
    kind <- .resolve_kind(kind)
    mode <- match.arg(mode)
    if (!all(picks %in% .series)) stop("picks takes ", paste(.series, collapse = ", "))
    if (isTRUE(verbose)) message("verbose")
    if (flag) sel else check
  }
  environment(f) <- env

  kind <- ctgui_function_arg(f, "kind")
  expect_true(kind$found)
  expect_identical(kind$choices, c("unit", "observed", "shock"))
  expect_identical(ctgui_arg_placeholder(kind), 'default: "unit"')

  expect_true(ctgui_function_arg(f, "flag")$logical)
  expect_identical(ctgui_arg_options(ctgui_function_arg(f, "flag")), c("FALSE", "TRUE"))
  # A numeric default the body tests with isTRUE() takes a logical too.
  expect_identical(ctgui_arg_options(ctgui_function_arg(f, "verbose")), c("0", "TRUE", "FALSE"))

  mode <- ctgui_function_arg(f, "mode")
  expect_identical(mode$choices, c("fast", "exact"))
  expect_identical(ctgui_arg_placeholder(mode), 'default: "fast"')

  # A vector default without match.arg is a selection, not a set of choices.
  sel <- ctgui_function_arg(f, "sel")
  expect_identical(sel$choices, character())
  expect_identical(ctgui_arg_placeholder(sel), 'default: c("y", "yprior")')

  # A check that every value is in a set declares the set.
  expect_identical(ctgui_function_arg(f, "picks")$choices, c("y", "yprior", "ysmooth"))
  # A vector default that is a selection from the set shows whole.
  pick2 <- ctgui_function_arg(f, "picks")
  pick2$default <- c("y", "yprior")
  pick2$default_text <- 'c("y", "yprior")'
  expect_identical(ctgui_arg_placeholder(pick2), 'default: c("y", "yprior")')

  expect_identical(ctgui_arg_placeholder(ctgui_function_arg(f, "check")), "no default")
  expect_false(ctgui_function_arg(f, "absent")$found)
  expect_identical(ctgui_arg_placeholder(ctgui_function_arg(f, "absent")), "")
})

test_that("a field left blank or at the default is not passed", {
  arg <- list(found = TRUE, default_text = "100", default = 100, logical = FALSE, choices = character())
  expect_null(ctgui_arg_value(NULL, arg))
  expect_null(ctgui_arg_value(NA, arg))
  expect_null(ctgui_arg_value("  ", arg))
  expect_null(ctgui_arg_value(100, arg))
  expect_identical(ctgui_arg_value(25, arg), 25)

  priors <- list(found = TRUE, default_text = '"randomCorr"', default = "randomCorr",
    logical = TRUE, choices = character())
  expect_null(ctgui_arg_value("randomCorr", priors))
  expect_identical(ctgui_arg_value("TRUE", priors), TRUE)
  expect_identical(ctgui_arg_value("FALSE", priors), FALSE)

  expect_identical(ctgui_arg_value(c("1", "3")), c(1, 3))
  expect_identical(ctgui_arg_value(c("y", "ysmooth")), c("y", "ysmooth"))
  expect_identical(ctgui_arg_value("c(0, 10)"), c(0, 10))
  expect_identical(ctgui_arg_value("2"), 2)
  # Text naming a function, or nothing at all, stays text.
  expect_identical(ctgui_arg_value("all"), "all")
  expect_identical(ctgui_arg_value("popmean"), "popmean")
})

test_that("the installed ctsem's defaults reach the fields", {
  skip_if_not_installed("ctsem")
  priors <- ctgui_ctsem_arg("ctFit", "priors")
  expect_true(priors$found)
  expect_identical(priors$default_text, paste(deparse(formals(ctsem::ctFit)$priors), collapse = " "))
  expect_identical(ctgui_arg_options(priors)[1L], as.character(eval(formals(ctsem::ctFit)$priors)))
  expect_true(all(c("TRUE", "FALSE") %in% ctgui_arg_options(priors)))

  impulse <- ctgui_dynamics_impulse_param()
  expect_true(impulse %in% names(formals(ctsem::ctDiscretePars)))
  if (identical(impulse, "impulseType")) {
    expect_true("unit" %in% ctgui_ctsem_arg("ctDiscretePars", "impulseType")$choices)
  }
})

test_that("generated code leaves out what was left at the default", {
  fit_code <- paste(ctgui_output_snippet("fit", list(optimize = TRUE, cores = 2L)), collapse = "\n")
  expect_false(grepl("priors", fit_code))
  fit_code <- paste(ctgui_output_snippet("fit", list(optimize = TRUE, priors = TRUE, cores = 2L)), collapse = "\n")
  expect_match(fit_code, "priors = TRUE", fixed = TRUE)

  kalman <- ctgui_output_snippet("kalman", list())
  expect_true("plot(prediction)" %in% kalman)
  kalman <- paste(ctgui_output_snippet("kalman", list(kalmanvec = c("y", "ysmooth"))), collapse = "\n")
  expect_match(kalman, 'kalmanvec = c("y", "ysmooth")', fixed = TRUE)
  expect_false(grepl("errorvec", kalman))

  acf <- paste(ctgui_output_snippet("residual_acf", list()), collapse = "\n")
  expect_false(grepl("varnames|nboot", acf))

  dynamics <- paste(ctgui_output_snippet("dynamics", list(impulseType = "observed")), collapse = "\n")
  expect_match(dynamics, 'impulseType = "observed"', fixed = TRUE)
  expect_false(grepl("observational", dynamics))
  dynamics <- paste(ctgui_output_snippet("dynamics", list()), collapse = "\n")
  expect_false(grepl("impulseType|observational", dynamics))

  tipred <- paste(ctgui_output_snippet("tipred", list()), collapse = "\n")
  expect_false(grepl("tipreds|timestep", tipred))
  generate <- paste(ctgui_output_snippet("generate_from_fit", list()), collapse = "\n")
  expect_false(grepl("nsamples", generate))
})

test_that("a field ctsem says nothing about is blank, and passes only what is typed", {
  catalog <- list(help_unknown = list(topic = "ctguiNoSuchFunction", param = "x", tooltip = "x"))
  arg <- ctgui_help_arg(catalog, "help_unknown")
  expect_false(arg$found)
  expect_identical(ctgui_arg_placeholder(arg), "")
  expect_identical(ctgui_arg_options(arg), character())

  text <- ctgui_arg_text_input("f1", "f1", catalog, "help_unknown")
  expect_false(grepl("placeholder=\"[^\"]", as.character(text)))
  select <- as.character(ctgui_arg_select_input("f2", "f2", catalog, "help_unknown"))
  # Only the empty option that keeps the field blank.
  expect_false(grepl('<option value="[^"]', select))
  expect_match(select, '"create":true', fixed = TRUE)
  expect_match(as.character(ctgui_arg_checkbox_input("f3", "f3", catalog, "help_unknown", fallback = TRUE)),
    "checked", fixed = TRUE)
  expect_false(grepl("checked", as.character(ctgui_arg_checkbox_input("f4", "f4", catalog, "help_unknown"))))

  expect_null(ctgui_arg_value("", arg))
  expect_identical(ctgui_arg_value("c(1, 2)", arg), c(1, 2))
  expect_identical(ctgui_arg_value("TRUE", arg), TRUE)
})

test_that("subject choices keep the data's ids apart from ctsem's numbering", {
  skip_if_not_installed("ctsem")
  fit <- ctsem::ctstantestfit
  subjects <- ctgui_fit_subjects(fit)
  expect_identical(subjects$internal, as.character(seq_along(subjects$internal)))
  expect_identical(subjects$original, as.character(fit$standata$idmap[, 1L]))
  expect_identical(ctgui_fit_subjects(list())$original, character())
})

test_that("subject ids are described in a line", {
  expect_identical(ctgui_describe_ids(1:30), "1\u201330")
  expect_identical(ctgui_describe_ids(c(3, 1, 2, 7, 10, 11)), "1\u20133, 7, 10\u201311")
  expect_identical(ctgui_describe_ids(c("a", "b")), "a, b")
  expect_identical(ctgui_describe_ids(paste0("p", 1:20), max_items = 3),
    "p1, p2, p3, \u2026 (20 subjects)")
  expect_identical(ctgui_describe_ids(character()), "")
})
