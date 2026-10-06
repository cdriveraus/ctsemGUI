test_that("More arguments are the function's own, less what the panel sets or shows", {
  skip_if_not_installed("ctsem")
  catalog <- ctgui_help_catalog_base()
  args <- ctgui_more_args_list("dynamics", "call", catalog)
  formal <- names(formals(ctgui_ctsem_function("ctDiscretePars")))
  expect_true(all(args %in% formal))
  # Shown on the panel, set by the GUI, or not an argument at all.
  expect_false(any(c("subjects", "times", "nsamples", "fit", "plot", "cores", "...") %in% args))
  # Deprecated, by ctsem's own help.
  expect_false("ctstanfitobj" %in% args)
  for (arg in args) expect_false(ctgui_arg_deprecated("ctDiscretePars", arg), info = arg)
})

test_that("every More argument has a catalog entry and a field", {
  skip_if_not_installed("ctsem")
  catalog <- ctgui_help_catalog()
  more <- Filter(function(help) isTRUE(help$more), catalog)
  expect_gt(length(more), 0L)
  for (help in more) {
    expect_true(help$param %in% names(formals(ctgui_ctsem_function(help$topic))), info = help$param)
  }
  section <- as.character(ctgui_more_args_ui("kalman", catalog, "help_ctPredict"))
  for (help in ctgui_more_args_entries(catalog, "kalman")) {
    expect_match(section, paste0('id="', ctgui_more_args_input_id("kalman", help$role, help$param), '"'),
      fixed = TRUE)
  }
  expect_match(section, "plot.ctKalmanDF", fixed = TRUE)
  expect_null(ctgui_more_args_ui("kalman", list()))
})

test_that("a More argument is passed only when changed from ctsem's default", {
  skip_if_not_installed("ctsem")
  catalog <- ctgui_help_catalog()
  input <- list()
  expect_identical(ctgui_more_args_values(input, catalog, "dynamics"), setNames(list(), character()))
  input[[ctgui_more_args_input_id("dynamics", "call", "standardise")]] <- "FALSE"
  expect_length(ctgui_more_args_values(input, catalog, "dynamics"), 0L)
  input[[ctgui_more_args_input_id("dynamics", "call", "standardise")]] <- "TRUE"
  values <- ctgui_more_args_values(input, catalog, "dynamics")
  expect_identical(values$standardise, TRUE)
  # Typed text is read as R. state is from ctsem 3.12.
  if ("state" %in% ctgui_more_args_list("dynamics", "call", ctgui_help_catalog_base())) {
    input[[ctgui_more_args_input_id("dynamics", "call", "state")]] <- "c(1, 2)"
    expect_identical(ctgui_more_args_values(input, catalog, "dynamics")$state, c(1, 2))
  }
  # A role's values are its own.
  input[[ctgui_more_args_input_id("kalman", "plot", "polygonsteps")]] <- "5"
  expect_identical(ctgui_more_args_values(input, catalog, "kalman", "plot")$polygonsteps, 5)
  expect_length(ctgui_more_args_values(input, catalog, "kalman", "call"), 0L)
})

test_that("no More argument is passed as the page first sets it", {
  skip_if_not_installed("ctsem")
  catalog <- ctgui_help_catalog()
  # What the browser holds before anyone touches a field: a text box is empty,
  # a single select holds its selected option or else its first one.
  initial <- function(html, id) {
    select <- regmatches(html, regexpr(paste0('(?s)<select[^>]*id="', id, '"[^>]*>.*?</select>'), html, perl = TRUE))
    if (!length(select)) return("")
    selected <- regmatches(select, regexec('<option value="([^"]*)" selected>', select))[[1L]]
    if (length(selected)) return(selected[2L])
    regmatches(select, regexec('<option value="([^"]*)"', select))[[1L]][2L]
  }
  for (panel in names(ctgui_more_args_panels())) {
    html <- as.character(ctgui_more_args_ui(panel, catalog))
    input <- list()
    for (help in ctgui_more_args_entries(catalog, panel)) {
      id <- ctgui_more_args_input_id(panel, help$role, help$param)
      input[[id]] <- initial(html, id)
    }
    for (role in names(ctgui_more_args_panels()[[panel]])) {
      expect_length(ctgui_more_args_values(input, catalog, panel, role), 0L)
    }
  }
})

test_that("a dropdown starts on the default ctsem uses, or is blank free entry", {
  vector_default <- list(found = TRUE, default_text = 'c("a", "b")', default = c("a", "b"),
    logical = FALSE, choices = c("a", "b"))
  expect_identical(ctgui_arg_effective_default(vector_default), "a")
  expect_null(ctgui_arg_value("a", vector_default))
  expect_identical(ctgui_arg_value("b", vector_default), "b")

  catalog <- list(help_x = list(topic = "f", param = "x"))
  html <- function(arg) {
    local_mocked_bindings(ctgui_help_arg = function(...) arg)
    as.character(ctgui_arg_select_input("x", "x", catalog, "help_x"))
  }
  expect_match(html(vector_default), '<option value="a" selected>', fixed = TRUE)
  expect_false(grepl('"create":true', html(vector_default), fixed = TRUE))
  # A logical says nothing of what else the argument takes, so values can be
  # typed beside TRUE and FALSE.
  open_set <- list(found = TRUE, default_text = '"auto"', default = "auto", logical = TRUE,
    choices = character())
  expect_match(html(open_set), '<option value="auto" selected>', fixed = TRUE)
  expect_match(html(open_set), '"create":true', fixed = TRUE)
  # A default that cannot be shown would leave the first option chosen and
  # passed, so the field is free entry instead.
  unshown <- list(found = TRUE, default_text = "other", default = NULL, logical = TRUE, choices = character())
  expect_false(grepl("selected", html(unshown), fixed = TRUE))
  expect_match(html(unshown), '"create":true', fixed = TRUE)
  # The browser takes a single select's first option when none is selected,
  # so that option has to be the empty one.
  expect_match(html(unshown), '<select[^>]*id="x"[^>]*><option value=""></option>', perl = TRUE)
})

test_that("generated code carries More arguments for every panel that takes them", {
  postpred <- paste(ctgui_output_snippet("postpred", list(extra_args = list(nsamples = 50L))), collapse = "\n")
  expect_match(postpred, "nsamples = 50L", fixed = TRUE)
  expect_true("postpred_plots <- ctsem::ctPostPredPlots(" %in% ctgui_output_snippet("postpred", list()))
  kalman <- paste(ctgui_output_snippet("kalman", list(plot_extra_args = list(polygonsteps = 5))), collapse = "\n")
  expect_match(kalman, "polygonsteps = 5", fixed = TRUE)
  tipred <- paste(ctgui_output_snippet("tipred", list(extra_args = list(doDynamics = FALSE))), collapse = "\n")
  expect_match(tipred, "doDynamics = FALSE", fixed = TRUE)
})

test_that("names are made unique by a counter", {
  expect_identical(ctgui_unique_name("fit", character()), "fit")
  expect_identical(ctgui_unique_name("fit", c("fit", "fit (2)")), "fit (3)")
})

test_that("every fit is kept as it completes, and can be renamed or removed", {
  skip_if_not_installed("ctsem")
  suppressWarnings(shiny::testServer(
    ctgui_app_server(ctgui_spec(), ctgui_help_catalog()), {
    first <- ctsem::ctstantestfit
    second <- ctsem::ctstantestfit
    second$marker <- "second"
    expect_identical(keep_fit(first, ctgui_spec()), "fit1")
    expect_identical(keep_fit(second, ctgui_spec()), "fit2")
    expect_identical(names(fit_registry()), c("fit1", "fit2"))
    expect_identical(names(fit_specs()), c("fit1", "fit2"))
    expect_identical(current_fit()$marker, "second")

    session$setInputs(active_fit_name = "fit1")
    expect_null(current_fit()$marker)

    session$setInputs(rename_fit = 1, rename_fit_name = "baseline", confirm_rename_fit = 1)
    expect_identical(names(fit_registry()), c("baseline", "fit2"))
    expect_identical(names(fit_specs()), c("baseline", "fit2"))

    # A name already taken is refused rather than overwriting that fit.
    session$setInputs(active_fit_name = "fit2")
    session$setInputs(rename_fit = 2, rename_fit_name = "baseline", confirm_rename_fit = 2)
    expect_identical(names(fit_registry()), c("baseline", "fit2"))

    session$setInputs(remove_fit = 1, confirm_remove_fit = 1)
    expect_identical(names(fit_registry()), "baseline")
    expect_null(current_fit())
    # A later fit takes the next number, not one already used.
    expect_identical(keep_fit(first, ctgui_spec()), "fit3")
  }))
})
