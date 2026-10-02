test_that("condition runner captures messages, warnings, and failures", {
  result <- ctgui_run_result(function() {
    message("working")
    warning("careful")
    42
  })
  expect_equal(result$value, 42)
  expect_equal(result$messages, "working")
  expect_equal(result$warnings, "careful")
  failure <- ctgui_run_result(function() stop("no fit"))
  expect_s3_class(failure$value, "error")
  expect_match(ctgui_result_text(failure, "Complete"), "no fit")
})

test_that("a counter rewritten with carriage returns is one line, as on a console", {
  # ctACFresiduals reports its bootstrap this way, once per percent.
  seen <- NULL
  result <- ctgui_run_result(function() {
    message("Timestep used: 0.5")
    for (percent in 1:100) message(sprintf("\r Y1 Y2 %3d%%", percent), appendLF = FALSE)
    message("")
    message("Estimating Hessian")
    1
  }, progress_callback = function(lines) seen <<- lines)
  expect_equal(result$messages, c("Timestep used: 0.5", " Y1 Y2 100%", "Estimating Hessian"))
  expect_equal(seen, result$messages)
})

test_that("plot collections normalize supported return shapes", {
  plot <- ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) + ggplot2::geom_point()
  single <- ctgui_plot_collection(plot)
  expect_named(single, "Plot")
  nested <- ctgui_plot_collection(list(Process = list(first = plot), plot))
  expect_equal(names(nested), c("Process / first", "Plot 2"))
  expect_length(ctgui_plot_collection(list()), 0)
  expect_length(ctgui_plot_collection(list(function() graphics::plot(1))), 1)
})

test_that("fit comparison statistics tolerate partial fits", {
  stats <- ctgui_fit_comparison_stats(list(stanfit = list(rawest = 1:3,
    transformedparsfull = list(ll = -10, llrow = matrix(0, 1, 5)), optimfit = list(value = -12))))
  expect_equal(stats$npars, 3L)
  expect_equal(stats$aic, 26)
  expect_equal(stats$bic, log(5) * 3 + 20)
})

test_that("diagnostic guards fail before any expensive ctsem invocation", {
  suppressWarnings(shiny::testServer(
    ctgui_app_server(ctgui_spec(), ctgui_help_catalog()), {
    session$setInputs(run_fit = 1)
    session$flushReact()
    expect_false(fit_busy())
    session$setInputs(run_cov_check = 1)
    expect_equal(output$diagnostics_status, "No fit diagnostics have been run.")
    session$setInputs(run_uncertainty = 1)
    expect_match(output$uncertainty_status, "No fit is available.", fixed = TRUE)
  }))
})
