test_that("uncertainty choices keep importance sampling unambiguous", {
  expect_true(all(c("hessian", "surrogate", "is", "bootstrap", "fullbootstrap", "sandwich", "opg") %in%
    unname(ctgui_uncertainty_method_choices())))
  expect_equal(unname(ctgui_uncertainty_draw_choices("is")), "imis")
  expect_true("empirical" %in% unname(ctgui_uncertainty_draw_choices("bootstrap")))
  expect_false("empirical" %in% unname(ctgui_uncertainty_draw_choices("hessian")))
})

test_that("uncertainty control normalizes optional values and fit control", {
  control <- ctgui_uncertainty_control(surrogate_npoints = "", surrogate_profile_target_drop = "")
  expect_null(control$surrogateNpoints)
  expect_null(control$surrogateProfileTargetDrop)
  optimcontrol <- ctgui_uncertainty_optimcontrol("is", "normal", finishsamples = 25, control = control)
  expect_equal(optimcontrol$uncertainty, "is")
  expect_equal(optimcontrol$uncertaintyDraws, "imis")
  expect_equal(optimcontrol$finishsamples, 25L)
  merged <- ctgui_uncertainty_merge_optimcontrol(optimcontrol, list(finishsamples = 40L))
  expect_equal(merged$finishsamples, 40L)
})

test_that("uncertainty summary exposes stored diagnostics", {
  fit <- list(stanfit = list(
    rawposterior = matrix(0, nrow = 12, ncol = 2),
    uncertainty = list(
      method = "is", draws = "imis",
      details = list(
        importance_sampling = list(ess = 8.5, df_used = 4),
        proposal_covariance = list(method = "ridge_info", infoRidgeApplied = TRUE)
      )
    )
  ))
  summary <- ctgui_uncertainty_summary(fit)
  expect_match(summary, "Method: is", fixed = TRUE)
  expect_match(summary, "Approximate draws: 12", fixed = TRUE)
  expect_match(summary, "ESS: 8.5")
  expect_match(summary, "ridge", fixed = TRUE)
})
