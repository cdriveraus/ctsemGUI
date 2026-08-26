# Layered panel explanations ---------------------------------------------------

# Two audiences use the same screens.  Someone meeting continuous-time models
# for the first time needs to know what a panel is for and what the result
# means; someone who has fitted a hundred of them needs the controls and
# nothing else.  Rather than choosing one, every explanation is written at two
# levels and the reader picks.
#
# `brief` is a single orienting line.  `detail` is the teaching text: what the
# panel is really doing, how to read its output, and what would make you
# distrust it.  Both carry a class the browser can hide, so switching levels
# costs no server round trip and no re-render.

ctgui_explanation_catalog <- function() {
  list(
    spec_data = list(
      brief = "Tell ctsem which column plays which role in your data.",
      detail = paste(
        "Every row of the data is one subject observed at one time. The ID column",
        "says who, the time column says when, and manifest variables are what was",
        "measured. Time is a real quantity here, not a wave number: intervals may",
        "differ between people and between occasions, and the model uses the actual",
        "values. Time-dependent predictors are covariates that change within a",
        "person and push the processes at the moment they occur. Time-independent",
        "predictors are stable subject characteristics that shift parameters up or",
        "down between people. Names typed here need not exist in the data yet, so a",
        "model can be specified before its data arrive."
      )
    ),
    matrices = list(
      brief = "Fixed numbers hold a cell constant; a label makes it a free parameter to estimate.",
      detail = paste(
        "Each cell is either a number you are asserting or a name you are asking",
        "ctsem to estimate. Two cells sharing a label share one estimate, which is",
        "how equality constraints are expressed. Select a free cell to set whether",
        "it varies between subjects, which transform keeps it in a sensible range,",
        "and which time-independent predictors moderate it. Appending ||FALSE to a",
        "label turns off subject variation where ctsem supports it."
      )
    ),
    raw_visuals = list(
      brief = "Look at trajectories, relationships, time gaps and missingness before fitting.",
      detail = paste(
        "Most fitting problems are visible here first. Check that trajectories look",
        "like the process you think you are modelling, that the time gaps are what",
        "you expect, and that missingness is not concentrated in particular people",
        "or occasions. A variable measured on a very different scale from the rest",
        "makes optimisation harder, and a process observed only a few times per",
        "subject will not support subject-level variation in its dynamics."
      )
    ),
    model_visuals = list(
      brief = "See what the current model structure implies before any fit is run.",
      detail = paste(
        "These views are drawn from the specification alone, so they show what you",
        "have asked for rather than what the data support. Use them to confirm that",
        "the paths you meant to free are free and that nothing is connected that",
        "should not be."
      )
    ),
    fit_registry = list(
      brief = "Store fitted models here to compare several candidate specifications.",
      detail = paste(
        "Each stored fit keeps the specification that produced it, so the comparison",
        "table can show what actually differs between candidates rather than only",
        "their names and fit statistics. Comparing models fitted to different data,",
        "or with different variables, tells you very little; comparing nested",
        "specifications on the same data is the case these numbers are for."
      )
    ),
    kalman = list(
      brief = "Compare observed data against model predictions and smoothed latent states.",
      detail = paste(
        "Predicted values use only information up to each time point, so they show",
        "how well the model forecasts. Smoothed states use the whole series and show",
        "the model's best account of what the latent process was doing. Systematic",
        "gaps between observed and predicted in particular people or stretches of",
        "time point at structure the model is missing."
      )
    ),
    postpred = list(
      brief = "Compare patterns in your data against data generated from the fitted model.",
      detail = paste(
        "If the fitted model were true, data generated from it should look like the",
        "data you collected. Where the observed pattern falls outside the generated",
        "range, the model cannot reproduce something real about your data. This",
        "catches misspecification that likelihood comparisons between two similar",
        "models will not."
      )
    ),
    acf = list(
      brief = "Residual autocorrelation shows predictable structure the model has not explained.",
      detail = paste(
        "A correctly specified model leaves residuals with no remaining time",
        "dependence. Autocorrelation that survives usually means the model order is",
        "too low for the process, or that a trend or cyclical component is missing.",
        "In continuous time the autocorrelation is computed over actual elapsed",
        "time rather than over observation index."
      )
    ),
    dynamics = list(
      brief = "Impulse responses show how effects propagate and decay over time.",
      detail = paste(
        "A continuous-time model implies a different set of regression coefficients",
        "at every time interval, and these plots show that whole curve rather than",
        "one arbitrary lag. Read the auto-effect curve as persistence: how long a",
        "disturbance takes to fade. Read a cross-effect curve as transmission: how",
        "long one process takes to move another, and when that influence peaks.",
        "Compare the interval range shown here against the intervals you actually",
        "observed, because the curve outside that range is extrapolation."
      )
    ),
    generation = list(
      brief = "Generate data from the current model to see what it implies.",
      detail = paste(
        "Generating from a specification is the quickest way to find out whether it",
        "describes the process you have in mind, and it needs no real data. It is",
        "also how you check that a model can be recovered at all: fit the generated",
        "data and see whether the estimates return the values you generated from.",
        "TDPREDMEANS and TDPREDVAR describe the predictors being generated here and",
        "are not fitted parameters."
      )
    ),
    uncertainty = list(
      brief = "How the interval around each estimate is worked out.",
      detail = paste(
        "The Hessian method reads curvature at the optimum and is fast, but it",
        "assumes the likelihood is well approximated by a quadratic there, which",
        "gets worse for parameters near a boundary. Importance sampling and full",
        "bootstrap relax that assumption at substantially greater cost. If the",
        "methods disagree markedly, trust the more expensive one and treat the",
        "disagreement as a sign the likelihood is awkwardly shaped."
      )
    ),
    validation = list(
      brief = "Checks that the specification can be turned into a model ctsem will accept.",
      detail = paste(
        "These are structural checks on the specification: matrix dimensions,",
        "names that do not resolve, cells ctsem will reject. Passing them means the",
        "model is well formed, not that it is identified or that your data can",
        "support it. Whether the parameters can actually be estimated shows up",
        "during fitting and in the uncertainty around the estimates."
      )
    )
  )
}

# The brief line is always present; detail appears only at the enhanced level.
# Both are marked so the browser can switch levels without asking the server.
ctgui_explanation_ui <- function(key, catalog = ctgui_explanation_catalog()) {
  entry <- catalog[[key]]
  if (is.null(entry)) return(NULL)
  shiny::div(
    class = "ctgui-explain",
    if (!is.null(entry$brief)) shiny::tags$p(class = "help-note", entry$brief),
    if (!is.null(entry$detail)) {
      shiny::tags$p(class = "help-note ctgui-explain-detail", entry$detail)
    }
  )
}

ctgui_explanation_keys <- function(catalog = ctgui_explanation_catalog()) {
  names(catalog)
}
