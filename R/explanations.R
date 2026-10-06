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
    measurement = list(
      brief = "How each observed variable relates to the process behind it.",
      detail = paste(
        "Treating an ordinal item or a count as continuous can mislead a dynamic",
        "model: it puts the measurement error on the wrong scale and lets the",
        "model predict values the instrument could never produce. Two of these",
        "types need something the data cannot supply. An ordinal variable needs",
        "its number of categories before any data is seen, because the model has",
        "to know how many thresholds to estimate. A censored variable needs the",
        "limits of the instrument, which are known constants rather than",
        "parameters. Both are asked for beside the type. Everything except",
        "continuous and binary is fitted by the Julia engine only."
      )
    ),
    report = list(
      brief = "The whole analysis as a Quarto document you can run, edit and publish.",
      detail = paste(
        "The report contains the model, the code that produced every result,",
        "the diagnostics you ran, the warnings and the readings, in the order",
        "they belong in a write-up rather than the order you happened to click",
        "them. Because the results come from code rather than being copied in,",
        "rendering the document reproduces the analysis instead of describing",
        "it. It downloads as Quarto source, so it needs no Quarto installation",
        "to produce and you can edit it before rendering."
      )
    ),
    interpretation = list(
      brief = "What the estimates say, in the time units you collected your data in.",
      detail = paste(
        "These are readings, not conclusions, and they are here rather than",
        "beside the estimates so they never stand in for looking at them. Each",
        "one is arithmetic on the fitted drift matrix at its point estimate:",
        "half-lives, the size of an effect over your median observation",
        "interval, and the interval at which each effect is largest. The",
        "uncertainty around all of it is in the fit summary and the dynamics",
        "plot, which is where to go before quoting any of these numbers.",
        "What the model implies at intervals outside the range you observed",
        "rests on the model's form more than on the data."
      )
    ),
    generate_from_fit = list(
      brief = "Data simulated from the fitted model, which most diagnostics need.",
      detail = paste(
        "Posterior predictive checks and the covariance check both compare your",
        "data against data the fitted model produces, so they cannot run until",
        "this has. It happens automatically after a fit unless you turn that off",
        "in Fit settings, and runs in the same background process as the fit,",
        "which already has the model compiled. More samples give steadier",
        "diagnostics and take longer."
      )
    ),
    fitting = list(
      brief = "Fitting runs in a separate process, so the interface stays usable and can be stopped.",
      detail = paste(
        "The messages below are the fit's own output, shown live while it runs,",
        "and ctsem reports there when it has doubts about the optimum or the",
        "uncertainty around it. Those reports are worth reading before the",
        "estimates. Because the fit runs in its own process you can keep reading",
        "the model, the data or the equations while it runs. Stop ends that",
        "process, so the next fit starts a fresh one and compiles its model",
        "again; nothing else in this session is lost. Turning background fitting",
        "off runs it in this session instead, which freezes the interface until",
        "it finishes and cannot be stopped."
      )
    ),
    history = list(
      brief = "Every change to the model, most recent first, and a way back to any of them.",
      detail = paste(
        "Undo and redo move one step at a time; Go to step jumps straight to an",
        "earlier state. Changing the model from an earlier step discards the",
        "steps that came after it, the same way any editor behaves. Only the",
        "model is tracked: data, fits and diagnostics are not, and going back",
        "clears the current fit because it no longer belongs to the model on",
        "screen."
      )
    ),
    examples = list(
      brief = "Complete projects you can open and fit straight away.",
      detail = paste(
        "Each example brings its own data, a model, and a note on what to look",
        "at once it has fitted. Most generate their data from parameter values",
        "the example tells you, so fitting becomes a check on whether the model",
        "recovers what produced the data rather than a demonstration you have to",
        "take on trust. Two of them hand you a model that differs from the one",
        "that generated the data on purpose: one has effects the truth lacks,",
        "the other lacks a part of the truth."
      )
    ),
    build = list(
      brief = "Start from a standard model shape, or add more processes to the model you have.",
      detail = paste(
        "Build a fresh model and the template creates its own latent processes,",
        "manifest variables and every matrix cell together, so you do not have",
        "to set up the variables first; anything already there is replaced. Add",
        "processes instead and the template describes only the new ones: your",
        "existing processes keep their shape and their parameters, and the",
        "effects between old and new start fixed at zero so you decide which",
        "connections to free. Either way you end up with a complete model that",
        "ctsem will accept and that you can edit anywhere else; whether your data",
        "can identify it is a separate question. Templates are starting points,",
        "not recommendations."
      )
    ),
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
        "Some problems are easier to see here than after fitting: trajectories",
        "that do not look like the process you think you are modelling, time gaps",
        "that are not what you expect, missingness concentrated in particular",
        "people or occasions. A variable measured on a very different scale from",
        "the rest can make optimisation harder."
      )
    ),
    fit_registry = list(
      brief = "Every fit is kept here as it completes, so candidate specifications can be compared.",
      detail = paste(
        "Fits stay in memory for the session; remove those no longer needed.",
        "Each fit keeps the specification that produced it, so the comparison",
        "table can show what actually differs between candidates rather than only",
        "their names and fit statistics. Likelihoods and information criteria are",
        "comparable only between models fitted to the same data and variables;",
        "nested specifications are the clearest case, and even there a difference",
        "is evidence about these data rather than a verdict on the process."
      )
    ),
    kalman = list(
      brief = "Compare observed data against model predictions and smoothed latent states.",
      detail = paste(
        "Predicted values use only the observations before each time point, so",
        "they show how well the model forecasts. Smoothed states use the whole",
        "series and show the model's account of what the latent process was",
        "doing, which is only as good as the model. Even a correct model leaves",
        "gaps between observed and predicted, because new process noise and",
        "measurement error cannot be forecast; gaps that persist in particular",
        "people or stretches of time may point to structure the model is missing."
      )
    ),
    postpred = list(
      brief = "Compare patterns in your data against data generated from the fitted model.",
      detail = paste(
        "If the fitted model were a good description, data generated from it",
        "would look broadly like the data you collected. Where the observed",
        "pattern falls well outside the generated range, the model may not be",
        "reproducing something about your data -- though with many patterns",
        "checked, an occasional one will fall outside by chance. This can catch",
        "misspecification that a likelihood comparison between two similar",
        "models would miss."
      )
    ),
    acf = list(
      brief = "Autocorrelation in the one-step-ahead prediction errors, over elapsed time.",
      detail = paste(
        "The residuals are standardised prediction errors, each made before its",
        "observation was seen. If the model captured the time dependence in the",
        "data, little of it would remain in them. Autocorrelation that remains could",
        "reflect dynamics the model leaves out -- another process, a trend, a",
        "cycle -- or individual differences it does not allow for, and it can",
        "also arise from a few unusual subjects or by chance, so treat it as a",
        "prompt to look further rather than a diagnosis. The autocorrelation is",
        "approximated over elapsed time rather than over observation index."
      )
    ),
    dynamics = list(
      brief = "The regression coefficients the fitted model implies at each time interval.",
      detail = paste(
        "A continuous-time model implies a different set of regression coefficients",
        "at every time interval, and these plots show them across a range of",
        "intervals rather than at a single lag. Each curve is the expected change",
        "in one process after a unit change in another (or in itself), with the",
        "other processes unchanged at the start; it includes effects that pass",
        "through them. Other impulse types (impulseType, or observational on ctsem",
        "before 3.12) let the starting change bring changes in other processes with",
        "it, as ctDiscretePars' help describes. The units are the processes' own, so the size of an effect in one",
        "direction is not directly comparable with the other unless the processes",
        "are on a common scale. These are the model's implications, so they are",
        "only as good as its specification, and the curve outside the intervals",
        "you actually observed is extrapolation."
      )
    ),
    generation = list(
      brief = "Generate data from the current model to see what it implies.",
      detail = paste(
        "Generating from a specification is a quick way to see whether it",
        "describes the process you have in mind, and it needs no real data. It is",
        "also a way to check whether a model could be recovered from data like",
        "yours: fit the generated data and see whether the estimates come back",
        "near the values you generated from. TDPREDMEANS and TDPREDVAR describe",
        "the predictors being generated here and are not fitted parameters."
      )
    ),
    uncertainty = list(
      brief = "How the interval around each estimate is worked out.",
      detail = paste(
        "The Hessian method reads curvature at the optimum and is fast, but it",
        "assumes the likelihood is close to quadratic there, which can fail for",
        "parameters near a boundary or weakly identified by the data. Importance",
        "sampling and the bootstrap methods make fewer of those assumptions at",
        "greater cost, and have limits of their own: importance sampling can end",
        "with few effective draws, and a bootstrap needs enough subjects to",
        "resample. The methods also answer slightly different questions, so they",
        "can disagree for several reasons: a likelihood far from quadratic, too",
        "few draws or resamples, or a misspecified model, under which curvature",
        "and between-subject resampling no longer agree. Marked disagreement is a",
        "reason for caution with the intervals."
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
