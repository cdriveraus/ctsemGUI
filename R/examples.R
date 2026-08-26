# Worked examples --------------------------------------------------------------

# A complete project you can open and fit immediately: data, a model, and a
# note saying what to look at once it has fitted.
#
# Most examples generate their data from parameter values written down here,
# which is worth more than shipping a fixed table.  It means the example can
# tell you what the answer is, so fitting it becomes a check on whether the
# model recovers what produced the data, and it lets an example hand you a
# model that deliberately disagrees with the truth so you can see what a
# misspecified fit actually looks like.  One example uses real ctsem data,
# because generated data is always tidier than the real thing.

ctgui_example_value <- function(matrix, row, col, value) {
  list(matrix = matrix, row = row, col = col, value = value)
}

ctgui_example_catalog <- function() {
  list(
    coupled = list(
      title = "Two processes influencing each other",
      brief = "Stress and sleep, each affecting the other with different strength and sign.",
      detail = paste(
        "The starting point for most applied work. The data are generated with",
        "a strong effect of poor sleep on stress and a weaker effect the other",
        "way, so the two directions should not come out alike."
      ),
      look_for = paste(
        "Fit, then open Diagnostics > Dynamics. The cross-effect curves rise",
        "from zero, peak, and decay: read where each peaks as how long that",
        "influence takes to arrive. Compare the two directions against the true",
        "values below, and note that neither is well summarised by a single",
        "cross-lagged coefficient."
      ),
      truth = list(
        structure = "coupled", processes = c("stress", "sleep"), indicators = 1L,
        values = list(
          ctgui_example_value("DRIFT", "stress", "stress", -0.4),
          ctgui_example_value("DRIFT", "sleep", "sleep", -0.25),
          ctgui_example_value("DRIFT", "stress", "sleep", -0.35),
          ctgui_example_value("DRIFT", "sleep", "stress", 0.1),
          ctgui_example_value("DIFFUSION", "stress", "stress", 1),
          ctgui_example_value("DIFFUSION", "sleep", "sleep", 1),
          ctgui_example_value("MANIFESTVAR", "stress_1", "stress_1", 0.2),
          ctgui_example_value("MANIFESTVAR", "sleep_1", "sleep_1", 0.2)
        )
      ),
      generate = list(n.subjects = 60, Tpoints = 12, dtmean = 1, logdtsd = 0.3, burnin = 20)
    ),

    no_coupling = list(
      title = "Two processes that are not connected",
      brief = "Data with no cross-effects at all, fitted with a model that allows them.",
      detail = paste(
        "The processes are generated completely independently. The model you",
        "are given still estimates effects in both directions, so this shows",
        "what an absent effect looks like when you go looking for one."
      ),
      look_for = paste(
        "Both cross-effects should be near zero with intervals covering it.",
        "Then switch the model to Independent processes in Build and compare",
        "the two fits under Output > Fit Comparison. This is the comparison",
        "worth making before believing any cross-effect."
      ),
      truth = list(
        structure = "independent", processes = c("processA", "processB"), indicators = 1L,
        values = list(
          ctgui_example_value("DRIFT", "processA", "processA", -0.3),
          ctgui_example_value("DRIFT", "processB", "processB", -0.5),
          ctgui_example_value("DIFFUSION", "processA", "processA", 1),
          ctgui_example_value("DIFFUSION", "processB", "processB", 1),
          ctgui_example_value("MANIFESTVAR", "processA_1", "processA_1", 0.2),
          ctgui_example_value("MANIFESTVAR", "processB_1", "processB_1", 0.2)
        )
      ),
      # The model handed over is deliberately richer than the truth.
      model = list(structure = "coupled", processes = c("processA", "processB"), indicators = 1L),
      generate = list(n.subjects = 60, Tpoints = 12, dtmean = 1, logdtsd = 0.3, burnin = 20)
    ),

    growth = list(
      title = "Growth with individual differences",
      brief = "Each subject follows a smooth trajectory with their own level and rate.",
      detail = paste(
        "A latent growth curve written as a dynamic system. There is no process",
        "noise, so every subject's path is determined by where they start and",
        "how fast they change, and everything else is measurement error."
      ),
      look_for = paste(
        "Look at Data > Visuals first: the trajectories should look like",
        "straight lines with scatter around them, not like wandering series.",
        "After fitting, Diagnostics > Prediction plots shows how tightly the",
        "smoothed latent level tracks the observations."
      ),
      truth = list(
        structure = "growth", processes = "ability", indicators = 1L,
        values = list(
          ctgui_example_value("T0VAR", "ability_level", "ability_level", 1),
          ctgui_example_value("T0VAR", "ability_slope", "ability_slope", 0.3),
          ctgui_example_value("MANIFESTVAR", "ability_1", "ability_1", 0.3),
          ctgui_example_value("DRIFT", "ability_level", "ability_level", -1e-4),
          ctgui_example_value("DRIFT", "ability_slope", "ability_slope", -1e-4),
          ctgui_example_value("DIFFUSION", "ability_level", "ability_level", 1e-6),
          ctgui_example_value("DIFFUSION", "ability_slope", "ability_slope", 1e-6)
        )
      ),
      generation_note = paste(
        "The near-zero drift and diffusion entries are not part of the model:",
        "ctGenerate needs an invertible system with non-degenerate noise, and a",
        "growth curve has neither. The model you are given has them at exactly",
        "zero, which is the model actually being fitted."
      ),
      generate = list(n.subjects = 80, Tpoints = 8, dtmean = 1, logdtsd = 0.2, burnin = 0)
    ),

    oscillator = list(
      title = "A process that cycles",
      brief = "A damped oscillator: it overshoots its baseline and swings back.",
      detail = paste(
        "The velocity of the process is a latent state of its own, never",
        "observed directly. This shape cannot be expressed by a discrete-time",
        "cross-lagged model at all, which is the clearest single argument for",
        "modelling in continuous time."
      ),
      look_for = paste(
        "Diagnostics > Dynamics shows the impulse response crossing zero and",
        "coming back rather than decaying straight to it. Then fit the same",
        "data as Coupled processes in Build and look at Residual ACF: a model",
        "without the velocity state leaves the cycle in the residuals."
      ),
      truth = list(
        structure = "oscillator", processes = "mood", indicators = 1L,
        values = list(
          ctgui_example_value("DRIFT", "mood_velocity", "mood", -1),
          ctgui_example_value("DRIFT", "mood_velocity", "mood_velocity", -0.2),
          ctgui_example_value("DIFFUSION", "mood_velocity", "mood_velocity", 1),
          ctgui_example_value("MANIFESTVAR", "mood_1", "mood_1", 0.1)
        )
      ),
      generate = list(n.subjects = 40, Tpoints = 30, dtmean = 0.4, logdtsd = 0.2, burnin = 10)
    ),

    ignored_trend = list(
      title = "A trend the model does not know about",
      brief = "Trending data fitted without a trend, so you can see where it goes wrong.",
      detail = paste(
        "Both processes drift steadily upward as well as responding to each",
        "other, but the model you are given has no trend in it. Fitting data",
        "like this without accounting for the trend pushes it into the",
        "auto-effects, which then look far more persistent than they are."
      ),
      look_for = paste(
        "Fit and check Diagnostics > Residual ACF and Post Predictive: both",
        "should show the model failing to reproduce the data. Then rebuild as",
        "Coupled processes with trends in Build, refit, and compare. This is",
        "what a misspecified model looks like when the diagnostics are working."
      ),
      truth = list(
        structure = "coupled_trend", processes = c("skill", "effort"), indicators = 1L,
        values = list(
          ctgui_example_value("DRIFT", "skill", "skill", -0.4),
          ctgui_example_value("DRIFT", "effort", "effort", -0.4),
          ctgui_example_value("DRIFT", "skill", "effort", 0.2),
          ctgui_example_value("DRIFT", "effort", "skill", 0.05),
          ctgui_example_value("CINT", "skill_trend", 1, 0.3),
          ctgui_example_value("CINT", "effort_trend", 1, 0.2),
          ctgui_example_value("DIFFUSION", "skill", "skill", 1),
          ctgui_example_value("DIFFUSION", "effort", "effort", 1),
          ctgui_example_value("MANIFESTVAR", "skill_1", "skill_1", 0.2),
          ctgui_example_value("MANIFESTVAR", "effort_1", "effort_1", 0.2),
          ctgui_example_value("DRIFT", "skill_trend", "skill_trend", -1e-4),
          ctgui_example_value("DRIFT", "effort_trend", "effort_trend", -1e-4),
          ctgui_example_value("DIFFUSION", "skill_trend", "skill_trend", 1e-6),
          ctgui_example_value("DIFFUSION", "effort_trend", "effort_trend", 1e-6)
        )
      ),
      generation_note = paste(
        "The near-zero entries on the trend processes are there because",
        "ctGenerate needs an invertible system with non-degenerate noise; a",
        "constant trend has neither. They are not part of what is being fitted."
      ),
      model = list(structure = "coupled", processes = c("skill", "effort"), indicators = 1L),
      generate = list(n.subjects = 60, Tpoints = 14, dtmean = 1, logdtsd = 0.2, burnin = 5)
    ),

    real_data = list(
      title = "Real data, with predictors",
      brief = "ctsem's own test dataset: two processes, a time-dependent and a time-independent predictor.",
      detail = paste(
        "Generated data is always tidier than real data. This one has unequal",
        "intervals, missing observations and predictors of both kinds, so it is",
        "the example to open when you want to see how the data roles and the",
        "predictor matrices are actually used."
      ),
      look_for = paste(
        "Start at Data > Summary and Data > Visuals to see the missingness and",
        "the spread of time intervals before fitting anything. After fitting,",
        "Diagnostics > TI moderation shows how the subject-level predictor",
        "shifts the trajectories."
      ),
      data = list(kind = "ctsem", dataset = "ctstantestdat", label = "ctsem::ctstantestdat"),
      # Latents and manifests share a namespace in ctsem, so the processes are
      # named apart from the data columns their indicators take.
      model = list(structure = "coupled", processes = c("etaY1", "etaY2"), indicators = 1L),
      roles = list(
        id = "id", time = "time",
        manifest_names = c("Y1", "Y2"),
        tdpred_names = "TD1", tipred_names = c("TI1", "TI2", "TI3")
      )
    )
  )
}

ctgui_example_ids <- function() names(ctgui_example_catalog())

ctgui_example <- function(id) {
  catalog <- ctgui_example_catalog()
  if (length(id) != 1L || is.na(id) || !id %in% names(catalog)) {
    stop("Unknown example: ", paste(id, collapse = ", "), call. = FALSE)
  }
  example <- catalog[[id]]
  example$id <- id
  example
}

ctgui_example_blueprint <- function(plan) {
  ctgui_blueprint(
    structure = plan$structure,
    processes = plan$processes,
    indicators = plan$indicators %||% 1L
  )
}

# The model the user is handed. Where an example sets one explicitly it differs
# from the truth on purpose, so the fit has something to get wrong.
ctgui_example_spec <- function(example, base = NULL) {
  if (is.null(base)) {
    base <- ctgui_spec(latent_names = character(), manifest_names = character())
  }
  plan <- example$model %||% example$truth
  spec <- ctgui_blueprint_apply(base, ctgui_example_blueprint(plan), mode = "replace")

  roles <- example$roles
  if (!is.null(roles)) {
    spec$id <- roles$id %||% spec$id
    spec$time <- roles$time %||% spec$time
    if (!is.null(roles$manifest_names)) {
      spec <- ctgui_respec_preserving(
        spec, latent_names = spec$latent_names, manifest_names = roles$manifest_names,
        tdpred_names = roles$tdpred_names %||% character(),
        tipred_names = roles$tipred_names %||% character(),
        rename = stats::setNames(roles$manifest_names, spec$manifest_names)
      )
    }
  }
  spec
}

# The specification the data actually came from: the same shape as the model
# where they agree, with the example's parameter values written in as numbers.
ctgui_example_truth_spec <- function(example) {
  truth <- example$truth
  if (is.null(truth)) return(NULL)
  base <- ctgui_spec(latent_names = character(), manifest_names = character())
  spec <- ctgui_blueprint_apply(base, ctgui_example_blueprint(truth), mode = "replace")
  for (value in truth$values %||% list()) {
    spec <- ctgui_set_matrix_value(
      spec, value$matrix, value$row, value$col, value = value$value
    )
  }
  spec
}

#' Load a worked example's data
#'
#' @param example An entry from [ctgui_example_catalog()].
#' @return A long-format data frame.
#' @keywords internal
ctgui_example_data <- function(example) {
  if (identical(example$data$kind, "ctsem")) {
    if (!ctgui_has_ctsem()) stop("ctsem must be installed to load this example", call. = FALSE)
    # ctsem's datasets are lazy-loaded, so they are reached as exports rather
    # than as objects sitting in the namespace environment.
    return(as.data.frame(
      getExportedValue("ctsem", example$data$dataset),
      stringsAsFactors = FALSE
    ))
  }

  settings <- example$generate %||% list()
  truth <- ctgui_example_truth_spec(example)
  if (is.null(truth)) stop("This example has no data source.", call. = FALSE)

  # Seeded so the example is the same every time it is opened: a worked example
  # whose numbers move between sessions cannot be talked about.
  withr_seed <- function(code) {
    if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      saved <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
      on.exit(assign(".Random.seed", saved, envir = globalenv()), add = TRUE)
    }
    set.seed(settings$seed %||% 20260826L)
    force(code)
  }

  data <- withr_seed(ctgui_generate_data(
    truth,
    n.subjects = settings$n.subjects %||% 50,
    Tpoints = settings$Tpoints %||% 10,
    burnin = settings$burnin %||% 0,
    dtmean = settings$dtmean %||% 1,
    logdtsd = settings$logdtsd %||% 0,
    free_defaults = TRUE
  ))

  # Generated columns are named for the specification that produced them, which
  # is also the model being handed over unless the example says otherwise.
  target <- ctgui_example_spec(example)
  if (!identical(example$model, example$truth) && !is.null(example$model)) {
    generated_manifests <- ctgui_blueprint_manifest_names(ctgui_example_blueprint(example$truth))
    wanted <- ctgui_blueprint_manifest_names(ctgui_example_blueprint(example$model))
    if (length(generated_manifests) == length(wanted)) {
      names(data)[match(generated_manifests, names(data))] <- wanted
    }
  }
  names(data)[names(data) == "id"] <- target$id
  names(data)[names(data) == "time"] <- target$time
  data
}

ctgui_example_data_label <- function(example) {
  if (identical(example$data$kind, "ctsem")) return(example$data$label %||% example$data$dataset)
  paste0("Generated for the ", example$title, " example")
}

# What the data were generated from, so fitting the example is a check rather
# than a demonstration.
ctgui_example_truth_note <- function(example) {
  truth <- example$truth
  if (is.null(truth)) {
    return("This example uses real data, so there are no true parameter values to compare against.")
  }
  values <- truth$values %||% list()
  if (!length(values)) return("")
  lines <- vapply(values, function(value) {
    sprintf("  %s[%s, %s] = %s", value$matrix, value$row, value$col, format(value$value))
  }, character(1L))

  header <- if (!is.null(example$model)) {
    paste(
      "The data were generated from a different model than the one you have",
      "been given. These are the values behind the data:"
    )
  } else {
    "The data were generated from these values, so a good fit should recover them:"
  }
  paste(c(
    header, lines,
    "Everything not listed took the generation defaults.",
    if (!is.null(example$generation_note)) c("", example$generation_note)
  ), collapse = "\n")
}
