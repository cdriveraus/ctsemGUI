# Model blueprints -------------------------------------------------------------

# A blueprint is a whole model described in the terms a user thinks in: how
# many processes, what they are called, how many indicators measure each, and
# what shape the dynamics take.  Turning one into a specification creates the
# latent processes, the manifest variables and every matrix cell together.
#
# The earlier builder filled in matrices but left variable names alone, so it
# could only help once the right latents and manifests already existed --
# paired level and slope processes for growth, matching trend processes for a
# trend model, all lined up positionally.  That is most of the work, and
# getting it wrong produced a model that looked plausible and was not.  Here
# the template owns its own variables, which is what makes it a template.

ctgui_blueprint_structures <- function() {
  list(
    independent = list(
      title = "Independent processes",
      latents = "single",
      summary = "Each process returns to its own baseline and is not connected to the others.",
      detail = paste(
        "Each process has a free auto-effect, which sets how quickly a",
        "disturbance fades, and its own noise. Fitted beside a model with",
        "cross-effects, it is one way to ask whether the data support those",
        "effects; a comparison that favours this model is not evidence that",
        "they are absent."
      )
    ),
    coupled = list(
      title = "Coupled processes",
      latents = "single",
      summary = "Every process can influence every other, in both directions.",
      detail = paste(
        "The continuous-time counterpart of a cross-lagged panel model. Each",
        "process has a free auto-effect and a free effect on every other",
        "process, so the implied regression coefficients are a function of the",
        "time interval rather than tied to one observed interval. System noise",
        "is correlated across processes unless you say",
        "otherwise."
      )
    ),
    coupled_trend = list(
      title = "Coupled processes with trends",
      latents = "pair",
      suffix = "trend",
      summary = "Coupled processes, each with a trend that shifts it steadily over time.",
      detail = paste(
        "Adds a trend process to each measured process. Each subject's trend",
        "starts at zero and changes at a rate of its own (the trend's CINT, which",
        "varies between subjects), and feeds into its process, so the level the",
        "process returns to moves steadily over time underneath the coupled",
        "dynamics. An unmodelled trend can be absorbed into the auto-effects,",
        "making the processes look more persistent than they are. A trend and",
        "slow dynamics can also be hard to tell apart over a short series."
      )
    ),
    growth = list(
      title = "Latent growth",
      latents = "pair",
      suffix = "slope",
      primary_suffix = "level",
      summary = "A level and a slope per process, with individual differences in both.",
      detail = paste(
        "A latent growth curve written as a dynamic system: the slope drives",
        "the level, and neither has process noise, so each subject follows a",
        "smooth trajectory set by their own starting point and rate. Subject",
        "differences live in the initial level and slope, so the level takes",
        "the place of a manifest intercept, which is fixed at zero. With no",
        "system noise, the model treats everything a straight line does not",
        "explain as measurement error."
      )
    ),
    oscillator = list(
      title = "Damped oscillator",
      latents = "pair",
      suffix = "velocity",
      summary = "A process that can swing back past its baseline and cycle.",
      detail = paste(
        "Each process gets a velocity that drives its position, while the",
        "position pulls the velocity back, which produces cycles unless the",
        "damping is strong. The damping parameter sets how quickly those cycles",
        "decay. A first-order model of a single observed process cannot produce",
        "cycles; this one does it through the unobserved velocity."
      )
    )
  )
}

ctgui_blueprint_structure <- function(structure) {
  structures <- ctgui_blueprint_structures()
  if (length(structure) != 1L || is.na(structure) || !structure %in% names(structures)) {
    stop("structure must be one of: ", paste(names(structures), collapse = ", "), call. = FALSE)
  }
  structures[[structure]]
}

#' Describe a model to build
#'
#' @param structure One of the names of `ctgui_blueprint_structures()`.
#' @param processes Names of the processes to create.
#' @param indicators Number of manifest indicators measuring each process, for
#'   any process `manifests` gives no names for.
#' @param manifests Optional list, one element per process in the same order,
#'   naming the manifest variables that measure it -- typically columns of the
#'   data. A process given none gets `indicators` numbered names.
#' @param free_noise_correlations Whether system noise may correlate across
#'   processes.
#' @param connect_existing When extending, whether effects between existing and
#'   new processes are freely estimated or held at zero.
#' @return A `ctsemgui_blueprint`.
#' @keywords internal
ctgui_blueprint <- function(structure = "coupled", processes = c("process1", "process2"),
    indicators = 1L, free_noise_correlations = TRUE, connect_existing = FALSE,
    indicator_type = 0L, indicator_ncategories = 5L,
    indicator_censormin = NA_real_, indicator_censormax = NA_real_,
    manifests = NULL) {
  ctgui_blueprint_structure(structure)
  processes <- ctgui_as_names(processes, "processes")
  if (!length(processes)) stop("Name at least one process.", call. = FALSE)
  if (anyDuplicated(processes)) stop("Process names must be unique.", call. = FALSE)
  indicators <- suppressWarnings(as.integer(indicators))
  if (length(indicators) != 1L || is.na(indicators) || indicators < 1L) {
    stop("indicators must be a whole number of at least 1.", call. = FALSE)
  }
  # A template creates every indicator alike, so the measurement model is one
  # choice rather than one per variable. Anything more specific is an edit
  # afterwards, in the Specification panel or the visual editor.
  indicator_type <- suppressWarnings(as.integer(indicator_type))
  if (is.na(indicator_type)) indicator_type <- 0L
  ctgui_manifest_type_entry(indicator_type)

  blueprint <- structure(
    list(
      structure = structure,
      processes = processes,
      indicators = indicators,
      free_noise_correlations = isTRUE(free_noise_correlations),
      connect_existing = isTRUE(connect_existing),
      indicator_type = indicator_type,
      indicator_ncategories = suppressWarnings(as.integer(indicator_ncategories)),
      indicator_censormin = suppressWarnings(as.numeric(indicator_censormin)),
      indicator_censormax = suppressWarnings(as.numeric(indicator_censormax))
    ),
    class = "ctsemgui_blueprint"
  )

  # Names chosen for a process replace its numbered ones. Checked here rather
  # than at apply, so the builder can say what is wrong while it is typed.
  manifests <- rep_len(as.list(manifests %||% list()), length(processes))
  blueprint$manifests <- stats::setNames(lapply(seq_along(processes), function(i) {
    chosen <- ctgui_as_names(manifests[[i]], "manifests", allow_empty = TRUE)
    if (length(chosen)) chosen else paste0(processes[i], "_", seq_len(indicators))
  }), processes)
  all_manifests <- ctgui_blueprint_manifest_names(blueprint)
  repeated <- unique(all_manifests[duplicated(all_manifests)])
  if (length(repeated)) {
    stop("Each manifest variable can measure only one process: ",
      paste(repeated, collapse = ", "), ".", call. = FALSE)
  }
  # ctsem keeps latent and manifest names in one namespace.
  shared <- intersect(all_manifests, ctgui_blueprint_latent_names(blueprint))
  if (length(shared)) {
    stop(paste(shared, collapse = ", "), " cannot name both a process and a manifest variable.",
      call. = FALSE)
  }
  blueprint
}

# The measurement description the blueprint gives every manifest it creates.
ctgui_blueprint_measurement <- function(blueprint, manifest_names) {
  n <- length(manifest_names)
  type <- blueprint$indicator_type %||% 0L
  needs <- ctgui_manifest_type_needs(type)
  ctgui_normalize_measurement(
    manifest_names,
    rep(type, n),
    if (identical(needs, "ncategories")) rep(blueprint$indicator_ncategories %||% 0L, n) else NULL,
    if (identical(needs, "censor")) rep(blueprint$indicator_censormin %||% -Inf, n) else NULL,
    if (identical(needs, "censor")) rep(blueprint$indicator_censormax %||% Inf, n) else NULL
  )
}

# Latent names are the process names, with a second latent appended for the
# structures that need one.  Manifests nobody named are always numbered,
# whatever the indicator count, so the naming rule does not change shape with k.
ctgui_blueprint_latents <- function(blueprint) {
  definition <- ctgui_blueprint_structure(blueprint$structure)
  if (identical(definition$latents, "single")) {
    return(stats::setNames(as.list(blueprint$processes), blueprint$processes))
  }
  primary_suffix <- definition$primary_suffix
  stats::setNames(lapply(blueprint$processes, function(process) {
    primary <- if (is.null(primary_suffix)) process else paste0(process, "_", primary_suffix)
    c(primary, paste0(process, "_", definition$suffix))
  }), blueprint$processes)
}

# The latent an indicator actually measures: the position, not the velocity;
# the level, not the slope.
ctgui_blueprint_measured_latent <- function(blueprint, process) {
  ctgui_blueprint_latents(blueprint)[[process]][1L]
}

ctgui_blueprint_manifests <- function(blueprint) {
  if (!is.null(blueprint$manifests)) return(blueprint$manifests)
  stats::setNames(lapply(blueprint$processes, function(process) {
    paste0(process, "_", seq_len(blueprint$indicators))
  }), blueprint$processes)
}

ctgui_blueprint_latent_names <- function(blueprint) {
  unname(unlist(ctgui_blueprint_latents(blueprint), use.names = FALSE))
}

ctgui_blueprint_manifest_names <- function(blueprint) {
  unname(unlist(ctgui_blueprint_manifests(blueprint), use.names = FALSE))
}

ctgui_blueprint_cell <- function(matrix, row, col, value) {
  list(matrix = matrix, row = row, col = col, value = as.character(value))
}

# Dynamics and system noise for the new processes.  Every cell in the block is
# written, including the zeros: a template that left cells at their defaults
# would silently inherit free parameters it never asked for.
ctgui_blueprint_dynamics_cells <- function(blueprint) {
  definition <- ctgui_blueprint_structure(blueprint$structure)
  latents <- ctgui_blueprint_latents(blueprint)
  all_latents <- ctgui_blueprint_latent_names(blueprint)
  primaries <- vapply(blueprint$processes, function(p) latents[[p]][1L], character(1L))
  cells <- list()

  add <- function(matrix, row, col, value) {
    cells[[length(cells) + 1L]] <<- ctgui_blueprint_cell(matrix, row, col, value)
  }

  for (row in all_latents) for (col in all_latents) add("DRIFT", row, col, 0)
  for (row in all_latents) for (col in all_latents) add("DIFFUSION", row, col, 0)

  coupled <- blueprint$structure %in% c("coupled", "coupled_trend")

  for (process in blueprint$processes) {
    primary <- latents[[process]][1L]
    secondary <- latents[[process]][2L]

    if (blueprint$structure %in% c("independent", "coupled", "coupled_trend")) {
      add("DRIFT", primary, primary, ctgui_auto_label("DRIFT", primary, primary))
      add("DIFFUSION", primary, primary, ctgui_auto_label("DIFFUSION", primary, primary))
    }
    if (identical(blueprint$structure, "coupled_trend")) {
      # A linear trend: the trend process starts at zero, rises at a rate of
      # its own (CINT), and feeds its process. Without the rate it is a
      # constant, which only shifts the level its process settles at -- a
      # second random intercept beside MANIFESTMEANS, and not separable from
      # it. Its initial variance goes with its initial mean; see
      # ctgui_blueprint_apply.
      add("DRIFT", primary, secondary, 1)
      add("CINT", secondary, "CINT", ctgui_auto_label("CINT", secondary, "CINT"))
      add("T0MEANS", secondary, "T0MEANS", 0)
    }
    if (identical(blueprint$structure, "growth")) {
      add("DRIFT", primary, secondary, 1)
    }
    if (identical(blueprint$structure, "oscillator")) {
      add("DRIFT", primary, secondary, 1)
      add("DRIFT", secondary, primary, ctgui_auto_label("DRIFT", secondary, primary))
      add("DRIFT", secondary, secondary, ctgui_auto_label("DRIFT", secondary, secondary))
      add("DIFFUSION", secondary, secondary, ctgui_auto_label("DIFFUSION", secondary, secondary))
    }
  }

  if (coupled) {
    for (target in primaries) for (source in primaries) {
      if (identical(target, source)) next
      add("DRIFT", target, source, ctgui_auto_label("DRIFT", target, source))
      if (blueprint$free_noise_correlations) {
        lower <- match(target, primaries) > match(source, primaries)
        if (lower) add("DIFFUSION", target, source, ctgui_auto_label("DIFFUSION", target, source))
      }
    }
  }
  cells
}

ctgui_blueprint_measurement_cells <- function(blueprint, all_manifests, all_latents) {
  manifests <- ctgui_blueprint_manifests(blueprint)
  cells <- list()
  add <- function(matrix, row, col, value) {
    cells[[length(cells) + 1L]] <<- ctgui_blueprint_cell(matrix, row, col, value)
  }

  for (row in all_manifests) for (col in all_latents) add("LAMBDA", row, col, 0)

  for (process in blueprint$processes) {
    measured <- ctgui_blueprint_measured_latent(blueprint, process)
    block <- manifests[[process]]
    for (index in seq_along(block)) {
      # The first loading fixes the latent's scale; the rest are estimated
      # relative to it.
      value <- if (index == 1L) 1 else ctgui_auto_label("LAMBDA", block[index], measured)
      add("LAMBDA", block[index], measured, value)
    }
    # A growth level never decays, so its subject-varying initial value is
    # each subject's intercept. A free MANIFESTMEANS on the same indicator
    # would be that intercept again, which the data cannot split between them.
    if (identical(blueprint$structure, "growth")) {
      add("MANIFESTMEANS", block[1L], "MANIFESTMEANS", 0)
    }
  }
  cells
}

ctgui_blueprint_apply_cells <- function(spec, cells) {
  for (cell in cells) {
    mat <- spec$matrices[[cell$matrix]]
    if (is.null(mat) || !is.matrix(mat)) next
    if (!cell$row %in% rownames(mat) || !cell$col %in% colnames(mat)) next
    mat[cell$row, cell$col] <- cell$value
    spec$matrices[[cell$matrix]] <- mat
  }
  spec
}

#' Build or extend a specification from a blueprint
#'
#' @param spec The current `ctsemgui_spec`; its data roles and options are kept.
#' @param blueprint A `ctsemgui_blueprint`.
#' @param mode `"replace"` to build a fresh model, `"extend"` to add the
#'   blueprint's processes to the existing one.
#' @return A `ctsemgui_spec`.
#' @keywords internal
ctgui_blueprint_apply <- function(spec, blueprint, mode = c("replace", "extend")) {
  ctgui_check_spec(spec)
  if (!inherits(blueprint, "ctsemgui_blueprint")) {
    stop("blueprint must be a ctsemgui_blueprint", call. = FALSE)
  }
  mode <- match.arg(mode)

  new_latents <- ctgui_blueprint_latent_names(blueprint)
  new_manifests <- ctgui_blueprint_manifest_names(blueprint)

  if (identical(mode, "extend")) {
    clashes <- c(
      intersect(new_latents, spec$latent_names),
      intersect(new_manifests, spec$manifest_names)
    )
    if (length(clashes)) {
      stop(
        "These names are already in the model: ", paste(unique(clashes), collapse = ", "),
        ". Choose different process names, or build a fresh model instead.",
        call. = FALSE
      )
    }
    latent_names <- c(spec$latent_names, new_latents)
    manifest_names <- c(spec$manifest_names, new_manifests)
  } else {
    latent_names <- new_latents
    manifest_names <- new_manifests
  }

  # Data roles, predictors and model options describe the data and the study,
  # not the dynamics, so they survive either mode.
  carrier <- spec
  if (identical(mode, "replace")) {
    carrier <- ctgui_spec(
      latent_names = spec$latent_names, manifest_names = spec$manifest_names,
      type = spec$type, id = spec$id, time = spec$time, Tpoints = spec$Tpoints,
      tdpred_names = spec$tdpred_names, tipred_names = spec$tipred_names,
      tipredDefault = spec$tipredDefault
    )
  }

  # Manifests the template is creating take its measurement model; manifests
  # that were already there keep their own, so extending a model of ordinal
  # items with a continuous one does not rewrite the items.
  new_measurement <- ctgui_blueprint_measurement(blueprint, new_manifests)
  measurement_at <- function(name, field, default) {
    made <- match(name, new_manifests)
    if (!is.na(made)) return(new_measurement[[field]][made])
    kept <- match(name, spec$manifest_names)
    if (is.na(kept) || length(spec[[field]]) < kept) return(default)
    spec[[field]][kept]
  }
  rebuilt <- ctgui_respec_preserving(
    carrier, latent_names = latent_names, manifest_names = manifest_names,
    manifest_type = vapply(manifest_names, function(name) {
      as.integer(measurement_at(name, "manifest_type", 0L))
    }, integer(1L)),
    ncategories = vapply(manifest_names, function(name) {
      as.integer(measurement_at(name, "ncategories", 0L))
    }, integer(1L)),
    censormin = vapply(manifest_names, function(name) {
      as.numeric(measurement_at(name, "censormin", -Inf))
    }, numeric(1L)),
    censormax = vapply(manifest_names, function(name) {
      as.numeric(measurement_at(name, "censormax", Inf))
    }, numeric(1L))
  )

  cells <- c(
    ctgui_blueprint_dynamics_cells(blueprint),
    ctgui_blueprint_measurement_cells(blueprint, new_manifests, latent_names)
  )

  if (identical(mode, "extend")) {
    existing_latents <- spec$latent_names
    # A resize gives brand-new cells default free labels. Between the existing
    # model and the added processes that would quietly free every connection
    # in both directions, which is a modelling decision the template has no
    # business making.
    connection <- if (blueprint$connect_existing) NULL else 0
    for (row in existing_latents) for (col in new_latents) {
      cells <- c(cells, list(ctgui_blueprint_cell("DRIFT", row, col,
        connection %||% ctgui_auto_label("DRIFT", row, col))))
      cells <- c(cells, list(ctgui_blueprint_cell("DIFFUSION", row, col, 0)))
    }
    for (row in new_latents) for (col in existing_latents) {
      cells <- c(cells, list(ctgui_blueprint_cell("DRIFT", row, col,
        connection %||% ctgui_auto_label("DRIFT", row, col))))
      cells <- c(cells, list(ctgui_blueprint_cell("DIFFUSION", row, col, 0)))
    }
    for (row in new_manifests) for (col in existing_latents) {
      cells <- c(cells, list(ctgui_blueprint_cell("LAMBDA", row, col, 0)))
    }
    for (row in spec$manifest_names) for (col in new_latents) {
      cells <- c(cells, list(ctgui_blueprint_cell("LAMBDA", row, col, 0)))
    }
  }

  if (identical(blueprint$structure, "coupled_trend")) {
    # Every subject's trend starts at zero, so it has no initial variance to
    # estimate, against the existing processes as much as the new ones.
    for (process in blueprint$processes) {
      trend <- ctgui_blueprint_latents(blueprint)[[process]][2L]
      for (other in latent_names) {
        cells <- c(cells, list(ctgui_blueprint_cell("T0VAR", trend, other, 0),
          ctgui_blueprint_cell("T0VAR", other, trend, 0)))
      }
    }
  }

  rebuilt <- ctgui_blueprint_apply_cells(rebuilt, cells)
  rebuilt <- ctgui_sync_model_from_matrices(rebuilt)

  if (identical(blueprint$structure, "growth")) {
    # Growth curves put the individual differences in where each subject starts
    # and how fast they change, rather than in the dynamics.
    # A vector matrix's metadata is keyed by the matrix name as its column; a
    # column of 1L matched nothing, and the call returned the spec unchanged.
    for (latent in new_latents) {
      rebuilt <- ctgui_set_parameter_metadata(
        rebuilt, "T0MEANS", latent, "T0MEANS", indvarying = TRUE
      )
    }
  }

  ctgui_commit_result(ctgui_commit_spec(previous = spec, updated = rebuilt, reason = "blueprint"))
}

# What the blueprint will do, in the terms the reader used to describe it.
# Applying replaces or grows the whole model, so it says so before acting.
ctgui_blueprint_summary <- function(spec, blueprint, mode = c("replace", "extend")) {
  mode <- match.arg(mode)
  definition <- ctgui_blueprint_structure(blueprint$structure)
  latents <- ctgui_blueprint_latent_names(blueprint)
  manifests <- ctgui_blueprint_manifest_names(blueprint)

  measurement <- ctgui_blueprint_measurement(blueprint, manifests)
  counts <- lengths(ctgui_blueprint_manifests(blueprint))
  lines <- c(
    paste0(definition$title, ": ", length(blueprint$processes), " process",
      if (length(blueprint$processes) == 1L) "" else "es", ", ",
      if (length(unique(counts)) == 1L) {
        paste0(counts[1L], " indicator", if (counts[1L] == 1L) "" else "s", " each.")
      } else {
        paste0(paste(counts, collapse = ", "), " indicators respectively.")
      }),
    paste0("Latent processes: ", paste(latents, collapse = ", ")),
    paste0("Manifest variables: ", paste(manifests, collapse = ", "))
  )
  if (length(manifests) && !ctgui_manifest_type_is_default(measurement$manifest_type[1L])) {
    lines <- c(lines, paste0("Measured as: ", ctgui_measurement_summary(
      manifests[1L], measurement$manifest_type[1L], measurement$ncategories[1L],
      measurement$censormin[1L], measurement$censormax[1L]
    )))
    backend_note <- ctgui_measurement_backend_message(manifests, measurement$manifest_type)
    if (nzchar(backend_note)) lines <- c(lines, backend_note)
  }

  if (identical(mode, "replace")) {
    existing <- length(spec$latent_names) + length(spec$manifest_names)
    lines <- c(lines, if (existing > 0L) {
      "This replaces the current model. Data roles, predictors and time settings are kept."
    } else {
      "Data roles, predictors and time settings are kept."
    })
  } else {
    lines <- c(lines, paste0(
      "These are added to the current model. Effects between the existing and new processes are ",
      if (blueprint$connect_existing) "freely estimated." else "fixed to zero; free them yourself where you want them."
    ))
  }
  paste(lines, collapse = "\n")
}
