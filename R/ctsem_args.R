# ctsem argument defaults and choices ------------------------------------------

# What the installed ctsem says about an argument the GUI exposes: its default,
# and the set of values it accepts where ctsem's own code declares one. Read
# from the function itself rather than copied here, because the GUI runs on
# more than one ctsem and the copies drifted: the GUI passed priors = FALSE on
# every fit while ctsem's default had moved on.
#
# A field left blank is not passed at all, so ctsem applies its own default;
# the default is shown in the empty field rather than written into it.

ctgui_ctsem_arg_cache <- new.env(parent = emptyenv())

ctgui_ctsem_function <- function(topic) {
  if (!ctgui_has_ctsem()) return(NULL)
  fun <- get0(topic, envir = asNamespace("ctsem"), inherits = FALSE)
  if (is.function(fun)) fun else NULL
}

# Describe one argument of an installed ctsem function
#
# @param topic Function name in ctsem.
# @param param Argument name.
# @return A list: `found`, `default_text` (deparsed default, `""` when there is
#   none), `default` (the evaluated default, or `NULL`), `logical` (whether the
#   argument takes TRUE or FALSE), and `choices` (character values ctsem's
#   code declares, via `match.arg()`).
ctgui_ctsem_arg <- function(topic, param) {
  version <- tryCatch(as.character(utils::packageVersion("ctsem")), error = function(e) "none")
  key <- paste(version, topic, param, sep = "\r")
  cached <- get0(key, envir = ctgui_ctsem_arg_cache, inherits = FALSE)
  if (!is.null(cached)) return(cached)

  out <- ctgui_function_arg(ctgui_ctsem_function(topic), param)
  assign(key, out, envir = ctgui_ctsem_arg_cache)
  out
}

ctgui_function_arg <- function(fun, param) {
  formal <- if (is.null(fun)) NULL else formals(fun)
  if (is.null(formal) || !param %in% names(formal)) {
    return(list(found = FALSE, default_text = "", default = NULL, logical = FALSE, choices = character()))
  }
  # An argument without a default holds the empty symbol, which cannot be
  # bound to a variable without R reporting it missing; test it in place.
  missing_default <- identical(formal[[param]], quote(expr = ))
  default_text <- if (missing_default) "" else paste(deparse(formal[[param]]), collapse = " ")
  default <- if (missing_default) NULL else
    tryCatch(eval(formal[[param]], envir = baseenv()), error = function(e) NULL)

  body_text <- paste(deparse(body(fun)), collapse = "\n")
  name <- gsub(".", "\\.", param, fixed = TRUE)
  logical <- is.logical(default) && length(default) == 1L && !is.na(default) ||
    grepl(paste0("(is\\.logical|isTRUE|isFALSE)\\(\\s*", name, "\\s*\\)"), body_text)

  # match.arg(param, <choices>) names them; match.arg(param) takes them from a
  # vector default. A vector default without match.arg is a selection, such as
  # kalmanvec's c("y", "yprior"), not the set of what is allowed.
  choices <- ctgui_match_arg_choices(body_text, name, environment(fun))
  if (identical(choices, TRUE)) choices <- if (is.character(default)) default else character()
  # Or a check that every value is in a set, all(param %in% <set>), as
  # plot.ctKalmanDF checks kalmanvec.
  if (!length(choices)) {
    set <- regmatches(body_text, regexec(paste0("all\\(\\s*", name, "\\s*%in%\\s*([^()]+?)\\s*\\)"), body_text))[[1L]]
    if (length(set) == 2L) {
      choices <- tryCatch(as.character(eval(str2lang(set[2L]), envir = environment(fun))),
        error = function(e) character())
    }
  }
  # Or the argument is handed whole to a one-argument ctsem function that
  # resolves it, as ctDiscretePars hands impulseType to .ctCompanionType.
  if (!length(choices)) {
    helpers <- regmatches(body_text, gregexpr(paste0("[.A-Za-z][.A-Za-z0-9_]*\\(\\s*", name, "\\s*\\)"), body_text))[[1L]]
    for (helper in unique(sub("\\(.*$", "", helpers))) {
      if (helper %in% c("match.arg", "missing", "is.logical", "isTRUE", "isFALSE")) next
      hfun <- get0(helper, envir = environment(fun), inherits = FALSE)
      if (!is.function(hfun) || length(formals(hfun)) != 1L) next
      found <- ctgui_match_arg_choices(paste(deparse(body(hfun)), collapse = "\n"), NULL, environment(hfun))
      if (is.character(found) && length(found)) {
        choices <- found
        break
      }
    }
  }
  list(found = TRUE, default_text = default_text, default = default,
    logical = isTRUE(logical), choices = unique(choices))
}

# The choices of a match.arg() call on `name` (any argument when NULL),
# evaluated where the function lives, since they are often a package constant.
# TRUE when the call names no choices, so they come from the formal's default.
ctgui_match_arg_choices <- function(body_text, name, envir) {
  target <- if (is.null(name)) "[^,)]+" else paste0("\\s*", name, "\\b")
  calls <- regmatches(body_text, gregexpr(paste0("match\\.arg\\(", target, "[^\n]*"), body_text))[[1L]]
  if (!length(calls)) return(character())
  if (is.null(name) && length(calls) != 1L) return(character())
  parsed <- tryCatch(str2lang(sub("^(match\\.arg\\(.*\\)).*$", "\\1", calls[1L])), error = function(e) NULL)
  if (!is.call(parsed)) return(character())
  if (length(parsed) < 3L) return(TRUE)
  tryCatch(as.character(eval(parsed[[3L]], envir = envir)), error = function(e) character())
}

ctgui_help_arg <- function(help_catalog, help_id) {
  help <- help_catalog[[help_id]]
  if (is.null(help$topic) || is.null(help$param)) return(NULL)
  ctgui_ctsem_arg(help$topic, help$param)
}

# The value ctsem uses when the argument is not given. A match.arg() vector
# default is the set of choices, and the first is used; any other vector
# default, such as kalmanvec's, is used whole.
ctgui_arg_effective_default <- function(arg) {
  if (is.null(arg)) return(NULL)
  if (length(arg$default) > 1L && identical(as.character(arg$default), arg$choices)) {
    return(arg$default[1L])
  }
  arg$default
}

ctgui_arg_placeholder <- function(arg) {
  if (is.null(arg) || !isTRUE(arg$found)) return("")
  if (!nzchar(arg$default_text)) return("no default")
  if (length(arg$default) > 1L && length(ctgui_arg_effective_default(arg)) == 1L) {
    return(paste0("default: \"", ctgui_arg_effective_default(arg), "\""))
  }
  paste("default:", arg$default_text)
}

# The values a select offers for an argument: its own default first, then the
# choices ctsem declares, then TRUE and FALSE where it takes a logical.
ctgui_arg_options <- function(arg) {
  if (is.null(arg) || !isTRUE(arg$found)) return(character())
  default <- arg$default
  first <- if (length(default) == 1L && !is.null(default) && !is.na(default)) as.character(default) else character()
  unique(c(first, arg$choices, if (isTRUE(arg$logical)) c("TRUE", "FALSE")))
}

# Shiny's numericInput takes no placeholder, so it is set on the input tag.
ctgui_set_placeholder <- function(tag, placeholder) {
  if (!nzchar(placeholder)) return(tag)
  if (inherits(tag, "shiny.tag")) {
    if (identical(tag$name, "input")) {
      tag$attribs$placeholder <- placeholder
      return(tag)
    }
    tag$children <- lapply(tag$children, ctgui_set_placeholder, placeholder = placeholder)
  } else if (is.list(tag)) {
    tag <- lapply(tag, ctgui_set_placeholder, placeholder = placeholder)
  }
  tag
}

# Input builders for ctsem arguments
#
# A text, numeric or session-filled field starts empty and shows the installed
# ctsem's default as placeholder text; a checkbox, or a dropdown of the values
# ctsem declares, starts at that default. Either way a field left at the
# default is not passed.
ctgui_arg_text_input <- function(id, label, help_catalog, help_id) {
  shiny::textInput(id, label, value = "",
    placeholder = ctgui_arg_placeholder(ctgui_help_arg(help_catalog, help_id)))
}

ctgui_arg_numeric_input <- function(id, label, help_catalog, help_id, ...) {
  ctgui_set_placeholder(shiny::numericInput(id, label, value = NA, ...),
    ctgui_arg_placeholder(ctgui_help_arg(help_catalog, help_id)))
}

# `fallback` is the state when the installed ctsem does not say, as when the
# argument is missing or its default is not a plain TRUE or FALSE.
ctgui_arg_checkbox_input <- function(id, label, help_catalog, help_id, fallback = FALSE) {
  default <- ctgui_help_arg(help_catalog, help_id)$default
  value <- if (is.logical(default) && length(default) == 1L && !is.na(default)) default else fallback
  shiny::checkboxInput(id, label, value = value)
}

# @param session_choices Whether the server fills the choices from the session
#   -- subject numbers, data columns. Such a field starts empty, shows the
#   default, and accepts typed values too. A field whose values ctsem declares
#   is a plain dropdown showing its default.
# @param multiple Whether more than one value may be chosen.
# @param choices Values offered beyond those the argument itself declares.
ctgui_arg_select_input <- function(id, label, help_catalog, help_id, session_choices = FALSE,
    multiple = FALSE, choices = character()) {
  arg <- ctgui_help_arg(help_catalog, help_id)
  options <- unique(c(ctgui_arg_options(arg), choices))
  # A plain dropdown starts on ctsem's default, so it can only be one where
  # that default is among the options; otherwise its first option would be
  # passed as though chosen. Without one, the field is free entry, blank.
  default <- ctgui_arg_effective_default(arg)
  shows_default <- length(default) == 1L && !is.na(default) && as.character(default) %in% options
  if (!session_choices && !multiple && shows_default) {
    # Closed only where ctsem's code declares the whole set. A logical, or a
    # default beside TRUE and FALSE, says nothing of what else is accepted --
    # intoverpop takes 'laplace' beside 'auto', TRUE and FALSE -- so other
    # values can be typed.
    if (length(arg$choices)) {
      return(shiny::selectInput(id, label, choices = options, selected = as.character(default)))
    }
    return(shiny::selectizeInput(id, label, choices = options, selected = as.character(default),
      options = list(create = TRUE, persist = FALSE)))
  }
  # A single select with nothing chosen takes its first option in the browser,
  # which would then be passed as though chosen; an empty first option keeps
  # it blank, showing the placeholder.
  shiny::selectizeInput(id, label, choices = if (multiple) options else c("", options),
    selected = character(), multiple = multiple,
    options = list(create = TRUE, persist = FALSE, placeholder = ctgui_arg_placeholder(arg)))
}

# Refill a session_choices field, keeping what is selected.
ctgui_update_arg_choices <- function(session, id, choices, selected, arg) {
  selected <- as.character(selected %||% character())
  shiny::updateSelectizeInput(session, id,
    choices = unique(c(ctgui_arg_options(arg), as.character(choices), selected)),
    selected = selected)
}

# ctsem 3.12 renamed ctGenerate's n.subjects to n, and warns on the old name;
# the GUI keeps n.subjects as its own option name and passes whichever the
# installed ctsem takes.
ctgui_generate_count_param <- function() {
  if (isTRUE(ctgui_ctsem_arg("ctGenerate", "n")$found)) "n" else "n.subjects"
}

# ctsem 3.12 replaced ctDiscretePars' observational with impulseType; the
# Dynamics field takes whichever the installed ctsem has.
ctgui_dynamics_impulse_param <- function() {
  if (isTRUE(ctgui_ctsem_arg("ctDiscretePars", "impulseType")$found)) "impulseType" else "observational"
}

ctgui_dynamics_impulse_input <- function(help_catalog) {
  param <- ctgui_dynamics_impulse_param()
  help_id <- paste0("help_dynamic_", param)
  label <- ctgui_arg_label(help_catalog, param, help_id, paste("ctDiscretePars argument:", param))
  if (identical(param, "impulseType")) {
    ctgui_arg_select_input("dynamic_impulse", label, help_catalog, help_id)
  } else {
    ctgui_arg_checkbox_input("dynamic_impulse", label, help_catalog, help_id)
  }
}

# Read an argument field: NULL when it should not be passed
#
# @param value The input's value.
# @param arg The argument's description, so a selection equal to ctsem's own
#   default is not passed either.
ctgui_arg_value <- function(value, arg = NULL) {
  if (is.null(value) || !length(value)) return(NULL)
  if (length(value) == 1L && (is.na(value) || (is.character(value) && !nzchar(trimws(value))))) return(NULL)
  default <- ctgui_arg_effective_default(arg)
  if (!is.character(value)) {
    if (!is.null(arg) && identical(value, default)) return(NULL)
    return(value)
  }
  if (!is.null(arg) && length(default) == 1L && length(value) == 1L &&
      identical(trimws(value), as.character(default))) return(NULL)
  if (length(value) > 1L) {
    numbers <- suppressWarnings(as.numeric(value))
    return(if (all(!is.na(numbers))) numbers else value)
  }
  text <- trimws(value)
  if (text %in% c("TRUE", "FALSE")) return(as.logical(text))
  parsed <- tryCatch(eval(parse(text = text), envir = baseenv()), error = function(e) e)
  if (inherits(parsed, "error") || is.function(parsed)) text else parsed
}
