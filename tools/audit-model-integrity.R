# Model-integrity audit: for every example and template, checks that what ctsem
# is handed matches what the GUI shows, and that edits which should change
# nothing (no-op commits in each editor, reorders, save and reload, exported
# code) leave it alone. Prints one line per finding; FINDINGS 0 is clean.
# tests/testthat/test-model-integrity.R pins the cases it has found.
#   Rscript tools/audit-model-integrity.R <ctsemgui tree> [library]

args <- commandArgs(TRUE)
if (length(args) >= 2 && nzchar(args[2])) .libPaths(c(args[2], .libPaths()))
suppressMessages(pkgload::load_all(args[1], quiet = TRUE))
cat("ctsem", as.character(packageVersion("ctsem")), "\n")
quiet <- function(x) suppressWarnings(suppressMessages(x))
findings <- list()
finding <- function(scenario, check, detail) {
  findings[[length(findings) + 1L]] <<- data.frame(scenario = scenario, check = check,
    detail = substr(detail, 1, 400), stringsAsFactors = FALSE)
}

# What ctsem is handed, one row per free parameter, by parameter name.
fingerprint <- function(spec) {
  m <- tryCatch(quiet(ctgui_to_ctsem_model(spec)), error = function(e) e)
  if (inherits(m, "error")) return(m)
  p <- m$pars
  p <- p[!is.na(p$param) & nzchar(p$param), , drop = FALSE]
  eff <- grep("_effect$", names(p), value = TRUE)
  out <- data.frame(param = p$param, matrix = p$matrix,
    transform = gsub("\\s+", "", as.character(p$transform)),
    indvarying = as.logical(p$indvarying), sdscale = as.numeric(p$sdscale), stringsAsFactors = FALSE)
  for (e in eff) out[[e]] <- as.logical(p[[e]])
  out <- unique(out)
  out[order(out$matrix, out$param), , drop = FALSE]
}
same <- function(a, b) {
  if (inherits(a, "error") || inherits(b, "error")) return(FALSE)
  rownames(a) <- rownames(b) <- NULL
  isTRUE(all.equal(a, b, check.attributes = FALSE))
}
diff_text <- function(a, b) {
  if (inherits(a, "error")) return(paste("before errors:", conditionMessage(a)))
  if (inherits(b, "error")) return(paste("after errors:", conditionMessage(b)))
  key <- function(x) paste(x$matrix, x$param)
  out <- character()
  gone <- setdiff(key(a), key(b)); new <- setdiff(key(b), key(a))
  if (length(gone)) out <- c(out, paste("lost:", paste(head(gone, 6), collapse = ", ")))
  if (length(new)) out <- c(out, paste("gained:", paste(head(new, 6), collapse = ", ")))
  both <- intersect(key(a), key(b))
  for (k in both) {
    ra <- a[key(a) == k, , drop = FALSE][1, ]; rb <- b[key(b) == k, , drop = FALSE][1, ]
    for (f in setdiff(intersect(names(a), names(b)), c("param", "matrix"))) {
      if (!identical(as.character(ra[[f]]), as.character(rb[[f]])))
        out <- c(out, sprintf("%s %s: %s -> %s", k, f, ra[[f]], rb[[f]]))
    }
    if (length(out) > 12) break
  }
  paste(head(out, 12), collapse = "; ")
}
expect_same <- function(scenario, check, before, after) {
  if (!same(before, after)) finding(scenario, check, diff_text(before, after))
}

# Does each displayed setting match what ctsem is handed?
display_matches_model <- function(scenario, spec) {
  fp <- fingerprint(spec)
  if (inherits(fp, "error")) return(finding(scenario, "model builds", conditionMessage(fp)))
  md <- spec$parameter_metadata
  if (is.null(md) || !nrow(md)) return()
  for (i in seq_len(nrow(md))) {
    mat <- spec$matrices[[md$matrix[i]]]
    value <- mat[md$row[i], md$col[i]]
    if (md$matrix[i] != "PARS" && ctgui_parameter_is_expression(value, spec$latent_names)) next
    row <- fp[fp$param == md$param[i] & fp$matrix == md$matrix[i], , drop = FALSE]
    if (!nrow(row)) next
    shown_t <- gsub("\\s+", "", ctgui_display_transform(md$transform[i]))
    if (nzchar(shown_t) && !identical(shown_t, row$transform[1]))
      finding(scenario, "shown transform = fitted", sprintf("%s %s shown %s fitted %s", md$matrix[i], md$param[i], shown_t, row$transform[1]))
    if (!identical(isTRUE(md$indvarying[i]), isTRUE(row$indvarying[1])))
      finding(scenario, "shown RandomEffects = fitted", sprintf("%s %s shown %s fitted %s", md$matrix[i], md$param[i], md$indvarying[i], row$indvarying[1]))
    for (tp in spec$tipred_names) {
      f <- paste0(tp, "_effect")
      shown <- f %in% names(md) && isTRUE(md[[f]][i])
      fitted <- f %in% names(row) && isTRUE(row[[f]][1])
      if (!identical(shown, fitted))
        finding(scenario, "shown TI effect = fitted", sprintf("%s %s %s shown %s fitted %s", md$matrix[i], md$param[i], tp, shown, fitted))
    }
  }
}

# ---- base specifications -------------------------------------------------
bases <- list()
for (id in ctgui_example_ids()) bases[[paste0("example:", id)]] <- quiet(ctgui_example_spec(ctgui_example(id)))
for (s in ctgui_blueprint_structure_ids()) bases[[paste0("template:", s)]] <-
  quiet(ctgui_blueprint_apply(ctgui_spec(latent_names = character(), manifest_names = character()),
    ctgui_blueprint(s, c("a", "b")), "replace"))
# A model a user has customised: a custom transform, random effects switched
# on and off, a scale, one TI effect, an equality constraint.
custom <- bases[["example:real_data"]]
custom <- quiet(ctgui_set_parameter_metadata(custom, "DRIFT", "etaY1", "etaY2", transform = "exp(param)"))
custom <- quiet(ctgui_set_parameter_metadata(custom, "DRIFT", "etaY1", "etaY1", indvarying = TRUE, sdscale = 2))
custom <- quiet(ctgui_set_parameter_metadata(custom, "T0MEANS", "etaY2", "T0MEANS", indvarying = FALSE))
custom <- quiet(ctgui_set_parameter_metadata(custom, "DRIFT", "etaY1", "etaY1", tipred_effects = "TI1"))
bases[["custom"]] <- custom
for (n in names(bases)) display_matches_model(n, bases[[n]])

# The Specification tab's payload as the browser sends it, then parsed by the
# server's own ctgui_spec_fields().
spec_fields <- function(spec, latent_names = spec$latent_names, tipred_names = spec$tipred_names) {
  ctgui_spec_fields(list(latent_names = paste(latent_names, collapse = ", "),
    manifest_names = paste(spec$manifest_names, collapse = ", "),
    tdpred_names = spec$tdpred_names, tipred_names = tipred_names, type = spec$type,
    tipredDefault = spec$tipredDefault, id = spec$id, time = spec$time), spec)
}

for (n in names(bases)) {
  spec <- bases[[n]]
  fp0 <- fingerprint(spec)

  # 1. Committing in a view without changing anything.
  for (view in c("state_space", "initial_state", "tipred_effects")) {
    g <- tryCatch(ctgui_visual_graph(spec, view), error = function(e) NULL)
    if (is.null(g)) next
    after <- tryCatch(quiet(ctgui_visual_apply_graph(spec, g)), error = function(e) e)
    if (inherits(after, "error")) { finding(n, paste("visual", view, "no-op"), conditionMessage(after)); next }
    expect_same(n, paste("visual", view, "no-op commit"), fp0, fingerprint(after))
  }

  # 2. The visual path inspector committing each path as shown.
  g <- ctgui_visual_graph(spec, "state_space"); edited <- spec
  for (e in g$edges) {
    if (isTRUE(e$visual_only) || is.null(e$matrix) || isTRUE(e$fixed)) next
    item <- e[c("matrix", "row", "col", "value", "transform", "indvarying", "sdscale", "tipred_effects", "extra_pars")]
    item <- Filter(Negate(is.null), item)
    r <- tryCatch(quiet(ctgui_visual_update_edge(edited, item)), error = function(err) err)
    if (inherits(r, "error")) { finding(n, "path inspector as shown", paste(e$id, conditionMessage(r))); next }
    edited <- r
  }
  expect_same(n, "path inspector commit as shown, every path", fp0, fingerprint(edited))

  # 3. The matrix inspector committing every cell as shown.
  md <- spec$parameter_metadata
  values <- lapply(seq_len(if (is.null(md)) 0L else nrow(md)), function(i) list(matrix = md$matrix[i], row = md$row[i],
    col = md$col[i], transform = ctgui_display_transform(md$transform[i]), indvarying = isTRUE(md$indvarying[i]),
    sdscale = md$sdscale[i], tipred_effects = spec$tipred_names[vapply(spec$tipred_names, function(t)
      isTRUE(md[[paste0(t, "_effect")]][i]), logical(1L))], extra_pars = md$extra_pars[i] %||% ""))
  r <- tryCatch(quiet(ctgui_apply_matrix_batch(spec, list(), values)), error = function(e) e)
  if (inherits(r, "error")) finding(n, "matrix inspector as shown", conditionMessage(r)) else {
    synced <- quiet(ctgui_sync_model_from_matrices(r$spec))
    expect_same(n, "matrix inspector commit as shown, every cell", fp0, fingerprint(synced))
  }

  # 4. The Specification tab committing the same fields.
  fields <- spec_fields(spec)
  r <- tryCatch(quiet(ctgui_commit_spec_fields(spec, fields)), error = function(e) e)
  if (inherits(r, "error")) finding(n, "specification no-op", conditionMessage(r)) else
    expect_same(n, "specification commit, nothing changed", fp0, fingerprint(ctgui_commit_result(r)))

  # 5. Reordering the processes in the Specification.
  if (length(spec$latent_names) > 1) {
    fields2 <- fields; fields2$latent_names <- rev(spec$latent_names)
    r <- tryCatch(quiet(ctgui_commit_result(ctgui_commit_spec_fields(spec, fields2))), error = function(e) e)
    expect_same(n, "specification: processes reordered", fp0, if (inherits(r, "error")) r else fingerprint(r))
  }

  # 6. Saving and reopening: autosave, and the model as an RDS.
  path <- tempfile(fileext = ".rds")
  quiet(ctgui_autosave_write(spec, path = path))
  back <- ctgui_autosave_read(path)
  expect_same(n, "autosave and recover", fp0, if (is.null(back)) simpleError("nothing recovered") else fingerprint(back$spec))
  model <- quiet(ctgui_to_ctsem_model(spec))
  reopened <- tryCatch(quiet(ctgui_project_spec(model)), error = function(e) e)
  expect_same(n, "save model RDS, load it back", fp0, if (inherits(reopened, "error")) reopened else fingerprint(reopened))

  # 7. The exported code builds the model the GUI fits.
  code <- ctgui_export_code(spec, "exported")
  env <- new.env()
  ok <- tryCatch({ quiet(eval(parse(text = code), envir = env)); TRUE }, error = function(e) e)
  if (!isTRUE(ok)) finding(n, "exported code runs", conditionMessage(ok)) else {
    ep <- env$exported$pars; gp <- model$pars
    cols <- intersect(c("matrix", "row", "col", "param", "value", "transform", "indvarying", "sdscale"), names(gp))
    norm <- function(p) { p <- p[, cols]; p$transform <- gsub("\\s+", "", p$transform); p[do.call(order, p[c("matrix", "row", "col")]), ] }
    if (!isTRUE(all.equal(norm(ep), norm(gp), check.attributes = FALSE)))
      finding(n, "exported code = model the GUI fits", "pars differ")
  }
}

# ---- scenarios that change something -------------------------------------
spec <- bases[["custom"]]
fp0 <- fingerprint(spec)

# Rename a process in the Specification.
f <- spec_fields(spec); f$latent_names[1] <- "renamed"
r <- tryCatch(quiet(ctgui_commit_result(ctgui_commit_spec_fields(spec, f))), error = function(e) e)
if (inherits(r, "error")) finding("custom", "rename a process in Specification", conditionMessage(r)) else {
  fr <- fingerprint(r)
  kept <- fr[fr$param == "drift_etaY1_etaY2" | grepl("etaY2_renamed|renamed_etaY2|etaY1_etaY2", fr$param), ]
  cat("\nRENAME etaY1 -> renamed in Specification. Parameters now:\n")
  print(fr[grepl("DRIFT|DIFFUSION|T0", fr$matrix), c("matrix", "param", "transform", "indvarying", "sdscale")], row.names = FALSE)
}

# Add a TI predictor to a model in the Specification, tipredDefault TRUE.
f <- spec_fields(spec); f$tipred_names <- c(spec$tipred_names, "TInew")
r <- tryCatch(quiet(ctgui_commit_result(ctgui_commit_spec_fields(spec, f))), error = function(e) e)
if (inherits(r, "error")) finding("custom", "add TI predictor", conditionMessage(r)) else display_matches_model("custom + TI predictor added", r)

# A model built with tipredDefault = FALSE, saved and loaded.
m <- quiet(ctsem::ctModel(LAMBDA = diag(2), manifestNames = c("y1", "y2"), latentNames = c("a", "b"),
  TIpredNames = "g", tipredDefault = FALSE, type = "ct", silent = TRUE))
loaded <- quiet(ctgui_project_spec(m))
before <- m$pars; after <- quiet(ctgui_to_ctsem_model(loaded))$pars
cat(sprintf("\nLOAD tipredDefault=FALSE model: TI effects in file %d, after GUI load %d\n",
  sum(as.logical(before$g_effect), na.rm = TRUE), sum(as.logical(after$g_effect), na.rm = TRUE)))
display_matches_model("loaded tipredDefault=FALSE model", loaded)

# Equality constraint typed into a second cell whose matrix default differs.
eq <- quiet(ctgui_set_matrix_value(bases[["example:coupled"]], "DRIFT", "stress", "sleep",
  label = as.character(bases[["example:coupled"]]$matrices$DRIFT["stress", "stress"])))
eqfp <- fingerprint(eq)
cat("\nEQUALITY: DRIFT[stress,sleep] given the diagonal's label:\n")
print(if (inherits(eqfp, "error")) conditionMessage(eqfp) else eqfp[eqfp$matrix == "DRIFT", c("param", "transform", "indvarying")], row.names = FALSE)
display_matches_model("equality constraint across diagonal and off-diagonal", eq)


res <- if (length(findings)) do.call(rbind, findings) else data.frame()
cat(sprintf("\nFINDINGS %d\n", nrow(res)))
if (nrow(res)) for (i in seq_len(nrow(res))) cat(sprintf("- [%s] %s: %s\n", res$scenario[i], res$check[i], res$detail[i]))
cat("DONE\n")
