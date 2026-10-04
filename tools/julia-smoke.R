# Run in CI before the tests on a leg that has Julia: proves the engine loads
# and fits, so a test run that quietly fell back to Stan cannot pass as one
# that used Julia. Also pays the engine's precompile here, where its time is
# visible, rather than inside whichever test happens to reach Julia first.

suppressPackageStartupMessages(library(ctsem))

status <- ctJuliaStatus()
if (!isTRUE(status$available)) {
  cat("::error title=Julia unavailable::ctJuliaStatus() reports Julia is not available\n")
  quit(status = 1)
}

dat <- as.data.frame(ctstantestdat)
dat <- dat[dat$id %in% 1:5, c("id", "time", "Y1")]
model <- ctModel(LAMBDA = matrix(1), manifestNames = "Y1", latentNames = "eta1",
  type = "ct", silent = TRUE)
# Five subjects identify this model poorly; only whether it runs is asked here.
fit <- tryCatch(suppressWarnings(ctFit(datalong = dat, model = model, backend = "julia", cores = 1)),
  error = function(e) e)
if (inherits(fit, "error") || !inherits(fit, "ctJuliaFit")) {
  why <- if (inherits(fit, "error")) conditionMessage(fit) else paste(class(fit), collapse = "/")
  cat(sprintf("::error title=Julia fit failed::%s\n", substr(gsub("[\r\n]+", " ", why), 1, 700)))
  quit(status = 1)
}
cat(sprintf("::notice title=Julia engine::Julia %s; a ctFit(backend = 'julia') fit completed\n",
  format(status$julia)))
