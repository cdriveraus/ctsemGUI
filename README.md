# ctsemgui

`ctsemgui` provides a Shiny graphical interface for building, fitting, and
checking `ctsem` models.

The GUI is intended for users who want to work through a model in visible steps:
load or generate data, specify variables, edit model matrices, fit the model,
inspect output, and run common diagnostics.

## Installation

Install from GitHub in R:

```r
install.packages("remotes")
remotes::install_github("cdriveraus/ctsemgui",dependencies=TRUE)
```

`ctsemgui` uses `ctsem` for model fitting, data generation, equations, and
diagnostics. If `ctsem` is not already installed, install it as well:

```r
install.packages("ctsem",dependencies=TRUE)
```

## Start The GUI

```r
library(ctsemgui)
ctgui_launch_app()
```

This starts a local Shiny app in your R session.

`ctgui_launch_app()` is the package's supported public API. Model-building,
matrix-editing, validation, and conversion helpers are implementation details
used by the app rather than functions intended for external scripts.

## What The GUI Does

The app is organised around the usual `ctsem` workflow:

- **Data**: import an existing long-format data frame, import a CSV, or generate
  preview data from the current model.
- **Model**: specify manifest variables, latent processes, ID and time columns,
  predictors, time type, manifest variable types, and editable ctsem matrices.
- **Equations and visuals**: inspect model equations and graph-style summaries of
  temporal dynamics, system noise, measurement links, and generated trajectories.
- **Fit**: run `ctFit()` with a small set of common options and view fit logs,
  warnings, and fit-equation output.
- **Diagnostics**: run prediction plots, residual ACF checks, posterior
  predictive checks, lagged covariance checks, dynamics plots, and TI-predictor
  effect plots.
- **Output**: view `summary(fit)`, `ctSummaryMatrices(fit)`, parameter tables,
  fit comparisons, and generated R code.

## Data Format

The GUI assumes long-format data: one row per observation occasion, with columns
for subject ID, time, observed variables, and any predictors.

