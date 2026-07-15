\name{ctgui_spec}
\alias{ctgui_spec}
\alias{ctgui_validate}
\alias{ctgui_matrix_names}
\alias{ctgui_matrix}
\alias{ctgui_set_matrix}
\alias{ctgui_set_matrix_value}
\alias{ctgui_to_ctsem_model}
\alias{ctgui_export_code}
\alias{ctgui_latex}
\alias{ctgui_generate_data}
\alias{ctgui_launch_app}
\title{Create, edit, validate, and export ctsem GUI specifications}
\usage{
ctgui_spec(latent_names = "eta1", manifest_names = "y1",
  type = c("ct", "dt"), id = "id", time = "time", Tpoints = NULL,
  manifest_type = rep(0, length(manifest_names)),
  tdpred_names = character(), tipred_names = character(),
  matrices = NULL, tipredDefault = TRUE)

ctgui_validate(spec)

ctgui_matrix_names(spec)

ctgui_matrix(spec, matrix)

ctgui_set_matrix(spec, matrix, value)

ctgui_set_matrix_value(spec, matrix, row, col = 1, value = NULL,
  label = NULL, free = NULL)

ctgui_to_ctsem_model(spec, silent = TRUE, tipredDefault = spec$tipredDefault)

ctgui_export_code(spec, object_name = "model")

ctgui_latex(spec, ...)

ctgui_generate_data(spec, n.subjects = 100, Tpoints = spec$Tpoints \%||\% 10,
  burnin = 0, dtmean = 1, logdtsd = 0, wide = FALSE,
  free_defaults = TRUE)

ctgui_launch_app(spec = ctgui_spec(), launch.browser = interactive(), ...)
}
\arguments{
\item{latent_names}{Character vector of latent process names.}
\item{manifest_names}{Character vector of manifest variable names.}
\item{type}{Model type passed to \code{ctsem::ctModel()}, currently \code{"ct"} or \code{"dt"}.}
\item{id}{Subject identifier column name.}
\item{time}{Time column name.}
\item{Tpoints}{Optional number of time points.}
\item{manifest_type}{Manifest variable type vector passed to ctsem.}
\item{tdpred_names}{Optional time-dependent predictor names.}
\item{tipred_names}{Optional time-independent predictor names.}
\item{matrices}{Optional named list of matrix overrides.}
\item{tipredDefault}{Passed to \code{ctsem::ctModel()}.}
\item{spec}{A \code{ctsemgui_spec} object.}
\item{matrix}{Matrix name.}
\item{value}{A replacement matrix or a scalar matrix cell value.}
\item{row}{Row index or row name.}
\item{col}{Column index or column name.}
\item{label}{Free-parameter label for a matrix cell.}
\item{free}{Logical; when \code{TRUE}, \code{label} or \code{value} is treated as a parameter label.}
\item{silent}{Passed to \code{ctsem::ctModel()}.}
\item{object_name}{Name used for the model object in exported code.}
\item{n.subjects}{Number of subjects to generate.}
\item{burnin}{Number of initial generated time points to discard.}
\item{dtmean}{Mean time interval for generated data.}
\item{logdtsd}{Log time interval standard deviation.}
\item{wide}{Logical; passed to \code{ctsem::ctGenerate()}.}
\item{free_defaults}{Logical; if \code{TRUE}, free matrix cells are replaced
with simple simulation defaults before generation. If \code{FALSE}, ctsem's
native generation behavior is used, where free labels are treated as zero.}
\item{launch.browser}{Passed to \code{shiny::runApp()}.}
\item{...}{Additional arguments passed to the underlying function.}
}
\description{
Create a GUI-neutral model specification, edit its matrix entries, validate the
result, convert it to a ctsem model, export reproducible R code, or launch a
minimal Shiny interface.
}
\value{
\code{ctgui_spec()} returns a \code{ctsemgui_spec} object.
\code{ctgui_validate()} returns a data frame of structured validation messages.
\code{ctgui_to_ctsem_model()} returns a ctsem model object.
\code{ctgui_export_code()} returns a character string.
}
\examples{
spec <- ctgui_spec(
  latent_names = c("eta1", "eta2"),
  manifest_names = c("Y1", "Y2")
)
spec <- ctgui_set_matrix_value(spec, "DRIFT", "eta1", "eta2", label = "cross12")
ctgui_validate(spec)
cat(ctgui_export_code(spec))
}
