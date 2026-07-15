\name{ctgui_build_model}
\alias{ctgui_build_matrices}
\alias{ctgui_build_measurement_matrices}
\alias{ctgui_build_model}
\alias{ctgui_structures}
\alias{ctgui_measurements}
\alias{ctgui_validate_data}
\alias{ctgui_graph_edges}
\title{Build guided ctsem GUI matrices}
\usage{
ctgui_build_matrices(spec, structure = c("dynamic_var", "linear_growth",
  "dynamic_var_trend"), options = list())

ctgui_build_measurement_matrices(spec, measurement = c("single_indicator",
  "marker", "fixed_loadings"), options = list())

ctgui_build_model(structure = c("dynamic_var", "linear_growth",
  "dynamic_var_trend"), measurement = c("single_indicator", "marker",
  "fixed_loadings"), names = list(), options = list())

ctgui_structures()

ctgui_measurements()

ctgui_validate_data(spec, data)

ctgui_graph_edges(spec, element = c("drift", "diffusion", "measurement",
  "trend"))
}
\arguments{
\item{spec}{A \code{ctsemgui_spec} object.}
\item{structure}{Matrix structure. One of \code{"linear_growth"},
\code{"dynamic_var"}, or \code{"dynamic_var_trend"}.}
\item{measurement}{Measurement model. One of \code{"single_indicator"},
\code{"marker"}, or \code{"fixed_loadings"}.}
\item{names}{Named list of model names. Common entries are
\code{factor_names}, \code{manifest_names}, \code{id}, and \code{time}.}
\item{options}{Named list of builder options, including \code{n},
\code{type}, \code{trend_type}, \code{indicators_per_factor},
\code{free_noise_correlations}, \code{fixed_loadings}, and
\code{trend_coupling}.}
\item{data}{A data frame to check against the model specification.}
\item{element}{Graph element: \code{"drift"}, \code{"diffusion"},
\code{"measurement"}, or \code{"trend"}.}
}
\value{
\code{ctgui_build_matrices()} and
\code{ctgui_build_measurement_matrices()} return updated
\code{ctsemgui_spec} objects without changing model names or data-role fields.
\code{ctgui_build_model()} returns a \code{ctsemgui_spec}.
\code{ctgui_structures()} and \code{ctgui_measurements()} return data frames.
\code{ctgui_validate_data()} returns a validation data frame.
\code{ctgui_graph_edges()} returns graph edge data for one model element.
}
\description{
Build composable beginner-oriented ctsem matrices from an existing model
specification. The Specification tab or \code{ctgui_spec()} defines names and
data-role fields; these helpers only populate or revise matrices.
}
\examples{
spec <- ctgui_spec(
  latent_names = c("affect", "stress"),
  manifest_names = c("Affect", "Stress")
)
spec <- ctgui_build_matrices(spec, "dynamic_var")
ctgui_graph_edges(spec, "drift")
ctgui_validate(spec)
}
