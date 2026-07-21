\name{ctgui_launch_app}
\alias{ctgui_launch_app}
\title{Launch ctsemgui}
\usage{
ctgui_launch_app(launch.browser = interactive(), ...)
}
\arguments{
\item{launch.browser}{Passed to \code{shiny::runApp()}.}

\item{...}{Additional arguments passed to \code{shiny::runApp()}.}
}
\description{
Launch the ctsemgui Shiny application. Model specification, data handling,
fitting, diagnostics, exports, and model-object transfer are performed within
the graphical interface.
}
\examples{
\dontrun{
ctgui_launch_app()
}
}
