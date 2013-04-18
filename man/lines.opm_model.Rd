\name{lines.opm_model}
\alias{lines.opm_model}
\title{Add spline fits to plot}
\usage{
  \method{lines}{opm_model} (x, col = "red", lty = "solid",
    lwd = 1, ...)
}
\arguments{
  \item{x}{Spline fit.}

  \item{col}{Color of fitted spline specified for example
  by a character string. For details see \code{\link{par}}
  (Section Color Specification).}

  \item{lty}{Line type of fitted spline. For details see
  \code{\link{par}}.}

  \item{lwd}{Line width of fitted spline. For details see
  \code{\link{par}}.}

  \item{...}{Further arguments to be passed to
  \code{\link{lines}}}
}
\description{
  Add a fitted spline function to an existing plot.
}
\author{
  Benjamin Hofner
}
\keyword{internal}

