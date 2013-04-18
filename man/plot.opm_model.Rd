\name{plot.opm_model}
\alias{plot.opm_model}
\title{Plot spline fits}
\usage{
  \method{plot}{opm_model} (x, plot.data = TRUE,
    plot.spline = TRUE, col = rgb(0, 0, 0, 0.3), pch = 20,
    col.spline = "red", lty.spline = "solid",
    lwd.spline = 1, ...)
}
\arguments{
  \item{x}{Spline fit.}

  \item{plot.data}{Logical. Should the data be added to the
  plot?}

  \item{plot.spline}{Logical. Should the spline be
  plotted?}

  \item{col}{Color of observed data. Per default a
  semi-transparent grey value is used. For details see
  \code{\link{par}} (Section Color Specification).}

  \item{pch}{Plotting character used for observed data
  points.  See \code{\link{points}} for details.}

  \item{col.spline}{Color of fitted spline specified for
  example by a character string. For details see
  \code{\link{par}} (Section Color Specification).}

  \item{lty.spline}{Line type of fitted spline.  For
  details see \code{\link{par}}.}

  \item{lwd.spline}{Line width of fitted spline.  For
  details see \code{\link{par}}.}

  \item{...}{Further arguments to be passed to
  \code{\link{plot}} and \code{\link{lines}}.}
}
\description{
  Plot splines derived from \code{\link{do_aggr}} or
  \code{\link{fit_spline}}.
}
\author{
  Benjamin Hofner
}
\keyword{internal}

