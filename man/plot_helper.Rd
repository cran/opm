\name{plot_helper}
\alias{plot_helper}
\title{Helper function for plotting}
\usage{
  plot_helper(data, plot.data = TRUE,
    col = rgb(0, 0, 0, 0.3), pch = 20, ...)
}
\arguments{
  \item{data}{raw data.}

  \item{plot.data}{Logical. Should the raw data be
  plotted?}

  \item{col}{Color for plotting.}

  \item{pch}{Point symbols to be plotted.}

  \item{...}{Further arguments to be passed to
  \code{\link{plot}} and \code{\link{points}}.}
}
\value{
  NULL
}
\description{
  Helper function for plotting internally used by
  \code{\link{plot.opm_model}}. Sets up a device and plots
  raw data if asked to.
}
\author{
  Benjamin Hofner
}
\keyword{internal}

