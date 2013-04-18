\name{add_parameters}
\alias{add_parameters}
\title{Add parameter estimates}
\usage{
  add_parameters(model, add.deriv = FALSE, col = "red",
    deriv.col = "grey", lty = "dashed", ...)
}
\arguments{
  \item{model}{Model fit.}

  \item{add.deriv}{Should the derivative be plotted?}

  \item{col}{Color for parameter estimates.}

  \item{deriv.col}{Color for first derivative.}

  \item{lty}{Line type.}

  \item{...}{Additional graphical parameters that are
  passed to \code{\link{lines}}, \code{\link{points}} and
  \code{\link{abline}}.}
}
\value{
  NULL
}
\description{
  Add parameter estimates for an estimated spline model to
  the current plot.
}
\author{
  Benjamin Hofner
}
\keyword{internal}

