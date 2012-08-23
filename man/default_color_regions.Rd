\name{default_color_regions}
\alias{default_color_regions}
\title{Colour regions}
\usage{
  default_color_regions(colors, space, bias, n)
}
\arguments{
  \item{colors}{Character or integer vector with at least
  two distinct colours. If \code{NULL} or empty, default
  colours are chosen.}

  \item{space}{Passed to \code{colorRampPalette}.}

  \item{bias}{Passed to \code{colorRampPalette}.}

  \item{n}{Passed to the function returned by
  \code{colorRampPalette}.}
}
\value{
  Character vector of colour codes.
}
\description{
  Create default colour regions for use with
  \code{\link{level_plot}}.
}
\keyword{internal}

