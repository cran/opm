\name{draw_ci}
\alias{draw_ci}
\title{Draw CI}
\arguments{
  \item{object}{Four-element numeric vector containing (i)
  the left margin of the CI; (ii) the point estimate; (iii)
  the right margin; (iv) the position on the y axis. The
  point estimate can be \code{NA} at any time; whether the
  margins can also be \code{NA} depends on
  \code{na.action}.}

  \item{col}{Character scalar. Name of the color to be
  used.}

  \item{cex}{Numeric scalar. Magnification for CI margin
  symbols and point estimate. Also affects line width, and
  proportionally so.}

  \item{na.action}{Character scalar. What to do if a margin
  value is \code{NA}.}
}
\value{
  \code{object}, returned invisibly.
}
\description{
  Draw a confidence interval.
}
\keyword{internal}

\docType{methods}
\alias{draw_ci-methods}
\alias{draw_ci,numeric-method}
\usage{
  \S4method{draw_ci}{numeric}(object, col = "blue", cex = 1,
    na.action = c("warn", "error", "ignore")) 
}
