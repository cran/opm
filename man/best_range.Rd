\name{best_range}
\alias{best_range}
\title{Best range}
\arguments{
  \item{object}{Numeric vector.}

  \item{target}{Numeric scalar. Target difference between
  min and max. If \code{NULL}, this is simply derived from
  the range of \code{object}.}

  \item{align}{Character scalar. Where to put the real
  values relative to min and max of \code{target}.}

  \item{offset}{Numeric scalar. A minimal distance to the
  margins.}

  \item{prop.offset}{Numeric scalar. As an alternative to
  \code{offset}, it can be specified as a proportion of
  \code{target}.}
}
\value{
  Optimal range (numeric vector of length two).
}
\description{
  Determine an optimal range for plotting.
}
\keyword{internal}

\docType{methods}
\alias{best_range-methods}
\alias{best_range,numeric-method}
\usage{
  \S4method{best_range}{numeric}(object, target,
    align = c("center", "left", "right"),
    offset = 0, prop.offset = 0) 
}
