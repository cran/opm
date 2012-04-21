\name{ranging}
\alias{ranging}
\title{Conduct ranging}
\arguments{
  \item{object}{Numeric vector or array.}

  \item{extended}{Logical scalar. Subtract the minimum in
  both numerator and denominator of the ranging formula? If
  \code{zscores} is \code{TRUE}, the meaning is different.}

  \item{zscores}{Logical scalar. Calculate z-scores instead
  of ranging? If \code{extended} is \code{FALSE}, this is
  done using the mean and the standard deviation;
  otherwise, the median and the MAD are used.}

  \item{na.rm}{Logical scalar. Remove \code{NA} values when
  calculating the relevant aggregated values (minimum,
  maximum, mean, standard deviation, median and/or MAD)?}

  \item{fac}{Numeric scalar. After conducting the proper
  ranging process, \code{object} is multiplied by
  \code{fac}.}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  Numeric vector or matrix.
}
\description{
  Range numbers, i.e. divide them by their maximum. In
  \sQuote{extended} mode, the minimum is subtracted
  beforehand. It is possible to replace ranging by
  standardization (z-scores).
}
\keyword{internal}

\docType{methods}
\alias{ranging-methods}
\alias{ranging,numeric-method}
\alias{ranging,MOA-method}
\usage{
  \S4method{ranging}{numeric}(object, extended = !zscores,
    zscores = FALSE, na.rm = TRUE, fac = 1) 

  \S4method{ranging}{MOA}(object, ...) 

}
