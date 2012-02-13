\name{negative_control}
\alias{negative_control}
\title{Negative control}
\arguments{
  \item{object}{\code{\link{OPMX}} object.}

  \item{neg.ctrl}{If \code{NULL} or \code{FALSE}, ignore
  \code{data} and return \code{NULL}. If \code{TRUE}, call
  \code{minmax} with \code{data} as sole argument. If a
  character scalar, call \code{max} with \code{data} as
  first and \code{neg.ctrl} as second argument. If
  \code{neg.ctrl} is a numeric value, it is returned.}
}
\value{
  Numeric scalar or \code{NULL}.
}
\description{
  Helper function to determine the value of a measurement
  interpretable as negative control.
}
\keyword{internal}

\docType{methods}
\alias{negative_control-methods}
\alias{negative_control,OPMX-method}
\usage{
  \S4method{negative_control}{OPMX}(object, neg.ctrl) 
}
