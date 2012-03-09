\name{improved_max}
\alias{improved_max}
\title{Maximum plus offset}
\arguments{
  \item{object}{Numeric vector or \sQuote{OPMX} object.}

  \item{theor.max}{Logical scalar. Use the theoretical or
  the real improved maximum? If \code{TRUE}, \code{by} is
  ignored.}

  \item{by}{Numeric scalar.}
}
\value{
  Numeric scalar. Let \code{n} be the smallest integer
  value for which \code{n * by >= object} holds. The result
  is then equal to \code{(n + 1) * by}.
}
\description{
  Return the maximal value of an object plus a certain
  offset.
}
\keyword{internal}

\docType{methods}
\alias{improved_max-methods}
\alias{improved_max,numeric-method}
\alias{improved_max,OPMX-method}
\usage{
  \S4method{improved_max}{numeric}(object, by = 10) 

  \S4method{improved_max}{OPMX}(object, theor.max = TRUE, by = 10) 

}
