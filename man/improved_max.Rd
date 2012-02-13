\name{improved_max}
\alias{improved_max}
\title{Maximum plus offset}
\arguments{
  \item{object}{Numeric vector}

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
\usage{
  \S4method{improved_max}{numeric}(object, by = 10) 
}
