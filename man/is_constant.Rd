\name{is_constant}
\alias{is_constant}
\title{Check for constantness}
\arguments{
  \item{x}{Vector, matrix, array or \sQuote{CMAT} object.}

  \item{strict}{Logical scalar. Has no effect unless
  \code{x} is a list. If its elements are integers,
  \code{FALSE} implies that objects are considered as
  identical if their intersection is non-empty. If the
  elements are floats, they are compared using \code{mean}
  and \code{sd}. They are identical if there ranges,
  defined as \code{n} standard deviations around the mean,
  overlap, with \code{n} being either \code{1} or \code{2},
  depending on \code{strict}.}

  \item{na.rm}{Logical scalar. Remove \code{NA} elements
  before determining constantness?}
}
\value{
  Logical scalar.
}
\description{
  Assess whether all elements in a collection are
  identical. This uses \code{duplicated} by default, but
  there is also an \sQuote{extended} mode for list-like
  objects.
}
\keyword{internal}

\docType{methods}
\alias{is_constant-methods}
\alias{is_constant,vector-method}
\alias{is_constant,list-method}
\alias{is_constant,MOA-method}
\alias{is_constant,CMAT-method}
\usage{
  \S4method{is_constant}{vector}(x, na.rm = TRUE) 

  \S4method{is_constant}{list}(x, na.rm = TRUE) 

  \S4method{is_constant}{MOA}(x, margin = 1L, na.rm = TRUE) 

  \S4method{is_constant}{CMAT}(x, strict, digits = opm_opt("digits"),
    na.rm = TRUE) 

}
