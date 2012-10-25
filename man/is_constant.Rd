\name{is_constant}
\alias{is_constant}
\title{Check for constantness}
\arguments{
  \item{x}{An \R object to which \code{duplicated} can be
  applied.}

  \item{set.like}{Logical scalar. Consider objects as
  identical if their intersection is non-empty?}

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
\usage{
  \S4method{is_constant}{vector}(x, na.rm = TRUE) 

  \S4method{is_constant}{list}(x, set.like = FALSE, na.rm = TRUE) 

  \S4method{is_constant}{MOA}(x, margin = 1L, na.rm = TRUE) 

}
