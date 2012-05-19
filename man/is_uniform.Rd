\name{is_uniform}
\alias{is_uniform}
\title{Check for uniformity}
\usage{
  is_uniform(x, na.rm = FALSE)
}
\arguments{
  \item{x}{An R object to which \code{duplicated} can be
  applied.}

  \item{na.rm}{Logical scalar. Remove \code{NA} elements
  before determining uniformity?}
}
\value{
  Either \code{TRUE} or a vector of the class of \code{x}
  containing all deviating elements.
}
\description{
  Assess whether all elements in a collection are
  identical.
}
\keyword{internal}

