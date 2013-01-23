\name{reduce_to_mode}
\alias{reduce_to_mode}
\title{Reduce an object}
\usage{
  reduce_to_mode(x, cutoff, use.na = TRUE)
}
\arguments{
  \item{x}{An \R object to which \code{table} can be
  applied.}

  \item{cutoff}{Numeric scalar. Relative frequency below
  which elements are discarded.}

  \item{use.na}{Logical scalar indicating whether ambiguous
  results should be converted to \code{NA}.}
}
\value{
  Vector of the same storage mode than \code{x}.
}
\description{
  Reduce a countable object to the most frequent
  element(s).
}
\keyword{internal}

