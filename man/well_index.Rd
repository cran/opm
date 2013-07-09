\name{well_index}
\alias{well_index}
\title{Translate well coordinates}
\usage{
  well_index(x, names)
}
\arguments{
  \item{x}{Vector, formula or missing. Basically any \R
  object.}

  \item{names}{Character vector. Ignored unless \code{x} is
  a formula.}
}
\value{
  Either \code{x}, \code{TRUE} or (if a formula) the result
  of evaluating \code{x} in the context of \code{names},
  converted to a mapping from elements to indexes.
}
\description{
  Translate well coordinates to numeric indexes, or clean
  well indexes given as character vector.
}
\keyword{internal}

