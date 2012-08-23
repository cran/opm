\name{join_discrete}
\alias{join_discrete}
\title{Join phylogenetic characters}
\arguments{
  \item{object}{Character or numeric vector or matrix.}

  \item{format}{Character scalar.}

  \item{groups}{Character vector or factor, determining
  which rows should be joined.}

  \item{digits}{Number of digits used for rounding. Has an
  effect only for numeric data.}
}
\value{
  Character scalar, for the matrix method a vector.
}
\description{
  Join discrete characters represented as strings of length
  1, considering ambiguities, if any. The outcome is
  suitable for phylogenetic studies with software such as
  PAUP*.
}
\keyword{internal}

\docType{methods}
\alias{join_discrete-methods}
\alias{join_discrete,character-method}
\alias{join_discrete,numeric-method}
\alias{join_discrete,matrix-method}
\usage{
  \S4method{join_discrete}{character}(object, format) 

  \S4method{join_discrete}{numeric}(object, format, digits = 3L) 

  \S4method{join_discrete}{matrix}(object, format,
    groups = TRUE, digits = 3L) 

}
