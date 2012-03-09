\name{join_discrete}
\alias{join_discrete}
\title{Join phylogenetic characters}
\arguments{
  \item{object}{Character vector or convertible to such, or
  matrix.}

  \item{margin}{Integer scalar. See \code{apply} from
  package \pkg{base} and \code{\link{join_discrete}}, which
  is applied to either rows or columns, depending on this
  argument.}
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
\alias{join_discrete,ANY-method}
\alias{join_discrete,matrix-method}
\usage{
  \S4method{join_discrete}{ANY}(object) 

  \S4method{join_discrete}{matrix}(object, margin = 2L) 

}
