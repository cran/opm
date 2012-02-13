\name{join_discrete,matrix}
\alias{join_discrete,matrix}
\title{Join phylogenetic characters (matrix version)}
\arguments{
  \item{object}{Matrix.}

  \item{margin}{Integer scalar. See \code{apply} from
  package \pkg{base} and \code{\link{join_discrete}}, which
  is applied to either rows or columns, depending on this
  argument.}
}
\value{
  Character vector.
}
\description{
  Join discrete characters represented as strings of length
  1, considering ambiguities, if any. The outcome is
  suitable for phylogenetic studies with software such as
  PAUP*.
}
\keyword{internal}

\docType{methods}
\alias{join_discrete,matrix-method}
\usage{
  \S4method{join_discrete}{matrix}(object, margin = 2L) 
}
