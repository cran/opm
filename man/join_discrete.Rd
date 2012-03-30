\name{join_discrete}
\alias{join_discrete}
\title{Join phylogenetic characters}
\arguments{
  \item{object}{Character vector or convertible to such, or
  matrix.}

  \item{format}{Character scalar.}

  \item{groups}{Character vector or factor, determining
  which rows should be joined.}
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
  \S4method{join_discrete}{ANY}(object, format) 

  \S4method{join_discrete}{matrix}(object, format, 
    groups = rownames(object)) 

}
