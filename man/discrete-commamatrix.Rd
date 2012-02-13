\name{discrete,matrix}
\alias{discrete,matrix}
\title{Convert to discrete characters (matrix version)}
\arguments{
  \item{x}{Numeric matrix or convertible to such.}

  \item{...}{Arguments passed to \code{\link{discrete}}.
  See there for details and further examples.}
}
\value{
  Matrix composed of a vector as produced by
  \code{\link{discrete}} and the original \code{dimensions}
  and \code{dimnames} attributes of \code{x}.
}
\description{
  Convert a matrix of continuous characters to discrete
  ones suitable for phylogenetic studies with software such
  as PAUP* and RAxML. This is just a wrapper for
  \code{\link{discrete}} which takes care of the matrix
  dimensions.
}
\examples{
x <- matrix(1:10, ncol = 2)
(y <- discrete(x, range = c(3.4, 4.5), gap = TRUE))
stopifnot(identical(dim(x), dim(y)))
# for the discretization approach itself see discrete() for numeric vectors
}
\seealso{
  cut

  Other phylogeny-functions: \code{\link{discrete}},
  \code{\link{phylo_data}}, \code{\link{safe_labels}}
}
\keyword{category}
\keyword{character}

\docType{methods}
\alias{discrete,matrix-method}
\usage{
  \S4method{discrete}{matrix}(x, ...) 
}
