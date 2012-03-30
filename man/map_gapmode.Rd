\name{map_gapmode}
\alias{map_gapmode}
\title{Map gap-mode discretized character codes}
\arguments{
  \item{object}{Matrix. See \code{\link{phylo_data}}.}
}
\value{
  Matrix
}
\description{
  Map character states that have been discretized with
  \code{\link{discrete}} and \code{gap} set to \code{TRUE}.
  This is mainly necessary for creating HTML tables for
  IJSEM.  See \code{\link{phylo_data}} for details.
}
\keyword{internal}

\docType{methods}
\alias{map_gapmode-methods}
\alias{map_gapmode,matrix-method}
\usage{
  \S4method{map_gapmode}{matrix}(object) 

}
