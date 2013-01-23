\name{opma_problems}
\alias{opma_problems}
\title{Check OPMA}
\arguments{
  \item{object}{Matrix of aggregated data or list
  describing the aggregation settings.}

  \item{orig.data}{Matrix of original, non-aggregated
  data.}

  \item{settings}{List or \code{NULL}. If a list, settings
  used for aggregating the data (currently only the
  \pkg{opm}-native programs are checked).}
}
\value{
  Character vector with description of problems, empty if
  there are none.
}
\description{
  Check whether a matrix fulfils the requirements for
  \code{\link{OPMA}} aggregated data, or check whether a
  list fulfils the requirements for \code{\link{OPMA}}
  aggregation settings. Called when constructing an object
  of the class.
}
\keyword{internal}

\docType{methods}
\alias{opma_problems-methods}
\alias{opma_problems,matrix-method}
\alias{opma_problems,list-method}
\usage{
  \S4method{opma_problems}{matrix}(object, orig.data, settings) 

  \S4method{opma_problems}{list}(object) 

}
