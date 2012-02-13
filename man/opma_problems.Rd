\name{opma_problems}
\alias{opma_problems}
\title{Check OPMA matrix}
\arguments{
  \item{object}{Matrix of aggregated data.}

  \item{orig}{Matrix of original, non-aggregated data.}

  \item{program}{Character scalar. Program used for
  aggregating the data (currently only \sQuote{grofit} is
  checked).}
}
\value{
  Character vector with description of problems, empty if
  there are none.
}
\description{
  Check whether a matrix fulfils the requirements for
  \code{\link{OPMA}} aggregated data. Called when
  constructing an object of the class.
}
\keyword{internal}

\docType{methods}
\alias{opma_problems-methods}
\alias{opma_problems,matrix-method}
\usage{
  \S4method{opma_problems}{matrix}(object, orig, program) 
}
