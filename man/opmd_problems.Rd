\name{opmd_problems}
\alias{opmd_problems}
\title{Check OPMD}
\arguments{
  \item{object}{Matrix of original, non-discretized data,
  or list describing the discretization settings.}

  \item{disc}{Vector of discretized data.}
}
\value{
  Character vector with description of problems, empty if
  there are none.
}
\description{
  Check whether a matrix fulfils the requirements for
  \code{\link{OPMD}} discretized data, or check whether a
  list fulfils the requirements for \code{\link{OPMD}}
  discretization settings. Called when constructing an
  object of the class.
}
\keyword{internal}

\docType{methods}
\alias{opmd_problems-methods}
\alias{opmd_problems,list-method}
\alias{opmd_problems,matrix-method}
\usage{
  \S4method{opmd_problems}{list}(object) 

  \S4method{opmd_problems}{matrix}(object, disc) 

}
