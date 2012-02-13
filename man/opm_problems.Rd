\name{opm_problems}
\alias{opm_problems}
\title{Check OPM matrix}
\arguments{
  \item{object}{Matrix.}
}
\value{
  Character vector with description of problems, empty if
  there are none.
}
\description{
  Check whether a matrix fulfils the requirements for
  \code{\link{OPM}} measurements. Called when constructing
  an object of the class.
}
\keyword{internal}

\docType{methods}
\alias{opm_problems-methods}
\alias{opm_problems,matrix-method}
\usage{
  \S4method{opm_problems}{matrix}(object) 
}
