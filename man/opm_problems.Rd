\name{opm_problems}
\alias{opm_problems}
\title{Check OPM}
\arguments{
  \item{object}{Matrix or character vector.}
}
\value{
  Character vector with description of problems, empty if
  there are none.
}
\description{
  Check whether a matrix fulfils the requirements for
  \code{\link{OPM}} measurements, or check whether a
  character vector fulfils the requirements for
  \code{\link{OPM}} CSV data. Called when constructing an
  object of the class.
}
\keyword{internal}

\docType{methods}
\alias{opm_problems-methods}
\alias{opm_problems,matrix-method}
\alias{opm_problems,character-method}
\usage{
  \S4method{opm_problems}{matrix}(object) 

  \S4method{opm_problems}{character}(object) 

}
