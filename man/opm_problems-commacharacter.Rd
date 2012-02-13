\name{opm_problems,character}
\alias{opm_problems,character}
\title{Check OPM CSV data}
\arguments{
  \item{object}{Character vector.}
}
\value{
  Character vector with description of problems, empty if
  there are none.
}
\description{
  Check whether a character vector fulfils the requirements
  for \code{\link{OPM}} CSV data. Called when constructing
  an object of the class.
}
\keyword{internal}

\docType{methods}
\alias{opm_problems,character-method}
\usage{
  \S4method{opm_problems}{character}(object) 
}
