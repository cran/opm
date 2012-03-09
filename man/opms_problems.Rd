\name{opms_problems}
\alias{opms_problems}
\title{Check OPMS list}
\arguments{
  \item{object}{List to be checked.}
}
\value{
  Character vector with description of problems, empty if
  there are none.
}
\description{
  Check whether a list fulfils the requirements for
  \code{\link{OPMS}} \code{\link{plates}}. Called when
  constructing an object of that class.
}
\keyword{internal}

\docType{methods}
\alias{opms_problems-methods}
\alias{opms_problems,list-method}
\usage{
  \S4method{opms_problems}{list}(object) 

}
