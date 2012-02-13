\name{opma_problems,list}
\alias{opma_problems,list}
\title{Check settings}
\arguments{
  \item{object}{List describing the aggregation settings.}
}
\value{
  Character vector with description of problems, empty if
  there none.
}
\description{
  Check whether a list fulfils the requirements for
  \code{\link{OPMA}} aggregation settings. Called when
  constructing an object of the class.
}
\keyword{internal}

\docType{methods}
\alias{opma_problems,list-method}
\usage{
  \S4method{opma_problems}{list}(object) 
}
