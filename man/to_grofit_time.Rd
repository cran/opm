\name{to_grofit_time}
\alias{to_grofit_time}
\title{Times for grofit}
\arguments{
  \item{object}{\code{\link{OPM}} object.}
}
\value{
  Data frame with time points in each row, repeated for
  each well (number of rows is number of wells).
}
\description{
  Construct time-points data frame as required by
  \code{grofit}.
}
\keyword{internal}

\docType{methods}
\alias{to_grofit_time-methods}
\alias{to_grofit_time,OPM-method}
\usage{
  \S4method{to_grofit_time}{OPM}(object) 

}
