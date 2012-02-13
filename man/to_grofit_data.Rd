\name{to_grofit_data}
\alias{to_grofit_data}
\title{Data for grofit}
\arguments{
  \item{object}{\code{\link{OPM}} object.}
}
\value{
  Dataframe with columns: (i) well ID, (ii) plate ID, (iii)
  dummy concentration, (iv - end) measurements, one row for
  each well.
}
\description{
  Construct dataframe with measurements as required by
  \code{grofit}.
}
\keyword{internal}

\docType{methods}
\alias{to_grofit_data-methods}
\alias{to_grofit_data,OPM-method}
\usage{
  \S4method{to_grofit_data}{OPM}(object) 
}
