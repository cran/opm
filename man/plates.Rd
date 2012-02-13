\name{plates}
\alias{plates}
\title{Get available plates}
\arguments{
  \item{object}{\code{\link{OPMS}} object.}
}
\value{
  List of \code{\link{OPM}} objects.
}
\description{
  Get all plates contained in an \code{\link{OPMS}} object.
}
\examples{
data(vaas_4)
x <- plates(vaas_4)
stopifnot(is.list(x), length(x) == 4L)
}
\seealso{
  list as.list

  Other conversion-functions: \code{\link{extract}},
  \code{\link{extract_columns}}, \code{\link{flatten}},
  \code{\link{flatten,OPMS}}, \code{\link{to_yaml}}
}
\keyword{attribute}

\docType{methods}
\alias{plates-methods}
\alias{plates,OPMS-method}
\usage{
  \S4method{plates}{OPMS}(object) 
}
