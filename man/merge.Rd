\name{merge}
\alias{merge}
\title{Merge plates}
\arguments{
  \item{x}{\code{\link{OPMS}} object.}

  \item{y}{Numeric vector indicating the time(s) (in hours)
  between two subsequent plates. Must be positive
  throughout, and its length should fit to the number of
  plates (e.g., either \code{1} or \code{length(x) - 1}
  would work). If missing, \code{0.25} is used.}
}
\value{
  \code{\link{OPM}} object. The \code{\link{metadata}} and
  \code{\link{csv_data}} will be taken from the first
  contained plate, but aggregated values, if any, will be
  dropped.
}
\description{
  Combine all plates in a single \code{\link{OPM}} object
  by treating them as originating from subsequent runs of
  the same experimental plate. Adjust the times
  accordingly.
}
\note{
  This function is intended for dealing with slowly growing
  or reacting organisms that need to be analyzed with
  subsequent runs of the same plate in PM mode. Results
  obtained with \emph{Geodermatophilus} strains and
  Generation-III plates indicate that this works well in
  practice.
}
\examples{
data(vaas_4)
summary(x <- merge(vaas_4))
stopifnot(is(x, "OPM"), dim(x) == c(sum(hours(vaas_4, "size")), 96))
}
\seealso{
  Other conversion-functions: \code{\link{extract}},
  \code{\link{extract_columns}}, \code{\link{flatten}},
  \code{\link{oapply}}, \code{\link{plates}},
  \code{\link{sort}}, \code{\link{to_yaml}}
}
\keyword{manip}

\docType{methods}
\alias{merge-methods}
\alias{merge,OPMS,numeric-method}
\alias{merge,OPMS,missing-method}
\usage{
  \S4method{merge}{OPMS,numeric}(x, y) 

  \S4method{merge}{OPMS,missing}(x) 

}
