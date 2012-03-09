\name{param_names}
\alias{param_names}
\title{Names of curve parameters}
\usage{
  param_names()
}
\value{
  Character vector.
}
\description{
  Yield the names of the estimated curve parameters used
  internally and in the output.
}
\examples{
(x <- param_names())
stopifnot(is.character(x), length(x) == 4L)
stopifnot(identical(unique(x), x))
}
\seealso{
  Other aggregation-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{do_aggr}},
  \code{\link{fast_estimate}}
}
\keyword{utilities}

