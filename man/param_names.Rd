\name{param_names}
\alias{param_names}
\title{Names of curve parameters}
\usage{
  param_names(what = c("param.names", "reserved.md.names", "split.at"))
}
\arguments{
  \item{what}{Character scalar. Which kind of names to
  obtain.}
}
\value{
  Character vector.
}
\description{
  Yield the names of the estimated curve parameters used
  internally and in the output. Alternatively, yield names
  that should not be used in metadata entries because they
  are used as predefined column names by functions such as
  \code{\link{flatten}}.
}
\details{
  In addition to the results of
  \code{param_names("reserved.md.names")}, it should be
  avoided to use metadata keys that start with a dot, as
  such keys might also be created intermediarily by methods
  that have to compile metadata together with other
  information.
}
\examples{
(x <- param_names())
stopifnot(is.character(x), length(x) > 1, identical(unique(x), x))
(x <- param_names("reserved"))
stopifnot(is.character(x), length(x) > 1, identical(unique(x), x))
stopifnot(param_names("split.at") \%in\% x)
}
\seealso{
  Other aggregation-functions: \code{\link{do_aggr}},
  \code{\link{fast_estimate}}
}
\keyword{utilities}

