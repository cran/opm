\name{aggr_settings}
\alias{aggr_settings}
\title{Get aggregation settings}
\arguments{
  \item{object}{\code{\link{OPMA}} object.}
}
\value{
  Named list. See the example for details.
}
\description{
  The settings used for aggregating the kinetic data.
}
\examples{
data(vaas_1)
x <- aggr_settings(vaas_1)
stopifnot(is.list(x), identical(names(x), c("program", "options")))
stopifnot(identical(x$program, "grofit"))
}
\seealso{
  Other aggregation-functions: \code{\link{aggregated}},
  \code{\link{do_aggr}}, \code{\link{fast_estimate}},
  \code{\link{param_names}}

  Other getter-functions: \code{\link{aggregated}},
  \code{\link{anyDuplicated}}, \code{\link{csv_data}},
  \code{\link{dim}}, \code{\link{duplicated}},
  \code{\link{filename}}, \code{\link{has_aggr}},
  \code{\link{hours}}, \code{\link{length}},
  \code{\link{max}}, \code{\link{measurements}},
  \code{\link{metadata}}, \code{\link{minmax}},
  \code{\link{plate_type}}, \code{\link{position}},
  \code{\link{select}}, \code{\link{seq}},
  \code{\link{setup_time}}, \code{\link{summary}},
  \code{\link{thin_out}}, \code{\link{well}},
  \code{\link{wells}}
}
\keyword{attribute}

\docType{methods}
\alias{aggr_settings-methods}
\alias{aggr_settings,OPMA-method}
\usage{
  \S4method{aggr_settings}{OPMA}(object)

}
