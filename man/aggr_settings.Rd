\name{aggr_settings}
\alias{aggr_settings}
\title{Get aggregation settings}
\arguments{
  \item{object}{\code{\link{OPMA}} object.}
}
\value{
  Named list.
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
  \code{\link{curve_params}}, \code{\link{do_aggr}},
  \code{\link{fast_estimate}}, \code{\link{param_names}}

  Other getter-functions: \code{\link{[}},
  \code{\link{[,OPMA}}, \code{\link{[,OPMS}},
  \code{\link{aggregated}}, \code{\link{csv_data}},
  \code{\link{dim}}, \code{\link{dim,OPMS}},
  \code{\link{filename}}, \code{\link{has_aggr}},
  \code{\link{hours}}, \code{\link{length}},
  \code{\link{max}}, \code{\link{max,OPMS}},
  \code{\link{measurements}}, \code{\link{metadata}},
  \code{\link{minmax}}, \code{\link{minmax,OPMS}},
  \code{\link{plate_type}}, \code{\link{position}},
  \code{\link{select}}, \code{\link{setup_time}},
  \code{\link{summary}}, \code{\link{summary,OPMS}},
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
