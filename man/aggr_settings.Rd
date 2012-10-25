\name{aggr_settings}
\alias{aggr_settings}
\title{Get aggregation settings}
\arguments{
  \item{object}{\code{\link{OPMA}} object.}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  Named list. See the examples for details.
}
\description{
  The settings used for aggregating the kinetic data.
}
\examples{
# 'OPM' method
data(vaas_1)
summary(x <- aggr_settings(vaas_1))
stopifnot(is.list(x), identical(names(x), c("program", "options")))
stopifnot(identical(x$program, "grofit"))

# 'OPMS' method
data(vaas_4)
summary(x <- aggr_settings(vaas_4))
stopifnot(is.list(x), length(x) == length(vaas_4), sapply(x, is.list))
}
\seealso{
  Other getter-functions: \code{\link{aggregated}},
  \code{\link{anyDuplicated}}, \code{\link{csv_data}},
  \code{\link{dim}}, \code{\link{disc_settings}},
  \code{\link{discretized}}, \code{\link{duplicated}},
  \code{\link{filename}}, \code{\link{has_aggr}},
  \code{\link{has_disc}}, \code{\link{hours}},
  \code{\link{length}}, \code{\link{max}},
  \code{\link{measurements}}, \code{\link{metadata}},
  \code{\link{minmax}}, \code{\link{plate_type}},
  \code{\link{position}}, \code{\link{select}},
  \code{\link{seq}}, \code{\link{setup_time}},
  \code{\link{show}}, \code{\link{summary}},
  \code{\link{well}}, \code{\link{wells}}
}
\keyword{attribute}

\docType{methods}
\alias{aggr_settings-methods}
\alias{aggr_settings,OPMA-method}
\alias{aggr_settings,OPMS-method}
\usage{
  \S4method{aggr_settings}{OPMA}(object) 

  \S4method{aggr_settings}{OPMS}(object, ...) 

}
