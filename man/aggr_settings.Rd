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
\note{
  See \code{\link{do_aggr}} for generating aggregated data.
}
\examples{
# 'OPM' method
data(vaas_1)
(x <- aggr_settings(vaas_1)) # yields named list
stopifnot(is.list(x), !is.null(names(x)))

# 'OPMS' method
data(vaas_4)
summary(x <- aggr_settings(vaas_4)) # list of named lists, one per plate
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
  \code{\link{subset}}, \code{\link{summary}},
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
