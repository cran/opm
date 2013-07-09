\name{disc_settings}
\alias{disc_settings}
\title{Get discretization settings}
\arguments{
  \item{object}{\code{\link{OPMD}} object.}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  Named list. See the examples for details.
}
\description{
  The settings used for discretizing the aggregated kinetic
  data.
}
\note{
  See \code{\link{do_disc}} for generating discretized
  data.
}
\examples{
# 'OPM' method
data(vaas_1)
(x <- disc_settings(vaas_1)) # => named list
stopifnot(is.list(x), !is.null(names(x)))

# 'OPMS' method
data(vaas_4)
summary(x <- disc_settings(vaas_4)) # => list of named lists, one per plate
stopifnot(is.list(x), is.null(names(x)), length(x) == length(vaas_4))
stopifnot(duplicated(x)[-1])
}
\seealso{
  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{discretized}}, \code{\link{duplicated}},
  \code{\link{filename}}, \code{\link{has_aggr}},
  \code{\link{has_disc}}, \code{\link{hours}},
  \code{\link{length}}, \code{\link{max}},
  \code{\link{measurements}}, \code{\link{metadata}},
  \code{\link{minmax}}, \code{\link{plate_type}},
  \code{\link{position}}, \code{\link{seq}},
  \code{\link{setup_time}}, \code{\link{subset}},
  \code{\link{summary}}, \code{\link{well}},
  \code{\link{wells}}
}
\keyword{attribute}

\docType{methods}
\alias{disc_settings-methods}
\alias{disc_settings,OPMD-method}
\alias{disc_settings,OPMS-method}
\usage{
  \S4method{disc_settings}{OPMD}(object) 

  \S4method{disc_settings}{OPMS}(object, ...) 

}
