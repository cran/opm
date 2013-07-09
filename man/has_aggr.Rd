\name{has_aggr}
\alias{has_aggr}
\title{Are aggregated data present?}
\arguments{
  \item{object}{\code{\link{OPM}} or \code{\link{OPMS}}
  object.}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  Logical vector, one element per plate.
}
\description{
  Check whether aggregated data are present. This always
  returns \code{FALSE} for the \code{\link{OPM}} class, but
  not necessarily for its child classes.
}
\note{
  See \code{\link{do_aggr}} for generating aggregated data.
}
\examples{
# 'OPM' method
data(vaas_1)
stopifnot(has_aggr(vaas_1))

# 'OPMS' method
data(vaas_4)
stopifnot(has_aggr(vaas_4))
}
\seealso{
  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{disc_settings}}, \code{\link{discretized}},
  \code{\link{duplicated}}, \code{\link{filename}},
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
\alias{has_aggr-methods}
\alias{has_aggr,OPM-method}
\alias{has_aggr,OPMS-method}
\usage{
  \S4method{has_aggr}{OPM}(object) 

  \S4method{has_aggr}{OPMS}(object, ...) 

}
