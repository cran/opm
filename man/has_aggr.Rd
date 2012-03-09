\name{has_aggr}
\alias{has_aggr}
\title{Are aggregated data present?}
\arguments{
  \item{object}{\code{\link{OPM}} object.}
}
\value{
  Logical scalar.
}
\description{
  Check whether aggregated data are present. This always
  returns \code{FALSE} for the \code{\link{OPM}} class, but
  not necessarily for its child classes.
}
\examples{
data(vaas_1)
stopifnot(has_aggr(vaas_1))
}
\seealso{
  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{duplicated}}, \code{\link{filename}},
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
\alias{has_aggr-methods}
\alias{has_aggr,OPM-method}
\usage{
  \S4method{has_aggr}{OPM}(object) 

}
