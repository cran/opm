\name{setup_time}
\alias{setup_time}
\title{Setup time of the measuring}
\arguments{
  \item{object}{\code{\link{OPM}} object.}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  Character scalar.
}
\description{
  Get the setup time of the PM experiment as recorded by
  the OmniLog(R) device. This is a convenience function for
  one of the more important entries of
  \code{\link{csv_data}}.
}
\examples{
# 'OPM' method
data(vaas_1)
(x <- setup_time(vaas_1))
# WARNING: It is unlikely that all OmniLog output has this setup time format
(parsed <- strptime(x, format = "\%m/\%d/\%Y \%I:\%M:\%S \%p"))
stopifnot(inherits(parsed, "POSIXlt"), length(parsed) == 1)

# 'OPMS' method
data(vaas_4)
(x <- setup_time(vaas_4))
(parsed <- strptime(x, format = "\%m/\%d/\%Y \%I:\%M:\%S \%p"))
stopifnot(inherits(parsed, "POSIXlt"), length(parsed) == 4)
}
\seealso{
  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{duplicated}}, \code{\link{filename}},
  \code{\link{has_aggr}}, \code{\link{hours}},
  \code{\link{length}}, \code{\link{max}},
  \code{\link{measurements}}, \code{\link{metadata}},
  \code{\link{minmax}}, \code{\link{plate_type}},
  \code{\link{position}}, \code{\link{select}},
  \code{\link{seq}}, \code{\link{summary}},
  \code{\link{thin_out}}, \code{\link{well}},
  \code{\link{wells}}
}
\keyword{attribute}

\docType{methods}
\alias{setup_time-methods}
\alias{setup_time,OPM-method}
\alias{setup_time,OPMS-method}
\usage{
  \S4method{setup_time}{OPM}(object) 

  \S4method{setup_time}{OPMS}(object, ...) 

}
