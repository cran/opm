\name{has_disc}
\alias{has_disc}
\title{Are discretized data present?}
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
  Check whether discretized data are present. This always
  returns \code{FALSE} for the \code{\link{OPM}} class, but
  not necessarily for its child classes.
}
\examples{
# 'OPM' method
data(vaas_1)
(x <- has_disc(vaas_1))
stopifnot(x)

# 'OPMS' method
data(vaas_4)
(x <- has_disc(vaas_4))
stopifnot(x)
}
\seealso{
  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{disc_settings}}, \code{\link{discretized}},
  \code{\link{duplicated}}, \code{\link{filename}},
  \code{\link{has_aggr}}, \code{\link{hours}},
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
\alias{has_disc-methods}
\alias{has_disc,OPM-method}
\alias{has_disc,OPMS-method}
\usage{
  \S4method{has_disc}{OPM}(object) 

  \S4method{has_disc}{OPMS}(object, ...) 

}
