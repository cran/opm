\name{position}
\alias{position}
\title{Position of a plate}
\arguments{
  \item{object}{\code{\link{OPM}} object.}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  Character scalar.
}
\description{
  Get the position of the plate within the OmniLog(R)
  device. This is a convenience function for one of the
  more important entries of \code{\link{csv_data}}.
}
\examples{
# 'OPM' method
data(vaas_1)
(x <- position(vaas_1))
stopifnot(identical(x, " 7-B"))

# 'OPMS' method
data(vaas_4)
(x <- position(vaas_4))
stopifnot(is.character(x), length(x) == length(vaas_4))
}
\seealso{
  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{duplicated}}, \code{\link{filename}},
  \code{\link{has_aggr}}, \code{\link{hours}},
  \code{\link{length}}, \code{\link{max}},
  \code{\link{measurements}}, \code{\link{metadata}},
  \code{\link{minmax}}, \code{\link{select}},
  \code{\link{seq}}, \code{\link{setup_time}},
  \code{\link{show}}, \code{\link{summary}},
  \code{\link{thin_out}}, \code{\link{well}},
  \code{\link{wells}}
}
\keyword{attribute}

\docType{methods}
\alias{position-methods}
\alias{position,OPM-method}
\alias{position,OPMS-method}
\usage{
  \S4method{position}{OPM}(object) 

  \S4method{position}{OPMS}(object, ...) 

}
