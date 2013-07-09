\name{length}
\alias{length}
\title{OPMS length (number of plates)}
\arguments{
  \item{x}{\code{\link{OPMS}} object.}
}
\value{
  Numeric scalar.
}
\description{
  Get the number of plates stored in an \code{\link{OPMS}}
  object.
}
\examples{
data(vaas_4)
(x <- length(vaas_4))
stopifnot(identical(x, 4L)) # 4 plates contained
}
\seealso{
  base::length

  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{disc_settings}}, \code{\link{discretized}},
  \code{\link{duplicated}}, \code{\link{filename}},
  \code{\link{has_aggr}}, \code{\link{has_disc}},
  \code{\link{hours}}, \code{\link{max}},
  \code{\link{measurements}}, \code{\link{metadata}},
  \code{\link{minmax}}, \code{\link{plate_type}},
  \code{\link{position}}, \code{\link{seq}},
  \code{\link{setup_time}}, \code{\link{subset}},
  \code{\link{summary}}, \code{\link{well}},
  \code{\link{wells}}
}
\keyword{attribute}

\docType{methods}
\alias{length-methods}
\alias{length,OPMS-method}
\usage{
  \S4method{length}{OPMS}(x) 

}
