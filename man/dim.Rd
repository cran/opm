\name{dim}
\alias{dim}
\title{Dimensions}
\arguments{
  \item{x}{\code{\link{OPM}} object.}
}
\value{
  Two-element numeric vector (number of time points and
  number of wells).
}
\description{
  Get the dimensions of the measurements.
}
\examples{
data(vaas_1)
(x <- dim(vaas_1))
stopifnot(identical(x, c(384L, 96L)))
}
\seealso{
  dim

  Other getter-functions: \code{\link{[}},
  \code{\link{[,OPMA}}, \code{\link{[,OPMS}},
  \code{\link{aggr_settings}}, \code{\link{aggregated}},
  \code{\link{csv_data}}, \code{\link{dim,OPMS}},
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
\alias{dim-methods}
\alias{dim,OPM-method}
\usage{
  \S4method{dim}{OPM}(x) 
}
