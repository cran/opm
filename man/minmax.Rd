\name{minmax}
\alias{minmax}
\title{Smallest maximum}
\arguments{
  \item{x}{\code{\link{OPM}} object.}

  \item{...}{Coordinate of one to several wells. If
  missing, the smallest maximum of all wells is returned.
  See \code{\link{well}} for details. If only as single
  well is selected, the result is actually identical to the
  one of \code{\link{max}}.}

  \item{na.rm}{Logical scalar. See \code{\link{max}}.}
}
\value{
  Numeric scalar.
}
\description{
  Get the smallest maximum among all wells.
}
\examples{
data(vaas_1)
(x <- max(vaas_1))
(y <- minmax(vaas_1))
stopifnot(x > y)
}
\seealso{
  min max

  Other getter-functions: \code{\link{[}},
  \code{\link{[,OPMA}}, \code{\link{[,OPMS}},
  \code{\link{aggr_settings}}, \code{\link{aggregated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{dim,OPMS}}, \code{\link{filename}},
  \code{\link{has_aggr}}, \code{\link{hours}},
  \code{\link{length}}, \code{\link{max}},
  \code{\link{max,OPMS}}, \code{\link{measurements}},
  \code{\link{metadata}}, \code{\link{minmax,OPMS}},
  \code{\link{plate_type}}, \code{\link{position}},
  \code{\link{select}}, \code{\link{setup_time}},
  \code{\link{summary}}, \code{\link{summary,OPMS}},
  \code{\link{thin_out}}, \code{\link{well}},
  \code{\link{wells}}
}
\keyword{attribute}
\keyword{dplot}

\docType{methods}
\alias{minmax-methods}
\alias{minmax,OPM-method}
\usage{
  \S4method{minmax}{OPM}(x, ..., na.rm = FALSE) 
}
