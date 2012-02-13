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
stopifnot(identical(x, 4L))
}
\seealso{
  length

  Other getter-functions: \code{\link{[}},
  \code{\link{[,OPMA}}, \code{\link{[,OPMS}},
  \code{\link{aggr_settings}}, \code{\link{aggregated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{dim,OPMS}}, \code{\link{filename}},
  \code{\link{has_aggr}}, \code{\link{hours}},
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
\alias{length-methods}
\alias{length,OPMS-method}
\usage{
  \S4method{length}{OPMS}(x) 
}
