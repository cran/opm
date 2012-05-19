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
  base::length

  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{duplicated}}, \code{\link{filename}},
  \code{\link{has_aggr}}, \code{\link{hours}},
  \code{\link{max}}, \code{\link{measurements}},
  \code{\link{metadata}}, \code{\link{minmax}},
  \code{\link{position}}, \code{\link{select}},
  \code{\link{seq}}, \code{\link{setup_time}},
  \code{\link{show}}, \code{\link{summary}},
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
