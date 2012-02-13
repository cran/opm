\name{dim,OPMS}
\alias{dim,OPMS}
\title{Dimensions (OPMS versions)}
\arguments{
  \item{x}{\code{\link{OPMS}} object.}
}
\value{
  Numeric vector with (i) number of contained
  \code{\link{OPM}} objects, (ii) and (iii) dimensions of
  first plate. See \code{\link{dim}}.
}
\description{
  Get the dimensions of an \code{\link{OPMS}} object. Note
  that this function cannot be used to determine the
  correspondence of the time points between all plates as
  it reports only the time points of the first plate.
  Instead the \code{\link{OPMS}} version of
  \code{\link{hours}} must be used.
}
\examples{
data(vaas_4)
(x <- dim(vaas_4))
stopifnot(identical(x, c(4L, 384L, 96L)))
}
\seealso{
  dim

  Other getter-functions: \code{\link{[}},
  \code{\link{[,OPMA}}, \code{\link{[,OPMS}},
  \code{\link{aggr_settings}}, \code{\link{aggregated}},
  \code{\link{csv_data}}, \code{\link{dim}},
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
\alias{dim,OPMS-method}
\usage{
  \S4method{dim}{OPMS}(x) 
}
