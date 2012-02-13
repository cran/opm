\name{max,OPMS}
\alias{max,OPMS}
\title{Maximum (OPMS version)}
\arguments{
  \item{x}{\code{\link{OPMS}} object.}

  \item{...}{Passed to to the eponymous method of the
  \code{\link{OPM}} class, \code{\link{max}}.}

  \item{na.rm}{Passed to to the eponymous method of the
  \code{\link{OPM}} class, \code{\link{max}}.}
}
\value{
  Single-element numeric vector.
}
\description{
  Maximum value of all wells or (a) specified one(s). This
  works by calling \code{\link{max}} on all plates and then
  determining the overall maximum.
}
\examples{
data(vaas_4)
(x <- max(vaas_4))
(y <- max(vaas_4, 1)) # this is the negative control
stopifnot(x > y)
}
\seealso{
  max

  Other getter-functions: \code{\link{[}},
  \code{\link{[,OPMA}}, \code{\link{[,OPMS}},
  \code{\link{aggr_settings}}, \code{\link{aggregated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{dim,OPMS}}, \code{\link{filename}},
  \code{\link{has_aggr}}, \code{\link{hours}},
  \code{\link{length}}, \code{\link{max}},
  \code{\link{measurements}}, \code{\link{metadata}},
  \code{\link{minmax}}, \code{\link{minmax,OPMS}},
  \code{\link{plate_type}}, \code{\link{position}},
  \code{\link{select}}, \code{\link{setup_time}},
  \code{\link{summary}}, \code{\link{summary,OPMS}},
  \code{\link{thin_out}}, \code{\link{well}},
  \code{\link{wells}}
}
\keyword{attribute}
\keyword{dplot}

\docType{methods}
\alias{max,OPMS-method}
\usage{
  \S4method{max}{OPMS}(x, ..., na.rm = FALSE) 
}
