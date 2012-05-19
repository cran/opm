\name{minmax}
\alias{minmax}
\title{Smallest maximum}
\arguments{
  \item{x}{\code{\link{OPM}} or \code{\link{OPMS}} object.}

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
  Get the smallest maximum among all wells. The
  \code{\link{OPMS}} method works by calling the
  \code{\link{OPM}} method on all plates and then
  determining the overall minimum.
}
\examples{
# OPM method
data(vaas_1)
(x <- max(vaas_1))
(y <- minmax(vaas_1))
stopifnot(x > y)

# OPMS method
data(vaas_4)
(x <- max(vaas_4))
(y <- minmax(vaas_4))
stopifnot(x > y)
}
\seealso{
  base::min base::max

  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{duplicated}}, \code{\link{filename}},
  \code{\link{has_aggr}}, \code{\link{hours}},
  \code{\link{length}}, \code{\link{max}},
  \code{\link{measurements}}, \code{\link{metadata}},
  \code{\link{position}}, \code{\link{select}},
  \code{\link{seq}}, \code{\link{setup_time}},
  \code{\link{show}}, \code{\link{summary}},
  \code{\link{thin_out}}, \code{\link{well}},
  \code{\link{wells}}
}
\keyword{attribute}
\keyword{dplot}

\docType{methods}
\alias{minmax-methods}
\alias{minmax,OPM-method}
\alias{minmax,OPMS-method}
\usage{
  \S4method{minmax}{OPM}(x, ..., na.rm = FALSE) 

  \S4method{minmax}{OPMS}(x, ..., na.rm = FALSE) 

}
