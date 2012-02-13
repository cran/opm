\name{hours}
\alias{hours}
\title{Overall measuring hours}
\arguments{
  \item{object}{\code{\link{OPM}} object.}

  \item{what}{Character scalar determining the output mode
  as follows: \describe{ \item{all}{Numeric vector: all
  time points, in order.} \item{interval}{The difference
  between each pair of adjacent time points, \code{NA} if
  this is irregular or only one time point is left.}
  \item{max}{Numeric scalar: the largest time point.}
  \item{minmax}{Numeric scalar: the smallest maximum. For
  \code{\link{OPM}} objects this is apparently identical to
  \sQuote{max}.} \item{size}{Integer scalar: the number of
  time points.} \item{summary}{Display a summary.} }}
}
\value{
  Dependent on the \code{what} argument; see there.
}
\description{
  Get the total number of measurements hours as stored in
  an \code{\link{OPM}} object.
}
\examples{
data(vaas_1)
(x <- hours(vaas_1)) # the default is 'max'
stopifnot(identical(x, 95.75))
(x <- hours(vaas_1, "minmax"))
stopifnot(identical(x, 95.75))
(x <- hours(vaas_1, "summary"))
stopifnot(is.table(x))
(x <- hours(vaas_1, "interval"))
stopifnot(identical(x, 0.25))
(x <- hours(vaas_1, "size"))
stopifnot(identical(x, 384L))
}
\seealso{
  Other getter-functions: \code{\link{[}},
  \code{\link{[,OPMA}}, \code{\link{[,OPMS}},
  \code{\link{aggr_settings}}, \code{\link{aggregated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{dim,OPMS}}, \code{\link{filename}},
  \code{\link{has_aggr}}, \code{\link{length}},
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
\alias{hours-methods}
\alias{hours,OPM-method}
\usage{
  \S4method{hours}{OPM}(object, 
    what = c("max", "all", "size", "summary", "interval", "minmax")) 
}
