\name{max}
\alias{max}
\title{Maximum}
\arguments{
  \item{x}{\code{\link{OPM}} object.}

  \item{...}{Coordinate of one to several wells. If
  missing, the maximum of all wells is returned. See
  \code{\link{well}} or details.}

  \item{na.rm}{Logical scalar. See \code{max} from the
  \pkg{base} package. Has no effect here because \code{NA}
  values are not allowed within the measurements.}
}
\value{
  Numeric scalar.
}
\description{
  Get the maximal value of all wells or (a) specified
  one(s).
}
\examples{
data(vaas_1)
(x <- max(vaas_1))
(y <- max(vaas_1, 1)) # this is the negative control
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
  \code{\link{length}}, \code{\link{max,OPMS}},
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
\alias{max-methods}
\alias{max,OPM-method}
\usage{
  \S4method{max}{OPM}(x, ..., na.rm = FALSE) 
}
