\name{max}
\alias{max}
\title{Maximum}
\arguments{
  \item{x}{\code{\link{OPM}} or \code{\link{OPMS}} object.}

  \item{...}{Coordinate of one to several wells. If
  missing, the maximum of all wells is returned. See
  \code{\link{well}} for details.}

  \item{na.rm}{Logical scalar. See \code{max} from the
  \pkg{base} package. Has no effect here because \code{NA}
  values are not allowed within the measurements.}
}
\value{
  Numeric scalar.
}
\description{
  Get the maximal value of all wells or (a) specified
  one(s). The \code{\link{OPMS}} method works by calling
  the \code{\link{OPM}} method on all plates and then
  determining the overall maximum.
}
\examples{
# OPM method
data(vaas_1)
(x <- max(vaas_1))
(y <- max(vaas_1, 1)) # this is the negative control
stopifnot(x > y)

# OPMS method
data(vaas_4)
(x <- max(vaas_4))
(y <- max(vaas_4, 1)) # this is the negative control
stopifnot(x > y)
}
\seealso{
  base::max

  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{disc_settings}}, \code{\link{discretized}},
  \code{\link{duplicated}}, \code{\link{filename}},
  \code{\link{has_aggr}}, \code{\link{has_disc}},
  \code{\link{hours}}, \code{\link{length}},
  \code{\link{measurements}}, \code{\link{metadata}},
  \code{\link{minmax}}, \code{\link{plate_type}},
  \code{\link{position}}, \code{\link{select}},
  \code{\link{seq}}, \code{\link{setup_time}},
  \code{\link{show}}, \code{\link{summary}},
  \code{\link{well}}, \code{\link{wells}}
}
\keyword{attribute}
\keyword{dplot}

\docType{methods}
\alias{max-methods}
\alias{max,OPM-method}
\alias{max,OPMS-method}
\usage{
  \S4method{max}{OPM}(x, ..., na.rm = FALSE) 

  \S4method{max}{OPMS}(x, ..., na.rm = FALSE) 

}
