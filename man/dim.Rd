\name{dim}
\alias{dim}
\title{Dimensions}
\arguments{
  \item{x}{\code{\link{OPM}} or \code{\link{OPMS}} object.}
}
\value{
  For the \code{\link{OPM}} method, a two-element numeric
  vector (number of time points and number of wells). For
  the \code{\link{OPMS}} method, a numeric vector with (i)
  the number of contained \code{\link{OPM}} objects, and
  (ii) and (iii) the dimensions of the first plate.
}
\description{
  Get the dimensions of the measurements of an
  \code{\link{OPM}} object, or get the dimensions of an
  \code{\link{OPMS}} object. Note that this function cannot
  be used to determine the correspondence of the time
  points between all plates as it reports only the time
  points of the first plate. Instead the \code{\link{OPMS}}
  method of \code{\link{hours}} must be used.
}
\examples{
# OPM method
data(vaas_1)
(x <- dim(vaas_1))
stopifnot(identical(x, c(384L, 96L)))

# OPMS method
data(vaas_4)
(x <- dim(vaas_4)) # 2nd value needs not be correct for all plates
stopifnot(identical(x, c(4L, 384L, 96L)))
}
\seealso{
  base::dim

  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{csv_data}}, \code{\link{disc_settings}},
  \code{\link{discretized}}, \code{\link{duplicated}},
  \code{\link{filename}}, \code{\link{has_aggr}},
  \code{\link{has_disc}}, \code{\link{hours}},
  \code{\link{length}}, \code{\link{max}},
  \code{\link{measurements}}, \code{\link{metadata}},
  \code{\link{minmax}}, \code{\link{plate_type}},
  \code{\link{position}}, \code{\link{seq}},
  \code{\link{setup_time}}, \code{\link{subset}},
  \code{\link{summary}}, \code{\link{well}},
  \code{\link{wells}}
}
\keyword{attribute}

\docType{methods}
\alias{dim-methods}
\alias{dim,OPM-method}
\alias{dim,OPMS-method}
\usage{
  \S4method{dim}{OPM}(x) 

  \S4method{dim}{OPMS}(x) 

}
