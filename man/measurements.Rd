\name{measurements}
\alias{measurements}
\title{Stored measurements}
\arguments{
  \item{object}{\code{\link{OPM}} object.}

  \item{i}{Optional character or numeric vector with
  name(s) or position(s) of well(s).}
}
\value{
  Numeric matrix with column names indicating the well
  coordinate and a first column containing the time points.
}
\description{
  Return the measurements. The first column contains the
  hours, the other ones contain the values from each well.
  There is one row per time point. Column names are
  appropriately set, but not translated (as, e.g., to
  substrate names). It is possible to select wells, but the
  time points are always included as first column (in
  contrast to \code{\link{well}}). The \code{i} argument
  refers only to the remaining matrix.
}
\examples{
data(vaas_1)
x <- measurements(vaas_1)
stopifnot(is.matrix(x), is.numeric(x))
stopifnot(identical(dim(x), c(384L, 97L)))
y <- measurements(vaas_1, "B03")
stopifnot(is.matrix(y), is.numeric(y))
stopifnot(identical(dim(y), c(384L, 2L)))
}
\seealso{
  Other getter-functions: \code{\link{[}},
  \code{\link{[,OPMA}}, \code{\link{[,OPMS}},
  \code{\link{aggr_settings}}, \code{\link{aggregated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{dim,OPMS}}, \code{\link{filename}},
  \code{\link{has_aggr}}, \code{\link{hours}},
  \code{\link{length}}, \code{\link{max}},
  \code{\link{max,OPMS}}, \code{\link{metadata}},
  \code{\link{minmax}}, \code{\link{minmax,OPMS}},
  \code{\link{plate_type}}, \code{\link{position}},
  \code{\link{select}}, \code{\link{setup_time}},
  \code{\link{summary}}, \code{\link{summary,OPMS}},
  \code{\link{thin_out}}, \code{\link{well}},
  \code{\link{wells}}
}
\keyword{attribute}

\docType{methods}
\alias{measurements-methods}
\alias{measurements,OPM-method}
\usage{
  \S4method{measurements}{OPM}(object, i) 
}