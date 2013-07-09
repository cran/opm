\name{measurements}
\alias{measurements}
\title{Stored measurements}
\arguments{
  \item{object}{\code{\link{OPM}} or \code{\link{OPMS}}
  object.}

  \item{i}{Optional character or numeric vector or formula
  with name(s) or position(s) of well(s).}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  Numeric matrix with column names indicating the well
  coordinate and a first column containing the time points.
}
\description{
  Return the measurements. The first column contains the
  hours, the other ones contain the values from each well.
  There is one row per time point.
}
\details{
  Column names are appropriately set, but not translated
  (as, e.g., to substrate names). It is possible to select
  wells, but the time points are always included as first
  column (in contrast to \code{\link{well}}). The \code{i}
  argument refers only to the remaining matrix.
}
\examples{
# 'OPM' method
data(vaas_1)
head(x <- measurements(vaas_1))[, 1:5] # => numeric matrix
stopifnot(is.matrix(x), is.numeric(x))
stopifnot(dim(x) == c(384, 97))
head(x <- measurements(vaas_1, "B03"))
stopifnot(is.matrix(x), is.numeric(x), dim(x) == c(384, 2))
head(y <- measurements(vaas_1, ~B03)) # => same result with formula
stopifnot(identical(y, x))

# 'OPMS' method
data(vaas_4)
summary(x <- measurements(vaas_4)) # => list of numeric matrices
stopifnot(is.list(x), length(x) == length(vaas_4))
stopifnot(sapply(x, is.matrix), sapply(x, is.numeric))
}
\seealso{
  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{disc_settings}}, \code{\link{discretized}},
  \code{\link{duplicated}}, \code{\link{filename}},
  \code{\link{has_aggr}}, \code{\link{has_disc}},
  \code{\link{hours}}, \code{\link{length}},
  \code{\link{max}}, \code{\link{metadata}},
  \code{\link{minmax}}, \code{\link{plate_type}},
  \code{\link{position}}, \code{\link{seq}},
  \code{\link{setup_time}}, \code{\link{subset}},
  \code{\link{summary}}, \code{\link{well}},
  \code{\link{wells}}
}
\keyword{attribute}

\docType{methods}
\alias{measurements-methods}
\alias{measurements,OPM-method}
\alias{measurements,OPMS-method}
\usage{
  \S4method{measurements}{OPM}(object, i) 

  \S4method{measurements}{OPMS}(object, ...) 

}
