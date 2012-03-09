\name{thin_out}
\alias{thin_out}
\title{Thin out the measurements}
\arguments{
  \item{object}{\code{\link{OPM}} object.}

  \item{factor}{Numeric scalar >= 1 indicating how much the
  dataset shall be thinned out.}

  \item{drop}{Logical scalar. See \code{\link{[}}.}
}
\value{
  \code{\link{OPM}} object.
}
\description{
  Thin out some \code{\link{OPM}} measurements by keeping
  only each n-th time point. A mainly experimental function
  that might be of use in testing.
}
\note{
  Thinning the plates out is experimental insofar as it has
  \strong{not} been tested whether and how this could
  sensibly be applied before aggregating the data.
}
\examples{
data(vaas_1)
(x <- dim(vaas_1))
stopifnot(identical(x, c(384L, 96L)))
copy <- thin_out(vaas_1, 10) # keep every 10th time point and measurement
(x <- dim(copy))
stopifnot(identical(x, c(38L, 96L)), has_aggr(copy))
copy <- thin_out(vaas_1, 10, drop = TRUE) # also remove the parameters
(x <- dim(copy))
stopifnot(identical(x, c(38L, 96L)), !has_aggr(copy))
}
\seealso{
  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{duplicated}}, \code{\link{filename}},
  \code{\link{has_aggr}}, \code{\link{hours}},
  \code{\link{length}}, \code{\link{max}},
  \code{\link{measurements}}, \code{\link{metadata}},
  \code{\link{minmax}}, \code{\link{plate_type}},
  \code{\link{position}}, \code{\link{select}},
  \code{\link{seq}}, \code{\link{setup_time}},
  \code{\link{summary}}, \code{\link{well}},
  \code{\link{wells}}
}
\keyword{manip}

\docType{methods}
\alias{thin_out-methods}
\alias{thin_out,OPM-method}
\usage{
  \S4method{thin_out}{OPM}(object, factor, drop = FALSE) 

}
