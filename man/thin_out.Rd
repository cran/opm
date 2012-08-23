\name{thin_out}
\alias{thin_out}
\title{Thin out the measurements}
\arguments{
  \item{object}{\code{\link{OPM}} object.}

  \item{factor}{Numeric scalar >= 1 indicating how much the
  dataset shall be thinned out.}

  \item{drop}{Logical scalar. See \code{\link{[}}.}

  \item{...}{Optional arguments passed between the
  methods.}
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
# 'OPM' method
data(vaas_1)
(x <- dim(vaas_1))
stopifnot(identical(x, c(384L, 96L)))
copy <- thin_out(vaas_1, 10) # keep every 10th time point and measurement
(x <- dim(copy))
stopifnot(identical(x, c(38L, 96L)), has_aggr(copy))
copy <- thin_out(vaas_1, 10, drop = TRUE) # also remove the parameters
(x <- dim(copy))
stopifnot(identical(x, c(38L, 96L)), !has_aggr(copy))

# 'OPMS' method
data(vaas_4)
(x <- dim(vaas_4))
stopifnot(identical(x, c(4L, 384L, 96L)))
copy <- thin_out(vaas_4, 10)
(x <- dim(copy))
stopifnot(identical(x, c(4L, 38L, 96L)))
}
\seealso{
  Other conversion-functions: \code{\link{extract}},
  \code{\link{extract_columns}}, \code{\link{flatten}},
  \code{\link{gen_iii}}, \code{\link{merge}},
  \code{\link{oapply}}, \code{\link{plates}},
  \code{\link{rep}}, \code{\link{rev}}, \code{\link{sort}},
  \code{\link{to_yaml}}
}
\keyword{manip}

\docType{methods}
\alias{thin_out-methods}
\alias{thin_out,OPM-method}
\alias{thin_out,OPMS-method}
\usage{
  \S4method{thin_out}{OPM}(object, factor, drop = FALSE) 

  \S4method{thin_out}{OPMS}(object, ...) 

}
