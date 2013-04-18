\name{well}
\alias{well}
\title{Measurements from selected wells}
\arguments{
  \item{object}{\code{\link{OPM}} object.}

  \item{i}{Character or numeric vector with name(s) or
  position(s) of well(s).  Wells are originally named
  \sQuote{A01} to \sQuote{H12} but might have been subset
  beforehand. \code{i} can also be a formula, allowing for
  sequences of well coordinates. See the examples.}

  \item{drop}{Logical scalar. If only a single well was
  selected, simplify it to a vector?}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  Numeric matrix or vector (depending on \code{i} and
  \code{drop}).
}
\description{
  Get measurements from specified well(s) stored in an
  \code{\link{OPM}} object. This function will always
  ignore the time points, in contrast to
  \code{\link{measurements}}.
}
\note{
  Do not confuse this with \code{\link{wells}}.
}
\examples{
# 'OPM' method
data(vaas_1)
head(x <- well(vaas_1, "B04")) # => numeric vector
stopifnot(is.numeric(x), length(x) == 384)
head(x <- well(vaas_1, c("B08", "C07"))) # => numeric matrix
stopifnot(is.matrix(x), dim(x) == c(384, 2))
# selecting adjacent wells is easer if using a formula
head(x <- well(vaas_1, c("B12", "C01", "C02")))
stopifnot(is.matrix(x), dim(x) == c(384, 3))
head(y <- well(vaas_1, ~ B12:C02)) # => same result
stopifnot(identical(x, y))

# 'OPMS' method
data(vaas_4)
head(x <- well(vaas_4, "B04"))[, 1:5] # => numeric matrix
stopifnot(is.matrix(x), dim(x) == c(4, 384))
head(y <- well(vaas_4, ~ B04))[, 1:5] # using a formula
stopifnot(identical(x, y)) # => same result
}
\seealso{
  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{anyDuplicated}},
  \code{\link{csv_data}}, \code{\link{dim}},
  \code{\link{disc_settings}}, \code{\link{discretized}},
  \code{\link{duplicated}}, \code{\link{filename}},
  \code{\link{has_aggr}}, \code{\link{has_disc}},
  \code{\link{hours}}, \code{\link{length}},
  \code{\link{max}}, \code{\link{measurements}},
  \code{\link{metadata}}, \code{\link{minmax}},
  \code{\link{plate_type}}, \code{\link{position}},
  \code{\link{select}}, \code{\link{seq}},
  \code{\link{setup_time}}, \code{\link{subset}},
  \code{\link{summary}}, \code{\link{wells}}
}
\keyword{attribute}

\docType{methods}
\alias{well-methods}
\alias{well,OPM-method}
\alias{well,OPMS-method}
\usage{
  \S4method{well}{OPM}(object, i, drop = TRUE) 

  \S4method{well}{OPMS}(object, ...) 

}
