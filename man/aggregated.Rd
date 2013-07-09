\name{aggregated}
\alias{aggregated}
\title{Get aggregated kinetic data}
\arguments{
  \item{object}{\code{\link{OPMA}} object.}

  \item{subset}{Character vector. If not \code{NULL},
  restrict to this or these parameter(s). See
  \code{\link{param_names}} for the possible values.}

  \item{ci}{Logical scalar. Include the estimates of
  confidence intervals (CIs) in the output?}

  \item{trim}{Character scalar. Parameter estimates from
  intrinsically negative reactions (i.e., no respiration)
  are sometimes biologically unreasonable because they are
  too large or too small, and some corrections might be
  appropriate. \describe{ \item{no}{No modification.}
  \item{full}{Negative lamdda estimates are set to zero.}
  \item{medium}{Lambda estimates larger than
  \code{\link{hours}(object)} (i.e., the maximum time value
  observed) are set to that value. Negative lambda
  estimates smaller than \code{-hours(object)} are set to
  this value (i.e., the negative maximum time).}
  \item{full}{Like \sQuote{medium}, but all negative values
  are set to zero, which is a less moderate treatment.} }
  Currently the other parameters are not checked, and all
  \code{NA} values, if any, also remain unchanged.}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  Numeric matrix.
}
\description{
  The aggregated values are the curve parameters. If
  bootstrapping was used, their CIs are included. The
  columns represent the wells, the rows the estimated
  parameters and their CIs.
}
\note{
  See \code{\link{do_aggr}} for generating aggregated data.
}
\examples{
# 'OPMA' method
data(vaas_1)
# Get full matrix
(x <- aggregated(vaas_1))[, 1:3]
stopifnot(is.matrix(x), identical(dim(x), c(12L, 96L)))
# Subsetting
(x <- aggregated(vaas_1, "lambda"))[, 1:3]
stopifnot(is.matrix(x), identical(dim(x), c(3L, 96L)), any(x < 0))
# Now with lambda correction
(x <- aggregated(vaas_1, "lambda", trim = "full"))[, 1:3]
stopifnot(is.matrix(x), identical(dim(x), c(3L, 96L)), !any(x < 0))

# 'OPMS' method
data(vaas_4)
summary(x <- aggregated(vaas_4)) # => one matrix per OPM object
stopifnot(is.list(x), length(x) == length(vaas_4), sapply(x, is.matrix))
}
\seealso{
  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{anyDuplicated}}, \code{\link{csv_data}},
  \code{\link{dim}}, \code{\link{disc_settings}},
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
\alias{aggregated-methods}
\alias{aggregated,OPMA-method}
\alias{aggregated,OPMS-method}
\usage{
  \S4method{aggregated}{OPMA}(object, subset = NULL, ci = TRUE,
    trim = c("no", "full", "medium")) 

  \S4method{aggregated}{OPMS}(object, ...) 

}
