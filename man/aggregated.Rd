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
  too large or too small. If \code{trim} is \sQuote{medium}
  or \sQuote{full}, lambda estimates larger than
  \code{\link{hours}} are set to that value. Negative
  lambda estimates smaller than \code{\link{hours}} are set
  to this value if \code{trim} is \sQuote{medium}; this is
  a more moderate treatment than setting all negative
  values to zero, which is done if \code{trim} is
  \sQuote{full}. Currently the other parameters are not
  checked, and all \code{NA} values also remain unchanged.
  If \code{trim} is \sQuote{no}, lambda is not modified
  either.}
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
\examples{
data(vaas_1)
# Get full matrix
x <- aggregated(vaas_1)
stopifnot(is.matrix(x), identical(dim(x), c(12L, 96L)))
# Subsetting
x <- aggregated(vaas_1, "lambda")
stopifnot(is.matrix(x), identical(dim(x), c(3L, 96L)), any(x < 0))
# Now with lambda correction
x <- aggregated(vaas_1, "lambda", trim = "full")
stopifnot(is.matrix(x), identical(dim(x), c(3L, 96L)), !any(x < 0))
}
\seealso{
  Other aggregation-functions: \code{\link{aggr_settings}},
  \code{\link{do_aggr}}, \code{\link{fast_estimate}},
  \code{\link{param_names}}

  Other getter-functions: \code{\link{aggr_settings}},
  \code{\link{anyDuplicated}}, \code{\link{csv_data}},
  \code{\link{dim}}, \code{\link{duplicated}},
  \code{\link{filename}}, \code{\link{has_aggr}},
  \code{\link{hours}}, \code{\link{length}},
  \code{\link{max}}, \code{\link{measurements}},
  \code{\link{metadata}}, \code{\link{minmax}},
  \code{\link{plate_type}}, \code{\link{position}},
  \code{\link{select}}, \code{\link{seq}},
  \code{\link{setup_time}}, \code{\link{summary}},
  \code{\link{thin_out}}, \code{\link{well}},
  \code{\link{wells}}
}
\keyword{attribute}

\docType{methods}
\alias{aggregated-methods}
\alias{aggregated,OPMA-method}
\usage{
  \S4method{aggregated}{OPMA}(object, subset = NULL, ci = TRUE,
    trim = c("no", "full", "medium")) 

}
