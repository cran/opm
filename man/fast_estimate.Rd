\name{fast_estimate}
\alias{fast_estimate}
\title{Fast curve-parameter estimation}
\arguments{
  \item{x}{Matrix as output by \code{\link{measurements}},
  i.e. with the time points in the first columns and the
  measurements in the remaining columns (there must be at
  least two). For deviations from this scheme see
  \code{time.pos} and \code{transposed}.}

  \item{what}{Character scalar. Which parameter to
  estimate. Currently only two are supported.}

  \item{boot}{Integer scalar. Number of bootstrap
  replicates. Note that under the default settings for
  \code{as.pe}, bootstrapping is also necessary to obtain
  the point estimate.}

  \item{ci}{Confidence interval to use in the output.
  Ignored if \code{boot} is not positive.}

  \item{as.pe}{Character scalar determining what to output
  as the point estimate. Either \sQuote{median},
  \sQuote{mean} or \sQuote{pe}; the first two calculate the
  point estimate from the bootstrapping replicates, the
  third one use the point estimate from the raw data. If
  \code{boot} is 0, \code{as.pe} is reset to \sQuote{pe},
  if necessary, and a warning is issued.}

  \item{ci.type}{Character scalar determining the way the
  confidence intervals are calculated. Either
  \sQuote{norm}, \sQuote{basic} or \sQuote{perc}; see
  \code{boot.ci} from the \pkg{boot} package for details.}

  \item{time.pos}{Character or integer scalar indicating
  the position of the column (or row, see next argument)
  with the time points.}

  \item{transposed}{Character or integer scalar indicating
  whether the matrix is transposed compared to the
  default.}

  \item{raw}{Logical scalar. Return the raw bootstrapping
  result without CI estimation and construction of the
  usually resulting matrix?}

  \item{...}{Optional arguments passed to \code{boot} from
  the eponymous package.}
}
\value{
  Numeric matrix with three rows (point estimate, lower and
  upper CI) and as many columns as data columns (or rows)
  in \code{x}. If \code{raw} is \code{TRUE}, an object of
  the class \sQuote{boot}.
}
\description{
  Quickly estimate the curve parameters AUC (area under the
  curve) or A (maximum height). This is normally not
  directly called by an \pkg{opm} user but via
  \code{\link{do_aggr}}.
}
\examples{
data(vaas_1)
(x <- fast_estimate(measurements(vaas_1)))[, 1:3]
stopifnot(identical(dim(x), c(3L, 96L)))
}
\seealso{
  boot::boot grofit::grofit

  Other aggregation-functions: \code{\link{do_aggr}},
  \code{\link{param_names}}
}
\keyword{smooth}

\docType{methods}
\alias{fast_estimate-methods}
\alias{fast_estimate,matrix-method}
\usage{
  \S4method{fast_estimate}{matrix}(x, what = c("AUC", "A"),
    boot = 100L, ci = 0.95, as.pe = "median", ci.type = "norm",
    time.pos = 1L, transposed = FALSE, raw = FALSE, ...) 

}
