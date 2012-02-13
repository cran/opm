\name{curve_params}
\alias{curve_params}
\title{Estimate curve parameters}
\arguments{
  \item{object}{\code{\link{OPM}} object.}

  \item{boot}{Integer scalar. Number of bootstrap
  replicates used to estimate 95-percent confidence
  intervals (CIs) for the parameter. Set this to zero to
  omit bootstrapping, resulting in \code{NA} entries for
  the CIs.}

  \item{verbose}{Logical scalar. Print progress messages?}

  \item{cores}{Integer scalar. Number of cores to use.
  Setting this to a value > 1 requires the \pkg{multicore}
  package. Has no effect if \sQuote{opm-fast} is chosen
  (see below).}

  \item{options}{List. For its use in \sQuote{grofit} mode,
  see \code{grofit.control} in the \pkg{grofit} package.
  The \code{boot} and \code{verbose} settings, as the most
  important ones, are added separately (see above). The
  verbose mode is not very useful in parallel processing.
  For its use in \sQuote{opm-fast} mode, see
  \code{\link{fast_estimate}}.}

  \item{program}{Character scalar. The aggregation method
  to use. Currently only the following methods are
  supported: \describe{ \item{grofit}{The \code{grofit}
  function in the eponymous package, with spline fitting as
  default.} \item{opm-fast}{The native, faster parameter
  estimation. This will only yield two of the four
  parameters, the area under the curve and the maximum
  height. The area under the curve is estimated as the sum
  of the areas given by the trapezoids defined by each pair
  of adjacent time points. The maximum height is just the
  result of \code{max}. By default, however, the median
  bootstrap value is preferred as point estimate over the
  real point estimate.} }}
}
\value{
  A numeric matrix of the same structure than the one
  returned by \code{\link{aggregated}} but with an
  additional \sQuote{settings} attribute containing the
  (potentially modified) list proved via the
  \code{settings} argument, and a \sQuote{program}
  attribute corresponding to the \code{program} argument.
}
\description{
  Infer curve parameters from the kinetic data stored in an
  \code{\link{OPM}} object using the \pkg{grofit} package.
  This is not normally directly called by an \pkg{opm} user
  because \code{\link{do_aggr}} is available.
}
\examples{
# Examples are given under do_aggr(), as only the return value is
# different: Let x be the result of do_aggr(). The matrix returned by
# curve_params() could then be received using aggregated(x), whereas
# the 'program' and the 'settings' attributes could be obtained as
# components of the list returned by aggr_settings(x).
}
\references{
  Brisbin IL, Collins CT, White GC and McCallum DA. A new
  paradigm for the analysis and interpretation of growth
  data: the shape of things to come. The Auk 1987;
  104:552-553.

  Efron B. Bootstrap methods: another look at the
  jackknife.  Annals of Statistics 1979; 7:1-26.

  Kahm M, Hasenbrink G, Lichtenberg-Frate H, Ludwig J,
  Kschischo M. grofit: Fitting biological growth curves
  with R. Journal of Statistical Software 2010; 33:1-21.

  Vaas LAI, Sikorski J, Michael V, Goeker M, Klenk H-P.
  Visualization and curve parameter estimation strategies
  for efficient exploration of Phenotype Microarray
  kinetics. PLoS ONE 2012; in press.
}
\seealso{
  grofit::grofit

  Other aggregation-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{do_aggr}},
  \code{\link{fast_estimate}}, \code{\link{param_names}}
}
\keyword{smooth}

\docType{methods}
\alias{curve_params-methods}
\alias{curve_params,OPM-method}
\usage{
  \S4method{curve_params}{OPM}(object, boot = 100L, verbose = FALSE,
    cores = 1L, options = list(), program = "grofit") 
}
