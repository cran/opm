\name{do_aggr}
\alias{do_aggr}
\title{Aggregate kinetics using curve-parameter estimation}
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

  \item{plain}{Logical scalar. If \code{TRUE}, only the
  aggregated values are returned (as a matrix, for details
  see below). Otherwise they are integrated in an
  \code{\link{OPMA}} object together with \code{object}.}

  \item{...}{Optional arguments passed between the
  methods.}
}
\value{
  If \code{plain} is \code{FALSE}, an \code{\link{OPMA}}
  object.  Otherwise a numeric matrix of the same structure
  than the one returned by \code{\link{aggregated}} but
  with an additional \sQuote{settings} attribute containing
  the (potentially modified) list proved via the
  \code{settings} argument, and a \sQuote{program}
  attribute corresponding to the \code{program} argument.
}
\description{
  Aggregate the kinetic data using curve-parameter
  estimation, i.e. infer parameters from the kinetic data
  stored in an \code{\link{OPM}} object using either the
  \pkg{grofit} package or the built-in method. Optionally
  include the aggregated values in a novel
  \code{\link{OPMA}} object together with previously
  collected information.
}
\note{
  \itemize{ \item The \sQuote{OPMS} method just applies the
  \sQuote{OPM} method to each contained plate in turn;
  there are not interdependencies.  \item Examples with
  \code{plain = TRUE} are not given, as only the return
  value is different: Let x be the normal result of
  \code{do_aggr()}. The matrix returned if \code{plain} is
  \code{TRUE} could then be received using
  \code{aggregated(x)}, whereas the \sQuote{program} and
  the \sQuote{settings} attributes could be obtained as
  components of the list returned by
  \code{aggr_settings(x)}. }
}
\examples{
data(vaas_1)

# Run a fast estimate of A and AUC without bootstrapping
copy <- do_aggr(vaas_1, program = "opm-fast", boot = 0,
  options = list(as.pe = "pe"))
stopifnot(has_aggr(vaas_1), has_aggr(copy))
stopifnot(identical(aggr_settings(vaas_1)$program, "grofit"))
stopifnot(identical(aggr_settings(copy)$program, "opm-fast"))

# Compare the results to the ones precomputed with grofit
a.grofit <- aggregated(vaas_1, "A", ci = FALSE)
a.fast <-  aggregated(copy, "A", ci = FALSE)
plot(a.grofit, a.fast)
stopifnot(cor.test(a.fast, a.grofit)$estimate > 0.999)
auc.grofit <- aggregated(vaas_1, "AUC", ci = FALSE)
auc.fast <-  aggregated(copy, "AUC", ci = FALSE)
plot(auc.grofit, auc.fast)
stopifnot(cor.test(auc.fast, auc.grofit)$estimate > 0.999)

\dontrun{

  # Without confidence interval (CI) estimation
  x <- do_aggr(vaas_1, boot = 0, verbose = TRUE)
  aggr_settings(x)
  aggregated(x)

  # Calculate CIs with 100 bootstrap (BS) replicates, using 4 cores
  x <- do_aggr(vaas_1, boot = 100, verbose = TRUE, cores = 4)
  aggr_settings(x)
  aggregated(x)
}
}
\references{
  Brisbin, I. L., Collins, C. T., White, G. C., McCallum,
  D. A.  1986 A new paradigm for the analysis and
  interpretation of growth data: the shape of things to
  come. \emph{The Auk} \strong{104}, 552--553.

  Efron, B. 1979 Bootstrap methods: another look at the
  jackknife.  \emph{Annals of Statistics} \strong{7},
  1--26.

  Kahm, M., Hasenbrink, G., Lichtenberg-Frate, H., Ludwig,
  J., Kschischo, M. grofit: Fitting biological growth
  curves with R.  \emph{Journal of Statistical Software}
  \strong{33}, 1--21.

  Vaas, L. A. I., Sikorski, J., Michael, V., Goeker, M.,
  Klenk H.-P. 2012 Visualization and curve parameter
  estimation strategies for efficient exploration of
  Phenotype Microarray kinetics. \emph{PLoS ONE}
  \strong{7}, e34846.
}
\seealso{
  grofit::grofit

  Other aggregation-functions: \code{\link{aggr_settings}},
  \code{\link{aggregated}}, \code{\link{fast_estimate}},
  \code{\link{param_names}}
}
\keyword{smooth}

\docType{methods}
\alias{do_aggr-methods}
\alias{do_aggr,OPM-method}
\alias{do_aggr,OPMS-method}
\usage{
  \S4method{do_aggr}{OPM}(object, boot = 100L, verbose = FALSE,
    cores = 1L, options = list(), program = "grofit", plain = FALSE) 

  \S4method{do_aggr}{OPMS}(object, ...) 

}
