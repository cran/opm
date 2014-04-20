\name{do_aggr}
\alias{do_aggr}
\title{Aggregate kinetics using curve-parameter estimation}
\arguments{
  \item{object}{\code{\link{OPM}}, \code{\link{OPMS}} or
  \code{\link{MOPMX}} object or matrix as output by
  \code{\link{measurements}}, i.e. with the time points in
  the first columns and the measurements in the remaining
  columns (there must be at least two). For deviations from
  this scheme see \code{time.pos} and \code{transposed}.}

  \item{boot}{Integer scalar. Number of bootstrap
  replicates used to estimate 95-percent confidence
  intervals (\acronym{CI}s) for the parameters. Set this to
  zero to omit bootstrapping, resulting in \code{NA}
  entries for the \acronym{CI}s. Note that under the
  default settings of the matrix method for \code{as.pe},
  bootstrapping is also necessary to obtain the point
  estimate.}

  \item{verbose}{Logical scalar. Print progress messages?}

  \item{cores}{Integer scalar. Number of cores to use.
  Setting this to a value > 1 requires that \code{mclapply}
  from the \pkg{parallel} package can be run with more than
  1 core, which is impossible under Windows. The
  \code{cores} argument has no effect if \kbd{opm-fast} is
  chosen (see below).}

  \item{options}{List. For its use in \pkg{grofit} mode,
  see \code{grofit.control} in that package. The
  \code{boot} and \code{verbose} settings, as the most
  important ones, are added separately (see above). The
  verbose mode is not very useful in parallel processing.
  With \code{method} \dQuote{spline.fit}, options can be
  specified using the function
  \code{\link{set_spline_options}}.}

  \item{method}{Character scalar. The aggregation method to
  use. Currently only the following methods are supported:
  \describe{ \item{splines}{Fit various splines (smoothing
  splines and P-splines from \pkg{mgcv} and smoothing
  splines via \code{smooth.spline}) to \acronym{PM} data.
  Recommended.} \item{grofit}{The \code{grofit} function in
  the eponymous package, with spline fitting as default.}
  \item{opm-fast}{The native, faster parameter estimation
  implemented in the matrix method. This will only yield
  two of the four parameters, the area under the curve and
  the maximum height. The area under the curve is estimated
  as the sum of the areas given by the trapezoids defined
  by each pair of adjacent time points. The maximum height
  is just the result of \code{max}. By default the median
  of the bootstrap values is used as point estimate. For
  details see \code{as.pe}.} }}

  \item{plain}{Logical scalar. If \code{TRUE}, only the
  aggregated values are returned (as a matrix, for details
  see below). Otherwise they are integrated in an
  \code{\link{OPMA}} object together with \code{object}.}

  \item{what}{Character scalar. Which parameter to
  estimate. Currently only two are supported.}

  \item{ci}{Confidence interval to use in the output.
  Ignored if \code{boot} is not positive.}

  \item{as.pe}{Character scalar determining what to output
  as the point estimate. Either \kbd{median}, \kbd{mean} or
  \kbd{pe}; the first two calculate the point estimate from
  the bootstrapping replicates, the third one use the point
  estimate from the raw data. If \code{boot} is 0,
  \code{as.pe} is reset to \kbd{pe}, if necessary, and a
  warning is issued.}

  \item{ci.type}{Character scalar determining the way the
  confidence intervals are calculated. Either \kbd{norm},
  \kbd{basic} or \kbd{perc}; see \code{boot.ci} from the
  \pkg{boot} package for details.}

  \item{time.pos}{Character or integer scalar indicating
  the position of the column (or row, see next argument)
  with the time points.}

  \item{transposed}{Character or integer scalar indicating
  whether the matrix is transposed compared to the
  default.}

  \item{raw}{Logical scalar. Return the raw bootstrapping
  result without \acronym{CI} estimation and construction
  of the usually resulting matrix?}

  \item{...}{Optional arguments passed between the methods
  or to \code{boot} from the eponymous package.}
}
\value{
  If \code{plain} is \code{FALSE}, an \code{\link{OPMA}}
  object.  Otherwise a numeric matrix of the same structure
  than the one returned by \code{\link{aggregated}} but
  with an additional \sQuote{settings} attribute containing
  the (potentially modified) list proved via the
  \code{settings} argument, and a \sQuote{method} attribute
  corresponding to the \code{method} argument.

  The matrix method returns a numeric matrix with three
  rows (point estimate, lower and upper \acronym{CI}) and
  as many columns as data columns (or rows) in
  \code{object}. If \code{raw} is \code{TRUE}, it returns
  an object of the class \sQuote{boot}.
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
\details{
  Behaviour is special if the \code{\link{plate_type}} is
  one of those that have to be explicitly set using
  \code{\link{gen_iii}} and there is just one point
  measurement. Because this behaviour is usual for plates
  measured either in Generation-III (identification) mode
  or on a
  MicroStation\eqn{\textsuperscript{\texttrademark}}{(TM)},
  the point estimate is simply regarded as \sQuote{A}
  parameter (maximum height) and all other parameters are
  set to \code{NA}.

  The \code{\link{OPMS}} method just applies the
  \code{\link{OPM}} method to each contained plate in turn;
  there are no inter-dependencies.

  Examples with \code{plain = TRUE} are not given, as only
  the return value is different: Let \code{x} be the normal
  result of \code{do_aggr()}. The matrix returned if
  \code{plain} is \code{TRUE} could then be received using
  \code{aggregated(x)}, whereas the \sQuote{method} and the
  \sQuote{settings} attributes could be obtained as
  components of the list returned by
  \code{aggr_settings(x)}.

  The matrix method quickly estimates the curve parameters
  \acronym{AUC} (area under the curve) or A (maximum
  height). This is not normally directly called by an
  \pkg{opm} user but via the other \code{do_aggr} methods.

  The aggregated values can be queried for using
  \code{\link{has_aggr}} and received using
  \code{\link{aggregated}}.
}
\examples{
# OPM method

# Run a fast estimate of A and AUC without bootstrapping
copy <- do_aggr(vaas_1, method = "opm-fast", boot = 0,
  options = list(as.pe = "pe"))
aggr_settings(vaas_1)
aggr_settings(copy)
stopifnot(has_aggr(vaas_1), has_aggr(copy))

# Compare the results to the ones precomputed with grofit
# (1) A
a.grofit <- aggregated(vaas_1, "A", ci = FALSE)
a.fast <- aggregated(copy, "A", ci = FALSE)
plot(a.grofit, a.fast)
stopifnot(cor.test(a.fast, a.grofit)$estimate > 0.999)
# (2) AUC
auc.grofit <- aggregated(vaas_1, "AUC", ci = FALSE)
auc.fast <- aggregated(copy, "AUC", ci = FALSE)
plot(auc.grofit, auc.fast)
stopifnot(cor.test(auc.fast, auc.grofit)$estimate > 0.999)

\dontrun{ # Without confidence interval (CI) estimation
  x <- do_aggr(vaas_1, boot = 0, verbose = TRUE)
  aggr_settings(x)
  aggregated(x)

  # Calculate CIs with 100 bootstrap (BS) replicates, using 4 cores
  # (do not try to use > 1 core on Windows)
  x <- do_aggr(vaas_1, boot = 100, verbose = TRUE, cores = 4)
  aggr_settings(x)
  aggregated(x)
}

# matrix method
(x <- do_aggr(measurements(vaas_1)))[, 1:3]
stopifnot(identical(dim(x), c(3L, 96L)))
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
}
\keyword{smooth}

%% BEGIN INSERTION BY repair_S4_docu.rb
\docType{methods}
\alias{do_aggr-methods}
\alias{do_aggr,MOPMX-method}
\alias{do_aggr,OPM-method}
\alias{do_aggr,OPMS-method}
\alias{do_aggr,matrix-method}
\usage{
  \S4method{do_aggr}{MOPMX}(object, ...) 
  \S4method{do_aggr}{OPM}(object, boot = 100L, verbose = FALSE,
    cores = 1L, options = list(), method = "grofit", plain = FALSE) 
  \S4method{do_aggr}{OPMS}(object, ...) 
  \S4method{do_aggr}{matrix}(object, what = c("AUC", "A"),
    boot = 100L, ci = 0.95, as.pe = "median", ci.type = "norm",
    time.pos = 1L, transposed = FALSE, raw = FALSE, ...) 
}
%% END INSERTION BY repair_S4_docu.rb
