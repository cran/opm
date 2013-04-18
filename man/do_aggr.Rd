\name{do_aggr}
\alias{do_aggr}
\title{Aggregate kinetics using curve-parameter estimation}
\arguments{
  \item{object}{\code{\link{OPM}} or \code{\link{OPMS}}
  object. There is also a helper method for matrix
  objects.}

  \item{boot}{Integer scalar. Number of bootstrap
  replicates used to estimate 95-percent confidence
  intervals (CIs) for the parameters. Set this to zero to
  omit bootstrapping, resulting in \code{NA} entries for
  the CIs.}

  \item{verbose}{Logical scalar. Print progress messages?}

  \item{cores}{Integer scalar. Number of cores to use.
  Setting this to a value > 1 requires that \code{mclapply}
  from the \pkg{parallel} package can be run with more than
  1 core, which is impossible under Windows. The
  \code{cores} argument has no effect if \sQuote{opm-fast}
  is chosen (see below).}

  \item{options}{List. For its use in \sQuote{grofit} mode,
  see \code{grofit.control} in the \pkg{grofit} package.
  The \code{boot} and \code{verbose} settings, as the most
  important ones, are added separately (see above). The
  verbose mode is not very useful in parallel processing.
  For its use in \sQuote{opm-fast} mode, see
  \code{\link{fast_estimate}}.  With \code{method}
  \dQuote{spline.fit}, options can be specified using the
  function \code{\link{set_spline_options}}.}

  \item{method}{Character scalar. The aggregation method to
  use. Currently only the following methods are supported:
  \describe{ \item{splines}{Fit various splines (smoothing
  splines and P-splines from \pkg{mgcv} and smoothing
  splines via \code{smooth.spline}) to opm data.
  Recommended.} \item{grofit}{The \code{grofit} function in
  the eponymous package, with spline fitting as default.}
  \item{opm-fast}{The native, faster parameter estimation.
  This will only yield two of the four parameters, the area
  under the curve and the maximum height. The area under
  the curve is estimated as the sum of the areas given by
  the trapezoids defined by each pair of adjacent time
  points. The maximum height is just the result of
  \code{max}. By default the median of the bootstrap values
  is used as point estimate. For details see the argument
  \code{as.pe} of the function
  \code{\link{fast_estimate}}.} }}

  \item{program}{Deprecated. Use \sQuote{method} instead.
  If provided, \sQuote{program} has precedence over
  \sQuote{method}, but \sQuote{program} will eventually be
  removed.}

  \item{plain}{Logical scalar. If \code{TRUE}, only the
  aggregated values are returned (as a matrix, for details
  see below). Otherwise they are integrated in an
  \code{\link{OPMA}} object together with \code{object}.}

  \item{by}{List, passed by the matrix method to
  \code{aggregate} from the \pkg{stats} package. Can also
  be another vector, which is then used as single list
  element.}

  \item{fun}{Function, passed by the matrix method as
  \code{FUN} argument to \code{aggregate} from the
  \pkg{stats} package.}

  \item{sep}{Character scalar. Used for joining the vectors
  within \code{by} together to form row names.}

  \item{...}{Arguments passed from the \code{\link{OPMS}}
  to the \code{\link{OPM}} method, and from the matrix
  method to \code{fun}.}
}
\value{
  If \code{plain} is \code{FALSE}, an \code{\link{OPMA}}
  object.  Otherwise a numeric matrix of the same structure
  than the one returned by \code{\link{aggregated}} but
  with an additional \sQuote{settings} attribute containing
  the (potentially modified) list proved via the
  \code{settings} argument, and a \sQuote{method} attribute
  corresponding to the \code{method} argument. The matrix
  method returns a matrix.
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

  The \sQuote{OPMS} method just applies the \sQuote{OPM}
  method to each contained plate in turn; there are no
  inter-dependencies.

  Examples with \code{plain = TRUE} are not given, as only
  the return value is different: Let \code{x} be the normal
  result of \code{do_aggr()}. The matrix returned if
  \code{plain} is \code{TRUE} could then be received using
  \code{aggregated(x)}, whereas the \sQuote{method} and the
  \sQuote{settings} attributes could be obtained as
  components of the list returned by
  \code{aggr_settings(x)}.
}
\note{
  The aggregated values can be queried for using
  \code{\link{has_aggr}} and received using
  \code{\link{aggregated}}.
}
\examples{
# OPM method
data(vaas_1)

# Run a fast estimate of A and AUC without bootstrapping
copy <- do_aggr(vaas_1, method = "opm-fast", boot = 0,
  options = list(as.pe = "pe"))
aggr_settings(vaas_1)
aggr_settings(copy)
stopifnot(has_aggr(vaas_1), has_aggr(copy))

# Compare the results to the ones precomputed with grofit
# (1) A
a.grofit <- aggregated(vaas_1, "A", ci = FALSE)
a.fast <-  aggregated(copy, "A", ci = FALSE)
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
x <- matrix(1:10, ncol = 2, dimnames = list(letters[1:5], LETTERS[1:2]))
grps <- c("a", "b", "a", "b", "a")
(y <- do_aggr(x, by = grps, fun = mean))
stopifnot(is.matrix(y), dim(y) == c(2, 2), colnames(y) == colnames(x))
stopifnot(mode(y) == "numeric")
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

  Other aggregation-functions: \code{\link{fast_estimate}},
  \code{\link{param_names}}
}
\keyword{smooth}

\docType{methods}
\alias{do_aggr-methods}
\alias{do_aggr,OPM-method}
\alias{do_aggr,matrix-method}
\alias{do_aggr,OPMS-method}
\usage{
  \S4method{do_aggr}{OPM}(object, boot = 100L, verbose = FALSE,
    cores = 1L, options = list(), method = "grofit", plain = FALSE,
    program = method) 

  \S4method{do_aggr}{matrix}(object, by, fun, sep = ".", ...) 

  \S4method{do_aggr}{OPMS}(object, ...) 

}
