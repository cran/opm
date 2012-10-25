


################################################################################
################################################################################
#
# Curve parameter estimation
#


setGeneric("to_grofit_time",
  function(object, ...) standardGeneric("to_grofit_time"))
#' Times for grofit
#'
#' Construct time-points data frame as required by \code{grofit}.
#'
#' @param object \code{\link{OPM}} object.
#' @return Dataframe with time points in each row, repeated for each well
#'   (number of rows is number of wells).
#' @keywords internal
#'
setMethod("to_grofit_time", OPM, function(object) {
  tp <- hours(object, "all")
  as.data.frame(matrix(rep.int(tp, length(wells(object))), ncol = length(tp),
    byrow = TRUE))
}, sealed = SEALED)


################################################################################


setGeneric("to_grofit_data",
  function(object, ...) standardGeneric("to_grofit_data"))
#' Data for grofit
#'
#' Construct data frame with measurements as required by \code{grofit}.
#'
#' @param object \code{\link{OPM}} object.
#' @return Dataframe with columns: (i) well ID, (ii) plate ID, (iii) dummy
#'   concentration, (iv - end) measurements, one row for each well.
#' @keywords internal
#'
setMethod("to_grofit_data", OPM, function(object) {
  w <- wells(object)
  names <- matrix(nrow = length(w), ncol = 3L,
    dimnames = list(well = w, value = c("well", "plate_id", "concentration")))
  names[, 1L] <- w
  names[, 2L] <- paste(setup_time(object), position(object), collapse = "-")
  names <- as.data.frame(names, stringsAsFactors = FALSE)
  names[, 3L] <- 1L # dummy concentration
  cbind(names, as.data.frame(t(measurements(object)[, -1L, drop = FALSE])))
}, sealed = SEALED)


################################################################################


## NOTE: Not an S4 method because 'grofit' is an S3 class

extract_curve_params <- function(data) UseMethod("extract_curve_params")
#' Grofit extraction
#'
#' Extract and rename estimated curve parameters.
#'
#' @param data Object of class \sQuote{grofit}.
#' @return Matrix.
#'
#' @rdname extract_curve_params
#' @method extract_curve_params grofit
#' @keywords internal
#'
extract_curve_params.grofit <- function(data) {
  settings <- c(data$control)
  data <- summary(data$gcFit)
  map <- map_grofit_names()
  structure(.Data = t(as.matrix(data[, names(map)])),
    dimnames = list(map, data[, "TestId"]), settings = settings)
}


################################################################################


## NOTE: Not an S4 method because there are no arguments

#' Names of curve parameters
#'
#' Yield the names of the estimated curve parameters used internally and in the
#' output.
#'
#' @return Character vector.
#' @export
#' @family aggregation-functions
#' @keywords utilities
#' @examples
#' (x <- param_names())
#' stopifnot(is.character(x), length(x) == 4L)
#' stopifnot(identical(unique(x), x))
#'
param_names <- function() {
  CURVE_PARAMS
}


################################################################################


#' Aggregate kinetics using curve-parameter estimation
#'
#' Aggregate the kinetic data using curve-parameter estimation, i.e. infer
#' parameters from the kinetic data stored in an \code{\link{OPM}}
#' object using either the \pkg{grofit} package or the built-in method.
#' Optionally include the aggregated values in a novel \code{\link{OPMA}}
#' object together with previously collected information.
#'
#' @param object \code{\link{OPM}} or \code{\link{OPMS}} object. There is also
#'   a helper method for matrix objects.
#' @param boot Integer scalar. Number of bootstrap replicates used to estimate
#'   95-percent confidence intervals (CIs) for the parameter. Set this to zero
#'   to omit bootstrapping, resulting in \code{NA} entries for the CIs.
#' @param verbose Logical scalar. Print progress messages?
#' @param cores Integer scalar. Number of cores to use. Setting this to a
#'   value > 1 requires the \pkg{multicore} package. Has no effect if
#'   \sQuote{opm-fast} is chosen (see below).
#' @param options List. For its use in \sQuote{grofit} mode, see
#'   \code{grofit.control} in the \pkg{grofit} package. The \code{boot} and
#'   \code{verbose} settings, as the most important ones, are added separately
#'   (see above). The verbose mode is not very useful in parallel processing.
#'   For its use in \sQuote{opm-fast} mode, see \code{\link{fast_estimate}}.
#' @param program Character scalar. The aggregation method to use. Currently
#'   only the following methods are supported:
#'   \describe{
#'     \item{grofit}{The \code{grofit} function in the eponymous package, with
#'     spline fitting as default.}
#'     \item{opm-fast}{The native, faster parameter estimation. This will only
#'     yield two of the four parameters, the area under the curve and the
#'     maximum height. The area under the curve is estimated as the sum of the
#'     areas given by the trapezoids defined by each pair of adjacent time
#'     points. The maximum height is just the result of \code{max}. By default,
#'     however, the median bootstrap value is preferred as point estimate over
#'     the real point estimate.}
#'   }
#' @param plain Logical scalar. If \code{TRUE}, only the aggregated values are
#'   returned (as a matrix, for details see below). Otherwise they are
#'   integrated in an \code{\link{OPMA}} object together with \code{object}.
#' @param by List, passed by the matrix method to \code{aggregate} from the
#'   \pkg{stats} package. Can also be another vector, which is then used as
#'   single list element.
#' @param fun Function, passed by the matrix method as \code{FUN} argument to
#'   \code{aggregate} from the \pkg{stats} package.
#' @param sep Character scalar. Used for joining the vectors within \code{by}
#'   together to form row names.
#' @param ... Arguments passed from the \code{\link{OPMS}} to the
#'   \code{\link{OPM}} method, and from the matrix method to \code{fun}.
#'
#' @export
#' @return If \code{plain} is \code{FALSE}, an \code{\link{OPMA}} object.
#'   Otherwise a numeric matrix of the same structure than the one returned by
#'   \code{\link{aggregated}} but with an additional \sQuote{settings}
#'   attribute containing the (potentially modified) list proved via the
#'   \code{settings} argument, and a \sQuote{program} attribute corresponding
#'   to the \code{program} argument. The matrix method returns a matrix.
#'
#' @family aggregation-functions
#' @seealso grofit::grofit
#' @keywords smooth
#'
#' @note \itemize{
#'   \item The \sQuote{OPMS} method just applies the \sQuote{OPM} method to
#'     each contained plate in turn; there are not interdependencies.
#'   \item Examples with \code{plain = TRUE} are not given, as only the return
#'     value is different: Let \code{x} be the normal result of
#'     \code{do_aggr()}. The matrix returned if \code{plain} is \code{TRUE}
#'     could then be received using \code{aggregated(x)}, whereas the
#'     \sQuote{program} and the \sQuote{settings} attributes could be obtained
#'     as components of the list returned by \code{aggr_settings(x)}.
#' }
#'
#' @references Brisbin, I. L., Collins, C. T., White, G. C., McCallum, D. A.
#'   1986 A new paradigm for the analysis and interpretation of growth data:
#'   the shape of things to come. \emph{The Auk} \strong{104}, 552--553.
#' @references Efron, B. 1979 Bootstrap methods: another look at the jackknife.
#'   \emph{Annals of Statistics} \strong{7}, 1--26.
#' @references Kahm, M., Hasenbrink, G., Lichtenberg-Frate, H., Ludwig, J.,
#'   Kschischo, M. grofit: Fitting biological growth curves with R.
#'   \emph{Journal of Statistical Software} \strong{33}, 1--21.
#' @references Vaas, L. A. I., Sikorski, J., Michael, V., Goeker, M., Klenk
#'   H.-P. 2012 Visualization and curve parameter estimation strategies for
#'   efficient exploration of Phenotype Microarray kinetics. \emph{PLoS ONE}
#'   \strong{7}, e34846.
#'
#' @examples
#'
#' # OPM method
#' data(vaas_1)
#'
#' # Run a fast estimate of A and AUC without bootstrapping
#' copy <- do_aggr(vaas_1, program = "opm-fast", boot = 0,
#'   options = list(as.pe = "pe"))
#' stopifnot(has_aggr(vaas_1), has_aggr(copy))
#' stopifnot(identical(aggr_settings(vaas_1)$program, "grofit"))
#' stopifnot(identical(aggr_settings(copy)$program, "opm-fast"))
#'
#' # Compare the results to the ones precomputed with grofit
#' a.grofit <- aggregated(vaas_1, "A", ci = FALSE)
#' a.fast <-  aggregated(copy, "A", ci = FALSE)
#' plot(a.grofit, a.fast)
#' stopifnot(cor.test(a.fast, a.grofit)$estimate > 0.999)
#' auc.grofit <- aggregated(vaas_1, "AUC", ci = FALSE)
#' auc.fast <-  aggregated(copy, "AUC", ci = FALSE)
#' plot(auc.grofit, auc.fast)
#' stopifnot(cor.test(auc.fast, auc.grofit)$estimate > 0.999)
#'
#' \dontrun{
#'
#'   # Without confidence interval (CI) estimation
#'   x <- do_aggr(vaas_1, boot = 0, verbose = TRUE)
#'   aggr_settings(x)
#'   aggregated(x)
#'
#'   # Calculate CIs with 100 bootstrap (BS) replicates, using 4 cores
#'   x <- do_aggr(vaas_1, boot = 100, verbose = TRUE, cores = 4)
#'   aggr_settings(x)
#'   aggregated(x)
#' }
#'
#' # matrix method
#' x <- matrix(1:10, ncol = 2, dimnames = list(letters[1:5], LETTERS[1:2]))
#' grps <- c("a", "b", "a", "b", "a")
#' (y <- do_aggr(x, by = grps, fun = mean))
#' stopifnot(is.matrix(y), dim(y) == c(2, 2), colnames(y) == colnames(x))
#' stopifnot(mode(y) == "numeric")
#'
setGeneric("do_aggr", function(object, ...) standardGeneric("do_aggr"))

setMethod("do_aggr", OPM, function(object, boot = 100L, verbose = FALSE,
    cores = 1L, options = list(), program = "grofit", plain = FALSE) {

  # Convert to OPMA
  integrate_in_opma <- function(object, result) {
    settings <- list(program = attr(result, PROGRAM),
      options = attr(result, OPTIONS))
    attr(result, PROGRAM) <- NULL
    attr(result, OPTIONS) <- NULL
    new(OPMA, measurements = measurements(object),
      metadata = metadata(object), csv_data = csv_data(object),
      aggregated = result, aggr_settings = settings)
  }

  # Add our own changes of the default
  make_grofit_control <- function(verbose, boot, add) {
    result <- grofit::grofit.control()
    orig.class <- class(result)
    result <- insert(unclass(result), interactive = FALSE,
      suppress.messages = !verbose, fit.opt = "s", nboot.gc = boot,
      .force = TRUE)
    result <- insert(result, as.list(add), .force = TRUE)
    class(result) <- orig.class
    result
  }

  run_grofit <- function(time, data, control) {
    result <- grofit::grofit(time = time, data = data, ec50 = FALSE,
      control = control)
    extract_curve_params(result)
  }

  case(program <- match.arg(program, KNOWN_PROGRAMS),

    grofit = {
      control <- make_grofit_control(verbose, boot, add = options)
      grofit.time <- to_grofit_time(object)
      grofit.data <- to_grofit_data(object)
      result <- traverse(as.list(seq.int(nrow(grofit.data))),
        fun = function(row) {
          run_grofit(grofit.time[row, , drop = FALSE],
          grofit.data[row, , drop = FALSE], control)
        }, cores = cores)
      result <- do.call(cbind, result)
      attr(result, OPTIONS) <- unclass(control)
    },

    `opm-fast` = {
      options <- insert(as.list(options), boot = boot, .force = FALSE)
      mat <- measurements(object)
      result <- rbind(
        do.call(fast_estimate, c(list(x = mat, what = "AUC"), options)),
        do.call(fast_estimate, c(list(x = mat, what = "A"), options)),
        matrix(nrow = 6L, ncol = ncol(mat) - 1L, data = NA_real_)
      )
      rownames(result)[7L:9L] <- sub("^[^.]+", "lambda",
        rownames(result)[1L:3L])
      rownames(result)[10L:12L] <- sub("^[^.]+", "mu",
        rownames(result)[1L:3L])
      map <- map_grofit_names(opm.fast = TRUE)
      result <- result[names(map), , drop = FALSE]
      rownames(result) <- as.character(map)
      attr(result, OPTIONS) <- options
    }

  )

  attr(result, PROGRAM) <- program

  if (L(plain))
    return(result)
  integrate_in_opma(object, result)

}, sealed = SEALED)

setMethod("do_aggr", "matrix", function(object, by, fun, sep = ".", ...) {
  if (is.atomic(by))
    by <- list(by = by)
  result <- stats::aggregate(x = object, by = by, FUN = fun, ...,
    simplify = TRUE)
  rn <- result[, by.cols <- seq_len(length(by)), drop = FALSE]
  rn <- apply(rn, 1L, paste, collapse = sep)
  result <- as.matrix(result[, -by.cols, drop = FALSE])
  rownames(result) <- rn
  result
}, sealed = SEALED)

setMethod("do_aggr", "OPMS", function(object, ...) {
  new(OPMS, plates = lapply(object@plates, FUN = do_aggr, ...))
}, sealed = SEALED)


################################################################################


#' CI and point-estimate calculation
#'
#' Get point estimates and CIs (if possible) from the result of \code{boot}.
#'
#' @param x Object of class \sQuote{boot}.
#' @param ci Numeric scalar. See \code{\link{fast_estimate}}.
#' @param as.pe Character scalar. See \code{\link{fast_estimate}}.
#' @param type Character scalar. See \code{\link{boot.ci}} from the
#'   \pkg{boot} package.
#' @param fill.nas Logical scalar. Assume that if the CI borders are both
#'   \code{NA} bootstrapping yielded constant values if the point estimate is
#'   not \code{NA}, and replace the CI borders by the point estimate in such
#'   cases.
#' @param ... Optional arguments passed to \code{\link{boot.ci}} from the
#'   \pkg{boot} package.
#' @return See \code{\link{fast_estimate}}.
#'
#' @keywords internal
#'
pe_and_ci <- function(x, ...) UseMethod("pe_and_ci")

#' @rdname pe_and_ci
#' @method pe_and_ci boot
#'
pe_and_ci.boot <- function(x, ci = 0.95, as.pe = c("median", "mean", "pe"),
    type = c("basic", "perc", "norm"), fill.nas = FALSE, ...) {
  LL(ci, fill.nas)
  as.pe <- match.arg(as.pe)
  type <- match.arg(type)
  if (nrow(x$t) == 0L) {
    if (as.pe != "pe") {
      warning("zero bootstrap replicates -- using real point estimate")
      as.pe <- "pe"
    }
    cis <- matrix(nrow = 2L, ncol = length(x$t0), data = NA_real_)
  } else {
    cis <- lapply(seq_along(x$t0), FUN = boot::boot.ci, boot.out = x,
      conf = ci, type = type, ...)
    ok <- !unlist(lapply(cis, is.null))
    cis[!ok] <- list(c(NA_real_, NA_real_))
    cis[ok] <- lapply(cis[ok], `[[`, type, exact = FALSE)
    cis[ok] <- lapply(cis[ok], FUN = last, i = 2L)
    cis <- do.call(cbind, cis)
  }
  rownames(cis) <- c("ci.low", "ci.high")
  point.est <- case(as.pe,
    median = apply(x$t, 2L, median),
    mean = colMeans(x$t),
    pe = x$t0
  )
  if (fill.nas) {
    boot.nas <- !is.na(x$t0) & is.na(cis[1L, ]) & is.na(cis[2L, ])
    cis[2L, boot.nas] <- cis[1L, boot.nas] <- x$t0[boot.nas]
  }
  rbind(point.est, cis)
}


################################################################################


#' Fast curve-parameter estimation
#'
#' Quickly estimate the curve parameters AUC (area under the curve) or A
#' (maximum height). This is not normally directly called by an \pkg{opm} user
#' but via \code{\link{do_aggr}}.
#'
#' @param x Matrix as output by \code{\link{measurements}}, i.e. with the
#'   time points in the first columns and the measurements in the remaining
#'   columns (there must be at least two). For deviations from this scheme see
#'   \code{time.pos} and \code{transposed}.
#' @param what Character scalar. Which parameter to estimate. Currently only
#'   two are supported.
#' @param boot Integer scalar. Number of bootstrap replicates. Note that under
#'   the default settings for \code{as.pe}, bootstrapping is also necessary to
#'   obtain the point estimate.
#' @param ci Confidence interval to use in the output. Ignored if \code{boot}
#'   is not positive.
#' @param as.pe Character scalar determining what to output as the point
#'   estimate. Either \sQuote{median}, \sQuote{mean} or \sQuote{pe}; the first
#'   two calculate the point estimate from the bootstrapping replicates, the
#'   third one use the real point estimate. If \code{boot} is 0, \code{as.pe}
#'   is reset to \sQuote{pe}, if necessary, and a warning is issued.
#' @param ci.type Character scalar determining the way the confidence intervals
#'   are calculated. Either \sQuote{norm}, \sQuote{basic} or \sQuote{perc}; see
#'   \code{boot.ci} from the \pkg{boot} package for details.
#' @param time.pos Character or integer scalar indicating the position of the
#'   column (or row, see next argument) with the time points.
#' @param transposed Character or integer scalar indicating whether the matrix
#'   is transposed compared to the default.
#' @param raw Logical scalar. Return the raw bootstrapping result without CI
#'   estimation and construction of the usually resulting matrix?
#' @param ... Optional arguments passed to \code{boot} from the eponymous
#'   package.
#'
#' @export
#' @return Numeric matrix with three rows (point estimate, lower and upper CI)
#'   and as many columns as data columns (or rows) in \code{x}. If \code{raw}
#'   is \code{TRUE}, an object of the class \sQuote{boot}.
#' @family aggregation-functions
#' @seealso boot::boot grofit::grofit
#' @keywords smooth
#'
#' @examples
#' data("vaas_1")
#' x <- fast_estimate(measurements(vaas_1))
#' stopifnot(identical(dim(x), c(3L, 96L)))
#'
setGeneric("fast_estimate", function(x, ...) standardGeneric("fast_estimate"))

setMethod("fast_estimate", "matrix", function(x, what = c("AUC", "A"),
    boot = 100L, ci = 0.95, as.pe = "median", ci.type = "norm",
    time.pos = 1L, transposed = FALSE, raw = FALSE, ...) {
  LL(time.pos, boot, ci, transposed, raw)
  if (transposed)
    x <- t(x)
  y <- x[, time.pos]
  x <- x[, -time.pos, drop = FALSE]
  x.colnames <- colnames(x)
  case(what <- match.arg(what),
    A = boot_fun <- function(x, w) apply(x[w, ], 2L, max),
    AUC = {
      n.obs <- nrow(x)
      y <- y[-1L] - y[-n.obs]
      x <- 0.5 * (x[-1L, , drop = FALSE] + x[-n.obs, , drop = FALSE])
      boot_fun <- function(x, w) colSums(x[w, , drop = FALSE] * y[w])
    }
  )
  result <- boot::boot(data = x, statistic = boot_fun, R = boot, ...)
  if (raw)
    return(result)
  result <- pe_and_ci(result, ci = ci, as.pe = as.pe, type = ci.type,
    fill.nas = what == "A")
  colnames(result) <- x.colnames
  rownames(result) <- paste(what, rownames(result), sep = ".")
  result
}, sealed = SEALED)


################################################################################





