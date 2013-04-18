


################################################################################
################################################################################
#
# Curve parameter estimation
#


#' Times for grofit
#'
#' Construct time-points data frame as required by \code{grofit}.
#'
#' @param object \code{\link{OPM}} object.
#' @return Data frame with time points in each row, repeated for each well
#'   (number of rows is number of wells).
#' @keywords internal
#'
setGeneric("to_grofit_time",
  function(object, ...) standardGeneric("to_grofit_time"))

setMethod("to_grofit_time", OPM, function(object) {
  tp <- hours(object, "all")
  as.data.frame(matrix(rep.int(tp, length(wells(object))), ncol = length(tp),
    byrow = TRUE))
}, sealed = SEALED)


################################################################################


#' Data for grofit
#'
#' Construct data frame with measurements as required by \code{grofit}.
#'
#' @param object \code{\link{OPM}} object.
#' @return Data frame with columns: (i) well ID, (ii) plate ID, (iii) dummy
#'   concentration, (iv - end) measurements, one row for each well.
#' @keywords internal
#'
setGeneric("to_grofit_data",
  function(object, ...) standardGeneric("to_grofit_data"))

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

#' Parameter extraction
#'
#' Extract and rename estimated curve parameters.
#'
#' @param x Object of class \sQuote{grofit} or \sQuote{opm_model}.
#' @param all Logic. Should [TODO]
#' @param ... Additional arguments.
#' @return Matrix.
#' @keywords internal
#'
extract_curve_params <- function(x, ...) UseMethod("extract_curve_params")

#' @rdname extract_curve_params
#' @method extract_curve_params grofit
#'
extract_curve_params.grofit <- function(x, ...) {
  settings <- c(x$control)
  x <- summary(x$gcFit)
  map <- map_grofit_names()
  structure(.Data = t(as.matrix(x[, names(map)])),
    dimnames = list(map, x[, "TestId"]), settings = settings)
}

#' @rdname extract_curve_params
#' @method extract_curve_params opm_model
#'
extract_curve_params.opm_model <- function(x, all = FALSE, ...) {
  pred <- fitted(x)
  x <- get_data(x)[, 1]
  ## quick and dirty
  deriv <- diff(pred) / diff(x)
  slope <- max(deriv)
  ## index of max. slope
  idx <- which.max(deriv):(which.max(deriv) + 1)
  ## x-value of max. slope
  x_ms <- mean(x[idx])
  ## y-value of max. slope
  y_ms <- mean(pred[idx])
  ## intercept
  intercept <- y_ms - slope * x_ms
  ## lag
  lag <- - (intercept / slope)
  ## maximum
  maximum <- max(pred)
  ## AUC
  AUC <- AUC(x, pred)
  if (all)
      return(list(mu = slope, lambda = lag, A = maximum, AUC = AUC,
        derivative = deriv, intercept = intercept))
  return(data.frame(mu = slope, lambda = lag, A = maximum, AUC = AUC))
}

################################################################################

## NOTE: Not an S4 method

#' Summary method for bootstraped splines
#'
#' Function for internal use; Creates confidence intervals based on bootstrap
#' replicates.
#'
#' @param object An object of class \code{splines_bootstrap}.
#' @param ... Further arguments. Currently not used.
#' @return vector of bootstrap confidence intervals
#' @author Benjamin Hofner
#' @keywords internal
#'
summary.splines_bootstrap <- function (object, ...) {

    # TODO: should rather rely on map_grofit_names()
    cnames <- c("mu", "lambda", "A", "AUC",
      "mu CI95 low", "lambda CI95 low", "A CI95 low", "AUC CI95 low",
      "mu CI95 high", "lambda CI95 high", "A CI95 high", "AUC CI95 high")

    res <- data.frame(t(sapply(object, extract_curve_params.opm_model)))
    res$mu <- unlist(res$mu)
    res$lambda <- unlist(res$lambda)
    res$A <- unlist(res$A)
    res$AUC <- unlist(res$AUC)

    mu <- mean(res$mu, na.rm = TRUE)
    lambda <- mean(res$lambda, na.rm = TRUE)
    A <- mean(res$A, na.rm = TRUE)
    AUC <- mean(res$AUC, na.rm = TRUE)
    mu.sd <- sd(res$mu, na.rm = TRUE)
    lambda.sd <- sd(res$lambda, na.rm = TRUE)
    A.sd <- sd(res$A, na.rm = TRUE)
    AUC.sd <- sd(res$AUC, na.rm = TRUE)
    table <- c(mu, lambda, A, AUC,
      mu - qnorm(0.975) * mu.sd, mu + qnorm(0.975) * mu.sd,
      lambda - qnorm(0.975) * lambda.sd, lambda + qnorm(0.975) * lambda.sd,
      A - qnorm(0.975) * A.sd, A + qnorm(0.975) * A.sd,
      AUC - qnorm(0.975) * AUC.sd, AUC + qnorm(0.975) * AUC.sd)
    table <- data.frame(t(table))
    colnames(table) <- cnames
    return(table)
}

################################################################################


## NOTE: Not an S4 method because there are no arguments

#' Names of curve parameters
#'
#' Yield the names of the estimated curve parameters used internally and in the
#' output. Alternatively, yield names that should not be used in metadata
#' entries because they are used as predefined column names by functions such
#' as \code{\link{flatten}}.
#'
#' @param what Character scalar. Which kind of names to obtain.
#' @return Character vector.
#' @details In addition to the results of
#'   \code{param_names("reserved.md.names")}, it should be avoided to use
#'   metadata keys that start with a dot, as such keys might also be created
#'   intermediarily by methods that have to compile metadata together with
#'   other information.
#' @export
#' @family aggregation-functions
#' @keywords utilities
#' @examples
#' (x <- param_names())
#' stopifnot(is.character(x), length(x) > 1, identical(unique(x), x))
#' (x <- param_names("reserved"))
#' stopifnot(is.character(x), length(x) > 1, identical(unique(x), x))
#' stopifnot(param_names("split.at") %in% x)
#'
param_names <- function(
    what = c("param.names", "reserved.md.names", "split.at")) {
  case(match.arg(what),
    param.names = CURVE_PARAMS,
    reserved.md.names = unname(RESERVED_NAMES),
    split.at = RESERVED_NAMES[["parameter"]]
  )
}


################################################################################


#' Aggregate kinetics using curve-parameter estimation
#'
#' Aggregate the kinetic data using curve-parameter estimation, i.e. infer
#' parameters from the kinetic data stored in an \code{\link{OPM}} object using
#' either the \pkg{grofit} package or the built-in method. Optionally include
#' the aggregated values in a novel \code{\link{OPMA}} object together with
#' previously collected information.
#'
#' @param object \code{\link{OPM}} or \code{\link{OPMS}} object. There is also
#'   a helper method for matrix objects.
#' @param boot Integer scalar. Number of bootstrap replicates used to estimate
#'   95-percent confidence intervals (CIs) for the parameters. Set this to zero
#'   to omit bootstrapping, resulting in \code{NA} entries for the CIs.
#' @param verbose Logical scalar. Print progress messages?
#' @param cores Integer scalar. Number of cores to use. Setting this to a value
#'   > 1 requires that \code{mclapply} from the \pkg{parallel} package can be
#'   run with more than 1 core, which is impossible under Windows. The
#'   \code{cores} argument has no effect if \sQuote{opm-fast} is chosen (see
#'   below).
#' @param options List. For its use in \sQuote{grofit} mode, see
#'   \code{grofit.control} in the \pkg{grofit} package. The \code{boot} and
#'   \code{verbose} settings, as the most important ones, are added separately
#'   (see above). The verbose mode is not very useful in parallel processing.
#'   For its use in \sQuote{opm-fast} mode, see \code{\link{fast_estimate}}.
#'   With \code{method} \dQuote{spline.fit}, options can be specified using the
#'   function \code{\link{set_spline_options}}.
#' @param method Character scalar. The aggregation method to use. Currently
#'   only the following methods are supported:
#'   \describe{
#'     \item{splines}{Fit various splines (smoothing splines and P-splines from
#'     \pkg{mgcv} and smoothing splines via \code{smooth.spline}) to opm data.
#'     Recommended.}
#'     \item{grofit}{The \code{grofit} function in the eponymous package, with
#'     spline fitting as default.}
#'     \item{opm-fast}{The native, faster parameter estimation. This will only
#'     yield two of the four parameters, the area under the curve and the
#'     maximum height. The area under the curve is estimated as the sum of the
#'     areas given by the trapezoids defined by each pair of adjacent time
#'     points. The maximum height is just the result of \code{max}. By default
#'     the median of the bootstrap values is used as point estimate. For details
#'     see the argument \code{as.pe} of the function
#'     \code{\link{fast_estimate}}.}
#'   }
#' @param program Deprecated. Use \sQuote{method} instead. If provided,
#'   \sQuote{program} has precedence over \sQuote{method}, but \sQuote{program}
#'   will eventually be removed.
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
#'   \code{\link{aggregated}} but with an additional \sQuote{settings} attribute
#'   containing the (potentially modified) list proved via the \code{settings}
#'   argument, and a \sQuote{method} attribute corresponding to the
#'   \code{method} argument. The matrix method returns a matrix.
#'
#' @family aggregation-functions
#' @seealso grofit::grofit
#' @keywords smooth
#'
#' @details Behaviour is special if the \code{\link{plate_type}} is one of those
#'   that have to be explicitly set using \code{\link{gen_iii}} and there is
#'   just one point measurement. Because this behaviour is usual for plates
#'   measured either in Generation-III (identification) mode or on a
#'   MicroStation\eqn{\textsuperscript{\texttrademark}}{(TM)}, the point
#'   estimate is simply regarded as \sQuote{A} parameter (maximum height) and
#'   all other parameters are set to \code{NA}.
#'
#'   The \sQuote{OPMS} method just applies the \sQuote{OPM} method to each
#'   contained plate in turn; there are no inter-dependencies.
#'
#'   Examples with \code{plain = TRUE} are not given, as only the return value
#'   is different: Let \code{x} be the normal result of \code{do_aggr()}. The
#'   matrix returned if \code{plain} is \code{TRUE} could then be received using
#'   \code{aggregated(x)}, whereas the \sQuote{method} and the \sQuote{settings}
#'   attributes could be obtained as components of the list returned by
#'   \code{aggr_settings(x)}.
#'
#' @note The aggregated values can be queried for using \code{\link{has_aggr}}
#'   and received using \code{\link{aggregated}}.
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
#' copy <- do_aggr(vaas_1, method = "opm-fast", boot = 0,
#'   options = list(as.pe = "pe"))
#' aggr_settings(vaas_1)
#' aggr_settings(copy)
#' stopifnot(has_aggr(vaas_1), has_aggr(copy))
#'
#' # Compare the results to the ones precomputed with grofit
#' # (1) A
#' a.grofit <- aggregated(vaas_1, "A", ci = FALSE)
#' a.fast <-  aggregated(copy, "A", ci = FALSE)
#' plot(a.grofit, a.fast)
#' stopifnot(cor.test(a.fast, a.grofit)$estimate > 0.999)
#' # (2) AUC
#' auc.grofit <- aggregated(vaas_1, "AUC", ci = FALSE)
#' auc.fast <- aggregated(copy, "AUC", ci = FALSE)
#' plot(auc.grofit, auc.fast)
#' stopifnot(cor.test(auc.fast, auc.grofit)$estimate > 0.999)
#'
#' \dontrun{ # Without confidence interval (CI) estimation
#'   x <- do_aggr(vaas_1, boot = 0, verbose = TRUE)
#'   aggr_settings(x)
#'   aggregated(x)
#'
#'   # Calculate CIs with 100 bootstrap (BS) replicates, using 4 cores
#'   # (do not try to use > 1 core on Windows)
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
    cores = 1L, options = list(), method = "grofit", plain = FALSE,
    program = method) {

  # Convert to OPMA
  integrate_in_opma <- function(object, result) {
    items <- c(METHOD, OPTIONS, SOFTWARE, VERSION)
    settings <- attributes(result)[items]
    for (item in items)
      attr(result, item) <- NULL
    new(OPMA, measurements = measurements(object),
      metadata = metadata(object), csv_data = csv_data(object),
      aggregated = result, aggr_settings = settings)
  }

  # Add our own changes of the default
  make_grofit_control <- function(verbose, boot, add) {
    result <- grofit.control()
    orig.class <- class(result)
    result <- insert(unclass(result), interactive = FALSE,
      suppress.messages = !verbose, fit.opt = "s", nboot.gc = boot,
      .force = TRUE)
    result <- insert(result, as.list(add), .force = TRUE)
    class(result) <- orig.class
    result
  }

  run_grofit <- function(time, data, control) {
    extract_curve_params(grofit(time = time, data = data,
      ec50 = FALSE, control = control))
  }

  run_mgcv <- function(x, y, data, options, boot) {
    mod <- fit_spline(y = y, x = x, data = data, options = options)
    if (boot > 0) {
      ## draw bootstrap sample
      folds <- rmultinom(boot, nrow(data), rep(1 / nrow(data), nrow(data)))
      res <- lapply(1:boot,
        function(i) {
          fit_spline(y = y, x = x, data = data, options = options,
            weights = folds[, i])
      })
      class(res) <- "splines_bootstrap"
      params <- as.vector(summary(res))
      return(list(params = params, model = mod))
    }
    list(params = extract_curve_params(mod), model = mod)
  }

  copy_A_param <- function(x) {
    map <- unlist(map_grofit_names(opm.fast = TRUE))
    result <- matrix(data = NA_real_, nrow = length(map), ncol = length(x),
      dimnames = list(unname(map), names(x)))
    result[map[["A.point.est"]], ] <- x
    result
  }

  if (!missing(program)) {
    warning("'program' is deprecated -- use 'method' instead")
    method <- program
  }

  if (plate_type(object) %in% SPECIAL_PLATES && dim(object)[1] < 2L) {
    result <- copy_A_param(well(object))
    attr(result, METHOD) <- "shortcut"
    attr(result, OPTIONS) <- list(boot = boot)
  } else {
    case(method <- match.arg(method, KNOWN_METHODS$aggregation),
      grofit = {
        control <- make_grofit_control(verbose, boot, add = options)
        grofit.time <- to_grofit_time(object)
        grofit.data <- to_grofit_data(object)
        result <- mclapply(X = as.list(seq.int(nrow(grofit.data))),
          FUN = function(row) {
            run_grofit(grofit.time[row, , drop = FALSE],
              grofit.data[row, , drop = FALSE], control)
          }, mc.cores = cores)
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
          rownames(result)[1L:3L], perl = TRUE)
        rownames(result)[10L:12L] <- sub("^[^.]+", "mu",
          rownames(result)[1L:3L], perl = TRUE)
        map <- map_grofit_names(opm.fast = TRUE)
        result <- result[names(map), , drop = FALSE]
        rownames(result) <- as.character(map)
        attr(result, OPTIONS) <- options
      },
      splines = {
        ## extract data
        data <- as.data.frame(measurements(object))
        ## get well names
        wells <- wells(object)
        indx <- as.list(seq.int(length(wells)))
        result <- mclapply(X = indx,
          FUN = function(i) {
            run_mgcv(x = HOUR, y = wells[i], data = data, options = options,
              boot = boot)
          }, mc.cores = cores)
        options <- insert(as.list(options), boot = boot)

        if (options$save.models) {
            opm_models <- lapply(result, function(x) x$model)
            names(opm_models) <- wells
            if (is.null(options$filename))
              options$filename <- paste("opm_models_",
                format(Sys.time(), "%Y-%m-%d_%H:%M:%S"), ".RData", sep = "")
            save("opm_models", file = options$filename)
            cat("Models saved as 'opm_models' on disk in file\n  ",
              getwd(), "/", options$filename, "\n\n", sep = "")
        }
        result <- sapply(result, function(x) x$params)
        rn <- rownames(result)
        result <- matrix(unlist(result),
          ncol = ncol(result), nrow = nrow(result))
        rownames(result) <- rn
        ## attach bootstrap CIs if necessary
        if (boot <= 0)
          result <- rbind(result,
            matrix(NA, nrow = 8L, ncol = ncol(result)))
        ## dirty hack:
        map <- map_grofit_names(opm.fast = TRUE)
        rownames(result) <- as.character(map)
        colnames(result) <- wells
        attr(result, OPTIONS) <- unclass(options)
      }
    )
    attr(result, METHOD) <- method
  }

  tmp <- opm_string(version = TRUE)
  attr(result, SOFTWARE) <- tmp[1L]
  attr(result, VERSION) <- tmp[2L]

  if (L(plain))
    return(result)
  integrate_in_opma(object, result)

}, sealed = SEALED)

setMethod("do_aggr", "matrix", function(object, by, fun, sep = ".", ...) {
  if (is.atomic(by))
    by <- list(by = by)
  ## TODO: check for faster alternatives
  result <- aggregate(x = object, by = by, FUN = fun, ..., simplify = TRUE)
  rn <- result[, by.cols <- seq_along(by), drop = FALSE]
  rn <- apply(rn, 1L, paste, collapse = sep)
  result <- as.matrix(result[, -by.cols, drop = FALSE])
  rownames(result) <- rn
  result
}, sealed = SEALED)

setMethod("do_aggr", "OPMS", function(object, ...) {
  new(OPMS, plates = lapply(X = object@plates, FUN = do_aggr, ...))
}, sealed = SEALED)


################################################################################


#' CI and point-estimate calculation
#'
#' Get point estimates and CIs (if possible) from the result of \code{boot}.
#'
#' @param x Object of class \sQuote{boot}.
#' @param ci Numeric scalar. See \code{\link{fast_estimate}}.
#' @param as.pe Character scalar. See \code{\link{fast_estimate}}.
#' @param type Character scalar. See \code{\link{boot.ci}} from the \pkg{boot}
#'   package.
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
  if (nrow(x$t)) {
    cis <- lapply(seq_along(x$t0), FUN = boot.ci, boot.out = x, conf = ci,
      type = type, ...)
    ok <- !vapply(cis, is.null, logical(1L))
    cis[!ok] <- list(c(NA_real_, NA_real_))
    cis[ok] <- lapply(cis[ok], `[[`, type, exact = FALSE)
    cis[ok] <- lapply(cis[ok], FUN = last, i = 2L)
    cis <- do.call(cbind, cis)
  } else {
    if (as.pe != "pe") {
      warning("zero bootstrap replicates -- using real point estimate")
      as.pe <- "pe"
    }
    cis <- matrix(nrow = 2L, ncol = length(x$t0), data = NA_real_)
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
#' (maximum height). This is normally not directly called by an \pkg{opm} user
#' but via \code{\link{do_aggr}}.
#'
#' @param x Matrix as output by \code{\link{measurements}}, i.e. with the time
#'   points in the first columns and the measurements in the remaining columns
#'   (there must be at least two). For deviations from this scheme see
#'   \code{time.pos} and \code{transposed}.
#' @param what Character scalar. Which parameter to estimate. Currently only two
#'   are supported.
#' @param boot Integer scalar. Number of bootstrap replicates. Note that under
#'   the default settings for \code{as.pe}, bootstrapping is also necessary to
#'   obtain the point estimate.
#' @param ci Confidence interval to use in the output. Ignored if \code{boot} is
#'   not positive.
#' @param as.pe Character scalar determining what to output as the point
#'   estimate. Either \sQuote{median}, \sQuote{mean} or \sQuote{pe}; the first
#'   two calculate the point estimate from the bootstrapping replicates, the
#'   third one use the point estimate from the raw data. If \code{boot} is 0,
#'   \code{as.pe} is reset to \sQuote{pe}, if necessary, and a warning is
#'   issued.
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
#'   and as many columns as data columns (or rows) in \code{x}. If \code{raw} is
#'   \code{TRUE}, an object of the class \sQuote{boot}.
#' @family aggregation-functions
#' @seealso boot::boot grofit::grofit
#' @keywords smooth
#'
#' @examples
#' data(vaas_1)
#' (x <- fast_estimate(measurements(vaas_1)))[, 1:3]
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
  ## i arguments are required by boot
  case(what <- match.arg(what),
    A = boot_fun <- function(x, i) apply(x[i, ], 2L, max),
    AUC = {
      n.obs <- nrow(x)
      y <- y[-1L] - y[-n.obs]
      x <- 0.5 * (x[-1L, , drop = FALSE] + x[-n.obs, , drop = FALSE])
      boot_fun <- function(x, i) colSums(x[i, , drop = FALSE] * y[i])
    }
  )
  result <- boot(data = x, statistic = boot_fun, R = boot, ...)
  if (raw)
    return(result)
  result <- pe_and_ci(result, ci = ci, as.pe = as.pe, type = ci.type,
    fill.nas = what == "A")
  colnames(result) <- x.colnames
  rownames(result) <- paste(what, rownames(result), sep = ".")
  result
}, sealed = SEALED)


################################################################################



