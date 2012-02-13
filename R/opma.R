

################################################################################
#
# OPMA class
#


setGeneric("opma_problems",
  function(object, ...) standardGeneric("opma_problems"))
#' Check OPMA matrix
#'
#' Check whether a matrix fulfils the requirements for  \code{\link{OPMA}}
#' aggregated data. Called when constructing an object of the class.
#'
#' @param object Matrix of aggregated data.
#' @param orig Matrix of original, non-aggregated data.
#' @param program Character scalar. Program used for aggregating the data
#'   (currently only \sQuote{grofit} is checked).
#' @return Character vector with description of problems, empty if there
#'   are none.
#' @keywords internal
#'
setMethod("opma_problems", "matrix", function(object, orig, program) {
  errs <- character()
  # Check content. In contrast to the raw measurements we have to allow NAs.
  if (!is.numeric(object))
    errs <- c(errs, "aggregated values are not numeric")
  # Compare column names with non-aggregated data
  cols <- colnames(object)
  bad <- cols[colnames(orig)[-1] != cols]
  if (length(bad))
    errs <- c(errs, paste("unknown column name in aggregated data:", bad))
  if (nrow(object) == 0L) {
    errs <- c(errs, "no rows in aggregated data")
    return(errs) # further checks are impossible in that case
  }
  # Check row names
  if (program %in% KNOWN_PROGRAMS) {
    got <- rownames(object)
    bad <- got[got != map_grofit_names()]
    if (length(bad) > 0L)
      errs <- c(errs, paste("missing row name in aggregated data:", bad))
  }
  errs
}, sealed = SEALED)


#' Check settings
#'
#' Check whether a list fulfils the requirements for \code{\link{OPMA}}
#' aggregation settings. Called when constructing an object of the class.
#'
#' @rdname opma_problems-list
#' @name opma_problems,list
#'
#' @param object List describing the aggregation settings.
#' @return Character vector with description of problems, empty if there
#'   none.
#' @keywords internal
#'
setMethod("opma_problems", "list", function(object) {
  errs <- character()
  program <- object[[PROGRAM]]
  if (!is.character(program) || length(program) != 1L)
    errs <- c(errs, sprintf("need character '%s' entry of length 1", PROGRAM))
  options <- object[[OPTIONS]]
  if (!is.list(options) || length(options) < 1L)
    errs <- c(errs, sprintf("need non-empty list as '%s' entry", OPTIONS))
  else if (is.null(names(options)) || any(!nzchar(names(options))))
    errs <- c(errs, sprintf("all '%s' elements must be named", OPTIONS))
  bad <- setdiff(names(object), c(PROGRAM, OPTIONS))
  if (length(bad))
    errs <- c(errs, paste("unknown 'aggr_settings' key:", bad[1L]))
  errs
}, sealed = SEALED)


#' OPMA class
#'
#' Class for holding single-plate OmniLog(R) phenotype microarray data
#' together with aggregated values. For further details see its parent class,
#' \code{\link{OPM}}. \sQuote{OPMA} is an acronym for \sQuote{OPM, aggregated}.
#'
#' @docType class
#'
#' @export
#' @seealso Methods
#' @family classes
#' @keywords methods
#'
setClass(OPMA,
  representation = representation(
    aggregated = "matrix",
    aggr_settings = "list"
  ),
  contains = OPM,
  validity = function(object) {
    errs <- opma_problems(object@aggr_settings)
    errs <- c(errs, opma_problems(object@aggregated, object@measurements,
      object@aggr_settings[[PROGRAM]]))
    if (length(errs) == 0L)
      TRUE
    else
      errs
  },
  sealed = SEALED
)


################################################################################
#
# Getter functions
#


setGeneric("aggregated", function(object, ...) standardGeneric("aggregated"))
#' Get aggregated kinetic data
#'
#' The aggregated values are the curve parameters. If bootstrapping was used,
#' their CIs are included. The columns represent the wells, the rows the
#' estimated parameters and their CIs.
#'
#' @param object \code{\link{OPMA}} object.
#' @param subset Character vector. If not \code{NULL}, restrict to this or 
#'   these parameter(s). See \code{\link{param_names}} for the possible values.
#' @param ci Logical scalar. Include the estimates of confidence intervals
#'   (CIs) in the output?
#' @param trim Character scalar. Parameter estimates from intrinsically negative
#'   reactions (i.e., no respiration) are sometimes biologically unreasonable 
#'   because they are too large or too small. If \code{trim} is \sQuote{medium} 
#'   or \sQuote{full}, lambda estimates larger than \code{\link{hours}} are set
#'   to that value. Negative lambda estimates smaller than \code{\link{hours}}
#'   are set to this value if \code{trim} is \sQuote{medium}; this is a more 
#'   moderate treatment than setting all negative values to zero, which is done 
#'   if \code{trim} is \sQuote{full}. Currently the other parameters are  
#'   not checked, and all \code{NA} values also remain unchanged. If \code{trim} 
#'   is \sQuote{no}, lambda is not modified either.
#' @export
#' @family getter-functions
#' @family aggregation-functions
#' @return Numeric matrix.
#' @keywords attribute
#' @examples
#' data(vaas_1)
#' # Get full matrix
#' x <- aggregated(vaas_1)
#' stopifnot(is.matrix(x), identical(dim(x), c(12L, 96L)))
#' # Subsetting
#' x <- aggregated(vaas_1, "lambda")
#' stopifnot(is.matrix(x), identical(dim(x), c(3L, 96L)), any(x < 0))
#' # Now with lambda correction
#' x <- aggregated(vaas_1, "lambda", trim = "full")
#' stopifnot(is.matrix(x), identical(dim(x), c(3L, 96L)), !any(x < 0))
#'
setMethod("aggregated", OPMA, function(object, subset = NULL, ci = TRUE,
    trim = c("no", "full", "medium")) {

  # lambda trimming functions
  trim_into_hours <- function(x, hour, trim) {
    if (trim == "no")
      return(x)
    ok <- !is.na(x)
    x[ok & x > hour] <- hour
    switch(trim,
      full = x[ok & x < 0] <- 0,
      medium = x[ok & x < -hour] <- -hour,
      stop(BUG_MSG)
    )
    x
  }
  trim_mat_into_hours <- function(x, hours, trim) {
    structure(trim_into_hours(x, hours, trim), dim = dim(x), 
      dimnames = dimnames(x))
  }
  trim_lambda <- function(x, hours, trim) {
    is.lambda <- grepl(LAMBDA, rownames(x), fixed = TRUE)
    x[is.lambda, ] <- trim_mat_into_hours(x[is.lambda, , drop = FALSE],
      hours, trim = trim)
    x
  }
  
  trim <- match.arg(trim)
  
  # no subset requested
  if (is.null(subset))
    return(trim_lambda(object@aggregated, hours(object), trim))
  
  # generate subset
  if (!(program <- object@aggr_settings[[PROGRAM]]) %in% KNOWN_PROGRAMS)
    warning("unknown 'program' entry (", program, "): subsetting may not work")
  wanted <- unlist(map_grofit_names(subset, ci))
  result <- object@aggregated[wanted, , drop = FALSE]
  if (LAMBDA %in% subset)
    result <- trim_lambda(result, hours(object), trim = trim)
  result

}, sealed = SEALED)


setGeneric("aggr_settings",
  function(object, ...) standardGeneric("aggr_settings"))
#' Get aggregation settings
#'
#' The settings used for aggregating the kinetic data.
#'
#' @param object \code{\link{OPMA}} object.
#' @return Named list.
#' @export
#' @family getter-functions
#' @family aggregation-functions
#' @keywords attribute
#' @examples
#' data(vaas_1)
#' x <- aggr_settings(vaas_1)
#' stopifnot(is.list(x), identical(names(x), c("program", "options")))
#' stopifnot(identical(x$program, "grofit"))
#'
setMethod("aggr_settings", OPMA, function(object) object@aggr_settings,
  sealed = SEALED)


################################################################################
#
# Subsetting etc.
#


#' Select subset (OPMA version)
#'
#' Select a subset of the measurements. This works much like \code{\link{[}},
#' but the function applies the subsetting to the original and the aggregated
#' data in parallel. The aggregated data may also be dropped entirely; this 
#' might be appropriate if a subset of the time points is selected, potentially
#' yielding aggregated values that do not fit to the measurements anymore.
#'
#' @rdname bracket-OPMA
#' @exportMethod "["
#' @name [,OPMA
#'
#' @param x \code{\link{OPMA}} object.
#' @param i Row(s). Vector or missing.
#' @param j Columns(s). Vector or missing.
#' @param ... Should currently not be set.
#' @param drop Logical scalar. Remove the aggregated data and return an
#'   \code{\link{OPM}} object?
#' @return \code{\link{OPMA}} or \code{\link{OPM}} object.
#' @family getter-functions
#' @seealso [ [[
#' @keywords manip
#' @examples
#'
#' data(vaas_1)
#'
#' copy <- vaas_1[]
#' stopifnot(has_aggr(copy))
#' stopifnot(identical(copy, vaas_1))
#'
#' copy <- vaas_1[drop = TRUE]
#' stopifnot(!has_aggr(copy))
#' stopifnot(!identical(copy, vaas_1))
#'
setMethod("[", OPMA, function(x, i, j, ..., drop = FALSE) {
  result <- callNextMethod()
  if (drop)
    return(as(result, OPM))
  result@aggregated <- result@aggregated[, j, ..., drop = FALSE]
  result
}, sealed = SEALED)


################################################################################
#
# Conversion functions: OPMA => other objects. For principle, see description
# of OPM class. Conversion of OPMA to matrix/dataframe is just repeated here
# from OPM because otherwise some elements would be missing.
#


setAs(from = OPMA, to = "matrix", function(from) {
  attach_attr(from, from@measurements)
})


setAs(from = OPMA, to = "data.frame", function(from) {
  attach_attr(from, as.data.frame(from@measurements))
})


setAs(from = OPMA, to = "list", function(from) {
  result <- as(as(from, OPM), "list")
  result$aggregated <- apply(aggregated(from), MARGIN = 2L, FUN = as.list)
  result$aggr_settings <- aggr_settings(from)
  result
})


################################################################################
#
# Extract curve parameters
#


setGeneric("do_aggr", function(object, ...) standardGeneric("do_aggr"))
#' Aggregate kinetics
#'
#' Aggregate data using curve parameter estimation and include them in a novel
#' object together with previously collected information.
#'
#' @param object \code{\link{OPM}} object
#' @param ... Passed to \code{\link{curve_params}}. See there.
#' @export
#' @return \code{\link{OPMA}} object.
#' @family aggregation-functions
#' @seealso grofit::grofit
#' @keywords smooth
#' @examples 
#'
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
setMethod("do_aggr", OPM, function(object, ...) {
  result <- curve_params(object, ...)
  settings <- list(program = attr(result, PROGRAM), 
    options = attr(result, OPTIONS))
  attr(result, PROGRAM) <- NULL
  attr(result, OPTIONS) <- NULL
  new(OPMA, measurements = measurements(object), metadata = metadata(object),
    csv_data = csv_data(object), aggregated = result, aggr_settings = settings)
})



