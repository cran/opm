

#' OPMX class
#'
#' This is a virtual class containing helper methods for plotting
#' \code{\link{OPM}} and \code{\link{OPMS}} objects. It is not directly applied
#' by an \pkg{opm} user. Regarding the name: \sQuote{OPMX} stands for
#' \sQuote{OPM or OPMS}.
#'
#' @name OPMX
#'
#' @docType class
#' @export
#' @seealso Methods
#' @family classes
#' @keywords methods
#'
NULL


# Currently the child classes must provide plate_type() and minmax() for the
# methods to work
#
setClassUnion(OPMX, c(OPM, OPMS))


################################################################################


setGeneric("main_title", function(object, ...) standardGeneric("main_title"))
#' Create main title
#'
#' Create a title used as value of the \sQuote{main} argument of the plotting
#' functions.
#'
#' @param object \code{\link{OPMX}} object.
#' @param settings See the \code{main} argument of \code{\link{xy_plot}}.
#' @return Character scalar or \code{NULL}.
#' @keywords internal
#'
setMethod("main_title", OPMX, function(object, settings) {
  if (is.character(settings) || is.expression(settings))
    settings <- list(predef = settings)
  else if (is.logical(settings))
    settings <- list(use = settings)
  else if (is.numeric(settings))
    settings <- list(max = settings)
  else
    settings <- as.list(settings)
  if (!is.null(settings$predef) && nzchar(settings$predef))
    return(settings$predef) # nzchar() works for expressions, too
  settings <- insert(settings, use = TRUE, full = TRUE, .force = FALSE)
  if (settings$use) {
    settings$use <- NULL
    do.call(plate_type, c(list(object = object), settings))
  } else
    NULL
}, sealed = SEALED)


################################################################################


setGeneric("negative_control",
  function(object, ...) standardGeneric("negative_control"))
#' Negative control
#'
#' Helper function to determine the value of a measurement interpretable as
#' negative control.
#'
#' @param object \code{\link{OPMX}} object.
#' @param neg.ctrl If \code{NULL} or \code{FALSE}, ignore \code{data} and
#'   return \code{NULL}. If \code{TRUE}, call \code{minmax} with \code{data} as
#'   sole argument. If a character scalar, call \code{max} with \code{data} as
#'   first and \code{neg.ctrl} as second argument. If \code{neg.ctrl} is a
#'   numeric value, it is returned.
#' @return Numeric scalar or \code{NULL}.
#' @keywords internal
#'
setMethod("negative_control", OPMX, function(object, neg.ctrl) {
  if (is.numeric(neg.ctrl) || is.null(neg.ctrl))
    neg.ctrl
  else if (is.character(neg.ctrl))
    tryCatch(minmax(object, neg.ctrl), error = function(e) {
      warning("cannot get negative control from selected position ",
        "(deleted?); error was: ", e$message)
      NULL
    })
  else if (is.logical(neg.ctrl)) {
    if (neg.ctrl)
      minmax(object)
    else
      NULL
  } else
    stop("object 'neg.ctrl' must be either 'NULL', 'character', 'logical' or ",
      "'numeric'")
}, sealed = SEALED)


################################################################################


#' Combination
#'
#' Combine a \code{\link{OPM}} or \code{\link{OPMS}} object with other objects.
#' If possible, create an \code{\link{OPMS}} object, otherwise return a list.
#'
#' @param x \code{\link{OPMX}} object.
#' @param ... Other R objects.
#' @param recursive Logical scalar. See \code{c} from the \pkg{base} package.
#' @export
#' @return \code{\link{OPMS}} object, list, or \code{\link{OPM}} object
#'   (if \code{...} is not given and \code{x} is such an object).
#' @family combination-functions
#' @seealso base::c
#' @keywords manip
#'
#' @examples
#'
#' data(vaas_1)
#' data(vaas_4)
#'
#' # Adding nothing
#' x <- c(vaas_1)
#' stopifnot(identical(x, vaas_1))
#' x <- c(vaas_4)
#' stopifnot(identical(x, vaas_4))
#'
#' # not particularly useful: adding identical plates!
#' x <- c(vaas_1, vaas_1) # yields a two-plate OPMS object
#' stopifnot(identical(dim(x), c(2L, dim(vaas_1))))
#'
#' # also not particularly useful: adding partially identical plates!
#' x <- c(vaas_4, vaas_1)
#' stopifnot(identical(dim(x), c(5L, dim(vaas_1))))
#'
setMethod("c", OPMX, function(x, ..., recursive = FALSE) {
  if (missing(...))
    return(x)
  try_opms(list(x, ...))
}, sealed = SEALED)


################################################################################


setGeneric("improved_max",
  function(object, ...) standardGeneric("improved_max"))
#' Maximum plus offset
#'
#' Return the maximal value of an object plus a certain offset.
#'
#' @param object Numeric vector or \sQuote{OPMX} object.
#' @param theor.max Logical scalar. Use the theoretical or the real improved
#'   maximum? If \code{TRUE}, \code{by} is ignored.
#' @param by Numeric scalar.
#' @return Numeric scalar. Let \code{n} be the smallest integer value for which
#'   \code{n * by >= object} holds. The result is then equal to
#'   \code{(n + 1) * by}.
#' @keywords internal
#'
setMethod("improved_max", "numeric", function(object, by = 10) {
  assert_length(by)
  ceiling(max(object) / by) * by + by # => error unless 'by' is numeric
}, sealed = SEALED)

setMethod("improved_max", OPMX, function(object, theor.max = TRUE, by = 10) {
  assert_length(theor.max)
  if (theor.max)
    return(THEOR_MAX)
  improved_max(max(object), by)
}, sealed = SEALED)


################################################################################


