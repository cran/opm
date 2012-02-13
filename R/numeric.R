

################################################################################
#
# Helper functions acting on numeric data
#


setGeneric("improved_max",
  function(object, ...) standardGeneric("improved_max"))
#' Maximum plus offset
#'
#' Return the maximal value of an object plus a certain offset.
#'
#' @param object Numeric vector
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


setGeneric("guess_cex", function(object, ...) standardGeneric("guess_cex"))
#' Estimate cex
#'
#' Guess a suitable \code{cex} parameter for \code{\link{level_plot}}. 0.5 is
#' fine for the original number of wells (96).
#'
#' @param object Numeric vector.
#' @return Numeric vector.
#' @keywords internal
#'
setMethod("guess_cex", "numeric", function(object) {
  0.5 * sqrt(96 / object)
}, sealed = SEALED)


setGeneric("best_layout",
  function(object, ...) standardGeneric("best_layout"))
#' Best two-dimensional layout
#'
#' Determine number of rows/columns in plot for given number of fields.
#'
#' @param object Numeric scalar.
#' @param by Numeric scalar (width/height relation).
#' @return Numeric vector of length 2.
#' @keywords internal
#'
setMethod("best_layout", "numeric", function(object, by = 0.75) {
  assert_length(object, by)
  if (object < 0)
    stop("a negative number of fields makes no sense")
  if (object < 2)
    return(c(object, object))
  large <- ceiling(sqrt((1 / by) * object)) # => error unless 'by' is numeric
  small <- ceiling(object / large)
  c(large, small)
}, sealed = SEALED)


setGeneric("best_range",
  function(object, ...) standardGeneric("best_range"))
#' Best range
#'
#' Determine an optimal range for plotting.
#'
#' @param object Numeric vector.
#' @param target Numeric scalar. Target difference between min and max. If
#'   \code{NULL}, this is simply derived from the range of \code{object}.
#' @param align Character scalar. Where to put the real values relative to min
#'   and max of \code{target}.
#' @param offset Numeric scalar. A minimal distance to the margins.
#' @param prop.offset Numeric scalar. As an alternative to \code{offset}, it
#'   can be specified as a proportion of \code{target}.
#' @return Optimal range (numeric vector of length two).
#' @keywords internal
#'
setMethod("best_range", "numeric", function(object, target,
    align = c("center", "left", "right"),
    offset = 0, prop.offset = 0) {
  orig.range <- range(object)
  orig.diff <- orig.range[2L] - orig.range[1L]
  if (length(target) == 0L)
    target <- orig.diff
  else
    assert_length(target)
  assert_length(offset, prop.offset)
  if (offset == 0)
    offset <- target * prop.offset
  total <- target + 2 * offset
  if (total < orig.diff) {
    fmt <- "target (%s) + 2 * offset (%s) smaller than original range (%s)"
    stop(sprintf(fmt, target, offset, orig.diff))
  }
  switch(match.arg(align),
    center = {
      add <- total / 2
      mean(orig.range) + c(-add, add)
    },
    left = orig.range[1L] + c(-offset, target + offset),
    right = orig.range[2L] + c(-target - offset, offset),
    stop(BUG_MSG)
  )
}, sealed = SEALED)

