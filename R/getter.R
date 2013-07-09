

################################################################################
################################################################################
#
# Getter functions for the measurements
#


#' Stored measurements
#'
#' Return the measurements. The first column contains the hours, the other ones
#' contain the values from each well. There is one row per time point.
#'
#' @param object \code{\link{OPM}} or \code{\link{OPMS}} object.
#' @param i Optional character or numeric vector or formula with name(s) or
#'   position(s) of well(s).
#' @param ... Optional arguments passed between the methods.
#' @return Numeric matrix with column names indicating the well coordinate and a
#'   first column containing the time points.
#' @details Column names are appropriately set, but not translated (as, e.g., to
#'   substrate names). It is possible to select wells, but the time points are
#'   always included as first column (in contrast to \code{\link{well}}). The
#'   \code{i} argument refers only to the remaining matrix.
#' @export
#' @family getter-functions
#' @keywords attribute
#' @examples
#'
#' # 'OPM' method
#' data(vaas_1)
#' head(x <- measurements(vaas_1))[, 1:5] # => numeric matrix
#' stopifnot(is.matrix(x), is.numeric(x))
#' stopifnot(dim(x) == c(384, 97))
#' head(x <- measurements(vaas_1, "B03"))
#' stopifnot(is.matrix(x), is.numeric(x), dim(x) == c(384, 2))
#' head(y <- measurements(vaas_1, ~B03)) # => same result with formula
#' stopifnot(identical(y, x))
#'
#' # 'OPMS' method
#' data(vaas_4)
#' summary(x <- measurements(vaas_4)) # => list of numeric matrices
#' stopifnot(is.list(x), length(x) == length(vaas_4))
#' stopifnot(sapply(x, is.matrix), sapply(x, is.numeric))
#'
setGeneric("measurements",
  function(object, ...) standardGeneric("measurements"))

setMethod("measurements", OPM, function(object, i) {
  if (missing(i))
    object@measurements
  else
    cbind(object@measurements[, 1L, drop = FALSE],
      object@measurements[, -1L, drop = FALSE][,
        well_index(i, colnames(object@measurements)[-1L]), drop = FALSE])
}, sealed = SEALED)


################################################################################


## NOTE: "[" is a primitive and needs no setGeneric().


#' Select subset
#'
#' Select a subset of the \code{\link{measurements}} (and, if present, of the
#' \code{\link{aggregated}} data and the \code{\link{discretized}} data) or
#' plates. Return this subset (or these subsets) together with the other slots
#' (which are unchanged).
#'
#' @rdname bracket
#' @exportMethod "["
#' @export
#'
#' @param x \code{\link{OPM}}, \code{\link{OPMA}} or \code{\link{OPMS}} object.
#' @param i Vector or missing. For the \code{\link{OPM}} and \code{\link{OPMA}}
#'   method, the indexes of one to several time points. For the
#'   \code{\link{OPMS}} method, the indexes of one to several plates. It is an
#'   error to select plates that are not present.
#' @param j Vector or missing. \itemize{
#'   \item For the \code{\link{OPM}} and \code{\link{OPMA}} method, the indexes
#'   or names of one to several wells. Can also be a formula, which allows for
#'   sequences of well coordinates, which are translated to their positions
#'   within the currently present well names. Be aware that this means that the
#'   content of a sequence of well coordinates is dependent on \code{x}!
#'   \item For the \code{\link{OPMS}} method, the indexes of one to several time
#'   points. In that case, if \code{j} is a list, its values are passed to the
#'   respective \code{\link{OPM}} object separately, allowing for individual
#'   choices of time points. Otherwise \code{j} is used as the \code{i} argument
#'   of the \code{\link{OPM}} and \code{\link{OPMA}} method.
#'   }
#' @param k Vector or missing. The \code{\link{OPMS}} method passes \code{k} as
#'   \code{j} argument of the \code{\link{OPM}} and \code{\link{OPMA}} method.
#'   That is, in that case \emph{this} parameter selects the wells. See \code{j}
#'   for details.
#' @param ... This should \strong{not} be set. It is an error to specify
#'   additional dimensions.
#' @param drop Logical scalar. Remove the aggregated data (and the discretized
#'   data, if any) and turn an \code{\link{OPMA}} or \code{\link{OPMD}} object
#'   to an \code{\link{OPM}} object? Has no effect if \code{x} already is an
#'   \code{\link{OPM}} object or contains only such objects.
#' @return \code{\link{OPM}}, \code{\link{OPMA}} or \code{\link{OPMS}} object,
#'   or \code{NULL}.
#'
#' @details The \code{\link{OPMA}} method works like the \code{\link{OPM}} one,
#'   but the function applies the subsetting to the original and the aggregated
#'   data in parallel. The \code{\link{OPMD}} method applies the selection also
#'   to the discretized data.
#'
#'   The aggregated and discretized data may also be dropped entirely; this
#'   might be appropriate if a subset of the time points is selected,
#'   potentially yielding aggregated values that do not fit to the measurements
#'   anymore.
#'
#'   In contrast to the usual `[` methods, with respect to the measurements this
#'   always return a matrix (as a component of the returned object), even if it
#'   could be simplified to a vector. The time column is not counted and always
#'   copied. It is an error to delete the entire matrix. In all other respects,
#'   this method behaves like the `[` methods from the \pkg{base} package.
#'
#'   The \code{\link{OPMS}} method selects a subset of the plates and/or the
#'   measurements of the individual plates. It simplifies the outcome to a
#'   \code{\link{OPM}} or \code{\link{OPMA}} object if only a single plate
#'   remains and to \code{NULL} if no plate remains. This behaves like
#'   subsetting a three-dimensional array with plates as first dimension, time
#'   points as second, and wells as third.
#'
#' @seealso base::`[` base::`[[`
#' @keywords manip
#'
#' @examples
#'
#' ## OPM(A) method
#' data(vaas_1)
#'
#' # complete dataset, full 96-well plates
#' (x <- dim(vaas_1))
#' stopifnot(x == c(384, 96))
#'
#' # selecting specific wells
#' copy <- vaas_1[, 11:22]
#' (x <- dim(copy))
#' stopifnot(x == c(384, 12))
#' # indexing with formulas allows for sequences of well coordinates
#' copy <- vaas_1[, ~ A11:B10] # "A11" is 11th, "B10" is 22th well name
#' stopifnot(dim(copy) == c(384, 12)) # same result as above
#' # can also be combined
#' copy <- vaas_1[, ~ A11:22]
#' stopifnot(dim(copy) == c(384, 12)) # same result as above
#'
#' # dropping aggregated data
#' copy <- vaas_1[] # normal selection
#' stopifnot(has_aggr(copy), identical(copy, vaas_1))
#' copy <- vaas_1[drop = TRUE] # selection with dropping
#' stopifnot(!has_aggr(copy), !identical(copy, vaas_1))
#'
#'
#' ## OPMS method
#' data(vaas_4)
#'
#' # Create OPMS object with fewer plates (the first two ones)
#' x <- vaas_4[1:2]
#' stopifnot(is(x, "OPMS"), dim(x) == c(2, 384, 96))
#'
#' # If only a single plate is selected, this is reduced to OPM(A)
#' x <- vaas_4[3]
#' stopifnot(!is(x, "OPMS"), dim(x) == c(384, 96))
#'
#' # Create OPMS object with fewer time points (the first 100 in that case;
#' # usually this would correspond to the first 25 hours)
#' x <- vaas_4[, 1:100]
#' stopifnot(dim(x) == c(4, 100, 96))
#'
#' # Create OPMS object with fewer wells
#' x <- vaas_4[, , 1:12]
#' stopifnot(dim(x) == c(4, 384, 12))
#'
#' # The same with well names
#' x <- vaas_4[, , ~ A01:A12] # within x, these are well names 1 to 12
#' stopifnot(dim(x) == c(4, 384, 12))
#' # to do this with a vector, one would need sprintf("A%02i", 1:12)
#'
#' # Select all plates that have aggregated values
#' x <- vaas_4[has_aggr(vaas_4)]
#' stopifnot(identical(x, vaas_4)) # all have such values!
#'
#' # Split into list of OPMS objects with the same overall measurement hours;
#' # the default split() method can be applied here based on "["
#' x <- split(vaas_4, hours(vaas_4))
#' stopifnot(class(x) == "list", length(x) == 1, class(x[[1]]) == "OPMS")
#' # ... because the running times were actually already identical, the list
#' # contains only a single element.
#'
#' # Traverse all contained OPM objects
#' for (i in seq(vaas_4)) { # OR: for (i in 1:length(vaas_4))
#'   x <- vaas_4[i]
#'   # now do something with 'x'...
#'   stopifnot(dim(x) == c(384, 96))
#' }
#' # see also oapply() for a more elegant approach
#'
setMethod("[", c(OPM, "ANY", "ANY", "ANY"), function(x, i, j, ...,
    drop = FALSE) {
  mat <- x@measurements[, -1L, drop = FALSE]
  mat <- mat[i, well_index(j, colnames(mat)), ..., drop = FALSE]
  if (!all(dim(mat)))
    stop("selection resulted in empty matrix")
  mat <- cbind(x@measurements[i, 1L, drop = FALSE], mat)
  names(dimnames(mat)) <- names(dimnames(x@measurements))
  x@measurements <- mat
  x
}, sealed = SEALED)

setMethod("[", c(OPMA, "ANY", "ANY", "ANY"), function(x, i, j, ...,
    drop = FALSE) {
  x <- callNextMethod(x, i, j, ..., drop = drop)
  if (drop)
    return(as(x, OPM))
  if (!missing(j))
    x@aggregated <- x@aggregated[, well_index(j, colnames(x@aggregated)), ...,
      drop = FALSE]
  x
}, sealed = SEALED)

setMethod("[", c(OPMD, "ANY", "ANY", "ANY"), function(x, i, j, ...,
    drop = FALSE) {
  x <- callNextMethod(x, i, j, ..., drop = drop)
  if (drop)
    return(x) # ... which is an OPM object in that case
  if (!missing(j))
    x@discretized <- x@discretized[well_index(j, names(x@discretized))]
  x
}, sealed = SEALED)

setMethod("[", c(OPMS, "ANY", "ANY", "ANY"), function(x, i, j, k, ...,
    drop = FALSE) {
  if (!missing(...))
    stop("incorrect number of dimensions")
  if (missing(i) || identical(i, TRUE))
    y <- x@plates
  else {
    y <- x@plates[i]
    if (any(bad <- vapply(y, is.null, NA))) {
      warning("plate indexes partially out of range")
      y <- y[!bad]
    }
    if (!length(y))
      return(NULL)
  }
  k <- well_index(k, colnames(y[[1L]]@measurements)[-1L])
  if (missing(j) || identical(j, TRUE)) {
    # no call of OPM method if j and k are missing/TRUE and drop is FALSE
    if (!identical(k, TRUE) || drop)
      y <- mapply(`[`, x = y, MoreArgs = list(j = k, drop = drop),
        SIMPLIFY = FALSE, USE.NAMES = FALSE)
  } else if (is.list(j)) {
    y <- mapply(`[`, x = y, i = j, MoreArgs = list(j = k, drop = drop),
      SIMPLIFY = FALSE, USE.NAMES = FALSE)
  } else
    y <- mapply(`[`, x = y, MoreArgs = list(i = j, j = k, drop = drop),
      SIMPLIFY = FALSE, USE.NAMES = FALSE)
  if (length(y) == 1L)
    return(y[[1L]])
  x@plates <- y
  x
}, sealed = SEALED)


################################################################################


#' Measurements from selected wells
#'
#' Get measurements from specified well(s) stored in an \code{\link{OPM}}
#' object. This function will always ignore the time points, in contrast to
#' \code{\link{measurements}}.
#'
#' @param object \code{\link{OPM}} object.
#' @param i Character or numeric vector with name(s) or position(s) of well(s).
#'   Wells are originally named \sQuote{A01} to \sQuote{H12} but might have been
#'   subset beforehand. \code{i} can also be a formula, allowing for sequences
#'   of well coordinates. See the examples.
#' @param drop Logical scalar. If only a single well was selected, simplify it
#'   to a vector?
#' @param ... Optional arguments passed between the methods.
#' @return Numeric matrix or vector (depending on \code{i} and \code{drop}).
#' @export
#' @family getter-functions
#' @keywords attribute
#' @note Do not confuse this with \code{\link{wells}}.
#' @examples
#'
#' # 'OPM' method
#' data(vaas_1)
#' head(x <- well(vaas_1, "B04")) # => numeric vector
#' stopifnot(is.numeric(x), length(x) == 384)
#' head(x <- well(vaas_1, c("B08", "C07"))) # => numeric matrix
#' stopifnot(is.matrix(x), dim(x) == c(384, 2))
#' # selecting adjacent wells is easer if using a formula
#' head(x <- well(vaas_1, c("B12", "C01", "C02")))
#' stopifnot(is.matrix(x), dim(x) == c(384, 3))
#' head(y <- well(vaas_1, ~ B12:C02)) # => same result
#' stopifnot(identical(x, y))
#'
#' # 'OPMS' method
#' data(vaas_4)
#' head(x <- well(vaas_4, "B04"))[, 1:5] # => numeric matrix
#' stopifnot(is.matrix(x), dim(x) == c(4, 384))
#' head(y <- well(vaas_4, ~ B04))[, 1:5] # using a formula
#' stopifnot(identical(x, y)) # => same result
#'
setGeneric("well", function(object, ...) standardGeneric("well"))

setMethod("well", OPM, function(object, i, drop = TRUE) {
  object@measurements[, -1L, drop = FALSE][,
    well_index(i, colnames(object@measurements)[-1L]), drop = drop]
}, sealed = SEALED)


################################################################################


#' Overall measuring hours
#'
#' Get the total number of measurements hours as stored in an \code{\link{OPM}}
#' object.
#'
#' @param object \code{\link{OPM}} object.
#' @param what Character scalar determining the output mode as follows:
#'   \describe{
#'     \item{all}{Numeric vector: all time points, in order.}
#'     \item{interval}{The difference between each pair of adjacent time
#'       points, \code{NA} if this is irregular or only one time point is left.}
#'     \item{max}{Numeric scalar: the largest time point.}
#'     \item{minmax}{Numeric scalar: the smallest maximum. For \code{\link{OPM}}
#'       objects this is apparently identical to \sQuote{max}.}
#'     \item{size}{Integer scalar: the number of time points.}
#'     \item{summary}{Display a summary.}
#'   }
#' @param ... Optional arguments passed between the methods.
#' @return Dependent on the \code{what} argument; see there.
#' @export
#' @family getter-functions
#' @keywords attribute
#' @examples
#'
#' # 'OPM' method
#' data(vaas_1)
#' (x <- hours(vaas_1)) # the default is 'max'
#' stopifnot(identical(x, 95.75))
#' (x <- hours(vaas_1, "minmax"))
#' stopifnot(identical(x, 95.75))
#' (x <- hours(vaas_1, "summary"))
#' stopifnot(is.table(x))
#' (x <- hours(vaas_1, "interval"))
#' stopifnot(identical(x, 0.25))
#' (x <- hours(vaas_1, "size"))
#' stopifnot(identical(x, 384L))
#'
#' # 'OPMS' method
#' data(vaas_4)
#' (x <- hours(vaas_4)) # all with the same overall running time
#' stopifnot(length(x) == 4, x == 95.75)
#'
setGeneric("hours", function(object, ...) standardGeneric("hours"))

setMethod("hours", OPM, function(object,
    what = c("max", "all", "size", "summary", "interval", "minmax")) {
  tp <- object@measurements[, HOUR]
  case(match.arg(what),
    all = tp,
    interval = {
      if (length(tp) < 2L)
        NA_real_
      else {
        diffs <- unique(tp[-1L] - tp[-length(tp)])
        if (length(diffs) > 1L)
          NA_real_
        else
          diffs[1L]
      }
    },
    minmax =,
    max = max(tp),
    size = length(tp),
    summary = summary(tp)
  )
}, sealed = SEALED)


################################################################################


## NOTE: 'max' is part of the S4 summary group generic and needs no
## setGeneric().


#' Maximum
#'
#' Get the maximal value of all wells or (a) specified one(s). The
#' \code{\link{OPMS}} method works by calling the \code{\link{OPM}} method on
#' all plates and then determining the overall maximum.
#'
#' @param x \code{\link{OPM}} or \code{\link{OPMS}} object.
#' @param ... Coordinate of one to several wells. If missing, the maximum of all
#'   wells is returned. See \code{\link{well}} for details.
#' @param na.rm Logical scalar. See \code{max} from the \pkg{base} package. Has
#'   no effect here because \code{NA} values are not allowed within the
#'   measurements.
#' @return Numeric scalar.
#' @export
#' @seealso base::max
#' @family getter-functions
#' @keywords attribute dplot
#' @examples
#'
#' # OPM method
#' data(vaas_1)
#' (x <- max(vaas_1))
#' (y <- max(vaas_1, 1)) # this is the negative control
#' stopifnot(x > y) # i.e., some stronger reactions present
#'
#' # OPMS method
#' data(vaas_4)
#' (x <- max(vaas_4))
#' (y <- max(vaas_4, 1)) # this is the negative control
#' stopifnot(x > y) # i.e., some stronger reactions present
#'
setMethod("max", OPM, function(x, ..., na.rm = FALSE) {
  if (missing(...))
    max(x@measurements[, -1L, drop = FALSE], na.rm = na.rm)
  else
    max(well(x, ...), na.rm = na.rm)
}, sealed = SEALED)

setMethod("max", OPMS, function(x, ..., na.rm = FALSE) {
  max(vapply(x@plates, FUN = max, 1, ..., na.rm = na.rm),
    na.rm = na.rm)
}, sealed = SEALED)


################################################################################


#' Smallest maximum
#'
#' Get the smallest maximum among all wells. The \code{\link{OPMS}} method works
#' by calling the \code{\link{OPM}} method on all plates and then determining
#' the overall minimum.
#'
#' @param x \code{\link{OPM}} or \code{\link{OPMS}} object.
#' @param ... Coordinate of one to several wells. If missing, the smallest
#'   maximum of all wells is returned. See \code{\link{well}} for details. If
#'   only as single well is selected, the result is actually identical to the
#'   one of \code{\link{max}}.
#' @param na.rm Logical scalar. See \code{\link{max}}.
#' @return Numeric scalar.
#' @export
#' @seealso base::min base::max
#' @family getter-functions
#' @keywords attribute dplot
#' @examples
#'
#' # OPM method
#' data(vaas_1)
#' (x <- minmax(vaas_1))
#' stopifnot(max(vaas_1) > x) # obviously
#'
#' # OPMS method
#' data(vaas_4)
#' (x <- minmax(vaas_4))
#' stopifnot(max(vaas_4) > x) # obviously
#'
setGeneric("minmax", function(x, ...) standardGeneric("minmax"))

setMethod("minmax", OPM, function(x, ..., na.rm = FALSE) {
  min(apply(x@measurements[, -1L, drop = FALSE][, ..., drop = FALSE], 2L,
    FUN = max, na.rm = na.rm))
}, sealed = SEALED)

setMethod("minmax", OPMS, function(x, ..., na.rm = FALSE) {
  min(vapply(x@plates, FUN = minmax, 1, ..., na.rm = na.rm))
}, sealed = SEALED)


################################################################################


## NOTE: 'dim' is a primitive and needs no setGeneric().

#' Dimensions
#'
#' Get the dimensions of the measurements of an \code{\link{OPM}} object, or get
#' the dimensions of an \code{\link{OPMS}} object. Note that this function
#' cannot be used to determine the correspondence of the time points between all
#' plates as it reports only the time points of the first plate. Instead the
#' \code{\link{OPMS}} method of \code{\link{hours}} must be used.
#'
#' @param x \code{\link{OPM}} or \code{\link{OPMS}} object.
#' @return For the \code{\link{OPM}} method, a two-element numeric vector
#'   (number of time points and number of wells). For the \code{\link{OPMS}}
#'   method, a numeric vector with (i) the number of contained \code{\link{OPM}}
#'   objects, and (ii) and (iii) the dimensions of the first plate.
#' @export
#' @family getter-functions
#' @keywords attribute
#' @seealso base::dim
#' @examples
#'
#' # OPM method
#' data(vaas_1)
#' (x <- dim(vaas_1))
#' stopifnot(identical(x, c(384L, 96L)))
#'
#' # OPMS method
#' data(vaas_4)
#' (x <- dim(vaas_4)) # 2nd value needs not be correct for all plates
#' stopifnot(identical(x, c(4L, 384L, 96L)))
#'
setMethod("dim", OPM, function(x) {
  dim(measurements(x)[, -1L, drop = FALSE])
}, sealed = SEALED)

setMethod("dim", OPMS, function(x) {
  c(length(x@plates), dim(x@plates[[1L]]))
}, sealed = SEALED)


################################################################################


#' OPMS length (number of plates)
#'
#' Get the number of plates stored in an \code{\link{OPMS}} object.
#'
#' @param x \code{\link{OPMS}} object.
#' @return Numeric scalar.
#' @export
#' @family getter-functions
#' @seealso base::length
#' @keywords attribute
#' @examples
#' data(vaas_4)
#' (x <- length(vaas_4))
#' stopifnot(identical(x, 4L)) # 4 plates contained
#'
setMethod("length", OPMS, function(x) {
  length(x@plates)
}, sealed = SEALED)


################################################################################


#' Sequence of plate indexes
#'
#' Get the indexes of all plates contained in an \code{\link{OPMS}} object. This
#' is mainly useful for looping over such objects. See \code{\link{[}} for a
#' loop-construct usage example, and note that \code{\link{oapply}} is also
#' available.
#'
#' @param ... \code{\link{OPMS}} objects. Several ones can be provided, but all
#'   but the first one are ignored. For reasons of comparability, the
#'   \code{\link{OPM}} method deliberately results in an error.
#' @return Integer vector (starting with 1 and at least of length 2).
#' @export
#' @family getter-functions
#' @keywords attribute
#' @seealso base::seq
#' @examples
#'
#' # 'OPMS' method
#' data(vaas_4)
#' (x <- seq(vaas_4))
#' stopifnot(identical(x, 1:4)) # indexes for 4 plates
#' (y <- seq(vaas_4, letters, LETTERS)) # other arguments are ignored
#' stopifnot(identical(x, y))
#'
#' # 'OPM' method
#' data(vaas_1)
#' (x <- try(seq(vaas_1), silent = TRUE)) # deliberately yields an error
#' stopifnot(inherits(x, "try-error"))
#'
setGeneric("seq")

setMethod("seq", OPM, function(...) {
  stop("one cannot loop over an object of class ", class(..1))
}, sealed = SEALED)

setMethod("seq", OPMS, function(...) {
  seq_along(..1@plates)
}, sealed = SEALED)


################################################################################


#' Available well names
#'
#' Get the names of the wells contained in an \code{\link{OPM}} object.
#' Optionally the full substrate names can be added in parentheses or brackets
#' or used instead of the coordinate, and trimmed to a given length.
#'
#' @param object \code{\link{OPM}} object, \code{\link{OPMS}} object or well
#'   name or index. If missing, defaults to the selection of all possible
#'   wells.
#' @param full Logical scalar. Return the full names of the wells (if available)
#'   or just their coordinates on the plate? The following arguments have no
#'   effect if \code{full} is \code{FALSE}.
#' @param in.parens Logical scalar. If \code{TRUE}, add the full name of the
#'   substrate in parentheses (or brackets) after the original name. If
#'   \code{FALSE}, replace by the full substrate name. Note that adding in
#'   parentheses (or brackets) is only done if the trimmed substrate names are
#'   not empty.
#' @param max Numeric scalar. Maximum number of characters allowed in the names.
#'   Longer names are truncated and the truncation is indicated by appending a
#'   dot.
#' @param brackets Logical scalar. Use brackets instead of parentheses?
#' @param clean Logical scalar. If \code{TRUE}, clean trimmed end of full
#'   substrate name from non-word characters; use an empty string if only the
#'   dot remained.
#' @param word.wise Logical scalar. If \code{TRUE}, abbreviation works by
#'   truncating each word separately, and removing vowels first.
#' @param paren.sep Character scalar. What to insert before the opening
#'   parenthesis (or bracket).
#' @param downcase Logical scalar indicating whether full names should be
#'   (carefully) converted to lower case. This uses \code{\link{substrate_info}}
#'   in \sQuote{downcase} mode; see there for details.
#' @param plate Name of the plate type. Several ones can be given unless
#'   \code{object} is of class \code{\link{OPM}} or \code{\link{OPMS}}.
#'   \code{\link{plate_type}} is applied before searching for the substrate
#'   names, and partial matching is allowed.
#' @param ... Optional arguments passed between the methods.
#' @return Character vector.
#' @export
#' @family getter-functions
#' @seealso base::strtrim base::abbreviate
#' @keywords attribute
#' @details The purpose of the \code{\link{OPM}} and \code{\link{OPMS}} methods
#'   should be obvious. The default method is intended for providing a quick
#'   overview of the substrates contained in one to several plates if
#'   \code{full} is \code{TRUE}. If \code{full} is \code{FALSE}, it can be used
#'   to study the effect of the well-index translation and well-name
#'   normalization approaches as used by \pkg{opm}, particularly by the
#'   subsetitting methods (see \code{\link{[}}).
#' @note Do not confuse this with \code{\link{well}}.
#' @examples
#'
#' # 'OPM' method
#' data(vaas_1)
#' (x <- wells(vaas_1, full = FALSE))[1:10]
#' (y <- wells(vaas_1, full = TRUE))[1:10]
#' (z <- wells(vaas_1, full = TRUE, in.parens = FALSE))[1:10]
#' # string lengths differ depending on selection
#' stopifnot(nchar(x) < nchar(y), nchar(z) < nchar(y))
#'
#' # 'OPM' method
#' data(vaas_4)
#' (xx <- wells(vaas_4, full = FALSE))[1:10]
#' # wells are guaranteed to be uniform within OPMS objects
#' stopifnot(identical(x, xx))
#'
#' # default method
#' x <- c("A01", "B10")
#' (y <- wells(x, plate = "PM1"))
#' stopifnot(nchar(y) > nchar(x))
#' (z <- wells(x, plate = "PM1", in.parens = TRUE))
#' stopifnot(nchar(z) > nchar(y))
#' # formula yields same result (except for row names)
#' stopifnot(y == wells(~ c(A01, B10), plate = "PM1"))
#' # using a sequence of well coordinates
#' stopifnot(nrow(wells(~ C02:C06)) == 5) # well sequence
#' stopifnot(nrow(wells(plate = "PM1")) == 96) # all wells by default
#'
setGeneric("wells", function(object, ...) standardGeneric("wells"))

setMethod("wells", OPM, function(object, full = FALSE, in.parens = TRUE,
    max = 100L, brackets = FALSE, clean = TRUE, word.wise = FALSE,
    paren.sep = " ", downcase = FALSE, plate = plate_type(object)) {
  result <- setdiff(colnames(measurements(object)), HOUR)
  if (L(full))
    map_well_names(result, L(plate), in.parens = in.parens,
      max = max, brackets = brackets, clean = clean, word.wise = word.wise,
      paren.sep = paren.sep, downcase = downcase)
  else
    result
}, sealed = SEALED)

setMethod("wells", "ANY", function(object, full = TRUE, in.parens = FALSE,
    max = 100L, brackets = FALSE, clean = TRUE, word.wise = FALSE,
    paren.sep = " ", downcase = FALSE, plate = "PM01") {
  result <- well_index(object, rownames(WELL_MAP))
  if (!is.character(result))
    result <- rownames(WELL_MAP)[result]
  result <- do.call(cbind, rep.int(list(result), length(plate)))
  pos <- pmatch(plate_type(plate), colnames(WELL_MAP))
  colnames(result) <- plate
  if (is.character(object))
    rownames(result) <- object
  if (!L(full))
    return(result)
  for (i in which(!is.na(pos)))
    result[, i] <- map_well_names(result[, i], colnames(WELL_MAP)[pos[i]],
      in.parens = in.parens, max = max, brackets = brackets, clean = clean,
      word.wise = word.wise, paren.sep = paren.sep, downcase = downcase)
  for (i in which(is.na(pos)))
    result[, i] <- NA_character_
  result
}, sealed = SEALED)

setMethod("wells", "missing", function(object, ...) {
  wells(object = TRUE, ...)
}, sealed = SEALED)


################################################################################
################################################################################
#
# Getter functions for the CSV data
#


#' Information from input CSV file
#'
#' Information about the plate as originally read from the input \acronym{CSV}
#' file. See \code{\link{read_opm}} and \code{\link{read_single_opm}} for
#' reading such files.
#'
#' @param object \code{\link{OPM}} or \code{\link{OPMS}} object.
#' @param keys Character vector (or other objects usable as vector index). An
#'   optional sub-selection. If empty (the default), all \acronym{CSV} data are
#'   returned. By default it is an error to select non-existing items.
#' @param strict Logical scalar indicating whether or not it is an error if
#'   \code{keys} are not found.
#' @param ... Optional arguments passed between the methods.
#' @return Named character vector.
#' @export
#' @family getter-functions
#' @keywords attribute
#' @examples
#'
#' # 'OPM' method
#' data(vaas_1)
#' (x <- csv_data(vaas_1, "Setup Time")) # compare this to setup_time()
#' stopifnot(identical(x, c(`Setup Time` = "8/30/2010 1:53:08 PM")))
#'
#' # 'OPMS' method
#' data(vaas_4)
#' (x <- csv_data(vaas_4, "Setup Time")) # one setup time per plate
#' stopifnot(is.character(x), length(x) == 4)
#'
setGeneric("csv_data", function(object, ...) standardGeneric("csv_data"))

setMethod("csv_data", OPM, function(object, keys = character(),
    strict = TRUE) {
  if (!length(keys) || all(is.na(keys) | !nzchar(keys)))
    return(object@csv_data)
  result <- object@csv_data[keys]
  if (any(isna <- is.na(result)))
    if (L(strict))
      stop("could not find key ", keys[isna][1L])
    else
      names(result)[isna] <- keys[isna]
  result
}, sealed = SEALED)


################################################################################


#' Original input filename
#'
#' Get the name of the original \acronym{CSV} input file. This is a convenience
#' function for one of the more important entries of \code{\link{csv_data}}.
#'
#' @param object \code{\link{OPM}} object.
#' @param ... Optional arguments passed between the methods.
#' @return Character scalar.
#' @export
#' @family getter-functions
#' @keywords attribute
#' @examples
#'
#' # 'OPM' method
#' data(vaas_1)
#' (x <- filename(vaas_1)) # one file name (of course)
#' stopifnot(is.character(x), length(x) == 1L)
#'
#' # 'OPMS' method
#' data(vaas_4)
#' (x <- filename(vaas_4))  # one file name per plate
#' stopifnot(is.character(x), length(x) == 4L)
#'
setGeneric("filename", function(object, ...) standardGeneric("filename"))

setMethod("filename", OPM, function(object) {
  object@csv_data[[CSV_NAMES[["FILE"]]]]
}, sealed = SEALED)


################################################################################


#' Plate type used or normalized
#'
#' Get the type of the OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} plate
#' used in the measuring.
#'
#' @param object \code{\link{OPM}} or \code{\link{OPMS}} object, or character
#'   vector of original plate name(s), or factor.
#' @param full Logical scalar. If \code{TRUE}, add (or replace by) the full name
#'   of the plate type (if available); otherwise, return it as-is.
#' @param in.parens Logical scalar. This and the five next arguments work like
#'   the eponymous ones of \code{\link{wells}}, but here are applied to the
#'   plate name. See there for details.
#' @param max Numeric scalar.
#' @param clean Logical scalar.
#' @param brackets Logical scalar.
#' @param word.wise Logical scalar.
#' @param paren.sep Character scalar.
#' @param downcase Logical scalar.
#' @param normalize Logical scalar. Attempt to normalize the plate-type string
#'   before interpreting it?
#' @param subtype Logical scalar. Keep the plate subtype indicator, if any? Only
#'   relevant for the character or factor method.
#' @param ... Optional arguments passed between the methods.
#'
#' @return Character scalar in the case of the \code{\link{OPM}} and
#'   \code{\link{OPMS}} methods, otherwise a character vector with the same
#'   length than \code{object}, or a corresponding factor.
#'
#' @details The \code{\link{OPM}} and \code{\link{OPMS}} methods are convenience
#'   methods for one of the more important entries of \code{\link{csv_data}}
#'   with additional options useful for creating plot titles.
#'
#' The character method normalizes the names of
#' OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} PM plates to the
#' internally used naming scheme. Unrecognized names are returned unchanged.
#' This needs not normally be called by the \pkg{opm} user but might be of
#' interest.
#'
#' Factors are treated by passing their levels through the character method.
#'
#' @export
#' @family getter-functions
#' @seealso base::strtrim base::abbreviate base::gsu
#' @keywords attribute utilities character
#' @examples
#'
#' ## 'OPM' method
#' data(vaas_1)
#' (x <- plate_type(vaas_1, full = FALSE))
#' (y <- plate_type(vaas_1, full = TRUE))
#' (z <- plate_type(vaas_1, full = TRUE, in.parens = FALSE))
#' # strings lengths differ depending on the selection
#' stopifnot(nchar(x) < nchar(y), nchar(z) < nchar(y))
#'
#' \dontrun{
#'
#' # Splitting a list of 'OPM' objects according to the plate type is easy:
#' x <- split(x), sapply(x, plate_type))
#' # but see also opms() and read_opm() which can do this internally
#' }
#'
#' ## 'OPMS' method
#' data(vaas_4)
#' (xx <- plate_type(vaas_4, full = FALSE))
#' # plate type is guaranteed to be uniform within an OPMS object
#' stopifnot(identical(x, xx))
#'
#' ## Character method
#'
#' # Entirely unrecognized strings are returned as-is
#' (x <- plate_type(letters))
#' stopifnot(identical(x, letters))
#'
#' # Something more realistic
#' (x <- plate_type(y <- c("PM1", "PM-11C", "PMM04-a"), subtype = TRUE))
#' stopifnot(x != y)
#'
#' # Factor method
#' (z <- plate_type(as.factor(y), subtype = TRUE))
#' stopifnot(is.factor(z), z == x) # same result after conversion
#'
setGeneric("plate_type", function(object, ...) standardGeneric("plate_type"))

setMethod("plate_type", OPM, function(object, ..., normalize = FALSE,
    subtype = FALSE) {
  plate_type(object = object@csv_data[[CSV_NAMES[["PLATE_TYPE"]]]], ...,
    normalize = normalize, subtype = subtype)
}, sealed = SEALED)

setMethod("plate_type", "character", function(object, full = FALSE,
    in.parens = TRUE, max = 100L, clean = TRUE, brackets = FALSE,
    word.wise = FALSE, paren.sep = " ", downcase = FALSE,
    normalize = TRUE, subtype = FALSE) {
  do_normalize <- function(object, subtype) {
    normalize_pm <- function(x, subtype) {
      x <- sub("^PMM", "PM-M", x, perl = TRUE)
      x <- sub("^PM-MTOX", "PM-M TOX", x, perl = TRUE)
      x <- sub("([A-Z]+)$", if (subtype)
        "-\\1"
      else
        "", x, perl = TRUE)
      sub("([^\\d])(\\d)([^\\d]|$)", "\\10\\2\\3", x, perl = TRUE)
    }
    normalize_sf <- function(x, subtype) {
      x <- if (subtype)
        sub("-$", "", sub(SP_PATTERN, "\\1-\\2", x, perl = TRUE), perl = TRUE)
      else
        sub(SP_PATTERN, "\\1", x, perl = TRUE)
      x <- sub("^(G|SF)([NP])", "SF-\\2", x, perl = TRUE)
      sub("^GENIII", "Gen III", x, perl = TRUE)
    }
    result <- toupper(gsub("\\W", "", object, perl = TRUE))
    pm <- grepl("^PM(M(TOX)?)?\\d+[A-Z]*$", result, perl = TRUE)
    result[pm] <- normalize_pm(result[pm], subtype)
    sf[sf] <- grepl(SP_PATTERN, result[sf <- !pm], perl = TRUE)
    result[sf] <- normalize_sf(result[sf], subtype)
    result[bad] <- object[bad <- !(pm | sf)]
    result
  }
  LL(full, downcase, in.parens, normalize, subtype)
  result <- if (normalize)
    do_normalize(object, subtype)
  else
    object
  if (!full)
    return(result)
  pos <- match(result, names(PLATE_MAP))
  if (any(bad <- is.na(pos))) {
    warning("cannot find full name of plate ", result[bad][1L])
    return(result)
  }
  full.name <- PLATE_MAP[pos]
  if (downcase)
    full.name <- substrate_info(full.name, "downcase")
  if (in.parens)
    result <- add_in_parens(str.1 = result, str.2 = full.name, max = max,
      clean = clean, brackets = brackets, word.wise = word.wise,
      paren.sep = paren.sep)
  else
    result <- trim_string(str = full.name, max = max, clean = clean,
      word.wise = word.wise)
  result
}, sealed = SEALED)

setMethod("plate_type", "factor", function(object, ...) {
  map_values(object = object, mapping = plate_type, ...)
}, sealed = SEALED)


################################################################################


#' Setup time of the measuring
#'
#' Get the setup time of the PM experiment as recorded by the
#' OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} instrument. This is a
#' convenience function for one of the more important entries of
#' \code{\link{csv_data}}.
#'
#' @param object \code{\link{OPM}} object.
#' @param ... Optional arguments passed between the methods.
#' @return Character vector or \sQuote{POSIXlt} object.
#' @seealso base::strptime
#' @export
#' @family getter-functions
#' @keywords attribute
#' @examples
#'
#' # 'OPM' method
#' data(vaas_1)
#' (x <- setup_time(vaas_1)) # single setup time (of course)
#' # WARNING: It is unlikely that all OmniLog output has this setup time format
#' (parsed <- strptime(x, format = "%m/%d/%Y %I:%M:%S %p"))
#' stopifnot(inherits(parsed, "POSIXlt"), length(parsed) == 1)
#'
#' # 'OPMS' method
#' data(vaas_4)
#' (x <- setup_time(vaas_4)) # one setup time per plate
#' (parsed <- strptime(x, format = "%m/%d/%Y %I:%M:%S %p"))
#' stopifnot(inherits(parsed, "POSIXlt"), length(parsed) == 4)
#'
setGeneric("setup_time", function(object, ...) standardGeneric("setup_time"))

setMethod("setup_time", OPM, function(object) {
  object@csv_data[[CSV_NAMES[["SETUP"]]]]
}, sealed = SEALED)


################################################################################


#' Position of a plate
#'
#' Get the position of the plate within the
#' OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} instrument. This is a
#' convenience function for one of the more important entries of
#' \code{\link{csv_data}}.
#'
#' @param object \code{\link{OPM}} object.
#' @param ... Optional arguments passed between the methods.
#' @return Character scalar.
#' @export
#' @family getter-functions
#' @keywords attribute
#' @examples
#'
#' # 'OPM' method
#' data(vaas_1)
#' (x <- position(vaas_1)) # single position (of course)
#' stopifnot(identical(x, " 7-B"))
#'
#' # 'OPMS' method
#' data(vaas_4)
#' (x <- position(vaas_4)) # one position per plate
#' stopifnot(is.character(x), length(x) == length(vaas_4))
#'
setGeneric("position", function(object, ...) standardGeneric("position"))

setMethod("position", OPM, function(object) {
  object@csv_data[[CSV_NAMES[["POS"]]]]
}, sealed = SEALED)


################################################################################
################################################################################
#
# Other getter functions
#


#' Are aggregated data present?
#'
#' Check whether aggregated data are present. This always returns \code{FALSE}
#' for the \code{\link{OPM}} class, but not necessarily for its child classes.
#'
#' @param object \code{\link{OPM}} or \code{\link{OPMS}} object.
#' @param ... Optional arguments passed between the methods.
#' @return Logical vector, one element per plate.
#' @export
#' @family getter-functions
#' @keywords attribute
#' @note  See \code{\link{do_aggr}} for generating aggregated data.
#' @examples
#'
#' # 'OPM' method
#' data(vaas_1)
#' stopifnot(has_aggr(vaas_1))
#'
#' # 'OPMS' method
#' data(vaas_4)
#' stopifnot(has_aggr(vaas_4))
#'
setGeneric("has_aggr", function(object, ...) standardGeneric("has_aggr"))

setMethod("has_aggr", OPM, function(object) {
  .hasSlot(object, "aggregated")
}, sealed = SEALED)


################################################################################


#' Are discretized data present?
#'
#' Check whether discretized data are present. This always returns \code{FALSE}
#' for the \code{\link{OPM}} class, but not necessarily for its child classes.
#'
#' @param object \code{\link{OPM}} or \code{\link{OPMS}} object.
#' @param ... Optional arguments passed between the methods.
#' @return Logical vector, one element per plate.
#' @export
#' @family getter-functions
#' @keywords attribute
#' @note  See \code{\link{do_disc}} for generating discretized data.
#' @examples
#'
#' # 'OPM' method
#' data(vaas_1)
#' (x <- has_disc(vaas_1))
#' stopifnot(x)
#'
#' # 'OPMS' method
#' data(vaas_4)
#' (x <- has_disc(vaas_4))
#' stopifnot(x)
#'
setGeneric("has_disc", function(object, ...) standardGeneric("has_disc"))

setMethod("has_disc", OPM, function(object) {
  .hasSlot(object, "discretized")
}, sealed = SEALED)


################################################################################


#' Summarize OPM or OPMS objects
#'
#' Generate a summary (which also prints nicely to the screen).
#'
#' @param object \code{\link{OPM}} or \code{\link{OPMS}} object.
#' @param ... Optional arguments passed to \code{formatDL}.
#' @export
#' @return For the \code{\link{OPM}} method, a named list of the class
#'   \sQuote{OPM_Summary}, returned invisibly. The \sQuote{metadata} entry is
#'   the number of non-list elements in \code{\link{metadata}}. For the
#'   \code{\link{OPMS}} method, a list of such lists (one per plate), also
#'   returned invisibly, with the class set to \sQuote{OPMS_Summary} and some
#'   information on the entire object in the attribute \sQuote{overall}.
#' @family getter-functions
#' @keywords attribute
#' @seealso base::summary base::formatDL
#' @examples
#'
#' # OPM method
#' data(vaas_1)
#' (x <- summary(vaas_1))
#' stopifnot(is.list(x), is.object(x))
#'
#' # OPMS method
#' data(vaas_4)
#' (x <- summary(vaas_4))
#' stopifnot(is.list(x), length(x) == 4L, all(sapply(x, is.list)),
#'   is.object(x))
#'
setGeneric("summary")

setMethod("summary", OPM, function(object, ...) {
  result <- list(
    Class = class(object),
    `From file` = filename(object),
    `Hours measured` = hours(object),
    `Number of wells` = length(wells(object)),
    `Plate type` = plate_type(object),
    Position = position(object),
    `Setup time` = setup_time(object),
    Metadata = sum(rapply(object@metadata, f = function(item) 1L)),
    Aggregated = has_aggr(object),
    Discretized = has_disc(object)
  )
  class(result) <- "OPM_Summary"
  result
}, sealed = SEALED)

setMethod("summary", OPMS, function(object, ...) {
  result <- lapply(object@plates, summary)
  x <- list(dimensions = dim(object),
    aggregated = length(which(has_aggr(object))),
    discretized = length(which(has_disc(object))),
    plate.type = plate_type(object))
  attr(result, "overall") <- x
  class(result) <- "OPMS_Summary"
  result
}, sealed = SEALED)


################################################################################
################################################################################
#
# Getter functions for aggregated data
#


#' Get aggregated kinetic data
#'
#' The aggregated values are the curve parameters. If bootstrapping was used,
#' their CIs are included. The columns represent the wells, the rows the
#' estimated parameters and their CIs.
#'
#' @param object \code{\link{OPMA}} object.
#' @param subset Character vector. If not \code{NULL}, restrict to this or these
#'   parameter(s). See \code{\link{param_names}} for the possible values.
#' @param ci Logical scalar. Include the estimates of confidence intervals (CIs)
#'   in the output?
#' @param trim Character scalar. Parameter estimates from intrinsically negative
#'   reactions (i.e., no respiration) are sometimes biologically unreasonable
#'   because they are too large or too small, and some corrections might be
#'   appropriate. \describe{
#'   \item{no}{No modification.}
#'   \item{full}{Negative
#'   lamdda estimates are set to zero.}
#'   \item{medium}{Lambda estimates larger than \code{\link{hours}(object)}
#'   (i.e., the maximum time value observed) are set to that value. Negative
#'   lambda estimates smaller than \code{-hours(object)} are set to this value
#'   (i.e., the negative maximum time).}
#'   \item{full}{Like \sQuote{medium}, but all negative values are set to zero,
#'   which is a less moderate treatment.}
#'   }
#'   Currently the other parameters are not checked, and all \code{NA} values,
#'   if any, also remain unchanged.
#' @param ... Optional arguments passed between the methods.
#' @note See \code{\link{do_aggr}} for generating aggregated data.
#' @export
#' @family getter-functions
#' @return Numeric matrix.
#' @keywords attribute
#' @examples
#'
#' # 'OPMA' method
#' data(vaas_1)
#' # Get full matrix
#' (x <- aggregated(vaas_1))[, 1:3]
#' stopifnot(is.matrix(x), identical(dim(x), c(12L, 96L)))
#' # Subsetting
#' (x <- aggregated(vaas_1, "lambda"))[, 1:3]
#' stopifnot(is.matrix(x), identical(dim(x), c(3L, 96L)), any(x < 0))
#' # Now with lambda correction
#' (x <- aggregated(vaas_1, "lambda", trim = "full"))[, 1:3]
#' stopifnot(is.matrix(x), identical(dim(x), c(3L, 96L)), !any(x < 0))
#'
#' # 'OPMS' method
#' data(vaas_4)
#' summary(x <- aggregated(vaas_4)) # => one matrix per OPM object
#' stopifnot(is.list(x), length(x) == length(vaas_4), sapply(x, is.matrix))
#'
setGeneric("aggregated", function(object, ...) standardGeneric("aggregated"))

setMethod("aggregated", OPMA, function(object, subset = NULL, ci = TRUE,
    trim = c("no", "full", "medium")) {

  # lambda trimming functions
  trim_into_hours <- function(x, hour, trim) {
    if (trim == "no")
      return(x)
    ok <- !is.na(x)
    x[ok & x > hour] <- hour
    case(trim,
      full = x[ok & x < 0] <- 0,
      medium = x[ok & x < -hour] <- -hour
    )
    x
  }
  trim_mat_into_hours <- function(x, hours, trim) {
    structure(trim_into_hours(x, hours, trim), dim = dim(x),
      dimnames = dimnames(x))
  }
  trim_lambda <- function(x, hours, trim) {
    is.lambda <- grepl(CURVE_PARAMS[2L], rownames(x), fixed = TRUE)
    x[is.lambda, ] <- trim_mat_into_hours(x[is.lambda, , drop = FALSE],
      hours, trim = trim)
    x
  }

  trim <- match.arg(trim)

  # NULL as software entry is allowed to increase backwards compatibility
  if (is.null(software <- object@aggr_settings[[SOFTWARE]]))
    warning(sprintf("object has no '%s' entry", SOFTWARE))
  else if (software != opm_string())
    warning(sprintf("unknown '%s' entry '%s': subsetting may not work",
      SOFTWARE, software))

  # generate subset
  wanted <- unlist(map_grofit_names(subset, ci))
  result <- object@aggregated[wanted, , drop = FALSE]
  if (CURVE_PARAMS[2L] %in% subset)
    result <- trim_lambda(result, hours(object), trim = trim)
  return(result)

}, sealed = SEALED)


################################################################################


#' Get aggregation settings
#'
#' The settings used for aggregating the kinetic data.
#'
#' @param object \code{\link{OPMA}} object.
#' @param ... Optional arguments passed between the methods.
#' @return Named list. See the examples for details.
#' @export
#' @family getter-functions
#' @keywords attribute
#' @note  See \code{\link{do_aggr}} for generating aggregated data.
#' @examples
#'
#' # 'OPM' method
#' data(vaas_1)
#' (x <- aggr_settings(vaas_1)) # yields named list
#' stopifnot(is.list(x), !is.null(names(x)))
#'
#' # 'OPMS' method
#' data(vaas_4)
#' summary(x <- aggr_settings(vaas_4)) # list of named lists, one per plate
#' stopifnot(is.list(x), length(x) == length(vaas_4), sapply(x, is.list))
#'
setGeneric("aggr_settings",
  function(object, ...) standardGeneric("aggr_settings"))

setMethod("aggr_settings", OPMA, function(object) {
  object@aggr_settings
}, sealed = SEALED)


################################################################################


#' Get discretized kinetic data
#'
#' This yields the discretized values of the curve parameter \sQuote{maximum
#' height}.
#'
#' @param object \code{\link{OPMD}} or \code{\link{OPMS}} object.
#' @param ... Optional arguments passed between the methods.
#' @export
#' @family getter-functions
#' @return Logical vector or matrix.
#' @keywords attribute
#' @note  See \code{\link{do_disc}} for generating discretized data.
#' @examples
#'
#' # 'OPM' method
#' data(vaas_1)
#' (x <- discretized(vaas_1))[1:3] # => logical vector
#' stopifnot(is.logical(x), !is.matrix(x), length(x) == dim(x)[2L])
#' stopifnot(names(x) == colnames(aggregated(vaas_1)))
#'
#' # 'OPMS' method
#' data(vaas_4)
#' (x <- discretized(vaas_4))[, 1:3] # => logical matrix
#' stopifnot(is.logical(x), is.matrix(x), ncol(x) == dim(x)[2L])
#' stopifnot(colnames(x) == colnames(aggregated(vaas_1)))
#'
setGeneric("discretized", function(object, ...) standardGeneric("discretized"))

setMethod("discretized", OPMD, function(object) {
  object@discretized
}, sealed = SEALED)


################################################################################


#' Get discretization settings
#'
#' The settings used for discretizing the aggregated kinetic data.
#'
#' @param object \code{\link{OPMD}} object.
#' @param ... Optional arguments passed between the methods.
#' @return Named list. See the examples for details.
#' @export
#' @family getter-functions
#' @keywords attribute
#' @note  See \code{\link{do_disc}} for generating discretized data.
#' @examples
#'
#' # 'OPM' method
#' data(vaas_1)
#' (x <- disc_settings(vaas_1)) # => named list
#' stopifnot(is.list(x), !is.null(names(x)))
#'
#' # 'OPMS' method
#' data(vaas_4)
#' summary(x <- disc_settings(vaas_4)) # => list of named lists, one per plate
#' stopifnot(is.list(x), is.null(names(x)), length(x) == length(vaas_4))
#' stopifnot(duplicated(x)[-1])
#'
setGeneric("disc_settings",
  function(object, ...) standardGeneric("disc_settings"))

setMethod("disc_settings", OPMD, function(object) {
  object@disc_settings
}, sealed = SEALED)


################################################################################
################################################################################
#
# Getter functions for metadata
#


#' Get metadata
#'
#' Get meta-information stored together with the data.
#'
#' @param object \code{\link{WMD}} object.
#' @param key \code{NULL}, vector, factor or formula. \itemize{
#'   \item If \code{NULL} or otherwise empty, return all metadata.
#'   \item If a non-empty list, treated as list of keys. Return value would be
#'   the list of corresponding metadata values. Here, character vectors of
#'   length > 1 can be used to query nested metadata lists.
#'   \item If neither empty nor a list nor a formula (i.e. usually a character
#'   or numeric vector), \code{key} is treated as a single list key. Factors are
#'   converted to \sQuote{character} mode.
#'   \item Formulas can also be used and are converted to a list or character or
#'   numeric vector using the rules described under \sQuote{Details}.
#'   \item It is in general not recommended to use numeric vectors as \code{key}
#'   arguments, either directly or within a list or formula.
#' }
#' @param exact Logical scalar. Use exact or partial matching of keys? Has no
#'   effect if \code{key} is empty.
#' @param strict Logical scalar. Is it an error if a \code{NULL} value results
#'   from fetching a metadata key?
#' @param ... Optional arguments passed between the methods.
#' @return List (empty if metadata were not set or if subselection using
#'   \code{key} did not result).
#' @export
#' @family getter-functions
#' @keywords attribute
#' @details If a named list is used as \code{key} argument, its names will be
#'   used within the first level of the resulting nested or non-nested list.
#'   That is, \code{key} can be used to translate names on the fly, and this can
#'   be used by all functions that call \code{metadata} indirectly, usually via
#'   an \code{as.labels} or \code{as.groups} argument.
#'
#'   Even though it is not technically impossible per se, it is usually a bad
#'   idea to select metadata entries using numeric (positional) or logical keys.
#'   The problem is that, in contrast to, e.g., data frames, their is no
#'   guarantee that metadata entries with the same name occur in the same
#'   position, even if they belong to \code{\link{OPM}} objects within a single
#'   \code{\link{OPMS}} object.
#'
#'   Formulas passed as \code{key} argument are treated by ignoring the left
#'   side (if any) and converting the right side to a list or other vector. Code
#'   enclosed in \code{I} is evaluated with a call to \code{eval}. It is up to
#'   the user to ensure that this call succeeds and yields a character vector or
#'   a list. Operators in all other code within the formula are used just as
#'   separators, and all names are converted to character scalars. The \code{$}
#'   operator binds tightly, i.e. it separates elements of a character vector
#'   (for nested querying) in the output. The same effect have other operators
#'   of high precedence such as \code{::} but their use is not recommended. All
#'   operators with a lower precedence than \code{$} separate list elements.
#'
#' @examples
#'
#' # 'OPM' method
#' data(vaas_1)
#' (x <- metadata(vaas_1, "Strain"))
#' stopifnot(x == "DSM30083T")
#' (y <- metadata(vaas_1, ~ Strain)) # using a formula => same result
#' stopifnot(identical(x, y))
#'
#' # 'OPMS' method
#' data(vaas_4)
#' (x <- metadata(vaas_4, "Strain"))
#' stopifnot(x == c("DSM18039", "DSM30083T", "DSM1707", "429SC1"))
#' (y <- metadata(vaas_4, ~ Strain)) # using a formula => same result
#' stopifnot(identical(x, y))
#'
setGeneric("metadata", function(object, ...) standardGeneric("metadata"))

setMethod("metadata", WMD, function(object, key = NULL, exact = TRUE,
    strict = FALSE) {
  LL(exact, strict)
  if (!length(key))
    return(object@metadata)
  key <- metadata_key(key, FALSE)
  fetch_fun <- if (strict)
    function(key) {
      if (is.null(result <- object@metadata[[key, exact = exact]]))
        stop(sprintf("got NULL value when using key '%s'",
          paste(key, collapse = " -> ")))
      result
    }
  else
    function(key) object@metadata[[key, exact = exact]]
  if (is.list(key))
    sapply(key, fetch_fun, simplify = FALSE)
  else # should be a (character) vector
    fetch_fun(key)
}, sealed = SEALED)


################################################################################


#' Select a subset of the plates (or time points)
#'
#' Select a subset of the plates in an \code{\link{OPMS}} object based on the
#' content of the metadata. Alternatively, select a common subset of time points
#' from all plates.
#'
#' @param x \code{\link{OPMS}} object.
#' @param query Logical, numeric or character vector, list, or formula. If a
#'   logical or numeric vector, \code{query} is directly used as the first
#'   argument of \code{\link{[}}, and all following arguments, if any, are
#'   ignored. If a list, formula or a character vector, it is used for
#'   conducting a query based on one of the infix operators as described below.
#' @param values Logical scalar. If \code{TRUE}, the values of \code{query} are
#'   also considered (by using \code{\link{infix.q}} or
#'   \code{\link{infix.largeq}}). If \code{FALSE} only the keys are considered
#'   (by using \code{\link{infix.k}}).
#'
#'   That is, choose either the plates for which certain metadata entries
#'   contain certain values, or choose the plates for which these metadata have
#'   been set at all (to some arbitrary value). See the mentioned functions for
#'   details, and note the special behavior if \code{query} is a character
#'   vector and \code{values} is \code{FALSE}.
#' @param invert Logical scalar. If \code{TRUE}, return the plates for which the
#'   condition is not \code{TRUE}.
#' @param exact Logical scalar. If the values of \code{query} are considered,
#'   should this be done using \code{\link{infix.q}} (when \code{FALSE}) or
#'   \code{\link{infix.largeq}} (when \code{TRUE})? See these functions and
#'   \code{\link{contains}}  for details.
#' @param time Logical scalar. If \code{TRUE}, all other arguments are ignored
#'   and the object is reduced to the common subset of time points (measurement
#'   hours and minutes).
#' @param positive Character scalar. If \sQuote{ignore}, not used. Otherwise all
#'   previous arguments except \code{object} are ignored, and the function
#'   yields an error unless all elements of \code{object} have discretized
#'   values.
#'
#'   In that case, if \sQuote{any}, wells are selected that contain positive
#'   reactions in at least one plate. If \sQuote{all}, wells are selected that
#'   contain positive reactions in all plates. Using \code{invert} means
#'   selecting all negative or weak reactions.
#' @param negative Character scalar. Like \code{positive}, but returns the
#'   negative reactions. Using \code{invert} means selecting all positive or
#'   weak reactions.
#' @param use Character scalar. An alternative way to specify the settings.
#'
#'   If \sQuote{i} or \sQuote{I}, ignored. If \sQuote{t} or \sQuote{T},
#'   \code{time} is set to \code{TRUE}. If \sQuote{p} or \sQuote{P},
#'   \code{positive} is set to \sQuote{any} or \sQuote{all}, respectively. If
#'   \sQuote{n} or \sQuote{N}, \code{negative} is set to \sQuote{any} or
#'   \sQuote{all}, respectively.
#'
#'   Otherwise, \code{use} is taken directly as the one-latter name of the infix
#'   operators to use for plate selection, overriding \code{values} and
#'   \code{exact}.
#' @export
#' @return \code{NULL} or \code{\link{OPM}} or \code{\link{OPMS}} object. This
#'   depends on how many plates are selected; see \code{\link{[}} for details.
#'
#' @family getter-functions
#' @keywords manip
#' @seealso base::`[` base::`[[` base::subset
#' @examples
#'
#' data(vaas_4) # example OPMS object
#' # simple object comparison function
#' mustbe <- function(a, b) stopifnot(identical(a, b))
#'
#' # all plates have that entry: selection identical to original object
#' mustbe(vaas_4, vaas_4["Species" %k% vaas_4, ])
#' mustbe(vaas_4, subset(vaas_4, list(Species = "Escherichia coli"),
#'   values  = FALSE)) # equivalent
#' mustbe(vaas_4, subset(vaas_4, ~ Species == "Escherichia coli",
#'   values  = FALSE)) # also equivalent
#'
#' # two plates also have that value: yielding OPMS object with only two plates
#' mustbe(vaas_4[1:2], vaas_4[list(Species = "Escherichia coli") %q% vaas_4, ])
#' mustbe(vaas_4[1:2], subset(vaas_4, list(Species = "Escherichia coli")))
#' mustbe(vaas_4[1:2], subset(vaas_4, ~ Species == "Escherichia coli"))
#'
#' # these are also equivalent
#' mustbe(vaas_4[c(1, 3)],
#'   vaas_4[list(Strain = c("DSM18039", "DSM1707")) %q% vaas_4])
#' mustbe(vaas_4[c(1, 3)],
#'   subset(vaas_4, list(Strain = c("DSM18039", "DSM1707"))))
#' mustbe(vaas_4[c(1, 3)],
#'   subset(vaas_4, ~ Strain %in% c("DSM18039", "DSM1707")))
#' mustbe(vaas_4[c(1, 3)],
#'   subset(vaas_4, ~ Strain == "DSM18039" || Strain == "DSM1707"))
#' # note that particularly formulas can be used to set up very complex queries
#'
#' # select all plates that have aggregated values
#' dim(x <- subset(vaas_4, has_aggr(vaas_4)))
#' mustbe(x, vaas_4) # all have such values
#'
#' # select a common set of time points
#' dim(x <- subset(vaas_4, time = TRUE))
#' mustbe(x, vaas_4) # the time points had already been identical
#' # create unequal time points
#' dim(copy <- vaas_4[, list(1:10, 1:20, 1:15, 1:10)])
#' mustbe(hours(copy), c(2.25, 4.75, 3.50, 2.25))
#' # now restrict to common subset
#' dim(x <- subset(copy, time = TRUE))
#' mustbe(hours(x), rep(2.25, 4))
#' # see also the example with split() given under "["
#'
#' # select all wells that have positive reactions
#' dim(x <- subset(vaas_4, use = "p")) # in at least one plate
#' stopifnot(dim(x)[3] < dim(vaas_4)[3])
#' dim(y <- subset(vaas_4, use = "P")) # in all plates
#' stopifnot(dim(y)[3] < dim(x)[3])
#'
#' # select all wells that have non-negative reactions in at least one plate
#' dim(y <- subset(vaas_4, use = "N", invert = TRUE))
#' stopifnot(dim(y)[3] > dim(x)[3])
#'
setGeneric("subset")

setMethod("subset", OPMS, function(x, query, values = TRUE,
    invert = FALSE, exact = FALSE, time = FALSE,
    positive = c("ignore", "any", "all"),
    negative = c("ignore", "any", "all"),
    use = c("i", "I", "k", "K", "n", "N", "p", "P", "q", "Q", "t", "T")) {
  select_binary <- function(x, invert.1, combine, invert.2) {
    y <- discretized(x)
    if (invert.1)
      y <- !y
    y[is.na(y)] <- FALSE
    y <- apply(y, 2L, combine)
    if (invert.2)
      y <- !y
    x[, , y]
  }
  case(match.arg(use),
    i =, I = NULL,
    k =, K = values <- FALSE,
    n = negative <- "any",
    N = negative <- "all",
    p = positive <- "any",
    P = positive <- "all",
    q = {
      values <- TRUE
      exact <- FALSE
    },
    Q = {
      values <- TRUE
      exact <- TRUE
    },
    t =, T = time <- TRUE
  )
  LL(values, invert, exact, time)
  case(negative <- match.arg(negative),
    ignore = NULL,
    any =,
    all = return(select_binary(x, TRUE, negative, invert))
  )
  case(positive <- match.arg(positive),
    ignore = NULL,
    any =,
    all = return(select_binary(x, FALSE, positive, invert))
  )
  if (time) {
    tp <- hours(x, what = "all")
    if (is.matrix(tp))
      tp <- lapply(seq_len(nrow(tp)), function(i) tp[i, ])
    if (length(maxs <- unique.default(vapply(tp, max, 1))) < 2L)
      return(x)
    min.max <- min(maxs)
    tp <- lapply(tp, function(x) which(x <= min.max))
    return(x[, tp])
  }
  if (is.logical(query) || is.numeric(query))
    return(x[query, , ])
  pos <- if (values) {
    if (exact)
      query %Q% x
    else
      query %q% x
  } else if (exact)
    query %K% x
  else
    query %k% x
  if (invert)
    pos <- !pos
  x[pos, , ]
}, sealed = SEALED)


################################################################################


#' Determine duplicated plates
#'
#' Check whether duplicated \code{\link{OPM}} or \code{\link{OPMA}} objects are
#' contained within an \code{\link{OPMS}} object. For reasons of consistency,
#' the \code{\link{OPM}} method always returns \code{FALSE}.
#'
#' @param x \code{\link{OPMS}} object.
#' @param  incomparables Vector passed to \code{duplicated} from the \pkg{base}
#'   package. By default this is \code{FALSE}.
#' @param what Indicating which parts of \code{x} should be compared. If a
#'   character scalar, the following entries are special: \describe{
#'   \item{all}{Compares entire \code{OPM} objects.}
#'   \item{csv}{Compares the \acronym{CSV} data entries \code{\link{setup_time}}
#'   and \code{\link{position}}.}
#'   \item{metadata}{Compares the entire metadata content.}
#'   }
#'   If \code{what} does not match any of these, or is not a character scalar at
#'   all, it is passed as \code{key} argument to \code{\link{metadata}}, and the
#'   resulting metadata subsets are compared.
#' @param ... Optional arguments passed to \code{duplicated} from the \pkg{base}
#'   package.
#' @export
#' @return Logical vector.
#' @family getter-functions
#' @keywords attribute
#' @seealso base::duplicated
#' @examples
#'
#' # 'OPM' method
#' data(vaas_1)
#' (x <- duplicated(vaas_1)) # 1 element => nothing duplicated
#' stopifnot(identical(x, FALSE))
#'
#' # 'OPMS' method
#' data(vaas_4)
#' stopifnot(!duplicated(vaas_4)) # => no complete plates duplicated
#' stopifnot(!duplicated(vaas_4, what = list("Species", "Strain")))
#' # => no organisms duplicated
#' stopifnot(duplicated(vaas_4, what = "Species") == rep(c(FALSE, TRUE), 2))
#' # => species duplicated
#' x <- vaas_4[c(1, 1)] # => complete plate duplicated
#' stopifnot(c(FALSE, TRUE) == duplicated(x))
#'
setGeneric("duplicated")

setMethod("duplicated", c(OPM, "ANY"), function(x, incomparables, ...) {
  FALSE
}, sealed = SEALED)

setMethod("duplicated", c(OPMS, "missing"), function(x, incomparables, ...) {
  duplicated(x = x, incomparables = FALSE, ...)
}, sealed = SEALED)

setMethod("duplicated", c(OPMS, "ANY"), function(x, incomparables,
    what = c("all", "csv", "metadata"), ...) {
  selection <- tryCatch(match.arg(what), error = function(e) "other")
  duplicated(x = case(selection,
    all = x@plates,
    csv = cbind(setup_time(x), position(x)),
    metadata = metadata(x),
    other = metadata(object = x, key = what)
  ), incomparables = incomparables, ...)
}, sealed = SEALED)


################################################################################


#' Determine whether plates are duplicated
#'
#' Check whether duplicated \code{\link{OPM}} or \code{\link{OPMA}} objects are
#' contained within an \code{\link{OPMS}} object.  For reasons of consistency,
#' the \code{\link{OPM}} method always returns \code{0}.
#'
#' @param x \code{\link{OPMS}} object.
#' @param incomparables Vector passed to \code{\link{duplicated}}. The default
#'   is \code{FALSE}.
#' @param ... Optional arguments passed to \code{\link{duplicated}}. See the
#'   examples.
#' @export
#' @return Integer scalar. \code{0} if no values are duplicated, the index of
#'   the first or last (depending on \code{fromLast}) duplicated object
#'   otherwise.
#' @seealso base::anyDuplicated
#' @family getter-functions
#' @keywords attribute
#' @examples
#'
#' # 'OPM' method
#' data(vaas_1)
#' (x <- anyDuplicated(vaas_1))
#' stopifnot(identical(x, 0L)) # no complete plate duplicated
#' (x <- anyDuplicated(vaas_1, what = list("Strain", "Species")))
#' stopifnot(identical(x, 0L)) # no organisms duplicated
#'
#' # 'OPMS' method
#' data(vaas_4)
#' stopifnot(identical(anyDuplicated(vaas_4), 0L))
#' stopifnot(identical(anyDuplicated(vaas_4, what = list("Strain")), 0L))
#' # => no strains duplicated
#' stopifnot(identical(anyDuplicated(vaas_4, what = list("Species")), 2L))
#' # => species duplicated
#' x <- vaas_4[c(1, 1)] # complete plate duplicated
#' stopifnot(identical(anyDuplicated(x), 2L))
#'
setGeneric("anyDuplicated")

setMethod("anyDuplicated", c(OPM, "ANY"), function(x, incomparables, ...) {
  0L
}, sealed = SEALED)

setMethod("anyDuplicated", c(OPMS, "missing"), function(x, incomparables, ...) {
  anyDuplicated(x = x, incomparables = FALSE, ...)
}, sealed = SEALED)

setMethod("anyDuplicated", c(OPMS, "ANY"), function(x, incomparables, ...) {
  dups <- which(duplicated(x = x, incomparables = incomparables, ...))
  case(length(dups), 0L, dups[1L])
}, sealed = SEALED)


################################################################################


# Applying OPM methods with function(object, ...) signature to the 1st plate
# only.
#
lapply(c(
    #+
    wells,
    plate_type
    #-
  ), FUN = function(func_) {
  setMethod(func_, OPMS, function(object, ...) {
    func_(object@plates[[1L]], ...)
  }, sealed = SEALED)
})


# OPM methods with function(object, ...) signature that can conditionally be
# simplified.
#
lapply(c(
    #+
    aggregated,
    aggr_settings,
    csv_data,
    discretized,
    disc_settings,
    has_aggr,
    has_disc,
    hours,
    measurements,
    metadata,
    filename,
    position,
    setup_time,
    well
    #-
  ), FUN = function(func_) {
  setMethod(func_, OPMS, function(object, ...) {
    simplify_conditionally <- function(x) { # instead of sapply()
      if (any(vapply(x, is.list, NA)) || any(vapply(x, is.matrix, NA)))
        return(x)
      if (length(n <- unique.default(vapply(x, length, 0L))) > 1L)
        return(x)
      if (n > 1L)
        do.call(rbind, x)
      else
        unlist(x)
    }
    simplify_conditionally(lapply(object@plates, FUN = func_, ...))
  }, sealed = SEALED)
})


################################################################################
################################################################################
#
# Infix operators
#


#' Search in metadata keys
#'
#' Search for the presence of metadata keys, either using a vector, factor,
#' list or formula. Depending on the arguments, the behaviour differs from
#' \code{\link{infix.largek}}.
#'
#' @name %k%
#' @aliases infix.k
#' @rdname infix.k
#'
#' @param x Character vector, factor, list or formula. See \sQuote{Details}.
#' @param table \code{\link{WMD}} or \code{\link{OPMS}} object.
#' @return Logical vector of the length of the \code{\link{WMD}} or
#'   \code{\link{OPMS}} object.
#' @exportMethod "%k%"
#' @export
#~ @family getter-functions
#' @keywords attribute
#' @note The two arguments can swap their places.
#' @details The behaviour of these methods depends on the object used as query.
#' \itemize{
#'   \item Using a character vector as query, this method tests whether all
#'   given keys are present in the top-level names of the metadata (these may be
#'   nested, but all sublists are ignored here). An empty query vector results
#'   in \code{TRUE}. Note that the values of the character vector, not its
#'   names, if any, are used for querying the metadata.
#'   \item Using a list as query, this method tests whether all given keys are
#'   present in the names of the metadata. This works like the character method,
#'   but because a query list is given, the comparison of keys can be applied
#'   recursively (by using, of course, a nested query list). This is based on
#'   \code{\link{contains}} with the \code{values} argument set to \code{FALSE}.
#'   \item The factor method first converts \code{x} to \sQuote{character} mode.
#'   \item The formula method attempts to evaluate the right side of the formula
#'   in the context of the metadata of \code{table} and returns whether or not
#'   this fails (yields an error). But symbols that are not found within the
#'   metadata are looked up in the enclosing environment. Note also that missing
#'   objects are not the only potential reason of failure.
#' }
#' See \code{\link{subset}} for usage examples with \code{\link{OPMS}} objects.
#'
#' @examples
#'
#' # The dataset contains the metadata keys 'Species' and 'Experiment' but
#' # neither 'Trial' nor 'Organism' nor 'Run':
#' data(vaas_1)
#' # In the following we use stopifnot(), which fails unless all arguments
#' # passed are TRUE.
#'
#' # Character method
#' stopifnot("Experiment" %k% vaas_1) # present
#' stopifnot("Species" %k% vaas_1) # present
#' stopifnot(!"Run" %k% vaas_1) # missing
#' stopifnot(c("Species", "Experiment") %k% vaas_1) # all present
#' stopifnot(!c("Species", "Trial") %k% vaas_1) # only one present
#' stopifnot(!c("Organism", "Experiment") %k% vaas_1) # only one present
#' stopifnot(character() %k% vaas_1) # empty query always results
#'
#' # List method
#' stopifnot(list(Experiment = "whatever") %k% vaas_1) # key present
#' stopifnot(list(Species = "ignored") %k% vaas_1) # key present
#'
#' # This fails because we query with a named sublist but the 'Species'
#' # metadata entry is not even a list.
#' stopifnot(!list(Species = list(Genus = "X", Epithet = "Y")) %k% vaas_1)
#'
#' # This is OK because we query with an unnamed sublist: it has no names that
#' # one would fail to find.
#' stopifnot(list(Species = list("X", "Y")) %k% vaas_1)
#'
#' # More non-nested query examples
#' stopifnot(!list(Run = 99) %k% vaas_1) # key not present
#' stopifnot(list(Species = "?", Experiment = NA) %k% vaas_1) # keys present
#' stopifnot(!list(Species = "?", Trial = NA) %k% vaas_1) # one key missing
#' stopifnot(!list(Organism = "?", Experiment = NA) %k% vaas_1) # likewise
#' stopifnot(list() %k% vaas_1) # empty query always results
#'
#' # Formulas for querying, compare with list examples above
#' stopifnot((~ Experiment) %k% vaas_1) # key present
#' stopifnot(vaas_1 %k% ~ Experiment) # key present, no parens needed
#' stopifnot(vaas_1 %k% ~ Species) # key present, no parens needed
#' stopifnot(!vaas_1 %k% ~ Species$Epithet) # nested key not present
#' stopifnot(!vaas_1 %k% ~ missing.name) # key not present
#' missing.name <- "abc"
#' stopifnot(vaas_1 %k% ~ missing.name) # key found in enclosing environment
#' rm(missing.name) # tidy up
#'
setGeneric("%k%", function(x, table) standardGeneric("%k%"))

setMethod("%k%", c("character", WMD), function(x, table) {
  all(x %in% names(table@metadata))
}, sealed = SEALED)

setMethod("%k%", c("list", WMD), function(x, table) {
  contains(table@metadata, x, values = FALSE)
}, sealed = SEALED)

setMethod("%k%", c("formula", WMD), function(x, table) {
  tryCatch({
    eval(x[[length(x)]], table@metadata, parent.frame())
    TRUE
  }, error = function(e) FALSE)
}, sealed = SEALED)


################################################################################


#' Search in metadata keys (strict version)
#'
#' Search for the presence of metadata keys, either using a vector, factor,
#' list or formula. Depending on the arguments, the behaviour differs from
#' \code{\link{infix.k}}.
#'
#' @name %K%
#' @aliases infix.largek
#' @rdname infix.largek
#'
#' @param x Character vector, factor, list or formula. See \sQuote{Details}.
#' @param table \code{\link{WMD}} or \code{\link{OPMS}} object.
#' @return Logical vector of the length of the \code{\link{WMD}} or
#'   \code{\link{OPMS}} object.
#' @note The two arguments can swap their places.
#' @details The behaviour of these methods depends on the object used as query.
#' \itemize{
#'   \item Using a character vector as query, this method tests whether a given
#'   key is present in the metadata and fetches an object that is not
#'   \code{NULL}. If the key has a length > 1, sublists are queried. An empty
#'   vector results in \code{TRUE}. Note that the values of the character
#'   vector, not its names, if any, are used for querying the metadata.
#'   \item Using a list as query, this function behaves like
#'   \code{\link{infix.k}}.
#'   \item The factor method first converts \code{x} to \sQuote{character} mode.
#'   \item The formula method attempts to evaluate the right side of the formula
#'   in the context of the metadata of \code{table} and returns whether or not
#'   this fails (yields an error). In contrast to \code{infix.k}, symbols that
#'   are not found within the metadata are looked up in the base environment.
#'   But note that missing objects are not the only potential reason of failure.
#' }
#' See \code{\link{subset}} for usage examples with \code{\link{OPMS}} objects.
#' @export
#' @exportMethod "%K%"
#~ @family getter-functions
#' @keywords attribute
#'
#' @examples
#'
#' # The dataset contains the metadata keys 'Species' and 'Experiment' but
#' # neither 'Trial' nor 'Organism' nor 'Run':
#' data(vaas_1)
#' # In the following we use stopifnot(), which fails unless all arguments
#' # passed are TRUE.
#'
#' # Character method
#'
#' # Single-element queries
#' stopifnot("Experiment" %K% vaas_1) # present
#' stopifnot("Species" %K% vaas_1)  # present
#' stopifnot(!"Run" %K% vaas_1) # not present
#' stopifnot(!"Trial" %K% vaas_1) # not present
#' stopifnot(!"Organism" %k% vaas_1) # not present
#'
#' # Zero-element queries
#' stopifnot(character() %K% vaas_1) # always results
#'
#' # Querying with vectors of length > 1 mean nested queries; compare this to
#' # the behavior of %k%!
#' stopifnot(!c("Species", "Experiment") %K% vaas_1)
#' # I.e. "Experiment" is not within "Species".
#'
#' # List method
#' # See %k% -- the behavior is identical for lists.
#'
#' # Formulas for querying, compare with %k%
#' stopifnot((~ Experiment) %K% vaas_1) # key present
#' stopifnot(vaas_1 %K% ~ Experiment) # key present, no parens needed
#' stopifnot(vaas_1 %K% ~ Species) # key present, no parens needed
#' stopifnot(!vaas_1 %K% ~ Species$Epithet) # nested key not present
#' stopifnot(!vaas_1 %K% ~ missing.name) # key not present
#' missing.name <- "abc"
#' stopifnot(!vaas_1 %K% ~ missing.name) # enclosing environment ignored
#' rm(missing.name) # tidy up
#'
setGeneric("%K%", function(x, table) standardGeneric("%K%"))

setMethod("%K%", c("character", WMD), function(x, table) {
  if (!length(x))
    return(TRUE) # for consistency with %k%
  tryCatch(!is.null(table@metadata[[x]]), error = function(e) FALSE)
}, sealed = SEALED)

setMethod("%K%", c("list", WMD), function(x, table) {
  contains(table@metadata, x, values = FALSE)
}, sealed = SEALED)

setMethod("%K%", c("formula", WMD), function(x, table) {
  tryCatch({
    eval(x[[length(x)]], table@metadata, baseenv())
    TRUE
  }, error = function(e) FALSE)
}, sealed = SEALED)


################################################################################


#' Query metadata (non-exact version)
#'
#' Search for the presence of metadata values for given keys, either using a
#' vector, factor, list or formula. Depending on the arguments, the behaviour
#' differs from \code{\link{infix.largeq}}.
#'
#' @name %q%
#' @aliases infix.q
#' @rdname infix.q
#'
#' @param x Character vector, factor, list or formula used as query. See
#'   \sQuote{Details}.
#' @param table \code{\link{WMD}} or \code{\link{OPMS}} object.
#' @return Logical vector of the length of the \code{\link{WMD}} or
#'   \code{\link{OPMS}} object.
#' @note The two arguments can swap their places.
#' @details The behaviour of these methods depends on the object used as query.
#' \itemize{
#'   \item Using a character vector as query, this tests whether all given query
#'   keys are present in the top-level names of the metadata and refer to the
#'   same query elements. The \code{names} of the vector are used to select
#'   elements from the top level of the metadata. These elements are then
#'   converted to \sQuote{character} mode before comparison with the values of
#'   \code{x}. A non-empty vector without a \code{names} attribute is accepted
#'   but will always yield \code{FALSE}. In contrast, an entirely empty vector
#'   yields \code{TRUE}.
#'   \item Using a list, a non-exact query with a query list is conducted.  The
#'   comparison is applied recursively using \code{\link{contains}} with the
#'   \code{values} argument set to \code{TRUE} but \code{exact} set to
#'   \code{FALSE}. The main advantage of using a list over the character-based
#'   search is that it allows for a nested query.
#'   \item The factor method first converts \code{x} to \sQuote{character} mode.
#'   \item The formula method attempts to evaluate the right side of the formula
#'   in the context of the metadata of \code{table} and returns the result. For
#'   the \code{\link{WMD}} method, is is up to the user to ensure that the
#'   result is a logical scalar, but the method would succeed anyway. The
#'   \code{\link{OPMS}} yields an error unless each plate yields a logical
#'   scalar. Symbols that are not found within the metadata are looked up in the
#'   enclosing environment. This is less strict than \code{\link{infix.largeq}}.
#'   Because of missing objects and other reasons the method might nevertheless
#'   fail.
#' }
#' See \code{\link{subset}} for usage examples with \code{\link{OPMS}} objects.
#' @exportMethod "%q%"
#' @export
#~ @family getter-functions
#' @keywords attribute
#'
#' @examples
#'
#' # The dataset contains the metadata keys 'Species' and 'Experiment' with the
#' # values 'Escherichia coli' and 'First replicate':
#' data(vaas_1)
#'
#' # Character method
#' stopifnot(!"Experiment" %q% vaas_1) # wrong query here; compare to %k%
#' stopifnot(!"First replicate" %q% vaas_1) # again wrong query
#' stopifnot(c(Experiment = "First replicate") %q% vaas_1) # correct query
#'
#' stopifnot(!"Species" %q% vaas_1) # wrong query
#' stopifnot(!"Escherichia coli" %q% vaas_1) # wrong query
#' stopifnot(c(Species = "Escherichia coli") %q% vaas_1) # correct query
#'
#' stopifnot(c(Species = "Escherichia coli",
#'   Experiment = "First replicate") %q% vaas_1) # combined query, all TRUE
#'
#' stopifnot(character() %q% vaas_1) # empty query always results
#'
#' # List method
#' stopifnot(list(Experiment = "First replicate") %q% vaas_1)
#'
#' # Choice among alternatives
#' stopifnot(list(Experiment = c("First replicate",
#'   "Second replicate")) %q% vaas_1) # one of them TRUE
#' stopifnot(!list(Experiment = c("Second replicate",
#'   "Third replicate")) %q% vaas_1) # none of them TRUE
#'
#' # Combined query together with choice among alternatives
#' stopifnot(list(Experiment = c("First replicate", "Second replicate"),
#'   Species = c("Escherichia coli", "Bacillus subtilis")) %q% vaas_1)
#'
#' stopifnot(list() %q% vaas_1) # empty query
#'
#' # Formulas for querying, compare with %Q%
#' stopifnot((~ Experiment == "First replicate") %q% vaas_1)
#' stopifnot(vaas_1 %q% ~ Experiment == "First replicate")
#' stopifnot(vaas_1 %q% ~ Species == "Escherichia coli")
#' stopifnot(vaas_1 %q% ~ Species != "Bacillus subtilis")
#' x <- try(vaas_1 %q% ~ missing.name == "abc", silent = TRUE) # fails
#' stopifnot(inherits(x, "try-error"))
#' missing.name <- "abc"  # enclosing environment considered
#' stopifnot(vaas_1 %q% ~ missing.name == "abc")
#' rm(missing.name) # tidy up
#'
#' # examples for OPMS methods are given under subset()
#'
setGeneric("%q%", function(x, table) standardGeneric("%q%"))

setMethod("%q%", c("character", WMD), function(x, table) {
  if (length(keys <- names(x)) == 0L && length(x) > 0L)
    return(FALSE)
  got <- unlist(table@metadata[keys])
  length(x) == length(got) && all(x == got)
}, sealed = SEALED)

setMethod("%q%", c("list", WMD), function(x, table) {
  contains(table@metadata, x, values = TRUE, exact = FALSE)
}, sealed = SEALED)

setMethod("%q%", c("formula", WMD), function(x, table) {
  eval(x[[length(x)]], table@metadata, parent.frame())
}, sealed = SEALED)


################################################################################


#' Query metadata (strict version)
#'
#' Search for the presence of metadata values for given keys, either using a
#' vector, factor, list or formula. Depending on the arguments, the behaviour
#' differs from \code{\link{infix.q}}.
#'
#' @name %Q%
#' @aliases infix.largeq
#' @rdname infix.largeq
#'
#' @param x Character vector, factor, list for formula used as query. See
#'   \sQuote{Details}.
#' @param table \code{\link{WMD}} or \code{\link{OPMS}} object.
#' @note The two arguments can swap their places.
#' @details The behaviour of these methods depends on the object used as query.
#' \itemize{
#'   \item Using a character vector as query, this tests whether all given query
#'   keys are present in the top-level names of the metadata and refer to the
#'   same query elements (without coercion to character). The result is
#'   identical to the one of \code{\link{infix.q}} except for the fact that
#'   metadata elements are not coerced to \sQuote{character} mode, making the
#'   query more strict.
#'   \item Using a list, this conducts an exact query with this query list. The
#'   comparison is applied recursively using \code{\link{contains}} with the
#'   arguments \code{values} and \code{exact} set to \code{TRUE}. This might be
#'   too strict for most applications. The main advantage of using a list over
#'   the character-based search is that it allows one a nested query.
#'   \item The factor method first converts \code{x} to \sQuote{character} mode.
#'   \item The formula method attempts to evaluate the right side of the formula
#'   in the context of the metadata of \code{table} and returns the result. For
#'   the \code{\link{WMD}} method, is is up to the user to ensure that the
#'   result is a logical scalar, but the method would succeed anyway. The
#'   \code{\link{OPMS}} yields an error unless each plate yield a logical
#'   scalar. Symbols that are not found within the metadata are looked up in the
#'   only in the base environment. This is stricter than \code{\link{infix.q}}.
#'   Because of missing objects and other reasons the method might fail.
#' }
#' See \code{\link{subset}} for usage examples with \code{\link{OPMS}} objects.
#' @return Logical scalar.
#' @exportMethod "%Q%"
#' @export
#~ @family getter-functions
#' @keywords attribute
#'
#' @examples
#'
#' # The dataset contains the metadata keys 'Species' and 'Experiment' with the
#' # values 'Escherichia coli' and 'First replicate':
#' data(vaas_1)
#'
#' # Character method
#' stopifnot(c(Experiment = "First replicate") %Q% vaas_1)
#'
#' # This does not work because the value has the wrong type
#' stopifnot(!c(`Plate number` = "6") %Q% vaas_1)
#' # Compare to %q%
#' stopifnot(c(`Plate number` = "6") %q% vaas_1)
#'
#' # Combined query
#' stopifnot(c(Species = "Escherichia coli",
#'   Experiment = "First replicate") %Q% vaas_1) # all present
#'
#' stopifnot(character() %Q% vaas_1) # empty query always results
#'
#' # List method
#' stopifnot(list(Experiment = "First replicate") %Q% vaas_1) # present
#'
#' # Choice among alternatives is not done here: this query fails unless this
#' # two-element vector is contained. Compare to %q%.
#' stopifnot(!list(Experiment = c("First replicate",
#'   "Second replicate")) %Q% vaas_1)
#'
#' stopifnot(list() %Q% vaas_1) # empty query always result
#'
#' # Formulas for querying, compare with %q%
#' stopifnot((~ Experiment == "First replicate") %Q% vaas_1)
#' stopifnot(vaas_1 %Q% ~ Experiment == "First replicate")
#' stopifnot(vaas_1 %Q% ~ Species == "Escherichia coli")
#' stopifnot(vaas_1 %Q% ~ Species != "Bacillus subtilis")
#' x <- try(vaas_1 %Q% ~ missing.name == "abc", silent = TRUE) # fails
#' stopifnot(inherits(x, "try-error"))
#' missing.name <- "abc" # enclosing environment will be ignored
#' x <- try(vaas_1 %Q% ~ missing.name == "abc", silent = TRUE) # still fails
#' stopifnot(inherits(x, "try-error"))
#' rm(missing.name) # tidy up
#'
setGeneric("%Q%", function(x, table) standardGeneric("%Q%"))

setMethod("%Q%", c("character", WMD), function(x, table) {
  if (length(keys <- names(x)) == 0L && length(x) > 0L)
    return(FALSE)
  all(vapply(keys, function(key) identical(x[[key]], table@metadata[[key]]),
    NA))
}, sealed = SEALED)

setMethod("%Q%", c("list", WMD), function(x, table) {
  contains(table@metadata, x, values = TRUE, exact = TRUE)
}, sealed = SEALED)

setMethod("%Q%", c("formula", WMD), function(x, table) {
  eval(x[[length(x)]], table@metadata, baseenv())
}, sealed = SEALED)


################################################################################
#
# Automatically generated factor methods
#

lapply(c(
    #+
    `%k%`,
    `%K%`,
    `%q%`,
    `%Q%`
    #-
  ), FUN = function(func_) {
  setMethod(func_, c("factor", WMD), function(x, table) {
    func_(structure(.Data = as.character(x), .Names = names(x)), table)
  }, sealed = SEALED)
})


################################################################################
#
# Automatically generated OPMS methods
#


# OPM methods with function(x, table, ...) signature (infix operators).
#
lapply(c(
    #+
    `%k%`,
    `%K%`,
    `%q%`,
    `%Q%`
    #-
  ), FUN = function(func_) {
  setMethod(func_, c("list", OPMS), function(x, table) {
    vapply(table@plates, func_, NA, x = x, USE.NAMES = FALSE)
  }, sealed = SEALED)
})

lapply(c(
    #+
    `%k%`,
    `%K%`,
    `%q%`,
    `%Q%`
    #-
  ), FUN = function(func_) {
  setMethod(func_, c("character", OPMS), function(x, table) {
    vapply(table@plates, func_, NA, x = x, USE.NAMES = FALSE)
  }, sealed = SEALED)
})

lapply(c(
    #+
    `%k%`,
    `%K%`,
    `%q%`,
    `%Q%`
    #-
  ), FUN = function(func_) {
  setMethod(func_, c("factor", OPMS), function(x, table) {
    vapply(table@plates, func_, NA, x = x, USE.NAMES = FALSE)
  }, sealed = SEALED)
})

lapply(c(
    #+
    `%k%`,
    `%K%`,
    `%q%`,
    `%Q%`
    #-
  ), FUN = function(func_) {
  setMethod(func_, c("formula", OPMS), function(x, table) {
    vapply(table@plates, func_, NA, x = x, USE.NAMES = FALSE)
  }, sealed = SEALED)
})

lapply(c(
    #+
    `%k%`,
    `%K%`,
    `%q%`,
    `%Q%`
    #-
  ), FUN = function(func_) {
  setMethod(func_, c(WMD, "ANY"), function(x, table) {
    func_(table, x)
  }, sealed = SEALED)
})

lapply(c(
    #+
    `%k%`,
    `%K%`,
    `%q%`,
    `%Q%`
    #-
  ), FUN = function(func_) {
  setMethod(func_, c(OPMS, "ANY"), function(x, table) {
    func_(table, x)
  }, sealed = SEALED)
})


################################################################################




