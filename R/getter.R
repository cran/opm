

################################################################################
################################################################################
#
# Getter functions for the measurements
#


#' Stored measurements
#'
#' Return the measurements. The first column contains the hours, the other
#' ones contain the values from each well. There is one row per time point.
#' Column names are appropriately set, but not translated (as, e.g., to
#' substrate names). It is possible to select wells, but the time points are
#' always included as first column (in contrast to \code{\link{well}}). The
#' \code{i} argument refers only to the remaining matrix.
#'
#' @param object \code{\link{OPM}} or \code{\link{OPMS}} object.
#' @param i Optional character or numeric vector with name(s) or position(s)
#'   of well(s).
#' @param ... Optional arguments passed between the methods.
#' @return Numeric matrix with column names indicating the well coordinate
#'   and a first column containing the time points.
#' @export
#' @family getter-functions
#' @keywords attribute
#' @examples
#'
#' # 'OPM' method
#' data(vaas_1)
#' x <- measurements(vaas_1)
#' stopifnot(is.matrix(x), is.numeric(x))
#' stopifnot(identical(dim(x), c(384L, 97L)))
#' y <- measurements(vaas_1, "B03")
#' stopifnot(is.matrix(y), is.numeric(y))
#' stopifnot(identical(dim(y), c(384L, 2L)))
#'
#' # 'OPMS' method
#' data(vaas_4)
#' x <- measurements(vaas_4)
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
      object@measurements[, -1L, drop = FALSE][, i, drop = FALSE])
}, sealed = SEALED)


################################################################################


## NOTE: "[" is a primitive and needs no setGeneric().


#' Select subset
#'
#' Select a subset of the measurements. Return other slots unchanged. In
#' contrast to the usual `[` functions, always return a matrix (as a component
#' of the returned object), even if it could be simplified to a vector.
#' The time column is not counted and always copied. It is an error to delete
#' the entire matrix. In all other respects, this method behaves like the `[`
#' functions from the \pkg{base} package. The \code{\link{OPMS}} method
#' selects a subset of the plates and/or the measurements of the individual
#' plates. It simplifies the outcome to a \code{\link{OPM}} or
#' \code{\link{OPMA}} object if only a single plate remains and to \code{NULL}
#' if no plate remains. This behaves like subsetting a three-dimensional array
#' with plates as first dimension, time points as second, and wells as third.
#'
#' @rdname bracket
#' @exportMethod "["
#' @export
#'
#' @param x \code{\link{OPM}}, \code{\link{OPMA}} or \code{\link{OPMS}} object.
#' @param i Vector or missing. For the \code{\link{OPM}} and \code{\link{OPMA}}
#'   method, the indexes of one to several time points. For the
#'   \code{\link{OPMS}} method, the indexes of one to several plates.
#' @param j Vector or missing. For the \code{\link{OPM}} and \code{\link{OPMA}}
#'   method, the indexes or names of one to several wells. For the
#'   \code{\link{OPMS}} method, the indexes of one to several time points. In
#'   that case, if \code{j} is a list, its values are passed to the respective
#'   \code{\link{OPM}} object separately, allowing for individual choices of
#'   time points. Otherwise \code{j} is used as the \code{i} argument of the
#'   \code{\link{OPM}} and \code{\link{OPMA}} method.
#' @param k Vector or missing. The \code{\link{OPMS}} method uses \code{k} as
#'   \code{j} argument of the \code{\link{OPM}} and \code{\link{OPMA}} method.
#'   That is, this parameter selects the wells.
#' @param ... This should \strong{not} be set. It is an error to specify
#'   additional dimensions.
#' @param drop Logical scalar. Remove the aggregated data and turn
#'   \code{\link{OPMA}} to \code{\link{OPM}} objects? Has no effect if \code{x}
#'   already is an \code{\link{OPM}} object or contains only such objects.
#' @return \code{\link{OPM}}, \code{\link{OPMA}} or \code{\link{OPMS}} object,
#'   or \code{NULL}.
#'
#' @details The \code{\link{OPMA}} method works like the \code{\link{OPM}} one,
#'   but the function applies the subsetting to the original and the aggregated
#'   data in parallel. The aggregated data may also be dropped entirely; this
#'   might be appropriate if a subset of the time points is selected,
#'   potentially yielding aggregated values that do not fit to the measurements
#'   anymore.
#' @seealso base::`[` base::`[[`
#' @keywords manip
#'
#' @examples
#'
#' # OPM(A) method
#' data(vaas_1)
#' (x <- dim(vaas_1))
#' stopifnot(identical(x, c(384L, 96L)))
#' copy <- vaas_1[, 11:22]
#' (x <- dim(copy))
#' stopifnot(identical(x, c(384L, 12L)))
#' copy <- vaas_1[]
#' stopifnot(has_aggr(copy))
#' stopifnot(identical(copy, vaas_1))
#' copy <- vaas_1[drop = TRUE]
#' stopifnot(!has_aggr(copy))
#' stopifnot(!identical(copy, vaas_1))
#'
#' # OPMS method
#' data(vaas_4)
#'
#' # Create OPMS object with fewer plates (the first two ones)
#' x <- vaas_4[1:2]
#' stopifnot(dim(x) == c(2, 384, 96))
#'
#' # If only a single plate is selected, this is reduced to OPM(A)
#' x <- vaas_4[3]
#' stopifnot(dim(x) == c(384, 96))
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
#' x <- vaas_4[, , sprintf("A%02i", 1:12)] # this yields A01...A12
#' stopifnot(dim(x) == c(4, 384, 12))
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
#' # see also opms_apply() for a more elegant approach
#'
setMethod("[", OPM, function(x, i, j, ..., drop = FALSE) {
  mat <- x@measurements[, -1L, drop = FALSE][i, j, ..., drop = FALSE]
  if (any(dim(mat) == 0L))
    stop("selection resulted in empty matrix")
  mat <- cbind(x@measurements[i, 1L, drop = FALSE], mat)
  names(dimnames(mat)) <- names(dimnames(x@measurements))
  result <- x
  result@measurements <- mat
  result
}, sealed = SEALED)

setMethod("[", OPMA, function(x, i, j, ..., drop = FALSE) {
  result <- callNextMethod(x = x, i = i, j = j, ..., drop = drop)
  if (drop)
    return(as(result, OPM))
  if (!missing(j))
    result@aggregated <- result@aggregated[, j, ..., drop = FALSE]
  result
}, sealed = SEALED)

setMethod("[", OPMD, function(x, i, j, ..., drop = FALSE) {
  result <- callNextMethod(x = x, i = i, j = j, ..., drop = drop)
  if (drop)
    return(result)
  if (!missing(j))
    result@discretized <- result@discretized[j]
  result
}, sealed = SEALED)

setMethod("[", OPMS, function(x, i, j, k, ..., drop = FALSE) {
  if (!missing(...))
    stop("incorrect number of dimensions")
  fetch <- function(obj, idx) obj[i = idx, j = k, drop = drop]
  result <- x@plates[i]
  if (no.k <- missing(k))
    k <- TRUE
  if (missing(j)) {
    if (!no.k || drop)
      result <- lapply(result, fetch, idx = TRUE)
  } else if (is.list(j))
    result <- mapply(fetch, result, j, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  else
    result <- lapply(result, fetch, idx = j)
  case(length(result), NULL, result[[1L]], {
      x@plates <- result
      x
    })
}, sealed = SEALED)


################################################################################


#' Measurements from selected wells
#'
#' Get measurements from specified well(s) stored in an \code{\link{OPM}}
#' object. This function will always ignore the time points, in contrast to
#' \code{\link{measurements}}.
#'
#' @param object \code{\link{OPM}} object.
#' @param i Character or numeric vector with name(s) or position(s) of
#'   well(s). Wells are originally named \sQuote{A01} to \sQuote{H12} but
#'   might have been subset beforehand.
#' @param drop Logical scalar. If only a single well was selected, simplify
#'   it to a vector?
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
#' (x <- well(vaas_1, "B04"))
#' stopifnot(is.numeric(x), length(x) == 384L)
#' (x <- well(vaas_1, c("B08", "C07")))
#' stopifnot(is.matrix(x), identical(dim(x), c(384L, 2L)))
#'
#' # 'OPMS' method
#' data(vaas_4)
#' (x <- well(vaas_4, "B04"))
#' stopifnot(is.matrix(x), dim(x) == c(4, 384))
#'
setGeneric("well", function(object, ...) standardGeneric("well"))

setMethod("well", OPM, function(object, i, drop = TRUE) {
  object@measurements[, -1L, drop = FALSE][, i, drop = drop]
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
#' (x <- hours(vaas_4))
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
#' \code{\link{OPMS}} method works by calling the \code{\link{OPM}} method
#' on all plates and then determining the overall maximum.
#'
#' @param x \code{\link{OPM}} or \code{\link{OPMS}} object.
#' @param ... Coordinate of one to several wells. If missing, the maximum of
#'   all wells is returned. See \code{\link{well}} for details.
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
#' stopifnot(x > y)
#'
#' # OPMS method
#' data(vaas_4)
#' (x <- max(vaas_4))
#' (y <- max(vaas_4, 1)) # this is the negative control
#' stopifnot(x > y)
#'
setMethod("max", OPM, function(x, ..., na.rm = FALSE) {
  if (missing(...))
    max(x@measurements[, -1L, drop = FALSE], na.rm = na.rm)
  else
    max(well(x, ...), na.rm = na.rm)
}, sealed = SEALED)

setMethod("max", OPMS, function(x, ..., na.rm = FALSE) {
  max(vapply(x@plates, FUN = max, numeric(1L), ..., na.rm = na.rm),
    na.rm = na.rm)
}, sealed = SEALED)


################################################################################


#' Smallest maximum
#'
#' Get the smallest maximum among all wells. The
#' \code{\link{OPMS}} method works by calling the \code{\link{OPM}} method
#' on all plates and then determining the overall minimum.
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
#' (x <- max(vaas_1))
#' (y <- minmax(vaas_1))
#' stopifnot(x > y)
#'
#' # OPMS method
#' data(vaas_4)
#' (x <- max(vaas_4))
#' (y <- minmax(vaas_4))
#' stopifnot(x > y)
#'
setGeneric("minmax", function(x, ...) standardGeneric("minmax"))

setMethod("minmax", OPM, function(x, ..., na.rm = FALSE) {
  min(apply(x@measurements[, -1L, drop = FALSE][, ..., drop = FALSE], 2L,
    FUN = max, na.rm = na.rm))
}, sealed = SEALED)

setMethod("minmax", OPMS, function(x, ..., na.rm = FALSE) {
  min(vapply(x@plates, FUN = minmax, numeric(1L), ..., na.rm = na.rm))
}, sealed = SEALED)


################################################################################


## NOTE: 'dim' is a primitive and needs no setGeneric().

#' Dimensions
#'
#' Get the dimensions of the measurements of an \code{\link{OPM}} object, or
#' get the dimensions of an \code{\link{OPMS}} object. Note that this function
#' cannot be used to determine the correspondence of the time points between
#' all plates as it reports only the time points of the first plate. Instead
#' the \code{\link{OPMS}} method of \code{\link{hours}} must be used.
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
#' (x <- dim(vaas_4))
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
#' stopifnot(identical(x, 4L))
#'
setMethod("length", OPMS, function(x) {
  length(x@plates)
}, sealed = SEALED)


################################################################################


#' Sequence of plate indexes
#'
#' Get the indexes of all plates contained in an \code{\link{OPMS}} object.
#' This is mainly useful for looping over such objects. See \code{\link{[}} for
#' a loop-construct usage example, and note that \code{\link{oapply}} is also
#' available.
#'
#' @param ... \code{\link{OPMS}} objects. Several ones can be provided, but
#'   all but the first one are ignored. For reasons of comparability, the
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
#' stopifnot(identical(x, 1:4))
#' (y <- seq(vaas_4, letters, LETTERS)) # other arguments are ignored
#' stopifnot(identical(x, y))
#'
#' # 'OPM' method
#' data(vaas_1)
#' (x <- try(seq(vaas_1), silent = TRUE))
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
#' @param object \code{\link{OPM}} object.
#' @param full Logical scalar. Return the full names of the wells (if
#'   available) or just their coordinates on the plate? The following arguments
#'   have no effect if \code{full} is \code{FALSE}.
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
#' @param ... Optional arguments passed between the methods.
#' @return Character vector.
#' @export
#' @family getter-functions
#' @seealso base::strtrim base::abbreviate
#' @keywords attribute
#' @note Do not confuse this with \code{\link{well}}.
#' @examples
#'
#' # 'OPM' method
#' data(vaas_1)
#' (x <- wells(vaas_1, full = FALSE))
#' (y <- wells(vaas_1, full = TRUE))
#' (z <- wells(vaas_1, full = TRUE, in.parens = FALSE))
#' stopifnot(nchar(x) < nchar(y), nchar(z) < nchar(y))
#'
#' # 'OPM' method
#' data(vaas_4)
#' (xx <- wells(vaas_4, full = FALSE))
#' stopifnot(identical(x, xx))
#'
setGeneric("wells", function(object, ...) standardGeneric("wells"))

setMethod("wells", OPM, function(object, full = FALSE, in.parens = TRUE,
    max = 100L, brackets = FALSE, clean = TRUE, word.wise = FALSE,
    paren.sep = " ") {
  result <- setdiff(colnames(measurements(object)), HOUR)
  if (full)
    map_well_names(result, plate_type(object), in.parens = in.parens,
      max = max, brackets = brackets, clean = clean, word.wise = word.wise,
      paren.sep = paren.sep)
  else
    result
}, sealed = SEALED)


################################################################################
################################################################################
#
# Getter functions for the CSV data
#


#' Information from input CSV file
#'
#' Information about the plate as originally read from the input CSV file. See
#' \code{\link{read_opm}} and \code{\link{read_single_opm}} for reading such
#' files.
#'
#' @param object \code{\link{OPM}} object.
#' @param keys Character vector (or other objects usable as vector index). An
#'   optional sub-selection. If empty (the default), all CSV data are returned.
#'   It is an error to select non-existing items.
#' @param ... Optional arguments passed between the methods.
#' @return Named character vector.
#' @export
#' @family getter-functions
#' @keywords attribute
#' @examples
#'
#' # 'OPM' method
#' data(vaas_1)
#' # compare this to setup_time()
#' (x <- csv_data(vaas_1, "Setup Time"))
#' stopifnot(identical(x, c(`Setup Time` = "8/30/2010 1:53:08 PM")))
#'
#' # 'OPMS' method
#' data(vaas_4)
#' (x <- csv_data(vaas_4, "Setup Time"))
#' stopifnot(is.character(x), length(x) == 4)
#'
setGeneric("csv_data", function(object, ...) standardGeneric("csv_data"))

setMethod("csv_data", OPM, function(object, keys = character()) {
  if (length(keys) == 0L || all(!nzchar(keys)))
    return(object@csv_data)
  result <- object@csv_data[keys]
  if (any(missing <- is.na(result)))
    stop("could not find key ", keys[missing][1L])
  result
}, sealed = SEALED)


################################################################################


#' Original input filename
#'
#' Get the name of the original CSV input file. This is a convenience function
#' for one of the more important entries of \code{\link{csv_data}}.
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
#' (x <- filename(vaas_1))
#' stopifnot(is.character(x), length(x) == 1L)
#'
#' # 'OPMS' method
#' data(vaas_4)
#' (x <- filename(vaas_4))
#' stopifnot(is.character(x), length(x) == 4L)
#'
setGeneric("filename", function(object, ...) standardGeneric("filename"))

setMethod("filename", OPM, function(object) {
  object@csv_data[[FILE]]
}, sealed = SEALED)


################################################################################


#' Plate type used or normalized
#'
#' Get the type of the OmniLog(R) plate used in the measuring. This is a
#' convenience function for one of the more important entries of
#' \code{\link{csv_data}} with additional options useful for creating plot
#' titles. The character method
#' normalizes the names of OmniLog(R) PM plates to the internally used naming
#' scheme. Unrecognized names are returned unchanged. This needs not normally
#' be called by the \pkg{opm} user but might be of interest.
#'
#' @param object \code{\link{OPM}} or \code{\link{OPMS}} object, or
#'   character vector of original plate name(s), or factor.
#' @param full Logical scalar. If \code{TRUE}, add (or replace by) the full
#'   name of the plate type (if available); otherwise, return it as-is.
#' @param in.parens Logical scalar. This and the five next arguments work like
#'   the eponymous ones of \code{\link{wells}}, but here are applied to the
#'   plate name. See there for details.
#' @param max Numeric scalar.
#' @param clean Logical scalar.
#' @param brackets Logical scalar.
#' @param word.wise Logical scalar.
#' @param paren.sep Character scalar.
#' @param subtype Logical scalar. Keep the plate subtype indicator, if any?
#'   Only relevant for the character or factor method.
#' @param ... Optional arguments passed between the methods.
#'
#' @return Character scalar in the case of the \code{\link{OPM}} and
#'   \code{\link{OPMS}} methods, otherwise a character vector with the same
#'   length than \code{object}, or a corresponding factor.
#'
#' @export
#' @family getter-functions
#' @seealso base::strtrim base::abbreviate base::gsub
#' @keywords attribute utilities character
#' @examples
#'
#' ## 'OPM' method
#' data(vaas_1)
#' (x <- plate_type(vaas_1, full = FALSE))
#' (y <- plate_type(vaas_1, full = TRUE))
#' (z <- plate_type(vaas_1, full = TRUE, in.parens = FALSE))
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
#' stopifnot(identical(x, xx))
#'
#' ## Character method
#'
#' # Entirely unrecognized strings are returned as-is
#' x <- plate_type(letters)
#' stopifnot(identical(x, letters))
#'
#' # Something more realistic
#' (x <- plate_type(y <- c("PM1", "PM-11C", "PMM04-a"), TRUE))
#' stopifnot(x != y)
#'
#' # Factor method
#' (z <- plate_type(as.factor(y), TRUE))
#' stopifnot(is.factor(z), z == x)
#'
setGeneric("plate_type", function(object, ...) standardGeneric("plate_type"))

setMethod("plate_type", OPM, function(object, full = FALSE, in.parens = TRUE,
    max = 100L, clean = TRUE, brackets = FALSE, word.wise = FALSE,
    paren.sep = " ") {
  result <- object@csv_data[[PLATE_TYPE]]
  if (!full)
    return(result)
  pos <- match(result, names(PLATE_MAP))
  if (is.na(pos))
    warning("cannot find full name of plate ", result)
  else if (in.parens)
    result <- add_in_parens(result, PLATE_MAP[pos], max = max, clean = clean,
      brackets = brackets, word.wise = word.wise, paren.sep = paren.sep)
  else
    result <- trim_string(PLATE_MAP[pos], max = max, clean = clean,
      word.wise = word.wise)
  result
}, sealed = SEALED)

setMethod("plate_type", "character", function(object, subtype = FALSE) {
  normalize_pm <- function(x, subtype) {
    x <- sub("^PMM", "PM-M", x, perl = TRUE)
    repl <- if (subtype)
      "-\\1"
    else
      ""
    x <- sub("([A-Z]+)$", repl, x, perl = TRUE)
    sub("([^\\d])(\\d)([^\\d]|$)", "\\10\\2\\3", x, perl = TRUE)
  }
  result <- toupper(gsub("\\W", "", object, perl = TRUE))
  ok <- grepl("^PMM?\\d+[A-Z]*$", result, perl = TRUE)
  result[ok] <- normalize_pm(result[ok], subtype = subtype)
  result[!ok] <- object[!ok]
  result
}, sealed = SEALED)

setMethod("plate_type", "factor", function(object, subtype = FALSE) {
  map_values(object, plate_type, subtype = subtype)
}, sealed = SEALED)


################################################################################


#' Setup time of the measuring
#'
#' Get the setup time of the PM experiment as recorded by the OmniLog(R)
#' device. This is a convenience function for one of the more important entries
#' of \code{\link{csv_data}}.
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
#' (x <- setup_time(vaas_1))
#' # WARNING: It is unlikely that all OmniLog output has this setup time format
#' (parsed <- strptime(x, format = "%m/%d/%Y %I:%M:%S %p"))
#' stopifnot(inherits(parsed, "POSIXlt"), length(parsed) == 1)
#'
#' # 'OPMS' method
#' data(vaas_4)
#' (x <- setup_time(vaas_4))
#' (parsed <- strptime(x, format = "%m/%d/%Y %I:%M:%S %p"))
#' stopifnot(inherits(parsed, "POSIXlt"), length(parsed) == 4)
#'
setGeneric("setup_time", function(object, ...) standardGeneric("setup_time"))

setMethod("setup_time", OPM, function(object) {
  object@csv_data[[SETUP]]
}, sealed = SEALED)


################################################################################


#' Position of a plate
#'
#' Get the position of the plate within the OmniLog(R) device. This is a
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
#' (x <- position(vaas_1))
#' stopifnot(identical(x, " 7-B"))
#'
#' # 'OPMS' method
#' data(vaas_4)
#' (x <- position(vaas_4))
#' stopifnot(is.character(x), length(x) == length(vaas_4))
#'
setGeneric("position", function(object, ...) standardGeneric("position"))

setMethod("position", OPM, function(object) {
  object@csv_data[[POS]]
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
#' Print summary information to screen.
#'
#' @param object \code{\link{OPM}} or \code{\link{OPMS}} object.
#' @param ... Optional arguments passed to \code{formatDL}.
#' @export
#' @return For the \code{\link{OPM}} method, a named list, returned invisibly.
#'   The \sQuote{metadata} entry is the number of non-list elements in
#'   \code{\link{metadata}}. For the \code{\link{OPMS}} method, a list of such
#'   lists (one per plate), also returned invisibly.
#' @family getter-functions
#' @keywords attribute
#' @seealso base::summary base::formatDL
#' @examples
#'
#' # OPM method
#' data(vaas_1)
#' x <- summary(vaas_1)
#' stopifnot(is.list(x))
#'
#' # OPMS method
#' data(vaas_4)
#' x <- summary(vaas_4)
#' stopifnot(is.list(x), length(x) == 4L, all(sapply(x, is.list)))
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
  lapply(formatDL(names(result), unlist(result), ...), FUN = message)
  invisible(result)
}, sealed = SEALED)

setMethod("summary", OPMS, function(object, ...) {
  result <- lapply(seq_along(object@plates), FUN = function(idx) {
    message(idx)
    summary(object@plates[[idx]], ...)
  })
  dims <- dim(object)
  tmpl <- paste("\n=> %s object with %i plates (%i aggregated) of type '%s',",
    " %i wells and about %i time points.")
  message(sprintf(tmpl, class(object), dims[1L],
    length(which(has_aggr(object))), plate_type(object), dims[3L], dims[2L]))
  invisible(result)
}, sealed = SEALED)


################################################################################


#' Show OPM or OPMS objects
#'
#' Display an \code{\link{OPM}} or \code{\link{OPMS}} object on screen.
#' Currently this is just a wrapper for the \code{\link{summary}} method for
#' these objects.
#'
#' @param object \code{\link{OPM}} or \code{\link{OPMS}} object.
#' @export
#' @return See \code{\link{summary}}.
#' @family getter-functions
#' @keywords attribute
#' @seealso methods::show base::print
#' @examples
#'
#' # OPMA method
#' data(vaas_1)
#' vaas_1
#'
#' # OPMS method
#' data(vaas_4)
#' vaas_4
#'
setMethod("show", OPM, function(object) {
  summary(object)
}, sealed = SEALED)

setMethod("show", OPMS, function(object) {
  summary(object)
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
#'   not checked, and all \code{NA} values also remain unchanged. If
#'   \code{trim} is \sQuote{no}, lambda is not modified either.
#' @param ... Optional arguments passed between the methods.
#' @export
#' @family getter-functions
#' @return Numeric matrix.
#' @keywords attribute
#' @examples
#'
#' # 'OPMA' method
#' data(vaas_1)
#' # Get full matrix
#' summary(x <- aggregated(vaas_1))
#' stopifnot(is.matrix(x), identical(dim(x), c(12L, 96L)))
#' # Subsetting
#' summary(x <- aggregated(vaas_1, "lambda"))
#' stopifnot(is.matrix(x), identical(dim(x), c(3L, 96L)), any(x < 0))
#' # Now with lambda correction
#' summary(x <- aggregated(vaas_1, "lambda", trim = "full"))
#' stopifnot(is.matrix(x), identical(dim(x), c(3L, 96L)), !any(x < 0))
#'
#' # 'OPMS' method
#' data(vaas_4)
#' summary(x <- aggregated(vaas_4))
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
#' @examples
#'
#' # 'OPM' method
#' data(vaas_1)
#' summary(x <- aggr_settings(vaas_1))
#' stopifnot(is.list(x), identical(names(x), c("program", "options")))
#' stopifnot(identical(x$program, "grofit"))
#'
#' # 'OPMS' method
#' data(vaas_4)
#' summary(x <- aggr_settings(vaas_4))
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
#' This yields the discretized values of the curve parameter
#' \sQuote{maximum height}.
#'
#' @param object \code{\link{OPMD}} or \code{\link{OPMS}} object.
#' @param ... Optional arguments passed between the methods.
#' @export
#' @family getter-functions
#' @return Logical vector or matrix.
#' @keywords attribute
#' @examples
#'
#' # 'OPM' method
#' data(vaas_1)
#' (x <- discretized(vaas_1))
#' stopifnot(is.logical(x), !is.matrix(x), length(x) == dim(x)[2L])
#' stopifnot(names(x) == colnames(aggregated(vaas_1)))
#'
#' # 'OPMS' method
#' data(vaas_4)
#' (x <- discretized(vaas_4))
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
#' @examples
#'
#' # 'OPM' method
#' data(vaas_1)
#' summary(x <- disc_settings(vaas_1))
#' stopifnot(is.list(x), identical(names(x), c("program", "options")))
#' stopifnot(identical(x$program, "kmeans"))
#'
#' # 'OPMS' method
#' data(vaas_4)
#' summary(x <- disc_settings(vaas_4))
#' stopifnot(is.list(x), is.null(names(x)), length(x) == length(vaas_4))
#' stopifnot(identical(x[[1]]$program, "kmeans"), duplicated(x)[-1])
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
#' @param key If \code{NULL} or otherwise empty, return all metadata. If a
#'   non-empty list, treat it as list of keys and return list of corresponding
#'   metadata values. Here, character vectors of length > 1 can be used to
#'   query nested metadata lists. If neither empty nor a list (i.e. usually a
#'   character or numeric scalar), treat \code{key} as a single list key.
#'   Factors are converted to \sQuote{character} mode.
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
#' @examples
#'
#' # 'OPM' method
#' data(vaas_1)
#' (x <- metadata(vaas_1, "Strain"))
#' stopifnot(identical(x, "DSM30083T"))
#'
#' # 'OPMS' method
#' data(vaas_4)
#' (x <- metadata(vaas_4, "Strain"))
#' stopifnot(x == c("DSM18039", "DSM30083T", "DSM1707", "429SC1"))
#'
setGeneric("metadata", function(object, ...) standardGeneric("metadata"))

setMethod("metadata", WMD, function(object, key = NULL, exact = TRUE,
    strict = FALSE) {
  LL(exact, strict)
  if (length(key) == 0L)
    return(object@metadata)
  fetch_fun <- if (strict)
    function(key) {
      if (is.factor(key))
        key <- as.character(key)
      if (is.null(result <- object@metadata[[key, exact = exact]]))
        stop(sprintf("got NULL value when using key '%s'",
          paste(key, collapse = " -> ")))
      result
    }
  else
    function(key) object@metadata[[key, exact = exact]]
  if (is.list(key)) {
    result <- lapply(key, fetch_fun)
    if (is.null(names(result)))
      names(result) <- unlist(key)
    result
  } else
    fetch_fun(key)
}, sealed = SEALED)


################################################################################


#' Select a subset of the plates (or time points)
#'
#' Select a subset of the plates in an \code{\link{OPMS}} object based on the
#' content of the metadata. Alternatively, select a common subset of time
#' points from all plates. The data-frame method selects columns that belong to
#' certain classes.
#'
#' @param object \code{\link{OPMS}} object.
#' @param query Logical, numeric or character vector, or list (other objects
#'   can be provided but are coerced to class \sQuote{character}). If a logical
#'   or numeric vector, \code{query} is directly used as the first argument of
#'   \code{\link{[}}, and all following arguments, if any, are ignored.
#'   If a list or a character vector, it is used for conducting a query based
#'   on one of the infix operators as described below. The data-frame method
#'   expects a character vector containing class names.
#' @param values Logical scalar. If \code{TRUE}, the values of \code{query}
#'   are also considered (by using \code{\link{infix.q}} or
#'   \code{\link{infix.largeq}}). If \code{FALSE} only the keys are considered
#'   (by using
#'   \code{\link{infix.k}}). That is, choose either the plates for which
#'   certain metadata entries contain certain values, or choose the plates
#'   for which these metadata have been set at all (to some arbitrary value).
#'   See the mentioned functions for details, and note the special behavior if
#'   \code{query} is a character vector and \code{values} is \code{FALSE}.
#' @param invert Logical scalar. If \code{TRUE}, return the plates for which
#'   the condition is not \code{TRUE}.
#' @param exact Logical scalar. If the values of \code{query} are considered,
#'   should this be done using \code{\link{infix.q}} (when \code{FALSE}) or
#'   \code{\link{infix.largeq}} (when \code{TRUE})? See these functions and
#'   \code{\link{contains}}  for details.
#' @param time Logical scalar. If \code{TRUE}, all other arguments are ignored
#'   and the object is reduced to the common subset of time points
#'   (measurement hours and minutes).
#' @param positive Character scalar. If \sQuote{ignore}, not used, Otherwise
#'   all previous arguments except \code{object} are ignored. If
#'   \sQuote{any}, wells are selected that contain positive reactions in at
#'   least one plate. If \sQuote{all}, wells are selected that contain
#'   positive reactions in all plates. This works only if all elements of
#'   \code{object} have discretized values. Using \code{invert} means selecting
#'   all negative or weak reactions.
#' @param negative Character scalar. Like \code{positive}, but returns
#'   the negative reactions. Using \code{invert} means selecting
#'   all positive or weak reactions.
#' @param use Character scalar. An alternative way to specify the settings. If
#'   \sQuote{i} or \sQuote{I}, ignored. If \sQuote{t} or \sQuote{T},
#'   \code{time} is set to \code{TRUE}. If \sQuote{p} or \sQuote{P},
#'   \code{positive} is set to \sQuote{any} or \sQuote{all}, respectively.
#'   If \sQuote{n} or \sQuote{N},
#'   \code{non.negative} is set to \sQuote{any} or \sQuote{all}, respectively.
#'   Otherwise, \code{use} is taken directly
#'   as the one-latter name of the infix operators to use for plate
#'   selection, overriding \code{values} and \code{exact}.
#' @export
#' @return \code{NULL} or \code{\link{OPM}} or \code{\link{OPMS}} object. This
#'   depends on how many plates are selected; see \code{\link{[}} for details.
#'   The data-frame method returns a data frame.
#'
#' @family getter-functions
#' @keywords manip
#' @seealso base::`[` base::`[[` base::subset
#' @examples
#'
#' ## 'OPMS' method
#' data(vaas_4)
#' # simple object comparison function
#' mustbe <- function(a, b) stopifnot(identical(a, b))
#'
#' # all plates have that entry: selection identical to original object
#' mustbe(vaas_4, vaas_4["Species" %k% vaas_4, ])
#' mustbe(vaas_4, select(vaas_4, list(Species = "Escherichia coli"),
#'   values  = FALSE)) # equivalent
#'
#' # two plates also have that value: yielding OPMS object with only two plates
#' mustbe(vaas_4[1:2], vaas_4[list(Species = "Escherichia coli") %q% vaas_4, ])
#' mustbe(vaas_4[1:2], select(vaas_4, list(Species = "Escherichia coli")))
#'
#' # select all plates that have aggregated values
#' x <- select(vaas_4, has_aggr(vaas_4))
#' mustbe(x, vaas_4) # all have such values
#'
#' # select a common set of time points
#' x <- select(vaas_4, time = TRUE)
#' mustbe(x, vaas_4) # the time points had already been identical
#' # create unequal time points
#' copy <- vaas_4[, list(1:10, 1:20, 1:15, 1:10)]
#' mustbe(hours(copy), c(2.25, 4.75, 3.50, 2.25))
#' # now restrict to common subset
#' x <- select(copy, time = TRUE)
#' mustbe(hours(x), rep(2.25, 4))
#' # see also the example with split() given under "["
#'
#' # select all wells that have positive reactions
#' summary(x <- select(vaas_4, use = "p")) # in at least one plate
#' stopifnot(dim(x)[3] < dim(vaas_4)[3])
#' summary(y <- select(vaas_4, use = "P")) # in all plates
#' stopifnot(dim(y)[3] < dim(x)[3])
#'
#' # select all wells that have non-negative reactions in at least one plate
#' summary(y <- select(vaas_4, use = "N", invert = TRUE))
#' stopifnot(dim(y)[3] > dim(x)[3])
#'
#' ## data-frame method
#' x <- data.frame(a = 1:5, b = letters[1:5], c = LETTERS[1:5])
#' (y <- select(x, "factor"))
#' stopifnot(dim(y) == c(5, 2))
#' (y <- select(x, "integer"))
#' stopifnot(dim(y) == c(5, 1))
#' (y <- select(x, c("factor", "integer")))
#' mustbe(x, y)
#'
setGeneric("select", function(object, query, ...) standardGeneric("select"))

setMethod("select", OPMS, function(object, query, values = TRUE,
    invert = FALSE, exact = FALSE, time = FALSE,
    positive = c("ignore", "any", "all"),
    negative = c("ignore", "any", "all"),
    use = c("i", "I", "k", "K", "n", "N", "p", "P", "q", "Q", "t", "T")) {
  select_binary <- function(object, invert.1, combine, invert.2) {
    x <- discretized(object)
    if (invert.1)
      x <- !x
    x[is.na(x)] <- FALSE
    x <- apply(x, 2L, combine)
    if (invert.2)
      x <- !x
    object[, , x]
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
    all = return(select_binary(object, TRUE, negative, invert))
  )
  case(positive <- match.arg(positive),
    ignore = NULL,
    any =,
    all = return(select_binary(object, FALSE, positive, invert))
  )
  if (time) {
    tp <- hours(object, what = "all")
    if (is.matrix(tp))
      tp <- lapply(seq.int(nrow(tp)), function(i) tp[i, ])
    if (length(maxs <- unique(vapply(tp, max, numeric(1L)))) < 2L)
      return(object)
    min.max <- min(maxs)
    tp <- lapply(tp, function(x) which(x <= min.max))
    return(object[, tp])
  }
  if (is.logical(query) || is.numeric(query))
    return(object[query, , ])
  if (!is.list(query) && !is.character(query))
    query <- as.character(query)
  pos <- if (values) {
    if (exact)
      query %Q% object
    else
      query %q% object
  } else
    query %k% object
  if (invert)
    pos <- !pos
  object[pos, , ]
}, sealed = SEALED)

setMethod("select", "data.frame", function(object, query) {
  object[, vapply(object, inherits, logical(1L), what = query), drop = FALSE]
}, sealed = SEALED)


################################################################################


#' Determine duplicated plates
#'
#' Check whether duplicated \code{\link{OPM}} or \code{\link{OPMA}} objects
#' are contained within an \code{\link{OPMS}} object. For reasons of
#' consistency, the \code{\link{OPM}} method always returns \code{FALSE}.
#'
#' @param x \code{\link{OPMS}} object.
#' @param  incomparables Vector passed to \code{duplicated} from the
#'   \pkg{base} package. By default this is \code{FALSE}.
#' @param what Character scalar indicating which parts of \code{x} should be
#'   compared. \sQuote{all} compares entire \code{OPM} objects; \sQuote{csv}
#'   compares the CSV data entries \code{\link{setup_time}} and
#'   \code{\link{position}}; \sQuote{metadata} compares the entire metadata
#'   content. If \code{what} does not match any of these, or is not a character
#'   scalar at all, it is passed as
#'   \code{key} argument to \code{\link{metadata}}, and the resulting metadata
#'   subsets are compared.
#' @param ... Optional arguments passed to \code{duplicated} from the
#'   \pkg{base} package.
#' @export
#' @return Logical vector.
#' @family getter-functions
#' @keywords attribute
#' @seealso base::duplicated
#' @examples
#'
#' # 'OPM' method
#' data(vaas_1)
#' (x <- duplicated(vaas_1))
#' stopifnot(identical(x, FALSE))
#'
#' # 'OPMS' method
#' data(vaas_4)
#' stopifnot(!duplicated(vaas_4))
#' stopifnot(!duplicated(vaas_4, what = list("Species", "Strain")))
#' stopifnot(duplicated(vaas_4, what = "Species") == rep(c(FALSE, TRUE), 2))
#' x <- vaas_4[c(1, 1)]
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
#' Check whether duplicated \code{\link{OPM}} or \code{\link{OPMA}} objects
#' are contained within an \code{\link{OPMS}} object.  For reasons of
#' consistency, the \code{\link{OPM}} method always returns \code{0}.
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
#' stopifnot(identical(x, 0L))
#' (x <- anyDuplicated(vaas_1, what = list("Strain", "Species")))
#' stopifnot(identical(x, 0L))
#'
#' # 'OPMS' method
#' data(vaas_4)
#' stopifnot(identical(anyDuplicated(vaas_4), 0L))
#' stopifnot(identical(anyDuplicated(vaas_4, what = list("Strain")), 0L))
#' stopifnot(identical(anyDuplicated(vaas_4, what = list("Species")), 2L))
#' x <- vaas_4[c(1, 1)]
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
      if (any(vapply(x, is.list, logical(1L))) ||
          any(vapply(x, is.matrix, logical(1L))))
        return(x)
      if (length(n <- unique(vapply(x, length, integer(1L)))) > 1L)
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


setGeneric("%k%", function(x, table) standardGeneric("%k%"))
#' Search in metadata keys
#'
#' Using a character vector as query, this method tests whether all given keys
#' are present in the top-level names of the metadata (these may be nested, but
#' all sublists are ignored here). An empty query vector results in
#' \code{TRUE}. Note that the values of the character vector, not its names, if
#' any, are used for querying the metadata.
#' Using a list as query, this method tests whether all given keys are present
#' in the names of the metadata.
#' This works like the character method, but because a query list is given, the
#' comparison of keys can be applied recursively (by using, of course, a nested
#' query list). This is based on \code{\link{contains}} with the \code{values}
#' argument set to \code{FALSE}.
#' The factor method first converts \code{x} to \sQuote{character} mode.
#'
#' @name %k%
#' @aliases infix.k
#' @rdname infix.k
#'
#' @param x Character vector, factor or list.
#' @param table \code{\link{WMD}} object.
#' @return Logical scalar.
#' @exportMethod "%k%"
#' @export
#~ @family getter-functions
#' @keywords attribute
#'
#' @examples
#'
#' # The dataset contains the metadata keys 'Species' and 'Experiment' but
#' # neither 'Trial' nor 'Organism' nor 'Run':
#' data(vaas_1)
#'
#' # Character method
#' stopifnot("Experiment" %k% vaas_1)
#' stopifnot("Species" %k% vaas_1)
#' stopifnot(!"Run" %k% vaas_1)
#' stopifnot(c("Species", "Experiment") %k% vaas_1)
#' stopifnot(!c("Species", "Trial") %k% vaas_1)
#' stopifnot(!c("Organism", "Experiment") %k% vaas_1)
#' stopifnot(character() %k% vaas_1)
#'
#' # List method
#' stopifnot(list(Experiment = "whatever") %k% vaas_1)
#' stopifnot(list(Species = "ignored") %k% vaas_1)
#'
#' # This fails because we query with a named sublist but 'Species' is not
#' # even a list
#' stopifnot(!list(Species = list(Genus = "X", Epithet = "Y")) %k% vaas_1)
#'
#' # This is OK because we query with an unnamed sublist: it has no names that
#' # one would fail to find
#' stopifnot(list(Species = list("X", "Y")) %k% vaas_1)
#'
#' # More non-nested query examples
#' stopifnot(!list(Run = 99) %k% vaas_1)
#' stopifnot(list(Species = "?", Experiment = NA) %k% vaas_1)
#' stopifnot(!list(Species = "?", Trial = NA) %k% vaas_1)
#' stopifnot(!list(Organism = "?", Experiment = NA) %k% vaas_1)
#' stopifnot(list() %k% vaas_1)
#'
setMethod("%k%", c("character", WMD), function(x, table) {
  all(x %in% names(table@metadata))
}, sealed = SEALED)

setMethod("%k%", c("list", WMD), function(x, table) {
  contains(table@metadata, x, values = FALSE)
}, sealed = SEALED)


################################################################################


setGeneric("%K%", function(x, table) standardGeneric("%K%"))
#' Search in metadata keys
#'
#' Using a character vector as query, this method tests whether a given key is
#' present in the metadata and fetches an object that is not \code{NULL}. If
#' the key has a length > 1, sublists are queried. An empty vector results in
#' \code{TRUE}. Note that the values of the character vector, not its names, if
#' any, are used for querying the metadata.
#' Using a list as query, this function behaves like \code{\link{infix.k}}.
#' The factor method first converts \code{x} to \sQuote{character} mode.
#'
#' @name %K%
#' @aliases infix.largek
#' @rdname infix.largek
#'
#' @param x Character vector, factor or list.
#' @param table \code{\link{WMD}} object.
#' @return Logical scalar.
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
#'
#' # Character method
#'
#' # Single-element queries
#' stopifnot("Experiment" %K% vaas_1)
#' stopifnot("Species" %K% vaas_1)
#' stopifnot(!"Run" %K% vaas_1)
#' stopifnot(!"Trial" %K% vaas_1)
#' stopifnot(!"Organism" %k% vaas_1)
#'
#' # Zero-element queries
#' stopifnot(character() %K% vaas_1)
#'
#' # Querying with vectors of length > 1 mean nested queries; compare this to
#' # the behavior of %k%!
#' stopifnot(!c("Species", "Experiment") %K% vaas_1)
#'
#' # List method
#' # See %k% -- the behavior is identical for lists.
#'
setMethod("%K%", c("character", WMD), function(x, table) {
  if (length(x) == 0L)
    return(TRUE) # for consistency with %k%
  tryCatch(!is.null(table@metadata[[x]]), error = function(e) FALSE)
}, sealed = SEALED)

setMethod("%K%", c("list", WMD), function(x, table) {
  contains(table@metadata, x, values = FALSE)
}, sealed = SEALED)


################################################################################


setGeneric("%q%", function(x, table) standardGeneric("%q%"))
#' Query metadata (non-exact version)
#'
#' Using a character vector, test whether all given query keys are present in
#' the top-level names of the metadata and refer to the same query elements.
#' Using a list, conduct a non-exact query with a query list.
#' The factor method first converts \code{x} to \sQuote{character} mode.
#'
#' @name %q%
#' @aliases infix.q
#' @rdname infix.q
#'
#' @param x Character vector, factor or list used as query.
#'   If a character vector, its \code{names} are used to select elements from
#'   the top level of the metadata. These elements are then converted to
#'   \sQuote{character} mode before comparison with the values of \code{x}. A
#'   non-empty vector without a \code{names} attribute is accepted but will
#'   always yield \code{FALSE}. In contrast, an entirely empty vector yields
#'   \code{TRUE}.
#'   If a list, the comparison is applied recursively using
#'   \code{\link{contains}} with the \code{values} argument set to \code{TRUE}
#'   but \code{exact} set to \code{FALSE}. The main advantage of using a list
#'   over the character-based search is that it allows one a nested query.
#' @param table \code{\link{WMD}} object.
#' @return Logical scalar.
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
#' stopifnot(c(Experiment = "First replicate") %q% vaas_1) # right query
#'
#' stopifnot(!"Species" %q% vaas_1)
#' stopifnot(!"Escherichia coli" %q% vaas_1)
#' stopifnot(c(Species = "Escherichia coli") %q% vaas_1)
#'
#' stopifnot(c(Species = "Escherichia coli",
#'   Experiment = "First replicate") %q% vaas_1) # Combined query
#'
#' stopifnot(character() %q% vaas_1) # Empty query
#'
#' # List method
#' stopifnot(list(Experiment = "First replicate") %q% vaas_1)
#'
#' # Choice among alternatives
#' stopifnot(list(Experiment = c("First replicate",
#'   "Second replicate")) %q% vaas_1)
#' stopifnot(!list(Experiment = c("Second replicate",
#'   "Third replicate")) %q% vaas_1)
#'
#' # Combined query together with choice among alternatives
#' stopifnot(list(Experiment = c("First replicate", "Second replicate"),
#'   Species = c("Escherichia coli", "Bacillus subtilis")) %q% vaas_1)
#'
#' stopifnot(list() %q% vaas_1) # Empty query
#'
setMethod("%q%", c("character", WMD), function(x, table) {
  if (length(keys <- names(x)) == 0L && length(x) > 0L)
    return(FALSE)
  # TODO: check behaviour -- unlist(lapply(...)) did not work!
  all(x == sapply(table@metadata[keys], as.character))
}, sealed = SEALED)

setMethod("%q%", c("list", WMD), function(x, table) {
  contains(table@metadata, x, values = TRUE, exact = FALSE)
}, sealed = SEALED)


################################################################################


setGeneric("%Q%", function(x, table) standardGeneric("%Q%"))
#' Query metadata (strict version)
#'
#' Using a character vector as query, test whether all given query keys are
#' present in the top-level names of the metadata and refer to the same query
#' elements (without coercion to character).
#' Using a list, conduct an exact query with this query list.
#' The factor method first converts \code{x} to \sQuote{character} mode.
#'
#' @name %Q%
#' @aliases infix.largeq
#' @rdname infix.largeq
#'
#' @param x Character vector, factor or list used as query.
#'   If a character vector, the result is identical to the
#'   one of \code{\link{infix.q}} except for the fact that metadata elements
#'   are not coerced to \sQuote{character} mode, making the query more strict.
#'   If a list, the comparison is applied recursively
#'   using \code{\link{contains}} with the arguments \code{values} and
#'   \code{exact} set to \code{TRUE}. This might be too strict for most
#'   applications. The main advantage of using a list over the character-based
#'   search is that it allows one a nested query.
#' @param table \code{\link{WMD}} object.
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
#'   Experiment = "First replicate") %Q% vaas_1)
#'
#' stopifnot(character() %Q% vaas_1) # Empty query
#'
#' # List method
#' stopifnot(list(Experiment = "First replicate") %Q% vaas_1)
#'
#' # Choice among alternatives is not done here: this query fails unless this
#' # two-element vector is contained. Compare to %q%.
#' stopifnot(!list(Experiment = c("First replicate",
#'   "Second replicate")) %Q% vaas_1)
#'
#' stopifnot(list() %Q% vaas_1) # Empty query
#'
setMethod("%Q%", c("character", WMD), function(x, table) {
  if (length(keys <- names(x)) == 0L && length(x) > 0L)
    return(FALSE)
  all(vapply(keys, function(key) identical(x[[key]], table@metadata[[key]]),
    logical(1L)))
}, sealed = SEALED)

setMethod("%Q%", c("list", WMD), function(x, table) {
  contains(table@metadata, x, values = TRUE, exact = TRUE)
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
    vapply(table@plates, func_, logical(1L), x = x, USE.NAMES = FALSE)
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
    vapply(table@plates, func_, logical(1L), x = x, USE.NAMES = FALSE)
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
    vapply(table@plates, func_, logical(1L), x = x, USE.NAMES = FALSE)
  }, sealed = SEALED)
})



################################################################################




