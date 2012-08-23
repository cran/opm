


################################################################################


#' Thin out the measurements
#'
#' Thin out some \code{\link{OPM}} measurements by keeping only each n-th time
#' point. A mainly experimental function that might be of use in testing.
#'
#' @param object \code{\link{OPM}} object.
#' @param factor Numeric scalar >= 1 indicating how much the dataset shall be
#'   thinned out.
#' @param drop Logical scalar. See \code{\link{[}}.
#' @param ... Optional arguments passed between the methods.
#' @export
#' @return \code{\link{OPM}} object.
#' @family conversion-functions
#' @note Thinning the plates out is experimental insofar as it has \strong{not}
#'   been tested whether and how this could sensibly be applied before
#'   aggregating the data.
#' @keywords manip
#'
#' @examples
#'
#' # 'OPM' method
#' data(vaas_1)
#' (x <- dim(vaas_1))
#' stopifnot(identical(x, c(384L, 96L)))
#' copy <- thin_out(vaas_1, 10) # keep every 10th time point and measurement
#' (x <- dim(copy))
#' stopifnot(identical(x, c(38L, 96L)), has_aggr(copy))
#' copy <- thin_out(vaas_1, 10, drop = TRUE) # also remove the parameters
#' (x <- dim(copy))
#' stopifnot(identical(x, c(38L, 96L)), !has_aggr(copy))
#'
#' # 'OPMS' method
#' data(vaas_4)
#' (x <- dim(vaas_4))
#' stopifnot(identical(x, c(4L, 384L, 96L)))
#' copy <- thin_out(vaas_4, 10)
#' (x <- dim(copy))
#' stopifnot(identical(x, c(4L, 38L, 96L)))
#'
setGeneric("thin_out", function(object, ...) standardGeneric("thin_out"))

setMethod("thin_out", OPM, function(object, factor, drop = FALSE) {
  if (L(factor) < 1)
    stop("'factor' must be >= 1")
  idx <- seq_len(dim(object)[1L])
  idx <- idx[idx %% factor == 0L]
  object[idx, , drop = drop]
}, sealed = SEALED)


################################################################################


#' Change to Generation III
#'
#' Change the plate type of an \code{\link{OPM}} object to
#' \sQuote{Generation III}. The actual spelling used might differ but is
#' internally consistent.
#'
#' @param object \code{\link{OPM}} object.
#' @param ... Optional arguments passed between the methods.
#' @return Novel \code{\link{OPM}} object.
#' @export
#' @note This is currently the only function to change plate names. It is
#'   intended for Generation-III plates which were run like PM plates. Usually
#'   they will be annotated as some PM plate by the OmniLog(R) system. In
#'   contrast, input ID-mode plates are automatically detected (see
#'   \code{\link{read_single_opm}}).
#' @keywords manip
#' @family conversion-functions
#' @examples
#'
#' # 'OPM' method
#' data(vaas_1)
#' summary(copy <- gen_iii(vaas_1))
#' stopifnot(identical(vaas_1, copy)) # the dataset already had that plate type
#'
#' # 'OPMS' method
#' data(vaas_4)
#' summary(copy <- gen_iii(vaas_4))
#' stopifnot(identical(vaas_4, copy)) # as above
#'
setGeneric("gen_iii", function(object, ...) standardGeneric("gen_iii"))

setMethod("gen_iii", OPM, function(object) {
  object@csv_data[[PLATE_TYPE]] <- GEN_III
  object
}, sealed = SEALED)


################################################################################
################################################################################
#
# Automatically generated conversion functions
#


# Based on OPM methods with function(object, ...) signature that return OPM(A)
# objects.
#
lapply(c(
    #+
    gen_iii,
    thin_out
    #-
  ), FUN = function(func_) {
  setMethod(func_, OPMS, function(object, ...) {
    new(OPMS, plates = lapply(object@plates, FUN = func_, ...))
  }, sealed = SEALED)
})



################################################################################
################################################################################
#
# Flattening functions
#


#' Flatten an OPM matrix or a list
#'
#' Convert into \sQuote{flat} data frame, including all measurements in a
#' single column (suitable, e.g., for \pkg{lattice}). The list method returns
#' a non-nested list.
#'
#' @param object \code{\link{OPM}} or \code{\link{OPMS}} object or list.
#' @param include \code{NULL}, character vector or list. If not \code{NULL},
#'   include this meta-information in the data frame, replicated in each row.
#'   Otherwise it converted to a list and passed to \code{\link{metadata}}. See
#'   there for details.
#' @param fixed \code{NULL} or list. If not \code{NULL}, include these items in
#'   the data frame, replicated in each row.
#' @param factors Logical scalar. See the \sQuote{stringsAsFactors} argument of
#'   \code{data.frame} and \code{as.data.frame}.
#' @param exact Logical scalar. Passed to \code{\link{metadata}}.
#' @param strict Logical scalar. Passed to \code{\link{metadata}}.
#' @param full Logical scalar. Replace well coordinates by full names?
#' @param ... Optional other arguments passed to \code{\link{wells}}, or from
#'   the \code{\link{OPMS}} to the \code{\link{OPM}} method.
#' @export
#' @return Dataframe. Column names are unchecked (not converted to variable
#'   names). The three last columns are: \sQuote{Time}, \sQuote{Well},
#'   \sQuote{Value}, with the obvious meanings. The \code{\link{OPMS}} method
#'   yields an additional column named \sQuote{Plate}, which contains each
#'   plate's number within \code{object}. The list method returns a list.
#' @family conversion-functions
#' @note The list method is based on 
#'   \url{http://stackoverflow.com/questions/8139677/how-to-flatten-a-list-to-a-list-without-coercion}
#'   with some slight improvements.
#' @keywords manip dplot
#' @seealso stats::reshape
#' @examples
#'
#' # OPM method
#' data(vaas_1)
#' x <- flatten(vaas_1)
#' stopifnot(is.data.frame(x), identical(dim(x), c(36864L, 3L)))
#' x <- flatten(vaas_1, fixed = "TEST")
#' stopifnot(is.data.frame(x), identical(dim(x), c(36864L, 4L)))
#' x <- flatten(vaas_1, fixed = "TEST", include = "Strain")
#' stopifnot(is.data.frame(x), identical(dim(x), c(36864L, 5L)))
#'
#' # OPMS method
#' data(vaas_4)
#' x <- flatten(vaas_4)
#' stopifnot(is.data.frame(x), identical(dim(x), c(147456L, 4L)))
#' x <- flatten(vaas_4, fixed = "TEST")
#' stopifnot(is.data.frame(x), identical(dim(x), c(147456L, 5L)))
#' x <- flatten(vaas_4, fixed = "TEST", include = "Strain")
#' stopifnot(is.data.frame(x), identical(dim(x), c(147456L, 6L)))
#'
#' # List method
#' x <- list(a = list(b = 1:5, c = letters[1:5]), d = LETTERS[1:3], 
#'   e = list(pi))
#' (y <- flatten(x))
#' stopifnot(is.list(y), length(y) == 4, !sapply(y, is.list))
#'
setGeneric("flatten", function(object, ...) standardGeneric("flatten"))

setMethod("flatten", OPM, function(object, include = NULL, fixed = NULL,
    factors = TRUE, exact = TRUE, strict = TRUE, full = TRUE, ...) {

  # Convert to flat data frame
  well.names <- wells(object, full = full, ...)
  use.reshape <- FALSE # the home-brewn solution was much faster
  if (use.reshape) {
    if (factors)
      well.names <- as.factor(well.names)
    result <- reshape(as.data.frame(object@measurements,
      stringsAsFactors = factors), direction = "long", idvar = "Hour",
      varying = wells(object), v.names = "Value", timevar = "Well",
      times = well.names)
    colnames(result)[1L] <- "Time"
  } else {
    times <- hours(object, "all")
    rep.times <- rep.int(times, length(well.names))
    rep.wells <- rep(well.names, each = length(times))
    result <- data.frame(Time = rep.times, Well = rep.wells,
      Value = as.vector(object@measurements[, -1L]), check.names = FALSE,
      stringsAsFactors = factors)
  }

  # Include fixed stuff
  if (length(fixed) > 0L)
    result <- cbind(as.data.frame(as.list(fixed), stringsAsFactors = factors),
      result)

  # Pick metadata and include them in the data frame
  if (length(include) > 0L) {
    result <- cbind(as.data.frame(metadata(object, as.list(include),
      exact = exact), stringsAsFactors = factors), result)
  }

  result

}, sealed = SEALED)

setMethod("flatten", OPMS, function(object, include = NULL, fixed = list(),
    ...) {
  plate.nums <- paste("Plate", seq_along(object@plates))
  do.call(rbind, mapply(FUN = function(plate, plate.num) {
    flatten(plate, include = include,
      fixed = c(list(Plate = plate.num), fixed), ...)
  }, object@plates, plate.nums, SIMPLIFY = FALSE))
}, sealed = SEALED)

setMethod("flatten", "list", function(object) {
  while(any(is.a.list <- vapply(object, is.list, logical(1L)))) {
    object[!is.a.list] <- lapply(object[!is.a.list], list)
    object <- unlist(object, recursive = FALSE)
  }
  object
}, sealed = SEALED)


################################################################################


#' Create data frame or vector from metadata
#'
#' Extract selected metadata entries for use as additional columns in a
#' data frame or (after joining) as character vector with labels. This is not
#' normally directly called by an \pkg{opm} user because
#' \code{\link{extract}} is available, which uses this function, but can be
#' used for testing the applied metadata selections beforehand. The data-frame
#' method is trivial: it extracts the selected columns and joins them to form a
#' character vector.
#'
#' @param object \code{\link{OPMS}} object or data frame.
#' @param what List of metadata keys to consider, or single such key; passed
#'   to \code{\link{metadata}}. For the data-frame method, just the names of
#'   the columns to extract, or their indices, as vector.
#' @param join Logical scalar. Join each row together to yield a character
#'   vector? Otherwise it is just attempted to construct a data frame.
#' @param sep Character scalar. Used as separator between the distinct metadata
#'   entries if these are to be pasted together. Ignored unless \code{join}
#'   is \code{TRUE}. The data-frame method always joins the data.
#' @param dups Character scalar specifying what to do in the case of duplicate
#'   labels: either \sQuote{warn}, \sQuote{error} or \sQuote{ignore}. Ignored
#'   unless \code{join} is \code{TRUE}.
#' @param exact Logical scalar. Also passed to \code{\link{metadata}}.
#' @param strict Logical scalar. Also passed to \code{\link{metadata}}.
#' @export
#' @return Data frame or character vector, depending on the \code{join}
#'   argument. The data-frame method always returns a character vector.
#' @family conversion-functions
#' @keywords dplot manip
#' @seealso base::data.frame base::as.data.frame base::cbind
#' @examples
#'
#' # 'OPMS' method
#' data(vaas_4)
#'
#' # Create data frame
#' (x <- extract_columns(vaas_4, what = list("Species", "Strain")))
#' stopifnot(is.data.frame(x), identical(dim(x), c(4L, 2L)))
#'
#' # Create a character vector
#' (x <- extract_columns(vaas_4, what = list("Species", "Strain"), join = TRUE))
#' stopifnot(is.character(x), length(x) == 4L)
#' (x <- try(extract_columns(vaas_4, what = list("Species"), join = TRUE,
#'    dups = "error"), silent = TRUE))
#' stopifnot(inherits(x, "try-error"))
#' (x <- try(extract_columns(vaas_4, what = list("Species"), join = TRUE,
#'   dups = "warn"), silent = TRUE))
#' stopifnot(is.character(x), length(x) == 4L)
#'
#' # data-frame method
#' x <- data.frame(a = 1:26, b = letters, c = LETTERS)
#' (y <- extract_columns(x, c("a", "b"), sep = "-"))
#' stopifnot(grepl("^\\s*\\d+-[a-z]$", y))
#'
setGeneric("extract_columns",
  function(object, ...) standardGeneric("extract_columns"))

setMethod("extract_columns", OPMS, function(object, what, join = FALSE,
    sep = " ", dups = c("warn", "error", "ignore"), exact = TRUE,
    strict = TRUE) {
  result <- metadata(object, as.list(what), exact = exact, strict = strict)
  result <- lapply(result, FUN = rapply, f = as.character)
  if (L(join)) {
    labels <- unlist(lapply(result, FUN = paste, collapse = sep))
    msg <- if (is.dup <- anyDuplicated(labels))
      paste("duplicated label:", labels[is.dup])
    else
      NULL
    if (!is.null(msg))
      case(match.arg(dups),
        ignore = NULL,
        warn = warning(msg),
        error = stop(msg)
      )
    labels
  } else
    must(as.data.frame(do.call(rbind, result)))
}, sealed = SEALED)

setMethod("extract_columns", "data.frame", function(object, what, sep = " ") {
  apply(object[, what, drop = FALSE], 1L, FUN = paste, collapse = sep)
}, sealed = SEALED)


################################################################################


#' Sort OPMS objects
#'
#' Sort an \code{\link{OPMS}} object based on one to several metadata or CSV
#' data entries. There is also an \code{\link{OPM}} method which returns the
#' input data (to avoid destructive effects due to the way the default
#' \code{sort} interacts with \code{\link{OPM}} indexing).
#'
#' @param x \code{\link{OPMS}} or \code{\link{OPM}} object.
#' @param decreasing Logical scalar. Passed to \code{order} from the \pkg{base}
#'   package.
#' @param by List or character vector. If a list, a list of one to several keys 
#'   passed as \code{key} argument to \code{\link{metadata}}. If a character 
#'   vector of length one, \code{by} must specify the name of one of the 
#'   functions \code{\link{setup_time}}, \code{\link{filename}} or 
#'   \code{\link{position}}. If longer, passed step-by-step to 
#'   \code{\link{csv_data}} as \code{keys} argument.
#' @param parse Logical scalar. Convert the \code{\link{setup_time}} via
#'   \code{strptime} before ordering? Has only an effect if \code{by} is
#'   \sQuote{setup_time}. It is an error if the time format is not recognized.
#' @param exact Logical scalar. Passed to \code{\link{metadata}}. Affects only
#'   metadata querying, not directly the sorting.
#' @param strict Logical scalar. Is it an error if metadata keys are not found?
#'   If \code{FALSE}, \code{x} gets ordered according to only the found keys,
#'   and remains in the original order if none of the keys in \code{by} are 
#'   found at all. Note that it is always an error if keys are found in the
#'   \code{\link{metadata}} of some of the\code{\link{plates}} but not in those
#'   of others.
#' @param na.last Logical scalar. Also passed to \code{order}.
#' @export
#' @return \code{\link{OPMS}} object with not necessarily the same order of
#'   plates than before, or \code{\link{OPM}} object.
#' @family conversion-functions
#' @keywords manip
#' @seealso base::order base::sort base::strptime
#' @examples
#'
#' ## 'OPMS' method
#' data(vaas_4)
#'
#' # Existing keys
#' stopifnot(is.unsorted(metadata(vaas_4, "Strain")))
#' x <- sort(vaas_4, by = list("Strain"))
#' stopifnot(is(x, "OPMS"), !is.unsorted(metadata(x, "Strain")))
#' x <- sort(vaas_4, by = list("Strain"), decreasing = TRUE)
#' stopifnot(is(x, "OPMS"), is.unsorted(metadata(x, "Strain")))
#'
#' # Non-existing keys
#' x <- try(sort(vaas_4, by = list("Not there", "Missing"), strict = TRUE))
#' stopifnot(inherits(x, "try-error"))
#' x <- try(sort(vaas_4, by = list("Not there", "Missing"), strict = FALSE))
#' stopifnot(identical(x, vaas_4))
#'
#' # CSV-data based
#' copy <- sort(vaas_4) # default: by setup time
#' setup_time(vaas_4)
#' setup_time(copy)
#' stopifnot(!identical(copy, vaas_4))
#' copy <- sort(vaas_4, by = c("Position", "Setup Time"))
#' position(vaas_4)
#' position(copy)
#' stopifnot(!is.unsorted(position(copy)), is.unsorted(position(vaas_4)))
#'
#' ## 'OPMS' method
#' data(vaas_1)
#' summary(x <- sort(vaas_1))
#' stopifnot(identical(x, vaas_1))
#'
setGeneric("sort")

setMethod("sort", OPM, function(x) {
  x
}, sealed = SEALED)

setMethod("sort", OPMS, function(x, decreasing = FALSE, by = "setup_time", 
    parse = TRUE, exact = TRUE, strict = TRUE, na.last = TRUE) {
  if (is.list(by)) {
    keys <- lapply(X = by, FUN = metadata, object = x, exact = exact, 
      strict = strict)
    if (!strict)
      if (!length(keys <- keys[!vapply(keys, is.null, logical(1L))]))
        return(x)
  } else if (is.character(by))
    keys <- case(length(by),
      stop("'by' must not be empty"),
      list(switch(by,
        setup_time = if (L(parse))
          must(parse_time(setup_time(x)))
        else
          setup_time(x),
        position = position(x),
        filename = filename(x),
        stop(sprintf("if a character scalar, 'by' must not be '%s'", by))
      )),
      lapply(X = by, FUN = csv_data, object = x)   
    )
  else
    stop("'by' must be a list or a character vector")
  keys <- insert(keys, decreasing = decreasing, na.last = na.last, 
    .force = TRUE)
  x@plates <- x@plates[do.call(base::order, keys)]
  x
}, sealed = SEALED)


################################################################################


#' Revert OPMS objects
#'
#' Revert the order of plates within an \code{\link{OPMS}} object. This should
#' be slightly more efficient than calling the default \code{rev} method. There
#' is also an \code{\link{OPM}} method which just returns the input data (to
#' avoid destructive effects due to the way the default \code{rev} interacts
#' with \code{\link{OPM}} indexing).
#'
#' @param x \code{\link{OPMS}} or \code{\link{OPM}} object.
#' @export
#' @return \code{\link{OPMS}} object with the reversed order of plates, or
#'   or \code{\link{OPM}} object.
#' @family conversion-functions
#' @keywords manip
#' @seealso base::rev
#' @examples
#'
#' ## 'OPMS' method
#' data(vaas_4)
#' summary(x <- rev(vaas_4))
#' stopifnot(dim(x) == dim(vaas_4), !identical(x, vaas_4))
#' stopifnot(identical(rev(x), vaas_4))
#'
#' ## 'OPMS' method
#' data(vaas_1)
#' summary(x <- rev(vaas_1))
#' stopifnot(identical(x, vaas_1))
#'
setGeneric("rev")

setMethod("rev", OPM, function(x) {
  x
}, sealed = SEALED)

setMethod("rev", OPMS, function(x) {
  x@plates <- x@plates[seq.int(length(x), 1L)]
  x
}, sealed = SEALED)


################################################################################


#' Repeat OPMS objects
#'
#' Repeat \code{\link{OPMS}} or \code{\link{OPM}} objects zero times, once, or
#' several times, and accordingly create a novel \code{\link{OPMS}} or 
#' \code{\link{OPM}} object (\code{NULL} if zero length is chosen).
#'
#' @param x \code{\link{OPMS}} or \code{\link{OPM}} object.
#' @param ... Optional parameters passed to \code{rep} from the \pkg{base}
#'   package.
#' @export
#' @return \code{\link{OPMS}} object with another number of plates, or
#'   or \code{\link{OPM}} object, or \code{NULL}.
#' @family conversion-functions
#' @keywords manip
#' @seealso base::rep
#' @examples
#'
#' ## 'OPMS' method
#' data(vaas_4)
#' summary(x <- rep(vaas_4))
#' stopifnot(identical(x, vaas_4))
#' summary(x <- rep(vaas_4, times = 2))
#' stopifnot(length(x) == length(vaas_4) * 2)
#' summary(y <- rep(vaas_4, each = 2))
#' stopifnot(length(y) == length(vaas_4) * 2, !identical(x, y))
#' stopifnot(is.null(rep(vaas_4, 0)))
#'
#' ## 'OPM' method
#' data(vaas_1)
#' summary(x <- rep(vaas_1, 1))
#' stopifnot(identical(x, vaas_1))
#' summary(x <- rep(vaas_1, 2))
#' stopifnot(length(x) == 2, is(x, "OPMS"))
#' stopifnot(is.null(rep(vaas_4, 0)))
#'
setGeneric("rep")

setMethod("rep", OPM, function(x, ...) {
  x <- rep(x = list(x), ...)
  case(length(x), NULL, x[[1L]], new(OPMS, plates = x))
}, sealed = SEALED)

setMethod("rep", OPMS, function(x, ...) {
  x <- rep(x = x@plates, ...)
  case(length(x), NULL, x[[1L]], new(OPMS, plates = x))
}, sealed = SEALED)


################################################################################
################################################################################
#
# Extraction of character matrices
#


#' Extract aggregated values
#'
#' Extract selected aggregated values into common matrix or data frame. The
#' data-frame method creates a matrix by extracting the numeric columns and
#' optionally adds row names.
#'
#' @param object \code{\link{OPMS}} object or data frame.
#' @param as.labels List. Metadata to be joined and used as row names (if
#'   \code{dataframe} is \code{FALSE}) or additional columns (if otherwise).
#'   Ignored if \code{NULL}.
#'
#' @param subset Character vector. The parameter(s) to put in the matrix.
#' @param ci Logical scalar. Also return the CIs?
#' @param trim Character scalar. See \code{\link{aggregated}} for details.
#' @param dataframe Logical scalar. Return data frame or matrix?
#'
#' @param as.groups List. Metadata to be joined and used as \sQuote{row.groups}
#'   attribute of the output matrix. See \code{\link{heat_map}} for its usage.
#'   Ignored if \code{NULL} and if \code{dataframe} is \code{FALSE}. For the
#'   data-frame method, a vector.
#' @param sep Character scalar. See \code{\link{extract_columns}}.
#' @param dups Character scalar. See \code{\link{extract_columns}}.
#'
#' @param exact Logical scalar. See \code{\link{extract_columns}}.
#' @param strict Logical scalar. See \code{\link{extract_columns}}.
#'
#' @param full Logical scalar indicating whether full substrate names shall
#'   be used. This is passed to \code{\link{wells}}, but in contrast to what
#'   \code{\link{flatten}} is doing the argument here refers to the generation
#'   of the column names.
#' @param max Numeric scalar. Passed to \code{\link{wells}}.
#' @param ... Optional other arguments passed to \code{\link{wells}}.
#'
#' @param what Character scalar. The name of the class to extract from the
#'   data frame to form the matrix values.
#'
#' @export
#' @return Numeric matrix or data frame; always a numeric matrix for the
#'   data-frame method.
#' @family conversion-functions
#' @seealso base::data.frame base::as.data.frame base::matrix base::as.matrix
#' @keywords manip dplot
#' @examples
#'
#' # 'OPMS' method
#' data(vaas_4)
#' # Matrix
#' (x <- extract(vaas_4, as.labels = list("Species", "Strain")))
#' stopifnot(is.matrix(x), identical(dim(x), c(4L, 96L)))
#' # Data frame
#' (x <- extract(vaas_4, as.labels = list("Species", "Strain"),
#'   dataframe = TRUE))
#' stopifnot(is.data.frame(x), identical(dim(x), c(4L, 99L)))
#' # All parameters in a single data frame
#' x <- lapply(param_names(), function(name) extract(vaas_4, subset = name,
#'   as.labels = list("Species", "Strain"), dataframe = TRUE))
#' x <- do.call(rbind, x)
#'
#' # data-frame method
#' x <- data.frame(a = 1:26, b = letters, c = LETTERS)
#' (y <- extract(x, as.labels = "b", what = "integer", as.groups = "c"))
#' stopifnot(is.matrix(y), dim(y) == c(26, 1), rownames(y) == x$b)
#' stopifnot(identical(attr(y, "row.groups"), x$c))
#'
setGeneric("extract", function(object, ...) standardGeneric("extract"))

setMethod("extract", OPMS, function(object, as.labels, subset = "A",
    ci = FALSE, trim = "full", dataframe = FALSE, as.groups = NULL, sep = " ",
    dups = "warn", exact = TRUE, strict = TRUE, full = TRUE, max = 10000L,
    ...) {

  do_extract <- function(what, join, dups = "ignore") {
    extract_columns(object, what = what, join = join, sep = sep, dups = dups,
      exact = exact, strict = strict)
  }

  # Collect parameters in a matrix
  subset <- match.arg(subset, unlist(map_grofit_names(plain = TRUE)))
  if (!all(has_aggr(object)))
    stop("all plates need aggregated data")
  result <- do.call(rbind, lapply(object@plates, FUN = aggregated,
    subset = subset, ci = ci, trim = trim))
  colnames(result) <- wells(object, full = full, max = max, ...)

  if (dataframe) {

    result <- as.data.frame(result)
    if (length(as.labels)) {
      columns <- do_extract(as.labels, join = FALSE)
      if (ci)
        columns <- columns[rep(seq.int(nrow(columns)), each = 3L), ,
          drop = FALSE]
      columns <- cbind(columns, Parameter = rownames(result))
      rownames(result) <- rownames(columns) # otherwise a warning is likely
      result <- cbind(columns, result)
    } else {
      params <- rownames(result)
      rownames(result) <- seq.int(nrow(result))
      result <- cbind(Parameter = params, result)
    }
    if (length(as.groups)) {
      to.add <- do_extract(as.groups, join = FALSE)
      if (ci)
        to.add <- to.add[rep(seq.int(nrow(to.add)), each = 3L), , drop = FALSE]
      result <- cbind(result, to.add)
    }

  } else {

    if (length(as.labels)) {
      labels <- do_extract(as.labels, join = TRUE, dups = dups)
      rownames(result) <- if (ci)
        paste(rep(labels, each = 3L), rownames(result))
      else
        labels
    } else {
      rownames(result) <- if (ci)
        paste(rownames(result), rep(seq.int(nrow(result) / 3L), each = 3L),
          sep = sep)
      else
        seq.int(nrow(result))
    }
    if (length(as.groups)) {
      rg <- "row.groups"
      attr(result, rg) <- as.factor(do_extract(as.groups, join = TRUE))
      if (ci)
        attr(result, rg) <- rep(attr(result, rg), each = 3L)
    }
  }

  result

}, sealed = SEALED)

setMethod("extract", "data.frame", function(object, as.labels, 
    as.groups = NULL, sep = " ", what = "numeric") {
  find_stuff <- function(x, what) {
    x <- select(x, query = what)
    if (ncol(x) == 0L)
      stop(sprintf("no data of class '%s' found", what))
    as.matrix(x)
  }
  result <- find_stuff(object, L(what))
  if (length(as.labels))
    rownames(result) <- extract_columns(object, what = as.labels, sep = sep)
  if (length(as.groups))
    attr(result, "row.groups") <- as.factor(extract_columns(object,
      what = as.groups, sep = sep))
  result
}, sealed = SEALED)


################################################################################


#' Merge plates
#'
#' Combine all plates in a single \code{\link{OPM}} object by treating them as
#' originating from subsequent runs of the same experimental plate. Adjust the
#' times accordingly.
#'
#' @param x \code{\link{OPMS}} object.
#' @param y Numeric vector indicating the time(s) (in hours) between two
#'   subsequent plates. Must be positive throughout, and its length should
#'   fit to the number of plates (e.g., either \code{1} or \code{length(x) - 1}
#'   would work). If missing, \code{0.25} is used.
#' @param sort.first Logical scalar. Sort the plates according to their setup
#'   times before merging? 
#' @param parse Logical scalar. Ignored unless \code{sort.first} is 
#'   \code{TRUE}. For sorting, parse the setup times using \code{strptime} from 
#'   the \pkg{base} package? It is an error if this does not work.
#' @export
#' @return \code{\link{OPM}} object. The \code{\link{metadata}} and
#'   \code{\link{csv_data}} will be taken from the first contained plate, but
#'   aggregated values, if any, will be dropped.
#' @note This function is intended for dealing with slowly growing or reacting 
#'   organisms that need to be analyzed with subsequent runs of the same plate
#'   in PM mode. Results obtained with \emph{Geodermatophilus} strains and
#'   Generation-III plates indicate that this works well in practice.
#' @family conversion-functions
#' @keywords manip
#' @examples
#' data(vaas_4) # merge() is biologically unreasonable for these data!
#' summary(x <- merge(vaas_4))
#' stopifnot(is(x, "OPM"), dim(x) == c(sum(hours(vaas_4, "size")), 96))
#'
setGeneric("merge")

setMethod("merge", c(OPMS, "numeric"), function(x, y, sort.first = TRUE, 
    parse = TRUE) {
  if (any(y <= 0))
    stop("'y' must be positive throughout")
  if (L(sort.first))
    x <- sort(x, by = "setup_time", parse = parse, na.last = TRUE)
  m <- do.call(rbind, measurements(x))
  if (is.matrix(tp <- hours(x, what = "all"))) {
    to.add <- c(0, must(cumsum(tp[-nrow(tp), ncol(tp), drop = FALSE]) + y))
    m[, 1L] <- as.vector(t(tp + to.add))
  } else if (is.list(tp)) {
    to.add <- c(0, must(cumsum(vapply(tp[-length(tp)], last, numeric(1L))) + y))
    m[, 1L] <- unlist(mapply(`+`, tp, to.add, SIMPLIFY = FALSE,
      USE.NAMES = FALSE))
  } else
    stop(BUG_MSG)
  new(OPM, measurements = m, csv_data = csv_data(x[1L]),
    metadata = metadata(x[1L]))
}, sealed = SEALED)

setMethod("merge", c(OPMS, "missing"), function(x, sort.first = TRUE, 
    parse = TRUE) {
  merge(x, 0.25, sort.first = sort.first, parse = parse)
}, sealed = SEALED)


################################################################################


#' Get available plates
#'
#' Get all plates contained in an \code{\link{OPMS}} object or a list, or
#' create a list containing a single \code{\link{OPM}} object as element. The
#' list method traverses the input recursively and skips all objects of other
#' classes than \code{\link{OPM}} (see also \code{\link{opms}}, which is
#' somewhat similar but more flexible).
#'
#' @param object List, \code{\link{OPM}} or \code{\link{OPMS}} object.
#' @return List of \code{\link{OPM}} objects (may be empty instead if 
#'   \code{object} is a list).
#' @export
#' @family conversion-functions
#' @keywords attribute
#' @seealso base::list base::as.list
#' @examples
#'
#' # 'OPM' method
#' data(vaas_1)
#' summary(x <- plates(vaas_1))
#' stopifnot(is.list(x), length(x) == 1L, sapply(x, inherits, what = "OPM"))
#'
#' # 'OPMS' method
#' data(vaas_4)
#' summary(x <- plates(vaas_4))
#' stopifnot(is.list(x), length(x) == 4L, sapply(x, inherits, what = "OPM"))
#'
#' # list method
#' x <- list(vaas_1, letters, vaas_4, 1:10)
#' summary(x <- plates(x))
#' stopifnot(is.list(x), length(x) == 5, sapply(x, inherits, what = "OPM"))
#'
setGeneric("plates", function(object, ...) standardGeneric("plates"))

setMethod("plates", OPMS, function(object) {
  object@plates
}, sealed = SEALED)

setMethod("plates", OPM, function(object) {
  list(object)
}, sealed = SEALED)

setMethod("plates", "list", function(object) {
  to_opm_list(object, precomputed = TRUE, skip = TRUE, group = FALSE)
}, sealed = SEALED)


################################################################################


#' Apply method for OPMS objects
#'
#' Apply a function to all \code{\link{OPM}} or \code{\link{OPMA}} objects
#' within an \code{\link{OPMS}} object. Optionally simplify the result to an
#' \code{\link{OPMS}} object if possible, or other structures simpler than a
#' list.
#'
#' @param object \code{\link{OPMS}} object.
#' @param fun A function. Should expect an  \code{\link{OPM}} (or
#'   \code{\link{OPMA}}) object as first argument.
#' @param ... Optional other arguments passed to \code{fun}.
#' @param simplify Logical scalar. If \code{FALSE}, the result is a list. If
#'   \code{TRUE}, it is attempted to simplify the result to a vector or matrix
#'   or to an \code{\link{OPMS}} object (if the result is a list of
#'   \code{\link{OPM}} or \code{\link{OPMA}} objects). If this is impossible,
#'   a list is returned.
#' @export
#' @return List, vector, matrix or \code{\link{OPMS}} object.
#' @family conversion-functions
#' @keywords manip
#' @seealso base::sapply
#' @examples
#' data(vaas_4)
#' x <- oapply(vaas_4, identity)
#' stopifnot(identical(x, vaas_4))
#' x <- oapply(vaas_4, identity, simplify = FALSE)
#' stopifnot(is.list(x), length(x) == 4, sapply(x, class) == "OPMA")
#'
setGeneric("oapply", function(object, ...) standardGeneric("oapply"))

setMethod("oapply", OPMS, function(object, fun, ..., simplify = TRUE) {
  result <- sapply(X = object@plates, FUN = fun, ..., simplify = simplify,
    USE.NAMES = FALSE)
  if (simplify && is.list(result))
    result <- try_opms(result)
  result
}, sealed = SEALED)


################################################################################
################################################################################
#
# YAML functions
#


#' Convert to YAML
#'
#' Convert some R object to YAML. If the package \pkg{yaml} is not installed, a
#' call of this function will result in an error.
#'
#' @param object Object of one of the classes belonging to
#'   \code{\link{YAML_VIA_LIST-class}}.
#' @param sep Logical scalar. Prepend YAML document separator \verb{---}?
#' @param line.sep Character scalar used as output line separator.
#' @param ... Optional other arguments passed to \code{as.yaml} from the
#'   \pkg{yaml} package.
#' @export
#' @return Character scalar (YAML string).
#' @family conversion-functions
#' @keywords character IO
#' @references \url{http://www.yaml.org/}
#' @note Many PM datasets can be batch-converted into YAML format using
#'   \code{\link{batch_opm_to_yaml}}. The output format for the child classes
#'   is described in detail there.
#' @seealso yaml::as.yaml yaml::yaml.load_file
#'
#' @examples \dontrun{
#'
#' # Let 'x' be a any convertible object
#' # Store the data in file 'out.yml' in YAML format.
#' write(to_yaml(x), "out.yml")
#' }
#'
setGeneric("to_yaml", function(object, ...) standardGeneric("to_yaml"))

setMethod("to_yaml", YAML_VIA_LIST, function(object, sep = TRUE,
    line.sep = "\n", ...) {
  LL(sep, line.sep)
  if (!require("yaml", quietly = TRUE, warn.conflicts = FALSE))
    stop("package 'yaml' is not available")
  result <- yaml::as.yaml(x = as(object, "list"), line.sep = line.sep, ...)
  if (sep)
    result <- sprintf(sprintf("---%s%%s%s", line.sep, line.sep), result)
  result
}, sealed = SEALED)



################################################################################




