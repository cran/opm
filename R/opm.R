

################################################################################
#
# OPM class: object construction functions
#


setGeneric("opm_problems",
  function(object, ...) standardGeneric("opm_problems"))
#' Check OPM matrix
#'
#' Check whether a matrix fulfils the requirements for \code{\link{OPM}}
#' measurements. Called when constructing an object of the class.
#'
#' @param object Matrix.
#' @return Character vector with description of problems, empty if there
#'   are none.
#' @keywords internal
#'
setMethod("opm_problems", "matrix", function(object) {

  errs <- character()

  # Check content
  if (any(is.na(object)))
    errs <- c(errs, "matrix contains NAs")
  if (!is.numeric(object))
    errs <- c(errs, "matrix is not numeric")

  # Check row names
  if (!is.null(rownames(object)))
    errs <- c(errs, "non-empty row names")

  # Check column names
  col.names <- colnames(object)
  pattern <- sprintf("^([A-H][01]\\d|%s)$", HOUR)
  bad <- grep(pattern, col.names, invert = TRUE, value = TRUE)
  if (length(bad))
    errs <- c(errs, paste("invalid entry in header:", bad[1L]))
  bad <- which(duplicated(col.names))
  if (length(bad))
    errs <- c(errs, paste("duplicated entry in header:", col.names[bad[1L]]))
  if (col.names[1L] != HOUR)
    errs <- c(errs, paste("first entry in header must be", HOUR))
  if (is.unsorted(col.names[-1L]))
    errs <- c(errs, "names of wells must be sorted")

  errs
}, sealed = SEALED)


#' Check OPM CSV data
#'
#' Check whether a character vector fulfils the requirements for 
#' \code{\link{OPM}} CSV data. Called when constructing an object of the class.
#'
#' @name opm_problems,character
#'
#' @param object Character vector.
#' @return Character vector with description of problems, empty if there are
#'   none.
#' @keywords internal
#'
setMethod("opm_problems", "character", function(object) {
  errs <- character()
  wanted <- c(FILE, PLATE_TYPE, POS, SETUP)
  missing <- !wanted %in% names(object)
  if (any(missing))
    errs <- c(errs, sprintf("need '%s' in CSV data", wanted[missing]))
  errs
}, sealed = SEALED)


#' OPM class
#'
#' Class for holding single-plate OmniLog(R) phenotype microarray data without
#' aggregated values, but with information read from the original input CSV
#' files as well as an additional arbitrary amount of arbitrarily organized 
#' metadata. \sQuote{OPM} is an acronym for \sQuote{OmniLog(R) Phenotype 
#' Microarray}.
#'
#' @docType class
#'
#' @note Regarding the coercion of this class to other classes (see the 
#'   \code{coerce} methods listed above and \code{as} from the \pkg{methods}
#'   package), consider the following:
#'   \itemize{
#'     \item The coercion of this class (and its child classes) to a list (and
#'       vice versa) relies on a mapping between slot names and keys in the 
#'       list, i.e. the list must be appropriately named. For instance, this is 
#'       the mechanism when reading from and writing to YAML, see 
#'       \code{\link{to_yaml}}.
#'     \item Coercions to other dataframes and matrices first coerce the 
#'       \code{\link{measurements}} and then add the other slots as attributes.
#'     \item Methods such as \code{\link{flatten}} might be more 
#'        appropriate for converting \code{\link{OPM}} objects.
#'   }  
#' @export
#' @seealso Methods
#' @family classes
#' @keywords methods
#'
setClass(OPM,
  representation = representation(
    measurements = "matrix",
    csv_data = "character"
  ),
  contains = WMD,
  validity = function(object) {
    errs <- c(opm_problems(object@measurements), opm_problems(object@csv_data))
    if (length(errs) == 0L)
      TRUE
    else
      errs
  },
  sealed = SEALED
)


#' Initialize
#'
#' Initialize method for the \code{\link{OPM}} class.
#'
#' @param .Object \code{\link{OPM}} object.
#' @param ... Additional arguments.
#' @return \code{\link{OPM}} object.
#' @keywords internal
#'
setMethod("initialize", OPM, function(.Object, ...) {
  .Object <- callNextMethod()
  .Object@csv_data[PLATE_TYPE] <- normalize_plate_name(
    .Object@csv_data[PLATE_TYPE])
  .Object
}, sealed = SEALED)


################################################################################
#
# Getter functions for the measurements
#


setGeneric("measurements",
  function(object, ...) standardGeneric("measurements"))
#' Stored measurements
#'
#' Return the measurements. The first column contains the hours, the other 
#' ones contain the values from each well. There is one row per time point.
#' Column names are appropriately set, but not translated (as, e.g., to 
#' substrate names). It is possible to select wells, but the time points are
#' always included as first column (in contrast to \code{\link{well}}). The
#' \code{i} argument refers only to the remaining matrix.
#'
#' @param object \code{\link{OPM}} object.
#' @param i Optional character or numeric vector with name(s) or position(s) 
#'   of well(s).
#' @return Numeric matrix with column names indicating the well coordinate
#'   and a first column containing the time points.
#' @export
#' @family getter-functions
#' @keywords attribute
#' @examples
#' data(vaas_1)
#' x <- measurements(vaas_1)
#' stopifnot(is.matrix(x), is.numeric(x))
#' stopifnot(identical(dim(x), c(384L, 97L)))
#' y <- measurements(vaas_1, "B03")
#' stopifnot(is.matrix(y), is.numeric(y))
#' stopifnot(identical(dim(y), c(384L, 2L)))
#'
setMethod("measurements", OPM, function(object, i) {
  if (missing(i))
    object@measurements
  else
    cbind(object@measurements[, 1L, drop = FALSE],
      object@measurements[, -1L, drop = FALSE][, i, drop = FALSE])
}, sealed = SEALED)


## NOTE: "[" is a primitive and needs no setGeneric().

#' Select subset
#'
#' Select a subset of the measurements. Return other slots unchanged. In
#' contrast to the usual `[` functions, always return a matrix (as a component
#' of the returned OPM object), even if it could be simplified to a vector.
#' The time column is not counted and always copied. It is an error to delete
#' the entire matrix. In all other respects, this method behaves like the `[`
#' functions from the \pkg{base} package.
#'
#' @rdname bracket
#' @exportMethod "["
#'
#' @param x \code{\link{OPM}} object.
#' @param i Row(s). Vector or missing.
#' @param j Columns(s). Vector or missing.
#' @param ... Should currently not be set.
#' @param drop Logical scalar. Has no effect here, but see \code{\link{[,OPMA}}.
#' @family getter-functions
#' @return \code{\link{OPM}} object.
#' @seealso [ [[
#' @keywords manip
#'
#' @examples
#' data(vaas_1)                                          
#' (x <- dim(vaas_1))
#' stopifnot(identical(x, c(384L, 96L)))
#' copy <- vaas_1[, 11:22]
#' (x <- dim(copy))                                       
#' stopifnot(identical(x, c(384L, 12L)))                                          
#'
setMethod("[", OPM, function(x, i, j, ..., drop = FALSE) {
  mat <- x@measurements[, -1L, ..., drop = FALSE][i, j, ..., drop = FALSE]
  if (any(dim(mat) == 0L))
    stop("selection resulted in empty matrix")
  mat <- cbind(x@measurements[i, 1L, ..., drop = FALSE], mat)
  names(dimnames(mat)) <- names(dimnames(x@measurements))
  result <- x
  result@measurements <- mat
  result
}, sealed = SEALED)


setGeneric("thin_out", function(object, ...) standardGeneric("thin_out"))
#' Thin out the measurements
#'
#' Thin out some \code{\link{OPM}} measurements by keeping only each n-th time 
#' point. A mainly experimental function that might be of use in testing.
#'
#' @param object \code{\link{OPM}} object.
#' @param factor Numeric scalar >= 1 indicating how much the dataset shall be
#'   thinned out.
#' @param drop Logical scalar. See \code{\link{[}} and \code{\link{[,OPMA}}.
#' @export
#' @return \code{\link{OPM}} object.
#' @family getter-functions
#' @note Thinning the plates out is experimental insofar as it has \strong{not}
#'   been tested whether and how this could sensibly be applied before 
#'   aggregating the data.
#' @keywords manip
#'
#' @examples
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
setMethod("thin_out", OPM, function(object, factor, drop = FALSE) {
  assert_length(factor)
  if (factor < 1)
    stop("'factor' must be >= 1")
  idx <- seq_len(dim(object)[1L])
  idx <- idx[idx %% factor == 0L]
  object[idx, , drop = drop]
}, sealed = SEALED)


setGeneric("well", function(object, ...) standardGeneric("well"))
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
#' @return Numeric matrix or vector (depending on \code{i} and \code{drop}).
#' @export
#' @family getter-functions
#' @keywords attribute
#' @note Do not confuse this with \code{\link{wells}}.
#' @examples
#' data(vaas_1)                                          
#' (x <- well(vaas_1, "B04"))
#' stopifnot(is.numeric(x), length(x) == 384L)
#' (x <- well(vaas_1, c("B08", "C07")))
#' stopifnot(is.matrix(x), identical(dim(x), c(384L, 2L)))
#'
setMethod("well", OPM, function(object, i, drop = TRUE) {
  object@measurements[, -1L, drop = FALSE][, i, drop = drop]
}, sealed = SEALED)


setGeneric("hours", function(object, ...) standardGeneric("hours"))
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
#' @return Dependent on the \code{what} argument; see there.
#' @export
#' @family getter-functions
#' @keywords attribute
#' @examples
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
setMethod("hours", OPM, function(object, 
    what = c("max", "all", "size", "summary", "interval", "minmax")) {
  tp <- object@measurements[, HOUR]
  switch(match.arg(what),
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
    summary = summary(tp),
    stop(BUG_MSG)
  )
}, sealed = SEALED)


## NOTE: 'max' is part of the S4 summary group generic and needs no
## setGeneric().


#' Maximum
#'
#' Get the maximal value of all wells or (a) specified one(s).
#'
#' @param x \code{\link{OPM}} object.
#' @param ... Coordinate of one to several wells. If missing, the maximum of 
#'   all wells is returned. See \code{\link{well}} or details.
#' @param na.rm Logical scalar. See \code{max} from the \pkg{base} package. Has
#'   no effect here because \code{NA} values are not allowed within the
#'   measurements.
#' @return Numeric scalar.
#' @export
#' @seealso max
#' @family getter-functions
#' @keywords attribute dplot
#' @examples
#' data(vaas_1)                                          
#' (x <- max(vaas_1))
#' (y <- max(vaas_1, 1)) # this is the negative control
#' stopifnot(x > y)
#'
setMethod("max", OPM, function(x, ..., na.rm = FALSE) {
  if (missing(...))
    max(x@measurements[, -1L, drop = FALSE], na.rm = na.rm)
  else
    max(well(x, ...), na.rm = na.rm)
}, sealed = SEALED)


setGeneric("minmax", function(x, ...) standardGeneric("minmax"))
#' Smallest maximum
#'
#' Get the smallest maximum among all wells.
#'
#' @param x \code{\link{OPM}} object.
#' @param ... Coordinate of one to several wells. If missing, the smallest
#'   maximum of all wells is returned. See \code{\link{well}} for details. If
#'   only as single well is selected, the result is actually identical to the
#'   one of \code{\link{max}}.
#' @param na.rm Logical scalar. See \code{\link{max}}.
#' @return Numeric scalar.
#' @export
#' @seealso min max
#' @family getter-functions
#' @keywords attribute dplot
#' @examples
#' data(vaas_1)                                          
#' (x <- max(vaas_1))
#' (y <- minmax(vaas_1))
#' stopifnot(x > y)
#'
setMethod("minmax", OPM, function(x, ..., na.rm = FALSE) {
  min(apply(x@measurements[, -1L, drop = FALSE][, ..., drop = FALSE], 2L, 
    FUN = max, na.rm = na.rm))
}, sealed = SEALED)


## NOTE: 'dim' is a primitive and needs no setGeneric().

#' Dimensions
#'
#' Get the dimensions of the measurements.
#'
#' @param x \code{\link{OPM}} object.
#' @return Two-element numeric vector (number of time points and number of
#'   wells).
#' @export
#' @family getter-functions
#' @keywords attribute
#' @seealso dim
#' @examples
#' data(vaas_1)                                          
#' (x <- dim(vaas_1))
#' stopifnot(identical(x, c(384L, 96L)))
#'
setMethod("dim", OPM, function(x) {
  dim(measurements(x)[, -1L, drop = FALSE])
}, sealed = SEALED)


setGeneric("wells", function(object, ...) standardGeneric("wells"))
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
#' @param word.wise Logical scalar. If \code{TRUE}, abbrevation works by 
#'   truncating each word separately, and removing vowels first.
#' @param paren.sep Character scalar. What to insert before the opening
#'   parenthesis (or bracket).
#' @return Character vector.
#' @export
#' @family getter-functions
#' @seealso strtrim abbreviate
#' @keywords attribute
#' @note Do not confuse this with \code{\link{well}}.
#' @examples
#' data(vaas_1)                                          
#' (x <- wells(vaas_1, full = FALSE))
#' (y <- wells(vaas_1, full = TRUE))
#' (z <- wells(vaas_1, full = TRUE, in.parens = FALSE))
#' stopifnot(nchar(x) < nchar(y), nchar(z) < nchar(y))
#'
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
#
# Getter and setter functions for the CSV data
#


setGeneric("csv_data", function(object, ...) standardGeneric("csv_data"))
#' Information from input CSV file
#'
#' Information about the plate as originally read from the input CSV file. See
#' \code{\link{read_opm}} and \code{\link{read_single_opm}} for reading such 
#' files.
#'
#' @param object \code{\link{OPM}} object.
#' @param keys Character vector (or other objects useable as vector index). An
#'   optional subselection. If empty (the default), all CSV data are returned.
#'   It is an error to select non-existing items.
#' @return Named character vector.
#' @export
#' @family getter-functions
#' @keywords attribute
#' @examples
#' data(vaas_1)
#' # compare this to setup_time()
#' (x <- csv_data(vaas_1, "Setup Time"))
#' stopifnot(identical(x, c(`Setup Time` = "8/30/2010 1:53:08 PM")))
#'
setMethod("csv_data", OPM, function(object, keys = character()) {
  if (length(keys) == 0L)
    return(object@csv_data)
  result <- object@csv_data[keys]
  missing <- is.na(result)
  if (any(missing))
    stop("could not find key ", keys[missing][1L])
  result
}, sealed = SEALED)


setGeneric("filename", function(object, ...) standardGeneric("filename"))
#' Original input filename
#'
#' Get the name of the original CSV input file. This is a convenience function
#' for one of the more important entries of \code{\link{csv_data}}.
#'
#' @param object \code{\link{OPM}} object.
#' @return Character scalar.
#' @export
#' @family getter-functions
#' @keywords attribute
#' @examples
#' data(vaas_1)                                          
#' (x <- filename(vaas_1))
#' stopifnot(is.character(x), length(x) == 1L)
#'
setMethod("filename", OPM, function(object) {
  object@csv_data[[FILE]]
}, sealed = SEALED)


setGeneric("plate_type", function(object, ...) standardGeneric("plate_type"))
#' Plate type used
#'
#' Get the type of the OmniLog(R) plate used in the measuring. This is a 
#' convenience function for one of the more important entries of 
#' \code{\link{csv_data}} with additional options useful for creating plot
#' titles.
#'
#' @param object \code{\link{OPM}} object.
#' @param full Logical scalar. If \code{TRUE}, add (or replace by) the full 
#'   name of the plate type (if available); otherwise, return it as-is.
#' @param in.parens Logical scalar. This and the following argument work like
#'   the eponymous ones of \code{\link{wells}}, but here are applied to the 
#'   plate name.
#' @param max Numeric scalar.
#' @param clean Logical scalar.
#' @param brackets Logical scalar.
#' @param word.wise Logical scalar.
#' @param paren.sep Character scalar.
#' @return Character scalar.
#' @export
#' @family getter-functions
#' @seealso strtrim abbreviate
#' @keywords attribute
#' @examples
#' data(vaas_1)
#' (x <- plate_type(vaas_1, full = FALSE))
#' (y <- plate_type(vaas_1, full = TRUE))
#' (z <- plate_type(vaas_1, full = TRUE, in.parens = FALSE))
#' stopifnot(nchar(x) < nchar(y), nchar(z) < nchar(y))
#'
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


setGeneric("setup_time", function(object, ...) standardGeneric("setup_time"))
#' Setup time of the measuring
#'
#' Get the setup time of the PM experiment as recorded by the OmniLog(R) 
#' device. This is a convenience function for one of the more important entries  
#' of \code{\link{csv_data}}
#'
#' @param object \code{\link{OPM}} object.
#' @return Character scalar.
#' @export
#' @family getter-functions
#' @keywords attribute
#' @examples
#' data(vaas_1)
#' (x <- setup_time(vaas_1))
#' # WARNING: It is unlikely that all OmniLog output has this setup time format
#' (parsed <- strptime(x, format = "%m/%d/%Y %I:%M:%S %p"))
#' stopifnot(is(parsed, "POSIXlt"))
#'
setMethod("setup_time", OPM, function(object) {
  object@csv_data[[SETUP]]
}, sealed = SEALED)


setGeneric("position", function(object, ...) standardGeneric("position"))
#' Position of a plate
#'
#' Get the position of the plate within the OmniLog(R) device. This is a 
#' convenience function for one of the more important entries of
#' \code{\link{csv_data}}.
#'
#' @param object \code{\link{OPM}} object.
#' @return Character scalar.
#' @export
#' @family getter-functions
#' @keywords attribute
#' @examples
#' data(vaas_1)
#' (x <- position(vaas_1))
#' stopifnot(identical(x, " 7-B"))
#'
setMethod("position", OPM, function(object) {
  object@csv_data[[POS]]
}, sealed = SEALED)


setGeneric("gen_iii", function(object, ...) standardGeneric("gen_iii"))
#' Change to Generation III
#'
#' Change the plate type of an \code{\link{OPM}} object to 
#' \sQuote{Generation III}. The actual spelling used might differ but is 
#' internally consistent.
#'
#' @param object \code{\link{OPM}} object.
#' @return Novel \code{\link{OPM}} object.
#' @export
#' @note This is currently the only function to change plate names. It is
#'   intended for Generation-III plates which were run like PM plates. Usually
#'   they will be annotated as some PM plate by the OmniLog(R) system.
#' @keywords manip
#' @examples
#' data(vaas_1)
#' copy <- gen_iii(vaas_1)
#' stopifnot(identical(vaas_1, copy)) # the dataset already had that plate type
#'
setMethod("gen_iii", OPM, function(object) {
  result <- object
  result@csv_data[[PLATE_TYPE]] <- GEN_III
  result
}, sealed = SEALED)


################################################################################
#
# Other getter functions
#


setGeneric("has_aggr", function(object, ...) standardGeneric("has_aggr"))
#' Are aggregated data present?
#'
#' Check whether aggregated data are present. This always returns \code{FALSE}
#' for the \code{\link{OPM}} class, but not necessarily for its child classes.
#'
#' @param object \code{\link{OPM}} object.
#' @return Logical scalar.
#' @export
#' @family getter-functions
#' @keywords attribute
#' @examples
#' data(vaas_1)
#' stopifnot(has_aggr(vaas_1))
#'
setMethod("has_aggr", OPM, function(object) {
  "aggregated" %in% slotNames(class(object))
}, sealed = SEALED)


setGeneric("summary", function(object, ...) standardGeneric("summary"))
#' Summary
#'
#' Print summary information to screen.
#'
#' @param object \code{\link{OPM}} object.
#' @param ... Optional arguments passed to \code{formatDL}.
#' @export
#' @return Named list, returned invisibly. The \sQuote{metadata} entry is the
#'   number of non-list elements in \code{\link{metadata}}.
#' @family getter-functions
#' @keywords attribute
#' @seealso summary
#' @examples
#' data(vaas_1)
#' x <- summary(vaas_1)
#' stopifnot(is.list(x))
#'
setMethod("summary", OPM, function(object, ...) {
  result <- list(
    Class = class(object),
    `From file` = filename(object),
    `Hours measured` = hours(object),
    `Number of wells` = length(wells(object)),
    `Plate type` = plate_type(object),
    Position = position(object),
    `Setup time` = setup_time(object),
    Metadata = sum(rapply(object@metadata, f = function(item) 1L))
  )
  lapply(formatDL(names(result), unlist(result), ...), FUN = message)
  invisible(result)
}, sealed = SEALED)


################################################################################
#
# Metadata functions
#


setGeneric("collect_template",
  function(object, ...) standardGeneric("collect_template"))
#' Collect template (OPM version)
#'
#' Collect a dataframe template assisting in later on adding metadata using 
#' \code{\link{include_metadata}}. 
#'
#' @name collect_template,OPM
#'
#' @param object \code{\link{OPM}} object.
#' @param selection Character vector passed to \code{\link{csv_data}}.
#' @param add.cols \code{NULL} or character vector. If not empty, names of 
#'   columns to add to the dataframe, initially containing \code{NA}.
#' @export
#' @return Dataframe with one row and the number columns equal to the sum of
#'   the lengths of \code{selection} and \code{add.cols}.
#' @family metadata-functions
#' @keywords attribute
#' @examples
#' data(vaas_1)
#' (x <- collect_template(vaas_1))
#' stopifnot(identical(dim(x), c(1L, 3L)))
#' (x <- collect_template(vaas_1, add.cols = c("A", "B")))
#' stopifnot(identical(dim(x), c(1L, 5L)))
#' # see include_metadata() for how to use this to add metadata information
#'
setMethod("collect_template", OPM, function(object, 
    selection = c(SETUP, POS, FILE), add.cols = NULL) {
  result <- as.list(csv_data(object, selection))
  result <- as.data.frame(result, stringsAsFactors = FALSE, optional = TRUE)
  if (length(add.cols) > 0L) {
    to.add <- matrix(nrow = nrow(result), ncol = length(add.cols), 
      dimnames = list(NULL, add.cols), data = NA_character_)
    result <- cbind(result, to.add, stringsAsFactors = FALSE)
  }
  result
}, sealed = SEALED)


setGeneric("include_metadata",
  function(object, ...) standardGeneric("include_metadata"))
#' Add metadata (from file or dataframe)
#'
#' Include metadata by mapping CSV data and column names in a dataframe.
#'
#' @param object \code{\link{OPM}} object.
#' @param md Dataframe containing keys as column names, or name of file
#'   from which to read the dataframe. Handled by \code{\link{to_metadata}} or
#'   \code{\link{to_metadata,character}}.
#' @param keys Character vector.
#' @param replace Logical scalar indicating whether the previous metadata, if
#'   any, shall be replaced by the novel ones, or whether these shall be
#'   appended.
#' @param stop.on.error Logical scalar. Stop with an error message if 
#'   (unambiguous) selection is impossible? Otherwise, create a warning only.
#' @param remove.csv.data Logical scalar. Remove all names present in
#'   \code{csv_data}, not only those in \code{keys}?
#' @param ... Optional argument passed to \code{\link{to_metadata}} or
#'   \code{\link{to_metadata,character}}.
#' @export
#' @return Novel \code{\link{OPM}} object.
#' @family metadata-functions
#' @keywords manip
#' @examples
#' data(vaas_1)
#' (x <- collect_template(vaas_1, add.cols = "Location")) # generate data frame
#' x[1, "Location"] <- "Braunschweig" # insert additional information
#' copy <- include_metadata(vaas_1, x) # include the data in new OPM object
#' stopifnot(is.null(metadata(vaas_1, "Location")))
#' stopifnot(identical(metadata(copy, "Location"), "Braunschweig"))
#'
setMethod("include_metadata", OPM, function(object, md,
    keys = c(SETUP, POS), replace = FALSE, stop.on.error = TRUE,
    remove.csv.data = TRUE, ...) {

  selection <- as.list(csv_data(object, keys))

  # Get and check metadata.
  md <- to_metadata(md, ...)
  absent.keys <- setdiff(keys, colnames(md))
  if (length(absent.keys) > 0L)
    stop("key missing in 'metadata': ", absent.keys[1L])

  # Try to select the necessary information from the metadata.
  found <- pick_from(md, selection)
  msg <- if ((nr <- nrow(found)) == 1L)
    NULL
  else if (nr == 0L)
    listing(selection, 
      header = "could not find this key/value combination in 'metadata':")
  else
    listing(selection, 
      header = "the selection resulted in more than one row for:")

  # Failures.
  if (!is.null(msg)) {
    if (stop.on.error)
      stop(msg)
    else {
      warning(msg)
      return(object)
    }
  }

  # Success.
  wanted <- setdiff(colnames(found), keys)
  if (remove.csv.data)
    wanted <- setdiff(wanted, names(csv_data(object)))
  found <- as.list(found[, wanted, drop = FALSE])
  result <- object
  result@metadata <- if (replace)
    found
  else
    c(metadata(result), found)

  result

}, sealed = SEALED)


################################################################################
#
# Conversion functions: OPM <=> lists. Detailed comments are given close
# to the class definition.
#


setGeneric("other_slots", function(object, ...) standardGeneric("other_slots"))
#' Other slots
#'
#' Return the names of all slots except the one holding the measurements.
#'
#' @param object \code{\link{OPM}} object.
#' @return Character vector.
#' @keywords internal
#'
setMethod("other_slots", OPM, function(object) {
  setdiff(slotNames(class(object)), "measurements")
}, sealed = SEALED)


setGeneric("attach_attr", function(object, ...) standardGeneric("attach_attr"))
#' Attach slots
#'
#' Attach the contents of all slots, except the measurements, to another object.
#' Useful in conversions (coercions).
#'
#' @param object \code{\link{OPM}} object.
#' @param other Arbitrary other object.
#' @return \code{other} with additional attributes.
#' @keywords internal
#'
setMethod("attach_attr", OPM, function(object, other) {
  lapply(other_slots(object), FUN = function(name) {
    attr(other, name) <<- slot(object, name)
  })
  other
}, sealed = SEALED)


setAs(from = OPM, to = "matrix", function(from) {
  attach_attr(from, from@measurements)
})


setAs(from = OPM, to = "data.frame", function(from) {
  attach_attr(from, as.data.frame(from@measurements))
})


setAs(from = OPM, to = "list", function(from) {
  list(
    metadata = repair_names(metadata(from), fill = TRUE),
    csv_data = as.list(csv_data(from)),
    measurements = as.list(as.data.frame(measurements(from)))
  )
})


################################################################################
#
# Curve parameter estimation
#


setGeneric("to_grofit_time",
  function(object, ...) standardGeneric("to_grofit_time"))
#' Times for grofit
#'
#' Construct time points dataframe as required by \code{grofit}.
#'
#' @param object \code{\link{OPM}} object.
#' @return Dataframe with time points in each row, repeated for each well
#'   (number of rows is number of wells).
#' @keywords internal
#'
setMethod("to_grofit_time", OPM, function(object) {
  tp <- hours(object, "all")
  as.data.frame(matrix(rep(tp, length(wells(object))), ncol = length(tp),
    byrow = TRUE))
}, sealed = SEALED)


setGeneric("to_grofit_data",
  function(object, ...) standardGeneric("to_grofit_data"))
#' Data for grofit
#'
#' Construct dataframe with measurements as required by \code{grofit}.
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
  cbind(names, as.data.frame(t(measurements(object)[, -1L])))
}, sealed = SEALED)


## NOTE: Not an S4 method because 'grofit' is an S3 class

extract_curve_params <- function(data) UseMethod("extract_curve_params")
#' Grofit extraction
#'
#' Extract and rename estimated curve parameters.
#'
#' @param data Object of class \sQuote{grofit}.
#' @return See \code{\link{curve_params}} (in \code{full} mode).
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


setGeneric("curve_params",
  function(object, ...) standardGeneric("curve_params"))
#' Estimate curve parameters
#'
#' Infer curve parameters from the kinetic data stored in an \code{\link{OPM}} 
#' object using the \pkg{grofit} package. This is not normally directly called
#' by an \pkg{opm} user because \code{\link{do_aggr}} is available.
#'
#' @param object \code{\link{OPM}} object.
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
#' @export
#' @return A numeric matrix of the same structure than the one returned by
#'   \code{\link{aggregated}} but with an additional \sQuote{settings} 
#'   attribute containing the (potentially modified) list proved via the
#'   \code{settings} argument, and a \sQuote{program} attribute corresponding 
#'   to the \code{program} argument.
#' @family aggregation-functions
#' @seealso grofit::grofit
#' @keywords smooth
#' @references Brisbin IL, Collins CT, White GC and McCallum DA. A new paradigm 
#'   for the analysis and interpretation of growth data: the shape of things to 
#'   come. The Auk 1987; 104:552-553.
#' @references Efron B. Bootstrap methods: another look at the jackknife. 
#'   Annals of Statistics 1979; 7:1-26.
#' @references Kahm M, Hasenbrink G, Lichtenberg-Frate H, Ludwig J, 
#'   Kschischo M. grofit: Fitting biological growth curves with R. Journal of 
#'   Statistical Software 2010; 33:1-21. 
#' @references Vaas LAI, Sikorski J, Michael V, Goeker M, Klenk H-P. 
#'   Visualization and curve parameter estimation strategies for efficient 
#'   exploration of Phenotype Microarray kinetics. PLoS ONE 2012; in press.
#' @examples
#' # Examples are given under do_aggr(), as only the return value is
#' # different: Let x be the result of do_aggr(). The matrix returned by 
#' # curve_params() could then be received using aggregated(x), whereas 
#' # the 'program' and the 'settings' attributes could be obtained as 
#' # components of the list returned by aggr_settings(x).
#'
setMethod("curve_params", OPM, function(object, boot = 100L, verbose = FALSE,
    cores = 1L, options = list(), program = "grofit") {
  
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
  
  switch(program <- match.arg(program, KNOWN_PROGRAMS),
         
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
      result <- result[names(map), ]
      rownames(result) <- as.character(map)
      attr(result, OPTIONS) <- options
    },
         
    stop(BUG_MSG)

  )
         
  attr(result, PROGRAM) <- program
  result
  
}, sealed = SEALED)


################################################################################
#
# Plots (and the flatten() function)
#


setGeneric("flatten", function(object, ...) standardGeneric("flatten"))
#' Flatten matrix
#'
#' Convert into \sQuote{flat} dataframe, including all measurements in a
#' single column (suitable, e.g., for \pkg{lattice}).
#'
#' @param object \code{\link{OPM}} object.
#' @param include \code{NULL}, character vector or list. If not \code{NULL},
#'   include this meta-information in the dataframe, replicated in each row.
#'   Otherwise it converted to a list and passed to \code{\link{metadata}}. See
#'   there for details.
#' @param fixed \code{NULL} or list. If not \code{NULL}, include these items in
#'   the dataframe, replicated in each row.
#' @param factors Logical scalar. See the \sQuote{stringsAsFactors} argument of
#'   \code{data.frame} and \code{as.data.frame}.
#' @param exact Logical scalar. Passed to \code{\link{metadata}}.
#' @param strict Logical scalar. Passed to \code{\link{metadata}}.
#' @param full Logical scalar. Replace well coordinates by full names?
#' @param ... Optional other arguments passed to \code{\link{wells}}.
#' @export
#' @return Dataframe. Column names are unchecked (not converted to variable
#'   names). The three last columns are: \sQuote{Time}, \sQuote{Well},
#'   \sQuote{Value}, with the obvious meanings.
#' @family conversion-functions
#' @keywords manip dplot
#' @seealso reshape
#' @examples
#' data(vaas_1)
#' x <- flatten(vaas_1)
#' stopifnot(is.data.frame(x), identical(dim(x), c(36864L, 3L)))
#' x <- flatten(vaas_1, fixed = "TEST")
#' stopifnot(is.data.frame(x), identical(dim(x), c(36864L, 4L)))
#' x <- flatten(vaas_1, fixed = "TEST", include = "Strain")
#' stopifnot(is.data.frame(x), identical(dim(x), c(36864L, 5L)))
#'
setMethod("flatten", OPM, function(object, include = NULL, fixed = NULL,
    factors = TRUE, exact = TRUE, strict = TRUE, full = TRUE, ...) {

  # Convert to flat dataframe
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
    rep.times <- rep(times, length(well.names))
    rep.wells <- rep(well.names, each = length(times))
    result <- data.frame(Time = rep.times, Well = rep.wells,
      Value = as.vector(object@measurements[, -1L]), check.names = FALSE,
      stringsAsFactors = factors)
  }
  
  # Include fixed stuff
  if (length(fixed) > 0L) 
    result <- cbind(as.data.frame(as.list(fixed), stringsAsFactors = factors),
      result)
  
  # Pick metadata and include them in the dataframe
  if (length(include) > 0L) { 
    result <- cbind(as.data.frame(metadata(object, as.list(include), 
      exact = exact), stringsAsFactors = factors), result)
  }
  
  result
  
}, sealed = SEALED)


setGeneric("xy_plot", function(x, ...) standardGeneric("xy_plot"))
#' XY plot
#'
#' Customized plotting of a single PM plate, using \code{xyplot} from the
#' \pkg{lattice} package. The optimal number of rows and columns is estimated 
#' from the number of selected wells. An optimal font size of the panel headers
#' is also chosen automatically, but can also be adapted by the user, much like
#' most aspects of the resulting graphics output.
#'
#' @param x \code{\link{OPM}} object.
#'
#' @param col Character scalar (color name) determining the line color.
#' @param lwd Numeric scalar determining the line width.
#'
#' @param neg.ctrl Determine the height of a horizontal baseline drawn in each
#'   panel. If \code{NULL} or \code{FALSE}, no baseline will be drawn. If
#'   \code{TRUE}, the baseline's height is the value of \code{\link{minmax}}.
#'   If a character scalar, \code{neg.ctrl} is interpreted as the name of the
#'   well regarded as negative control. Its overall maximum will determine the
#'   height. Set \code{neg.ctrl} to a numeric value to set the height directly
#'   (at your own risk).
#' @param base.col Character scalar. Baseline color (ignored if no baseline is
#'   drawn).
#' @param base.lwd Numeric scalar determining the width of the baseline
#'   (ignored if no baseline is drawn).
#'
#' @param main The settings controlling the construction of the main title.
#'   If a list, a named list with the following entries (if
#'   missing, they are replaced by the respective defaults):
#'   \describe{
#'     \item{predef}{Character scalar or expression. Predefined title. If set,
#'       the other entries are ignored.}
#'     \item{use}{Logical scalar. If \code{FALSE}, returns \code{NULL}.}
#'     \item{...}{Other arguments are passed to \code{\link{plate_type}}.}
#'   }
#'   If \code{settings} is not a list but a character scalar or an expression,
#'   this is used as the \sQuote{predef} entry of the above-mentioned list. If
#'   not a list but a logical scalar, it is used as the \sQuote{use} entry of
#'   this list. If not a list but a numeric value, it is used as the
#'   \sQuote{max} entry of this list.
#' @param xlab Character scalar. Title of x-axis. Use \code{NULL} to turn it
#'   off.
#' @param ylab Character scalar. Title of y-axis. Use \code{NULL} to turn it
#'   off.
#'
#' @param theor.max Logical scalar. Use the theoretical maximum as maximum of
#'   the y-axis? If \code{FALSE}, use the empirical maximum with a small 
#'   offset.
#'
#' @param draw.grid Logical scalar. Insert background grid?
#'
#' @param strip.fmt List controlling the format of the description strip above
#'   each panel. For instance, the background color is set using the \sQuote{bg}
#'   key. For further details, see \code{strip.custom} from the \pkg{lattice}
#'   package.
#' @param striptext.fmt List controlling the textual description at the top of
#'   each panel. For instance, the relative text size is set using the
#'   \sQuote{cex} key, the color by \sQuote{col}, the font by \sQuote{font}
#'   and the number of lines by \sQuote{lines}. The latter might be of interest
#'   in conjunction with the \code{paren.sep} argument of \code{\link{wells}}.
#'   See the argument \sQuote{par.strip.text} of \code{xyplot} from the 
#'   \pkg{lattice} package for details.
#'
#' @param ... Arguments that are passed to \code{\link{flatten}}.
#'
#' @export
#' @family plotting-functions
#' @return An object of class \sQuote{trellis}. See \code{xyplot} from the
#'   \pkg{lattice} package for details.
#' @references Sarkar D. Lattice: Multivariate Data Visualization with R. 2008;
#'    New York: Springer, 265p.
#' @references Vaas LAI, Sikorski J, Michael V, Goeker M, Klenk H-P. 
#'   Visualization and curve parameter estimation strategies for efficient 
#'   exploration of Phenotype Microarray kinetics. PLoS ONE 2012; in press.
#' @keywords hplot
#' @seealso lattice::xyplot
#' @examples 
#' 
#' data(vaas_1)  
#' xy_plot(vaas_1) # note the default main title built from the plate type
#'
#' x <- vaas_1[, 11:22]
#' # Gives a warning message: we have deleted the default negative control:
#' xy_plot(x)
#' # Turn the baseline off => no warning:
#' xy_plot(x, neg.ctrl = NULL)
#' # Or guess a baseline
#' xy_plot(x, neg.ctrl = 100)
#' # Some like it ugly:
#' xy_plot(x, neg.ctrl = 100, col = "pink", base.col = "yellow", main = "Ugly")
#'
setMethod("xy_plot", OPM, function(x, col = "midnightblue", lwd = 1,
    neg.ctrl = "A01", base.col = "grey10", base.lwd = lwd,
    main = list(), xlab = "Time [h]", ylab = "Value [OmniLog units]",
    theor.max = TRUE, draw.grid = TRUE,
    strip.fmt = list(), striptext.fmt = list(),
    ...) {

  ## BEGIN must be synchronized with xy_plot,OPMS
  
  # Setup
  layout <- best_layout(dim(x)[2L])
  y.max <- if (theor.max)
    THEOR_MAX
  else
    improved_max(x)
  main <- main_title(x, main)
  neg.ctrl <- negative_control(x, neg.ctrl)

  # Adding default to settings lists. insert() is used here: for some reason 
  # the later entries have precedence in striptext.fmt
  strip.fmt <- insert(as.list(strip.fmt), bg = "grey90")
  striptext.fmt <- insert(as.list(striptext.fmt), cex = 1.5 / sqrt(layout[2L]),
    lines = 1.25)

  ## END must be synchronized with xy_plot,OPMS

  # Plot
  lattice::xyplot(
    # Principally unchangeable arguments
    Value ~ Time | Well, data = flatten(x, ...), type = "l", layout = layout,
    as.table = TRUE,
    # Curve color and panel height
    col = col, ylim = c(0, y.max),
    # Axis annotation
    scales = list(x = list(rot = 90)),
    # Main annotation
    main = main, xlab = xlab, ylab = ylab,
    # Description above each panel
    strip = do.call(lattice::strip.custom, strip.fmt), 
      par.strip.text = striptext.fmt,
    # The panels
    panel = function(...) {
      if (draw.grid)
        lattice::panel.grid(h = -1, v = -1)
      if (!is.null(neg.ctrl))
        lattice::panel.abline(neg.ctrl, 0, col = base.col, lwd = base.lwd)
      lattice::panel.xyplot(..., lwd = lwd)
    })

}, sealed = SEALED)


setGeneric("level_plot", function(x, ...) standardGeneric("level_plot"))
#' Levelplot
#'
#' Levelplot for \code{\link{OPM}} objects using the \pkg{lattice} package.
#'
#' @param x \code{\link{OPM}} object.
#'
#' @param main The settings controlling the construction of the main title.
#'   Works like the \code{main} argument of \code{\link{xy_plot}}.
#' @param colors Character vector indicating the colors (at least two).
#'
#' @param cex Numeric scalar. Magnification of axis annotation. If \code{NULL},
#'   automatically adapted to the number of wells (at least a good guess is 
#'   made).
#'
#' @param ... Arguments that are passed to \code{\link{flatten}}.
#'
#' @export
#' @return An object of class \sQuote{trellis}. See \code{levelplot} from the
#'   \pkg{lattice} package for details.
#' @family plotting-functions
#' @keywords hplot
#' @references Jacobsen JS, Joyner DC, Borglin SE, Hazen TC, Arkin AP et al. 
#'   Visualization of growth curve data from phenotype microarray experiments.
#'   2007; 11th International Conference on Information Visualization (IV07), 
#'   Zuerich, Switzerland, July 4-6, 2007. Published by the IEEE Computer 
#'   Society.
#' @references Sarkar D. Lattice: Multivariate Data Visualization with R. 2008;
#'    New York: Springer, 265p.
#' @references Vaas LAI, Sikorski J, Michael V, Goeker M, Klenk H-P. 
#'   Visualization and curve parameter estimation strategies for efficient 
#'   exploration of Phenotype Microarray kinetics. PLoS ONE 2012; in press.
#' @seealso lattice::levelplot
#' @examples 
#' data(vaas_1)  
#' level_plot(vaas_1, main = "Levelplot example")
#'
setMethod("level_plot", OPM, function(x, main = list(), colors = NULL,
    cex = NULL, ...) {
  if (is.null(cex))
    cex <- guess_cex(dim(x)[2L])
  main <- main_title(x, main)
  lattice::levelplot(Value ~ Time * Well, data = flatten(x, ...), main = main,
    col.regions = default_color_regions(colors), #as.table = TRUE,
    scales = list(cex = cex, lineheight = 10))
}, sealed = SEALED)



