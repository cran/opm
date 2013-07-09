


################################################################################
################################################################################
#
# Miscellaneous methods
#


#' Merge plates
#'
#' Combine all plates in a single \code{\link{OPM}} object by treating them as
#' originating from subsequent runs of the same experimental plate. Adjust the
#' times accordingly.
#'
#' @param x \code{\link{OPMS}} object.
#' @param y Numeric vector indicating the time(s) (in hours) between two
#'   subsequent plates. Must be positive throughout, and its length should fit
#'   to the number of plates (e.g., either \code{1} or \code{length(x) - 1}
#'   would work). If missing, \code{0.25} is used.
#' @param sort.first Logical scalar. Sort the plates according to their setup
#'   times before merging?
#' @param parse Logical scalar. Ignored unless \code{sort.first} is \code{TRUE}.
#'   For sorting, parse the setup times using \code{strptime} from the
#'   \pkg{base} package? It is an error if this does not work, but see
#'   \sQuote{Details}.
#' @export
#' @return \code{\link{OPM}} object. The \code{\link{metadata}} and
#'   \code{\link{csv_data}} will be taken from the first contained plate, but
#'   aggregated values, if any, will be dropped.
#' @details This \code{\link{OPMS}} method is intended for dealing with slowly
#'   growing or reacting organisms that need to be analyzed with subsequent runs
#'   of the same plate in PM mode. Results obtained with \emph{Geodermatophilus}
#'   strains and Generation-III plates indicate that this works well in
#'   practice. See the references.
#'
#'   See the arguments \sQuote{time.fmt} and \sQuote{time.zone} of
#'   \code{\link{opm_opt}} for modifying the parsing of setup-time entries. If
#'   it does not work, additional time-string templates must be stored.
#'
#'   The \code{CMAT} method is only for internal use.
#' @references Montero-Calasanz, M. d. C., Goeker, M.,  Poetter, G., Rohde, M.,
#'   Sproeer, C., Schumann, P., Gorbushina, A. A., Klenk, H.-P. 2012
#'   \emph{Geodermatophilus arenarius} sp. nov., a xerophilic actinomycete
#'   isolated from Saharan desert sand in Chad. \emph{Extremophiles}
#'   \strong{16}, 903--909.
#' @references Montero-Calasanz, M. d. C., Goeker, M., Rohde, M., Schumann, P.,
#'   Poetter, G., Sproeer, C., Gorbushina, A. A., Klenk, H.-P. 2013
#'   \emph{Geodermatophilus siccatus} sp. nov., isolated from arid sand of the
#'   Saharan desert in Chad. \emph{Antonie van Leeuwenhoek} \strong{103},
#'   449--456.
#'
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
    to.add <- c(0, must(cumsum(vapply(tp[-length(tp)], tail, 1, 1L)) + y))
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

setMethod("merge", c(CMAT, "ANY"), function(x, y) {
  if (is.logical(y))
    if (L(y))
      groups <- as.factor(rownames(x))
    else
      groups <- as.factor(seq_len(nrow(x)))
  else
    groups <- as.factor(y)
  if (length(groups) != nrow(x)) # this also covers NULL row names
    stop("length of 'groups' not equal to number of rows")
  if (any(is.na(groups)))
    stop("'groups' must not contain NA values")
  if (length(levels(groups)) == length(groups))
    return(x)
  cn <- colnames(x) # later put back, avoiding correction of duplicate names
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  x <- aggregate(x, by = list(groups), FUN = c, recursive = TRUE,
    simplify = FALSE)
  x <- as.matrix(x[, -1L, drop = FALSE])
  x[] <- lapply(x, sort.int, na.last = TRUE)
  rownames(x) <- levels(groups)
  colnames(x) <- cn
  new(CMAT, x)
}, sealed = SEALED)


################################################################################


#' Get available plates
#'
#' Get all plates contained in an \code{\link{OPMS}} object or a list, or create
#' a list containing a single \code{\link{OPM}} object as element. The list
#' method traverses the input recursively and skips all objects of other classes
#' than \code{\link{OPM}}.
#'
#' @param object List, \code{\link{OPM}} or \code{\link{OPMS}} object.
#' @return List of \code{\link{OPM}} objects (may be empty instead if
#'   \code{object} is a list).
#' @export
#' @family conversion-functions
#' @keywords attribute
#' @note See also \code{\link{opms}}, which is somewhat similar but more
#'   flexible.
#' @seealso base::list base::as.list
#' @examples
#'
#' # 'OPM' method
#' data(vaas_1)
#' summary(x <- plates(vaas_1)) # => list of OPM objects
#' stopifnot(is.list(x), length(x) == 1L, sapply(x, inherits, what = "OPM"))
#'
#' # 'OPMS' method
#' data(vaas_4)
#' summary(x <- plates(vaas_4)) # => list of OPM objects
#' stopifnot(is.list(x), length(x) == 4L, sapply(x, inherits, what = "OPM"))
#'
#' # list method
#' x <- list(vaas_1, letters, vaas_4, 1:10)
#' summary(x <- plates(x)) # => list of OPM objects
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
  to_opm_list.list(object, TRUE, TRUE, FALSE)
}, sealed = SEALED)


################################################################################


#' Apply method for OPMS objects
#'
#' Apply a function to all \code{\link{OPM}} or \code{\link{OPMA}} objects
#' within an \code{\link{OPMS}} object. Optionally simplify the result to an
#' \code{\link{OPMS}} object if possible, or other structures simpler than a
#' list.
#'
#' @param object \code{\link{OPMS}} object. An \code{\link{OPM}} method is also
#'   defined but simply applies \code{fun} once (to \code{object}).
#' @param fun A function. Should expect an  \code{\link{OPM}} (or
#'   \code{\link{OPMA}}) object as first argument.
#' @param ... Optional other arguments passed to \code{fun}.
#' @param simplify Logical scalar. If \code{FALSE}, the result is a list. If
#'   \code{TRUE}, it is attempted to simplify the result to a vector or matrix
#'   or to an \code{\link{OPMS}} object (if the result is a list of
#'   \code{\link{OPM}} or \code{\link{OPMA}} objects). If this is impossible, a
#'   list is returned.
#' @export
#' @return List, vector, matrix or \code{\link{OPMS}} object.
#' @family conversion-functions
#' @keywords manip
#' @seealso base::sapply
#' @examples
#' data(vaas_4)
#' summary(x <- oapply(vaas_4, identity)) # trivial
#' stopifnot(identical(x, vaas_4))
#' summary(x <- oapply(vaas_4, identity, simplify = FALSE)) # => yields list
#' stopifnot(is.list(x), length(x) == 4, sapply(x, class) == "OPMD")
#'
setGeneric("oapply", function(object, ...) standardGeneric("oapply"))

setMethod("oapply", OPM, function(object, fun, ..., simplify = TRUE) {
  fun(object, ...)
}, sealed = SEALED)

setMethod("oapply", OPMS, function(object, fun, ..., simplify = TRUE) {
  result <- sapply(X = object@plates, FUN = fun, ..., simplify = simplify,
    USE.NAMES = FALSE)
  if (simplify && is.list(result))
    result <- try_opms.list(result)
  result
}, sealed = SEALED)



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
#' @details Thinning the plates out is experimental insofar as it has
#'   \strong{not} been tested whether and how this could sensibly be applied
#'   before aggregating the data.
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


#' Change to Generation III (or other plate type)
#'
#' Change the plate type of an \code{\link{OPM}} object to \sQuote{Generation
#' III} or another plate type.
#'
#' @param object \code{\link{OPM}} object.
#' @param to Character scalar indicating the plate type. User-defined plate
#'   types must be given literally. For generation-III plates, use
#'   \sQuote{gen.iii}; for ecoplates, use \sQuote{eco}; the remaining allowed
#'   values are only \sQuote{sf.n2} and \sQuote{sf.p2}, but matching is
#'   case-insensitive.
#' @param ... Optional arguments passed between the methods.
#' @return Novel \code{\link{OPM}} object.
#' @export
#' @details This is currently the only function to change plate names. It is
#'   intended for Generation-III plates which were run like PM plates. Usually
#'   they will be annotated as some PM plate by the
#'   OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} system. In contrast,
#'   input ID-mode plates are automatically detected (see
#'   \code{\link{read_single_opm}}).
#'
#'   The actual spelling of the plate type used might (in theory) differ between
#'   distinct versions of \pkg{opm} but is internally consistent. It is an error
#'   to set one of the PM plate types or to assign an unknown plate type.
#' @keywords manip
#' @family conversion-functions
#' @examples
#'
#' # 'OPM' method
#' data(vaas_1)
#' plate_type(copy <- gen_iii(vaas_1))
#' stopifnot(identical(vaas_1, copy)) # the dataset already had that plate type
#' plate_type(copy <- gen_iii(vaas_1, "eco")) # which is wrong, actually
#' stopifnot(!identical(vaas_1, copy))
#'
#' # 'OPMS' method
#' data(vaas_4)
#' plate_type(copy <- gen_iii(vaas_4))
#' stopifnot(identical(vaas_4, copy)) # as above
#' plate_type(copy <- gen_iii(vaas_4, "eco"))
#' stopifnot(!identical(vaas_4, copy)) # as above
#'
setGeneric("gen_iii", function(object, ...) standardGeneric("gen_iii"))

setMethod("gen_iii", OPM, function(object, to = "gen.iii") {
  to <- match.arg(tolower(to), names(SPECIAL_PLATES))
  object@csv_data[[CSV_NAMES[["PLATE_TYPE"]]]] <- SPECIAL_PLATES[[to]]
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


#' Flatten measurements from OPM or OPMS objects
#'
#' Convert into \sQuote{flat} data frame, including all measurements in a single
#' column (suitable, e.g., for \pkg{lattice}).
#'
#' @param object \code{\link{OPM}} or \code{\link{OPMS}} object or list.
#' @param include \code{NULL}, character vector, list or formula. If not empty,
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
#' @return Data frame. Column names are unchecked (not converted to variable
#'   names). The three last columns are coding for time, well and value, with
#'   the exact spelling of the column names given by \code{\link{param_names}}.
#'
#'   The \code{\link{OPMS}} method yields an additional column for the plate,
#'   the exact spelling of its name also being available via
#'   \code{\link{param_names}}. This column contains the position of each plate
#'   within \code{object}.
#' @family conversion-functions
#' @keywords manip dplot
#' @seealso stats::reshape
#' @examples
#'
#' # OPM method
#' data(vaas_1)
#' # distinct numbers of columns due to distinct selection settings
#' head(x <- flatten(vaas_1))
#' stopifnot(is.data.frame(x), identical(dim(x), c(36864L, 3L)))
#' head(x <- flatten(vaas_1, fixed = "TEST"))
#' stopifnot(is.data.frame(x), identical(dim(x), c(36864L, 4L)))
#' head(x <- flatten(vaas_1, fixed = "TEST", include = "Strain"))
#' stopifnot(is.data.frame(x), identical(dim(x), c(36864L, 5L)))
#'
#' # OPMS method
#' data(vaas_4)
#' # distinct numbers of columns due to distinct selection settings
#' head(x <- flatten(vaas_4))
#' stopifnot(is.data.frame(x), identical(dim(x), c(147456L, 4L)))
#' head(x <- flatten(vaas_4, fixed = "TEST"))
#' stopifnot(is.data.frame(x), identical(dim(x), c(147456L, 5L)))
#' head(x <- flatten(vaas_4, fixed = "TEST", include = ~ Strain))
#' stopifnot(is.data.frame(x), identical(dim(x), c(147456L, 6L)))
#'
setGeneric("flatten")

setMethod("flatten", OPM, function(object, include = NULL, fixed = NULL,
    factors = TRUE, exact = TRUE, strict = TRUE, full = TRUE, ...) {

  # Convert to flat data frame
  well.names <- wells(object, full = full, ...)
  ## the home-brewn solution was much faster than reshape():
  # if (factors)
  #   well.names <- as.factor(well.names)
  # result <- reshape(as.data.frame(object@measurements,
  #   stringsAsFactors = factors), direction = "long", idvar = "Hour",
  #   varying = wells(object), v.names = "Value", timevar = "Well",
  #   times = well.names)
  # colnames(result)[1L] <- "Time"
  times <- hours(object, "all")
  rep.times <- rep.int(times, length(well.names))
  rep.wells <- rep(well.names, each = length(times))
  result <- data.frame(time = rep.times, well = rep.wells,
    value = as.vector(object@measurements[, -1L]), check.names = FALSE,
    stringsAsFactors = factors)
  colnames(result) <- RESERVED_NAMES[colnames(result)]

  if (length(fixed)) # Include fixed stuff
    result <- cbind(as.data.frame(as.list(fixed), stringsAsFactors = factors),
      result)

  if (length(include)) # Pick metadata and include them in the data frame
    result <- cbind(as.data.frame(metadata(object, include,
      exact = exact, strict = strict), stringsAsFactors = factors), result)

  result

}, sealed = SEALED)

setMethod("flatten", OPMS, function(object, include = NULL, fixed = list(),
    ...) {
  nums <- paste(RESERVED_NAMES[["plate"]], seq_along(object@plates))
  nums <- lapply(as.list(nums), `names<-`, value = RESERVED_NAMES[["plate"]])
  nums <- lapply(nums, c, fixed, recursive = FALSE)
  do.call(rbind, mapply(flatten, object = object@plates, fixed = nums,
    MoreArgs = list(include = include, ...), SIMPLIFY = FALSE))
}, sealed = SEALED)


################################################################################


#' Factor from flattened data
#'
#' Extract all plate-specifying information from a data frame as created by
#' \code{\link{flatten}}. If metadata have been included, these will be joined
#' together; otherwise the plate identifiers (basically numbers) themselves are
#' used.
#'
#' @param object Object as returned by \code{\link{flatten}}.
#' @param sep Character scalar. Separator used for joining the columns together.
#' @return Factor with one entry per plate.
#' @keywords internal
#'
setGeneric("flattened_to_factor",
  function(object, ...) standardGeneric("flattened_to_factor"))

setMethod("flattened_to_factor", "data.frame", function(object, sep = " ") {
  LL(plate.pos <- which(colnames(object) == RESERVED_NAMES[["plate"]]), sep)
  if (plate.pos == 1L)
    return(unique(object[, plate.pos]))
  result <- aggregate(object[, seq.int(1L, plate.pos)],
    by = list(object[, plate.pos]), FUN = `[[`, i = 1L)
  result <- as.list(result[, seq.int(2L, ncol(result) - 1L), drop = FALSE])
  as.factor(do.call(paste, c(result, sep = sep)))
}, sealed = SEALED)


################################################################################


#' Create data frame or vector from metadata
#'
#' Extract selected metadata entries for use as additional columns in a data
#' frame or (after joining) as character vector with labels.
#'
#' @param object \code{\link{OPMS}} object or data frame.
#' @param what For the \code{\link{OPMS}} method, a list of metadata keys to
#'   consider, or single such key; passed to \code{\link{metadata}}. A formula
#'   is also possible; see there for details. A peculiarity of
#'   \code{extract_columns} is that including \code{J} as a pseudo-function call
#'   in the formula triggers the combination of metadata entries to new factors
#'   immediately after selecting them, as long as \code{join} is \code{FALSE}.
#'
#'   For the data-frame method, just the names of the columns to extract, or
#'   their indices, as vector, if \code{direct} is \code{TRUE}. Alternatively,
#'   the name of the class to extract from the data frame to form the matrix
#'   values.
#'
#'   In the \sQuote{direct} mode, \code{what} can also be a named list of
#'   vectors used for indexing. In that case a data frame is returned that
#'   contains the columns from \code{object} together with new columns that
#'   result from pasting the selected columns together.
#' @param join Logical scalar. Join each row together to yield a character
#'   vector? Otherwise it is just attempted to construct a data frame.
#' @param sep Character scalar. Used as separator between the distinct metadata
#'   entries if these are to be pasted together. Ignored unless \code{join} is
#'   \code{TRUE}. The data-frame method always joins the data unless \code{what}
#'   is a list.
#' @param dups Character scalar specifying what to do in the case of duplicate
#'   labels: either \sQuote{warn}, \sQuote{error} or \sQuote{ignore}. Ignored
#'   unless \code{join} is \code{TRUE}.
#' @param factors Logical scalar determining whether strings should be converted
#'   to factors. Note that this would only affect newly created data-frame
#'   columns.
#' @param exact Logical scalar. Also passed to \code{\link{metadata}}.
#' @param strict Logical scalar. Also passed to \code{\link{metadata}}.
#' @param as.labels Character vector. See \code{\link{extract}}.
#' @param as.groups Character vector. See \code{\link{extract}}.
#' @param direct Logical scalar. Extract column names directly, or search for
#'   columns of one to several given classes?
#' @export
#' @details This function is not normally directly called by an \pkg{opm} user
#'   because \code{\link{extract}} is available, which uses this function, but
#'   can be used for testing the applied metadata selections beforehand.
#'
#'   The data-frame method is partially trivial (extract the selected columns
#'   and join them to form a character vector or new data-frame columns),
#'   partially more useful (extract columns with data of a specified class).
#' @return For the \code{OPMS} method, a data frame or character vector,
#'   depending on the \code{join} argument. The data-frame method returns a
#'   character vector or a data frame, too, but depending on the \code{what}
#'   argument.
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
#' stopifnot(is.data.frame(x), dim(x) == c(4, 2))
#' (y <- extract_columns(vaas_4, what = ~ Species + Strain))
#' stopifnot(identical(x, y)) # same result using a formula
#' (y <- extract_columns(vaas_4, what = ~ J(Species + Strain)))
#' stopifnot(is.data.frame(y), dim(y) == c(4, 3)) # additional column created
#' stopifnot(identical(x, y[, -3]))
#'
#' # Create a character vector
#' (x <- extract_columns(vaas_4, what = list("Species", "Strain"), join = TRUE))
#' stopifnot(is.character(x), length(x) == 4L)
#' (x <- try(extract_columns(vaas_4, what = list("Species"), join = TRUE,
#'   dups = "error"), silent = TRUE)) # duplicates yield error
#' stopifnot(inherits(x, "try-error"))
#' (x <- try(extract_columns(vaas_4, what = list("Species"), join = TRUE,
#'   dups = "warn"), silent = TRUE)) # duplicates yield warning only
#' stopifnot(is.character(x), length(x) == 4L)
#'
#' # data-frame method, 'direct' running mode
#' x <- data.frame(a = 1:26, b = letters, c = LETTERS)
#' (y <- extract_columns(x, I(c("a", "b")), sep = "-"))
#' stopifnot(grepl("^\\s*\\d+-[a-z]$", y)) # pasted columns 'a' and 'b'
#'
#' # data-frame method, using class name
#' (y <- extract_columns(x, as.labels = "b", what = "integer", as.groups = "c"))
#' stopifnot(is.matrix(y), dim(y) == c(26, 1), rownames(y) == x$b)
#' stopifnot(identical(attr(y, "row.groups"), x$c))
#'
setGeneric("extract_columns",
  function(object, ...) standardGeneric("extract_columns"))

setMethod("extract_columns", OPMS, function(object, what, join = FALSE,
    sep = " ", dups = c("warn", "error", "ignore"), factors = TRUE,
    exact = TRUE, strict = TRUE) {
  what <- metadata_key(what, FALSE, NULL)
  result <- metadata(object, what, exact, strict)
  result <- if (is.list(result))
    lapply(result, rapply, f = as.character)
  else
    as.list(as.character(result))
  if (L(join)) {
    result <- unlist(lapply(result, FUN = paste0, collapse = sep))
    msg <- if (is.dup <- anyDuplicated(result))
      paste("duplicated label:", result[is.dup])
    else
      NULL
    if (length(msg))
      case(match.arg(dups), ignore = as.null, warn = warning, error = stop)(msg)
  } else {
    result <- must(do.call(rbind, result))
    result <- as.data.frame(result, optional = TRUE, stringsAsFactors = factors)
    if (ncol(result) > length(colnames(result)))
      colnames(result) <- paste(what, collapse = get("key.join", OPM_OPTIONS))
    if (is.list(attr(what, "combine")))
      result <- extract_columns(result, attr(what, "combine"),
        factors = factors, direct = TRUE)
  }
  result
}, sealed = SEALED)

setMethod("extract_columns", "data.frame", function(object, what,
    as.labels = NULL, as.groups = NULL, sep = opm_opt("comb.value.join"),
    factors = is.list(what), direct = inherits(what, "AsIs")) {
  join <- function(x, what, sep)
    do.call(paste, c(x[, what, drop = FALSE], list(sep = sep)))
  find_stuff <- function(x, what) {
    x <- x[, vapply(x, inherits, NA, what), drop = FALSE]
    if (!ncol(x))
      stop("no data of class(es) ", paste(what, collapse = "/"), " found")
    as.matrix(x)
  }
  LL(direct, factors)
  if (direct) {
    if (is.list(what)) {
      if (is.null(names(what)))
        stop("if 'what' is a list, it must have names")
      result <- object
      what <- what[!match(names(what), colnames(result), 0L)]
      if (factors)
        for (i in seq_along(what))
          result[, names(what)[i]] <- as.factor(join(object, what[[i]], sep))
      else
        for (i in seq_along(what))
          result[, names(what)[i]] <- join(object, what[[i]], sep)
      if (length(as.labels))
        rownames(result) <- join(object, as.labels, sep)
    } else {
      result <- join(object, what, sep)
      if (length(as.labels))
        names(result) <- join(object, as.labels, sep)
      if (factors)
        result <- as.factor(result)
    }
  } else {
    result <- find_stuff(object, what)
    if (length(as.labels))
      rownames(result) <- join(object, as.labels, sep)
  }
  if (length(as.groups))
    attr(result, "row.groups") <- as.factor(join(object, as.groups, sep))
  result
}, sealed = SEALED)


################################################################################
################################################################################
#
# Sorting etc.
#


#' Sort OPMS objects
#'
#' Sort an \code{\link{OPMS}} object based on one to several metadata or
#' \acronym{CSV} data entries. There is also an \code{\link{OPM}} method which
#' returns the input data (to avoid destructive effects due to the way the
#' default \code{sort} interacts with \code{\link{OPM}} indexing).
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
#'   See \code{\link{opm_opt}}, arguments \sQuote{time.fmt} and
#'   \sQuote{time.zone}, for modifying the parsing of setup-time entries.
#' @param exact Logical scalar. Passed to \code{\link{metadata}}. Affects only
#'   metadata querying, not directly the sorting.
#' @param strict Logical scalar. Is it an error if metadata keys are not found?
#'   If \code{FALSE}, \code{x} gets ordered according to only the found keys,
#'   and remains in the original order if none of the keys in \code{by} are
#'   found at all. Note that it is always an error if keys are found in the
#'   \code{\link{metadata}} of some of the \code{\link{plates}} but not in
#'   those of others.
#' @param na.last Logical scalar. Also passed to \code{order}.
#' @param ... Optional arguments passed between the methods.
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
#' stopifnot(inherits(x, "try-error")) # yields error
#' x <- try(sort(vaas_4, by = list("Not there", "Missing"), strict = FALSE))
#' stopifnot(identical(x, vaas_4)) # no error, but no new order
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

setMethod("sort", c(OPMX, "missing"), function(x, decreasing, ...) {
  sort(x = x, decreasing = FALSE, ...)
}, sealed = SEALED)

setMethod("sort", c(OPM, "logical"), function(x, decreasing, ...) {
  x
}, sealed = SEALED)

setMethod("sort", c(OPMS, "logical"), function(x, decreasing, by = "setup_time",
    parse = TRUE, exact = TRUE, strict = TRUE, na.last = TRUE) {
  if (is.list(by)) {
    keys <- lapply(X = by, FUN = metadata, object = x, exact = exact,
      strict = strict)
    if (!strict)
      if (!length(keys <- keys[!vapply(keys, is.null, NA)]))
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
  x@plates <- x@plates[do.call(order, keys)]
  x
}, sealed = SEALED)


################################################################################


#' Make OPMS objects unique
#'
#' Check whether duplicated \code{\link{OPM}} or \code{\link{OPMA}} objects are
#' contained within an \code{\link{OPMS}} object and remove the duplicated ones.
#' The \code{\link{OPM}} method just returns the object passed.
#'
#' @param x \code{\link{OPMS}} or \code{\link{OPM}} object.
#' @param incomparables Vector passed to \code{\link{duplicated}}. The default
#'   is \code{FALSE}.
#' @param ... Optional further arguments passed to \code{\link{duplicated}}. See
#'   the examples.
#' @export
#' @return \code{\link{OPMS}} or \code{\link{OPM}} object or \code{NULL}.
#' @family conversion-functions
#' @keywords manip
#' @seealso base::unique
#' @examples
#'
#' ## 'OPMS' method
#' data(vaas_4)
#' dim(x <- unique(vaas_4))
#' stopifnot(identical(x, vaas_4))
#' dim(x <- unique(c(vaas_4, vaas_4)))
#' stopifnot(identical(x, vaas_4))
#' dim(x <- unique(vaas_4, what = "Species")) # species are not unique
#' stopifnot(dim(x)[1L] < dim(vaas_4)[1L])
#' dim(x <- unique(vaas_4, what = list("Species", "Strain")))
#' stopifnot(identical(x, vaas_4)) # organisms are unique
#'
#' ## 'OPM' method
#' data(vaas_1)
#' dim(x <- unique(vaas_1)) # trivial
#' stopifnot(identical(x, vaas_1))
#' dim(x <- unique(vaas_1, what = list("Species", "Strain")))
#' stopifnot(identical(x, vaas_1))
#'
setGeneric("unique")

setMethod("unique", c(OPM, "ANY"), function(x, incomparables, ...) {
  x
}, sealed = SEALED)

setMethod("unique", c(OPMS, "missing"), function(x, incomparables, ...) {
  unique(x = x, incomparables = FALSE, ...)
}, sealed = SEALED)

setMethod("unique", c(OPMS, "ANY"), function(x, incomparables, ...) {
  x[!duplicated(x = x, incomparables = incomparables, ...)]
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
#'   \code{\link{OPM}} object.
#' @family conversion-functions
#' @keywords manip
#' @seealso base::rev
#' @examples
#'
#' ## 'OPMS' method
#' data(vaas_4)
#' dim(x <- rev(vaas_4))
#' stopifnot(dim(x) == dim(vaas_4), !identical(x, vaas_4))
#' stopifnot(identical(rev(x), vaas_4))
#'
#' ## 'OPMS' method
#' data(vaas_1)
#' dim(x <- rev(vaas_1)) # trivial
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
#' @return \code{\link{OPMS}} object with another number of plates, or or
#'   \code{\link{OPM}} object, or \code{NULL}.
#' @family conversion-functions
#' @keywords manip
#' @seealso base::rep
#' @examples
#'
#' ## 'OPMS' method
#' data(vaas_4)
#' dim(x <- rep(vaas_4))
#' stopifnot(identical(x, vaas_4))
#' dim(x <- rep(vaas_4, times = 2))
#' stopifnot(length(x) == length(vaas_4) * 2)
#' dim(y <- rep(vaas_4, each = 2))
#' stopifnot(length(y) == length(vaas_4) * 2, !identical(x, y))
#' stopifnot(is.null(rep(vaas_4, 0)))
#'
#' ## 'OPM' method
#' data(vaas_1)
#' dim(x <- rep(vaas_1, 1))
#' stopifnot(identical(x, vaas_1))
#' dim(x <- rep(vaas_1, 2)) # conversion to OPMS if > 1 element
#' stopifnot(length(x) == 2, is(x, "OPMS"))
#' stopifnot(is.null(rep(vaas_4, 0)))
#'
setGeneric("rep")

setMethod("rep", OPM, function(x, ...) {
  x <- rep(list(x), ...)
  case(length(x), NULL, x[[1L]], new(OPMS, plates = x))
}, sealed = SEALED)

setMethod("rep", OPMS, function(x, ...) {
  x <- rep(x@plates, ...)
  case(length(x), NULL, x[[1L]], new(OPMS, plates = x))
}, sealed = SEALED)


################################################################################
################################################################################
#
# Extraction of character matrices
#


#' Extract aggregated values
#'
#' Extract selected aggregated and/or discretized values into common matrix or
#' data frame. The data-frame method conducts normalisation and/or computes
#' normalized point-estimates and respective confidence intervals for
#' user-defined experimental groups. It is mainly a helper function for
#' \code{\link{ci_plot}}.
#'
#' @param object \code{\link{OPMS}} object or data frame with one column named
#'   as indicated by \code{split.at} (default given by
#'   \code{\link{param_names}("split.at")}), columns with factor variables
#'   before that column and columns with numeric vectors after that column.
#' @param as.labels List, character vector or formula indicating the metadata to
#'   be joined and used as row names (if \code{dataframe} is \code{FALSE}) or
#'   additional columns (if otherwise). Ignored if \code{NULL}.
#'
#'   If a \code{as.labels} is a formula and \code{dataframe} is \code{TRUE}, the
#'   pseudo-function \code{J} within the formula can be used to trigger
#'   combination of factors immediately after selecting them as data-frame
#'   columns, much like \code{as.groups}.
#'
#' @param subset Character vector. The parameter(s) to put in the matrix. If it
#'   is \sQuote{disc}, discretized data are returned, and \code{ci} is ignored.
#' @param ci Logical scalar. Also return the confidence intervals?
#' @param trim Character scalar. See \code{\link{aggregated}} for details.
#' @param dataframe Logical scalar. Return data frame or matrix?
#'
#' @param as.groups For the \code{\link{OPMS}} method, a list, character vector
#'   or formula indicating the metadata to be joined and either used as
#'   \sQuote{row.groups} attribute of the output matrix or as additional columns
#'   of the output data frame. See \code{\link{heat_map}} for its usage. Ignored
#'   if empty.
#'
#'   If a \code{as.groups} is a formula and \code{dataframe} is \code{TRUE}, the
#'   pseudo-function \code{J} within the formula can be used to trigger
#'   combination of factors immediately after selecting them as data-frame
#'   columns, much like \code{as.labels}.
#'
#'   If \code{as.groups} is a logical scalar, \code{TRUE} yields a trivial group
#'   that contains all elements, \code{FALSE} yields one group per element, and
#'   \code{NA} yields an error. The column name in which this factor is placed
#'   if \code{dataframe} is \code{TRUE} is determined using
#'   \code{opm_opt("group.name")}.
#'
#'   For the data-frame method, a logical, character or numeric vector
#'   indicating according to which columns (before the \code{split.at} column)
#'   the data should be aggregated by calculating means and confidence
#'   intervals. If \code{FALSE}, such an aggregation does not take place. If
#'   \code{TRUE}, all those columns are used for grouping.
#' @param sep Character scalar. See \code{\link{extract_columns}}.
#' @param dups Character scalar. See \code{\link{extract_columns}}. For the
#'   data-frame method, a character scalar defining the action to conduct if
#'   \code{as.groups} contains duplicates.
#'
#' @param exact Logical scalar. See \code{\link{extract_columns}}.
#' @param strict Logical scalar. See \code{\link{extract_columns}}.
#'
#' @param full Logical scalar indicating whether full substrate names shall be
#'   used. This is passed to \code{\link{wells}}, but in contrast to what
#'   \code{\link{flatten}} is doing the argument here refers to the generation
#'   of the column names.
#' @param max Numeric scalar. Passed to \code{\link{wells}}.
#' @param ... Optional other arguments passed to \code{\link{wells}}.
#'
#' @param norm.per Character scalar indicating the presence and direction of a
#'   normalization step.
#'   \describe{
#'   \item{none}{No normalization.}
#'   \item{row}{Normalization per row. By default, this would subtract the mean
#'     of each plate from each of its values (over all wells of that plate).}
#'   \item{column}{Normalization per column. By default, this would subtract the
#'     mean of each well from each of its values (over all plates in which this
#'     well is present).}
#'   }
#'   This step can further by modified by the next three arguments.
#' @param norm.by Vector indicating which wells (columns) or plates (rows) are
#'   used to calculate means used for the normalization. By default, the mean is
#'   calculated over all rows or columns if normalization is requested using
#'   \code{norm.per}. But if \code{direct} is \code{TRUE}, \code{norm.by} is
#'   directly interpreted as numeric vector used for normalization.
#' @param direct Logical scalar indicating how to use \code{norm.by}. See there
#'   for details.
#' @param subtract Logical scalar indicating whether normalization (if any) is
#'   done by subtracting or dividing.
#' @param split.at Character vector defining alternative names of the column at
#'   which the data frame shall be divided. Exactly one must match.
#'
#' @export
#' @return Numeric matrix or data frame; always a data frame for the data-frame
#'   method with the same column structure as \code{object} and, if grouping was
#'   used, a triplet structure of the rows, as indicated in the new
#'   \code{split.at} column: (i) group mean, (ii) lower and (iii) upper boundary
#'   of the group confidence interval. The data could then be visualized using
#'   \code{\link{ci_plot}}. See the examples.
#'
#' @family conversion-functions
#' @author Lea A.I. Vaas, Markus Goeker
#' @seealso \code{\link{aggregated}} for the extraction of aggregated values
#'   from a single \code{OPMA} objects.
#'
#'   boot::norm
#'   base::data.frame base::as.data.frame base::matrix base::as.matrix
#' @keywords manip dplot htest
#' @examples
#'
#' ## 'OPMS' method
#' data(vaas_4)
#' opm_opt("curve.param") # default parameter
#'
#' # generate matrix (containing the parameter given above)
#' (x <- extract(vaas_4, as.labels = list("Species", "Strain")))[, 1:3]
#' stopifnot(is.matrix(x), dim(x) == c(4, 96), is.numeric(x))
#' # using a formula also works
#' (y <- extract(vaas_4, as.labels = ~ Species + Strain))[, 1:3]
#' stopifnot(identical(x, y))
#'
#' # generate data frame
#' (x <- extract(vaas_4, as.labels = list("Species", "Strain"),
#'   dataframe = TRUE))[, 1:3]
#' stopifnot(is.data.frame(x), dim(x) == c(4, 99))
#' # using a formula
#' (y <- extract(vaas_4, as.labels = ~ Species + Strain,
#'   dataframe = TRUE))[, 1:3]
#' stopifnot(identical(x, y))
#' # using a formula, with joining into new columns
#' (y <- extract(vaas_4, as.labels = ~ J(Species + Strain),
#'   dataframe = TRUE))[, 1:3]
#' stopifnot(identical(x, y[, -3]))
#'
#' # put all parameters in a single data frame
#' x <- lapply(param_names(), function(name) extract(vaas_4, subset = name,
#'   as.labels = list("Species", "Strain"), dataframe = TRUE))
#' x <- do.call(rbind, x)
#'
#' # get discretized data
#' (x <- extract(vaas_4, subset = "disc", as.labels = list("Strain")))[, 1:3]
#' stopifnot(is.matrix(x), identical(dim(x), c(4L, 96L)), is.logical(x))
#'
#' ## data-frame method
#'
#' # extract data from OPMS-object as primary data frame
#' # second call to extract() then applied to this one
#' (x <- extract(vaas_4, as.labels = list("Species", "Strain"),
#'   dataframe = TRUE))[, 1:3]
#'
#' # no normalisation, but grouping for 'Species'
#' y <- extract(x, as.groups = "Species",  norm.per = "none")
#' # plotting using ci_plot()
#' ci_plot(y[, c(1:6, 12)], legend.field = NULL, x = 350, y = 1)
#'
#' # normalisation by plate means
#' y <- extract(x, as.groups = "Species",  norm.per = "row")
#' # plotting using ci_plot()
#' ci_plot(y[, c(1:6, 12)], legend.field = NULL, x = 130, y = 1)
#'
#' # normalisation by well means
#' y <- extract(x, as.groups = "Species",  norm.per = "column")
#' # plotting using ci_plot()
#' ci_plot(y[, c(1:6, 12)], legend.field = NULL, x = 20, y = 1)
#'
#' # normalisation by subtraction of the well means of well A10 only
#' y <- extract(x, as.groups = "Species",  norm.per = "row", norm.by = 10,
#'   subtract = TRUE)
#' # plotting using ci_plot()
#' ci_plot(y[, c(1:6, 12)], legend.field = NULL, x = 0, y = 0)
#'
setGeneric("extract", function(object, ...) standardGeneric("extract"))

setMethod("extract", OPMS, function(object, as.labels,
    subset = opm_opt("curve.param"), ci = FALSE, trim = "full",
    dataframe = FALSE, as.groups = NULL, sep = " ", dups = "warn",
    exact = TRUE, strict = TRUE, full = TRUE, max = 10000L, ...) {

  do_extract <- function(what, join, dups = "ignore") {
    extract_columns(object, what = what, join = join, sep = sep, dups = dups,
      exact = exact, strict = strict)
  }
  create_groups <- function(x, join, ci) {
    numeric_groups <- function(how) {
      if (L(how))
        rep.int(1L, length(object))
      else
        seq_len(length(object))
    }
    if (join) {
      result <- if (is.logical(x))
        numeric_groups(x)
      else
        do_extract(x, join = TRUE)
      result <- as.factor(result)
      if (ci)
        result <- rep(result, each = 3L)
    } else {
      if (is.logical(x)) {
        result <- as.data.frame(numeric_groups(x))
        rownames(result) <- get("group.name", OPM_OPTIONS)
      } else
        result <- do_extract(x, join = FALSE)
      if (ci)
        result <- result[rep(seq_len(nrow(result)), each = 3L), , drop = FALSE]
    }
    result
  }

  # Collect parameters in a matrix
  subset <- match.arg(subset, c(unlist(map_grofit_names(plain = TRUE)), "disc"))
  if (subset == "disc") {
    ci <- FALSE
    result <- discretized(object)
  } else {
    result <- do.call(rbind, lapply(object@plates, FUN = aggregated,
      subset = subset, ci = ci, trim = trim))
  }
  colnames(result) <- wells(object, full = full, max = max, ...)

  if (dataframe) {

    result <- as.data.frame(result)
    if (length(as.labels)) {
      columns <- do_extract(as.labels, join = FALSE)
      if (ci)
        columns <- columns[rep(seq_len(nrow(columns)), each = 3L), ,
          drop = FALSE]
      columns <- cbind(columns, rownames(result))
      colnames(columns)[ncol(columns)] <- RESERVED_NAMES[["parameter"]]
      rownames(result) <- rownames(columns) # otherwise a warning is likely
      result <- cbind(columns, result)
    } else {
      params <- rownames(result)
      rownames(result) <- seq_len(nrow(result))
      result <- cbind(params, result)
      colnames(result)[1L] <- RESERVED_NAMES[["parameter"]]
    }
    if (length(as.groups))
      result <- cbind(result, create_groups(as.groups, FALSE, ci))

  } else {

    if (length(as.labels)) {
      labels <- do_extract(as.labels, join = TRUE, dups = dups)
      rownames(result) <- if (ci)
        paste(rep(labels, each = 3L), rownames(result))
      else
        labels
    } else {
      rownames(result) <- if (ci)
        paste(rownames(result), rep(seq_len(nrow(result) / 3L), each = 3L),
          sep = sep)
      else
        seq_len(nrow(result))
    }
    if (length(as.groups))
      attr(result, "row.groups") <- create_groups(as.groups, TRUE, ci)
  }

  result

}, sealed = SEALED)

setMethod("extract", "data.frame", function(object, as.groups = TRUE,
    norm.per = c("row", "column", "none"), norm.by = TRUE, subtract = TRUE,
    direct = inherits(norm.by, "AsIs"), dups = c("warn", "error", "ignore"),
    split.at = param_names("split.at")) {

  do_norm <- function(x, row, by, direct, subtract) sweep(x, 2L - row,
    if (direct)
      by
    else if (row)
      rowMeans(x[, by, drop = FALSE])
    else
      colMeans(x[by, , drop = FALSE]), if (subtract)
      "-"
    else
      "/"
  )

  LL(subtract, direct)
  param.pos <- assert_splittable_matrix(object, split.at)

  num.pos <- seq.int(param.pos + 1L, ncol(object))
  case(match.arg(norm.per), # compute the normalisation if requested
    none = NULL,
    row = object[, num.pos] <- do_norm(object[, num.pos, drop = FALSE],
      TRUE, norm.by, direct, subtract),
    column = object[, num.pos] <- do_norm(object[, num.pos, drop = FALSE],
      FALSE, norm.by, direct, subtract)
  )

  if (!length(as.groups) || identical(c(as.groups), FALSE))
    return(object)

  # make list or vector from the grouping columns and note its length
  # metadata_key() enables lists to be passed as used for selecting metadata
  as.groups <- metadata_key(as.groups, FALSE)
  if (!is.logical(as.groups) && anyDuplicated(as.groups))
    case(match.arg(dups), ignore = as.null, warn = warning, error = stop)(
      "duplicated grouping values")
  as.groups <- unclass(object[, seq_len(param.pos - 1L), drop = FALSE][,
    as.groups, drop = FALSE])
  gl <- length(as.groups)

  # compute the means and CIs with respect to the stated grouping
  aggr.mean <- aggregate(object[, num.pos, drop = FALSE], by = as.groups,
    FUN = mean)
  aggr.CI <- aggregate(object[, num.pos, drop = FALSE], by = as.groups,
    FUN = var) # first the variances

  # The output has to be organized in a certain structure, three rows per group:
  # first the mean, second the lower CI limit third the upper CI limit. This
  # step creates the factor-data part up to the parameter column.
  result <- as.data.frame(sapply(aggr.mean[, seq_len(gl), drop = FALSE],
    rep, each = 3L))
  colnames(result) <- names(as.groups)
  result[, RESERVED_NAMES[["parameter"]]] <- as.factor(unlist(map_grofit_names(
    subset = as.character(object[1L, param.pos]), ci = TRUE)))

  # Reduce to numeric part and get CIs from means and variances.
  aggr.mean <- as.matrix(aggr.mean[, seq.int(gl + 1L, ncol(aggr.mean)),
    drop = FALSE])
  aggr.CI <- norm.ci(t0 = aggr.mean,
    var.t0 = aggr.CI[, seq.int(gl + 1L, ncol(aggr.CI)), drop = FALSE])
  aggr.CI <- as.matrix(aggr.CI[, -1L, drop = FALSE]) # remove the 'conf' column

  # Prepare the numerical part of the results.
  output <- matrix(ncol = 3L * nrow(aggr.mean), nrow = ncol(aggr.mean))
  pos.1 <- ncol(aggr.CI)
  pos.2 <- seq.int(pos.1 / 2L + 1L, pos.1)
  pos.1 <- seq.int(pos.1 / 2L)
  for (i in seq_len(nrow(aggr.mean)))
    output[, seq.int(i * 3L - 2L, 3L * i)] <- c(aggr.mean[i, , drop = TRUE],
      aggr.CI[i, pos.1, drop = TRUE], aggr.CI[i, pos.2, drop = TRUE])
  output <- t(output)
  colnames(output) <- colnames(aggr.mean)

  # Done.
  cbind(result, output)
}, sealed = SEALED)


################################################################################


#' Create data frame
#'
#' These methods create a data frame from aggregated and discretized values in
#' a manner distinct from \code{\link{extract}}.
#'
#' @param x Object of class \code{\link{OPM}}, its child classes, or
#'   \code{\link{OPMS}}. If an \code{\link{OPMS}} object, its elements must
#'   either all be \code{\link{OPM}} or all be \code{\link{OPMA}} or all be
#'   \code{\link{OPMD}} objects.
#' @param row.names Optional vector for use as row names of the resulting data
#'   frame. Here, it is not recommended to try to set row names explicitly.
#' @param optional Logical scalar passed to the list and matrix methods of
#'   \code{as.data.frame}.
#' @param sep Character scalar used as word separator in column names.
#' @param ... Optional arguments passed to the list and matrix methods
#'   of \code{as.data.frame}.
#' @param stringsAsFactors Logical scalar passed to these methods.
#' @return Data frame with one row for each combination of well and plate.
#' @details This function is mainly intended to produce objects that can easily
#'   be written to \acronym{CSV} files, for instance using \code{write.table}
#'   from the \pkg{utils} package. There are no \pkg{opm} methods other than
#'   \code{\link{batch_opm}} (which can write such files) that make use of the
#'   created kind of objects.
#'
#'   The following entries are contained in the generated data frame:
#'   \itemize{
#'   \item The \code{\link{csv_data}} entries that identify the plate.
#'   \item The names of the wells.
#'   \item For \code{\link{OPMA}} objects (and \code{\link{OPMS}} objects that
#'   contain them), the aggregated data (curve parameters), one column for each
#'   point estimate, upper and lower confidence interval of each parameter.
#'   \item For \code{\link{OPMA}} objects (and \code{\link{OPMS}} objects that
#'   contain them), the used aggregation settings, one column per entry, except
#'   for the \sQuote{options} entry (which is not a scalar).
#'   \item For \code{\link{OPMD}} objects (and \code{\link{OPMS}} objects that
#'   contain them), one column with the discretized data.
#'   \item For \code{\link{OPMD}} objects (and \code{\link{OPMS}} objects that
#'   contain them), the used discretization settings, one column per entry,
#'   except for the \sQuote{options} entry (which is not a scalar).
#'   }
#'
#'   The limits of using \acronym{CSV} as output format already show up in this
#'   list, and in general we recommend to generate \acronym{YAML} or
#'   \acronym{JSON} output instead.
#' @export
#' @family conversion-functions
#' @seealso utils::write.table
#' @keywords manip
#' @examples
#' ## OPMD method
#' data(vaas_1)
#' summary(x <- as.data.frame(vaas_1))
#' stopifnot(is.data.frame(x), nrow(x) == 96)
#'
#' ## OPMS method
#' data(vaas_4)
#' summary(x <- as.data.frame(vaas_4))
#' stopifnot(is.data.frame(x), nrow(x) == 96 * 4)
#'
setGeneric("as.data.frame")

setMethod("as.data.frame", OPM, function(x, row.names = NULL,
    optional = FALSE, sep = "_", ...,
    stringsAsFactors = default.stringsAsFactors()) {
  result <- cbind(as.data.frame(as.list(x@csv_data[CSV_NAMES]), NULL, optional,
    ..., stringsAsFactors = stringsAsFactors), Well = wells(x))
  rownames(result) <- row.names
  colnames(result) <- gsub("\\W+", sep, colnames(result), FALSE, TRUE)
  result
}, sealed = SEALED)

setMethod("as.data.frame", OPMA, function(x, row.names = NULL,
    optional = FALSE, sep = "_", ...,
    stringsAsFactors = default.stringsAsFactors()) {
  result <- as.data.frame(t(x@aggregated), NULL, optional, ...,
    stringsAsFactors = stringsAsFactors)
  colnames(result) <- gsub("\\W+", sep, colnames(result), FALSE, TRUE)
  result <- cbind(callNextMethod(x, row.names, optional, sep, ...,
    stringsAsFactors = stringsAsFactors), result)
  settings <- x@aggr_settings[c(SOFTWARE, VERSION, METHOD)]
  names(settings) <- paste("Aggr", names(settings), sep = sep)
  cbind(result, as.data.frame(settings, NULL, optional, ...,
    stringsAsFactors = stringsAsFactors))
}, sealed = SEALED)

setMethod("as.data.frame", OPMD, function(x, row.names = NULL,
    optional = FALSE, sep = "_", ...,
    stringsAsFactors = default.stringsAsFactors()) {
  result <- callNextMethod(x, row.names, optional, sep, ...,
    stringsAsFactors = stringsAsFactors)
  result$Discretized <- x@discretized
  settings <- x@disc_settings[c(SOFTWARE, VERSION, METHOD)]
  names(settings) <- paste("Disc", names(settings), sep = sep)
  cbind(result, as.data.frame(settings, NULL, optional, ...,
    stringsAsFactors = stringsAsFactors))
}, sealed = SEALED)

setMethod("as.data.frame", OPMS, function(x, row.names = NULL,
    optional = FALSE, sep = "_", ...,
    stringsAsFactors = default.stringsAsFactors()) {
  if (!length(row.names))
    row.names <- vector("list", length(x@plates))
  do.call(rbind, mapply(as.data.frame, x@plates, row.names, SIMPLIFY = FALSE,
    MoreArgs = list(optional = optional, sep = sep, ...,
    stringsAsFactors = stringsAsFactors), USE.NAMES = FALSE))
}, sealed = SEALED)


################################################################################
################################################################################
#
# YAML functions
#


#' Convert to YAML
#'
#' Convert some \R object to \acronym{YAML}. If the package \pkg{yaml} is not
#' installed, a call of this function will result in an error.
#'
#' @param object Object of one of the classes belonging to
#'   \code{\link{YAML_VIA_LIST}}.
#' @param sep Logical scalar. Prepend \acronym{YAML} document separator
#'   \verb{---}?
#' @param line.sep Character scalar used as output line separator.
#' @param json Logical scalar. Create \acronym{JSON} instead of \acronym{YAML}?
#'   If so, \code{sep}, \code{line.sep} and \code{...} are ignored.
#' @param listify Logical scalar indicating whether after conversion to a list
#'   its non-list elements should be converted to lists if they have names.
#'   (Names of named vector are \strong{not} conserved by default in output
#'   \acronym{YAML}).
#' @param ... Optional other arguments passed to \code{as.yaml} from the
#'   \pkg{yaml} package.
#' @export
#' @return Character scalar (\acronym{YAML} string).
#' @family conversion-functions
#' @keywords character IO
#' @references \url{http://www.yaml.org/}
#' @details \acronym{YAML} is a useful data-serialization standard that is
#'   understood by many programming languages. It is particularly more human
#'   readable than \acronym{XML}, and vector-like data structures (such as
#'   Phenotype MicroArray measurements) can be much more compactly encoded.
#'
#'   Many PM datasets at once can be batch-converted into \acronym{YAML} format
#'   using \code{\link{batch_opm}}. The output format for the child
#'   classes is described in detail there, as well as other aspects relevant in
#'   practice.
#'
#'   \acronym{JSON} is a subset of \acronym{YAML} and (in most cases) can also
#'   be parsed by a \acronym{YAML} parser. For generating \acronym{JSON}, the
#'   \code{toJSON} function from the \pkg{rjson} package would be used.
#'
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
    line.sep = "\n", json = FALSE, listify = FALSE, ...) {
  to_map <- function(items) if (is.null(names(items)))
    items
  else
    as.list(items)
  LL(sep, line.sep, json, listify)
  object <- as(object, "list")
  if (listify)
    object <- rapply(object, to_map, "ANY", NULL, "replace")
  if (json) {
    result <- toJSON(object, "C")
  } else {
    result <- as.yaml(x = object, line.sep = line.sep, ...)
    if (sep)
      result <- sprintf(sprintf("---%s%%s%s", line.sep, line.sep), result)
  }
  result
}, sealed = SEALED)


################################################################################
