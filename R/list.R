

################################################################################
################################################################################
#
# Conversions with as()
#


setAs(from = "list", to = OPM, function(from) {
  convert_measurements <- function(mat) {
    mat <- must(do.call(cbind, lapply(mat, as.numeric)))
    if (length(hour.pos <- which(colnames(mat) == HOUR)) != 1L)
      stop("uninterpretable column names in 'measurements' entry of ",
        "input list")
    sorted.names <- c(colnames(mat)[hour.pos], sort(colnames(mat)[-hour.pos]))
    mat[, sorted.names, drop = FALSE]
  }
  new(OPM, csv_data = unlist(from$csv_data),
    measurements = convert_measurements(from$measurements),
    metadata = as.list(from$metadata))
})


setAs(from = "list", to = OPMA, function(from) {
  convert_aggregated <- function(mat) {
    mat <- repair_na_strings(mat)
    mat <- as.matrix(as.data.frame(lapply(mat, unlist)))
    mat[, sort(colnames(mat)), drop = FALSE]
  }
  opm <- as(from, OPM)
  settings <- as.list(from$aggr_settings)
  mat <- convert_aggregated(from$aggregated)
  if (length(program <- settings[[PROGRAM]]) != 1L)
    stop("need single list entry called ", PROGRAM)
  if (program %in% KNOWN_PROGRAMS)
    mat <- mat[unlist(map_grofit_names()), , drop = FALSE]
  else
    warning("unknown aggregation program '", program,
      "' -- you might experience problems")
  new(OPMA, csv_data = csv_data(opm), measurements = measurements(opm),
    metadata = metadata(opm), aggr_settings = settings, aggregated = mat)
})


setAs(from = "list", to = OPMS, function(from) {
  opma.slots <- setdiff(slotNames(OPMA), slotNames(OPM))
  new(OPMS, plates = lapply(from, FUN = function(sublist) {
    as(sublist, if (all(opma.slots %in% names(sublist)))
      OPMA
    else
      OPM)
  }))
})


################################################################################
################################################################################
#
# Conditional conversions to OPMS
#


setGeneric("to_opm_list", function(object, ...) standardGeneric("to_opm_list"))
#' Convert to OPM list
#'
#' Convert to list of \code{\link{OPM}} objects. Used for building an
#' \code{\link{OPMS}} object. This method is used by \code{\link{opms}} and
#' \code{\link{try_opms}}.
#'
#' @param object List of objects that can be passed to \code{\link{opms}}
#' @param precomputed Logical scalar. See \code{\link{opms}}.
#' @param skip Logical scalar. See \code{\link{opms}}.
#' @param group Logical scalar. See \code{\link{opms}}.
#' @return List.
#' @keywords internal
#'
setMethod("to_opm_list", "list", function(object, precomputed = TRUE,
    skip = FALSE, group = FALSE) {
  assert_length(precomputed, skip, group)
  opma.slots <- setdiff(slotNames(OPMA), opm.slots <- slotNames(OPM))
  convert_recursively <- function(item) {
    if (!is.list(item))
      if (skip)
        return(NULL)
      else
        stop("non-list element encountered")
    keys <- names(item)
    if (all(opm.slots %in% keys))
      as(item, if (all(opma.slots %in% keys))
        OPMA
      else
        OPM)
    else
      lapply(item, FUN = convert_recursively)
  }
  get_plates <- function(item) {
    if (is(item, OPM))
      item
    else if (is(item, OPMS))
      plates(item)
    else if (skip)
      NULL
    else
      stop("need object derived from ", OPM, " or ", OPMS)
  }
  result <- if (precomputed)
    rapply(object, f = get_plates, how = "unlist")
  else
    c(convert_recursively(object), recursive = TRUE)
  if (group)
    result <- split(result, sapply(result, plate_type))
  result
}, sealed = SEALED)


################################################################################


setGeneric("try_opms", function(object, ...) standardGeneric("try_opms"))
#' Convert list to OPMS
#'
#' Conditionally convert a list to an \code{\link{OPMS}} object. This method
#' is used by \code{\link{c}}.
#'
#' @param object List.
#' @param precomputed Logical scalar. See \code{\link{opms}}.
#' @param skip Logical scalar. See \code{\link{opms}}.
#' @return \code{\link{OPMS}} object (if conversions was successful) or just
#'   the input \code{object} (if conversions was unsuccessful).
#' @keywords internal
#'
setMethod("try_opms", "list", function(object, precomputed = TRUE,
    skip = FALSE) {
  tryCatch(new(OPMS, plates = to_opm_list(object, precomputed = precomputed,
    skip = skip, group = FALSE)), error = function(e) object)
}, sealed = SEALED)


################################################################################


## Not an S4 method for flexibility regarding its first argument

#' OPMS constructor
#'
#' Easily build \code{\link{OPMS}} objects.
#'
#' @rdname opms.function
#'
#' @param ... One to several objects which are either potentially nested lists
#'   of \code{\link{OPMS}}, \code{\link{OPM}} or \code{\link{OPMA}} objects, or
#'   really nested lists whose sublists can be converted to an \code{\link{OPM}}
#'   or \code{\link{OPMA}} object.
#' @param precomputed Logical scalar. If \code{TRUE}, sublists have already been
#'   converted to one of the three classes. Otherwise, suitable sublists will be
#'   converted.
#' @param skip Logical scalar. If \code{precomputed} is \code{TRUE}, silently
#'   skip non-list elements of nested lists? If \code{precomputed} is
#'   \code{FALSE}, silently skip objects that do not belong to the three
#'   target classes? Otherwise, an error is generated if such a list element
#'   is encountered.
#' @param group Logical or character scalar. If \code{TRUE}, split the list of
#'   collected \code{\link{OPM}} objects according to the plate type and
#'   convert the sublists seperately if they contain more than one plate;
#'   otherwise just keep the \code{\link{OPM}} object. \code{FALSE} is the
#'   default: all plates are tried to be forced into a single
#'   \code{\link{OPMS}} object. If a character scalar, the name of the plate
#'   type to be extracted.
#' @export
#' @return \code{\link{OPMS}} object, or list of such objects (and/or
#'   \code{\link{OPM}} objects), or \code{\link{OPM}} object, or \code{NULL}.
#' @family combination-functions
#' @keywords manip
#' @details While otherwise rather flexible, this function will fail to return
#'   an \code{\link{OPMS}} object if the plate types do not match (simply
#'   because such \code{\link{OPMS}} objects are disallowed) and \code{group}
#'   is set to \code{FALSE}. But if \code{group} is set to \code{TRUE}, a
#'   list, not a single \code{\link{OPMS}} object will be returned; and if
#'   \code{group} is of mode \sQuote{character}, this extracts the plate type(s)
#'   of interest.
#' @note Consider also the plate-type selection options of
#'   \code{\link{read_opm}}.
#' @examples
#' # Testing distinct OPM/OPMS combinations -- all should work
#' data(vaas_1, vaas_4)
#' (x <- opms())
#' stopifnot(is.null(x))
#' summary((x <- opms(vaas_1)))
#' stopifnot(identical(x, vaas_1))
#' summary((x <- opms(vaas_4, group = plate_type(vaas_4))))
#' stopifnot(identical(x, vaas_4))
#' summary((x <- opms(vaas_4, group = "PM01")))
#' stopifnot(is.null(x))
#' summary(x <- opms(vaas_1, vaas_1))
#' stopifnot(is(x, "OPMS"), length(x) == 2L)
#' summary(x <- opms(vaas_4, vaas_1))
#' stopifnot(is(x, "OPMS"), length(x) == 5L)
#' summary(x <- opms(vaas_1, vaas_4))
#' stopifnot(is(x, "OPMS"), length(x) == 5L)
#' summary(x <- opms(vaas_4, vaas_4))
#' stopifnot(is(x, "OPMS"), length(x) == 8L)
#'
opms <- function(..., precomputed = TRUE, skip = FALSE, group = FALSE) {
  opms_or_opm <- function(x)  {
    case(length(x), NULL, x[[1L]], new(OPMS, plates = x))
  }
  if (is.character(group)) {
    wanted <- group
    group <- TRUE
  } else {
    wanted <- NULL
    group <- as.logical(group)
  }
  result <- to_opm_list(list(...), precomputed = precomputed, skip = skip,
    group = group)
  if (is.null(wanted)) {
    if (group)
      lapply(result, opms_or_opm)
    else
      opms_or_opm(result)
  } else
    opms_or_opm(result[[wanted]])
}


################################################################################
################################################################################
#
# Mapping functions
#


setAs(from = "ANY", to = "factor", function(from) as.factor(from))
setAs(from = "ANY", to = "ordered", function(from) as.ordered(from))


################################################################################


setGeneric("prepare_class_names",
  function(object) standardGeneric("prepare_class_names"))
#' Prepare class names
#'
#' Ensure that a vector of class names contains only unique values and
#' \sQuote{character}. Reduce it to \sQuote{ANY} if \sQuote{ANY} is contained.
#' See \code{\link{map_values}} for a use.
#'
#' @param object Character vector.
#' @return Character vector.
#' @keywords internal
#'
setMethod("prepare_class_names", "character", function(object) {
  object <- unique(c("character", object))
  if ("ANY" %in% object)
    "ANY"
  else
    object
}, sealed = SEALED)


################################################################################


setGeneric("map_values",
  function(object, mapping, ...) standardGeneric("map_values"))
#' Map values
#'
#' Map \sQuote{character} data using another \sQuote{character} vector, or
#' recursively apply a mapping function to all \sQuote{character} values within
#' a list, or non-recursively to a data frame. Optionally coerce other data
#' types to \sQuote{character}; return remaining ones unchanged. It is also
#' possible to map between classes using coercion functions. For convenience in
#' programming, methods for the \sQuote{NULL} class are also available.
#'
#' @param object List (may be nested), data frame or character vector. If it
#'   has names, they are preserved. \code{NULL} can also be given and yields
#'   \code{NULL} or an empty named character vector (if \code{mapping} is
#'   missing). \code{object} may also belong to the virtual class
#'   \code{\link{MOA}}, comprising matrices and arrays.
#' @param mapping Character vector, function, formula, or missing.
#'   If a character vector used as a mapping from its names to its
#'   values. Values from \code{object} are searched for in the \code{names}
#'   attribute of \code{mapping}; those found are replaced by the
#'   corresponding values of \code{mapping}. 
#'   If \code{mapping} is missing, a
#'   character vector is returned (sorted and with duplicates removed) whose
#'   names are identical to the values. This eases the construction of mapping
#'   vectors specific for \code{object}. If \code{mapping} is missing, the
#'   \code{coerce} argument must be named. 
#'   \code{mapping} changes its usage
#'   if \code{coerce} is \code{TRUE}. For \code{\link{MOA}} objects, if
#'   \code{mapping} was a function, it would be applied to
#'   \code{object} after conversion with \code{as.vector}, and it would be
#'   attempted to add the original attributes (particularly important are
#'   \sQuote{dim} and \sQuote{dimnames} back to the result. For
#'   \code{\link{MOA}} objects, if \code{mapping} is the usual character
#'   vector, it then is used for mapping the \code{storage.mode}, not the
#'   \code{class} of \code{object}.
#'   \code{mapping} can also be a formula, it is then used to compute on lists.
#'   The see examples below.
#' @param coerce Character vector with the names of classes that are coerced to
#'   \sQuote{character} to allow the mapping. Other classes are returned
#'   unchanged. Note that the coerced data are \strong{not} converted back to
#'   their original data type. \sQuote{ANY} can be used to indicate that all
#'   classes will be considered.
#'   Alternatively, \code{coerce} can be \code{TRUE}. \code{mapping} is then
#'   interpreted as a mapping between the names of classes, and \code{as} from
#'   the \pkg{methods} package is used for conducting the requested coercions.
#'   Attempting an undefined coercion will result in an error.
#' @param ... Optional further arguments to \code{mapping} (if it is a
#'   function).
#' @export
#' @return List, data frame, character vector or \code{NULL}.
#' @seealso base::rapply base::list base::as.list methods::as base::class
#'   base::storage.mode base::as.vector
#' @family list-functions
#' @keywords manip list
#' @note This function is not normally directly called by an \pkg{opm} user
#'   because \code{\link{map_metadata}} is available.
#' @examples
#'
#' # Character/character method
#' map <- letters
#' names(map) <- rev(LETTERS)
#' (x <- map_values(LETTERS, map))
#' stopifnot(rev(x) == letters)
#'
#' # Character/missing method
#' (x <- map_values(letters))
#' stopifnot(x == letters, names(x) == letters)
#'
#' # Character/function method
#' x <- letters[1:4]
#' names(x) <- LETTERS[1:4]
#' (y <- map_values(x, function(z) sprintf("%s%s", z, z)))
#' stopifnot(names(y) == names(x), y != x)
#'
#' # List/character method
#' x <- list(a = 1:8, c = 9, d = 'x')
#' map <- c(a = "b", e = "f", x = "y")
#' (y <- map_values(x, map))
#' stopifnot(identical(x[1:2], y[1:2]), !identical(x[3], y[3]))
#' (y <- map_values(x, map, coerce = "integer"))
#' stopifnot(identical(x[2], y[2]), !identical(x[1], y[1]),
#'   !identical(x[3], y[3]))
#' (y <- map_values(x, map, coerce = TRUE))
#' stopifnot(identical(x, y))
#' (y <- map_values(x, c(numeric = "character"), coerce = TRUE))
#' stopifnot(identical(x[1], y[1]), !identical(x[2], y[2]),
#'   identical(x[3], y[3]))
#'
#' # List/function method
#' (y <- map_values(x, identity, coerce = "ANY"))
#' stopifnot(identical(x, y))
#' (y <- map_values(x, class, coerce = "ANY"))
#' stopifnot(sapply(y, class) == "character", names(y) == names(x))
#'
#' # List/missing method
#' (y <- map_values(x))
#' stopifnot(y == "x", names(y) == y)
#' (y <- map_values(x, coerce = "integer"))
#' stopifnot(length(y) == 9, names(y) == y)
#' (y <- map_values(x, coerce = c("integer", "numeric")))
#' stopifnot(length(y) == 10, names(y) == y)
#' (y <- map_values(x, coerce = "ANY")) # same effect
#' stopifnot(length(y) == 10, names(y) == y)
#' (y <- map_values(x, coerce = TRUE))
#' stopifnot(y == c("character", "integer", "numeric"), names(y) == y)
#'
#' # List/formula method
#' (y <- map_values(x, ~ a + c))
#' stopifnot(is.numeric(y), y == c(10:17))
#' (y <- map_values(x, b ~ a + c))
#' stopifnot(is.list(y), y$b == c(10:17))
#'
#' # List/formula method applied to a data frame
#' x <- data.frame(a = 1:5, b = 6:10)
#' (y <- map_values(x, c ~ a + b))
#' stopifnot(is.data.frame(y), dim(y) == c(5, 3))
#' (z <- map_values(x, ~ a + b))
#' stopifnot(identical(z, y$c))
#'
#' # Data frame/character method
#' x <- data.frame(a = 1:3, b = letters[1:3])
#' stopifnot(sapply(x, class) == c("integer", "factor"))
#' map <- c(a = "A", b = "B", c = "C", `1` = "5")
#' (y <- map_values(x, map))
#' stopifnot(identical(x, y))
#' (y <- map_values(x, map, coerce = "factor"))
#' stopifnot(!identical(x, y), y[[2]] == c("A", "B", "C"))
#' (y <- map_values(x, map, coerce = "ANY"))
#' stopifnot(y[[1]] == c("5", "2", "3"), y[[2]] == c("A", "B", "C"))
#' (y <- map_values(x, map, coerce = TRUE))
#' stopifnot(identical(x, y))
#' map <- c(factor = "character", integer = "complex")
#' (y <- map_values(x, map, coerce = TRUE))
#' stopifnot(sapply(y, class) == c("complex", "character"))
#'
#' # Data frame/function method
#' (y <- map_values(x, `+`, coerce = "integer", y = 1L))
#' stopifnot(y$a == x$a + 1L)
#' (y <- map_values(x, as.character, coerce = "factor"))
#' stopifnot(sapply(y, class) == c("integer", "character"))
#'
#' # Data frame/missing method
#' (y <- map_values(x))
#' stopifnot(is.character(y), length(y) == 0)
#' (y <- map_values(x, coerce = "factor"))
#' stopifnot(is.character(y), y == letters[1:3], names(y) == y)
#' (y <- map_values(x, coerce = "ANY"))
#' stopifnot(is.character(y), length(y) == 6, names(y) == y)
#' (y <- map_values(x, coerce = TRUE))
#' stopifnot(is.character(y), y == c("factor", "integer"), names(y) == y)
#'
#' # Matrix/character method
#' (x <- matrix(1:6, nrow = 2))
#' (y <- map_values(x, c(integer = "numeric"), coerce = TRUE))
#' stopifnot(storage.mode(x) != storage.mode(y))
#' (y <- map_values(x, c(`1` = "7"), coerce = "integer"))
#' stopifnot(is.character(y), y[-1] == x[-1])
#'
#' # Matrix/function method
#' (y <- map_values(x, identity))
#' stopifnot(identical(x, y))
#' (y <- map_values(x, `+`, y = 1)) # useless because '+' is directly available
#' stopifnot(dim(y) == dim(x), y == x + 1)
#'
#' # Matrix/missing method
#' (y <- map_values(x))
#' stopifnot(y == "integer", names(y) == y)
#' (y <- map_values(x, coerce = "ANY"))
#' stopifnot(is.character(y), y == 1:6, names(y) == y)
#'
#' # Factor/function method
#' x <- as.factor(c("a", "b", "a"))
#' (y <- map_values(x, toupper))
#' stopifnot(is.factor(y), y == toupper(x))
#'
#' # Factor/character method
#' (y <- map_values(x, c(b = "c", k = "h")))
#' stopifnot(is.factor(y), levels(y) == c("a", "c"))
#'
#' # Factor/missing method
#' (y <- map_values(x))
#' stopifnot(levels(x) == y, names(y) == y)
#'
setMethod("map_values", c("list", "character"), function(object, mapping,
    coerce = character()) {
  if (isTRUE(coerce)) {
    if (is.null(coerce <- names(mapping)))
      return(object)
    mapfun <- function(item) as(item, map_values(class(item), mapping))
  } else
    mapfun <- if (length(coerce) == 0L || all(coerce == "character"))
      function(item) map_values(item, mapping)
    else
      function(item) {
        result <- map_values(as.character(item), mapping)
        mostattributes(result) <- attributes(item)
        result
      }
  map_values(object, mapping = mapfun, coerce = coerce)
}, sealed = SEALED)

setMethod("map_values", c("list", "function"), function(object, mapping,
    coerce = character(), ...) {
  rapply(object = object, f = mapping, classes = prepare_class_names(coerce),
    how = "replace", ...)
}, sealed = SEALED)

setMethod("map_values", c("list", "missing"), function(object,
    coerce = character()) {
  if (isTRUE(coerce)) {
    classes <- "ANY"
    mapfun <- class
  } else {
    classes <- prepare_class_names(coerce)
    mapfun <- as.character
  }
  map_values(rapply(object, mapfun, classes = classes))
}, sealed = SEALED)

setMethod("map_values", c("list", "formula"), function(object, mapping) {
  if (length(mapping) > 2L) {
    object[[parse_formula_head(mapping[[2L]])]] <- eval(expr = mapping[[3L]], 
      envir = object)
    object
  } else
    eval(expr = mapping[[2L]], envir = object)
}, sealed = SEALED)

#-------------------------------------------------------------------------------

setMethod("map_values", c("data.frame", "function"), function(object, mapping,
    coerce = character(), ...) {
  if (identical("ANY", coerce <- prepare_class_names(coerce)))
    coerce <- unique(sapply(object, class))
  for (i in which(sapply(object, class) %in% coerce))
    object[[i]] <- mapping(object[[i]], ...)
  object
}, sealed = SEALED)

setMethod("map_values", c("data.frame", "character"), function(object, mapping,
    coerce = character()) {
  if (isTRUE(coerce)) {
    if (is.null(coerce <- names(mapping)))
      return(object)
    mapfun <- function(item) as(item, map_values(class(item), mapping))
  } else
    mapfun <- function(item) map_values(as.character(item), mapping)
  map_values(object, mapping = mapfun, coerce = coerce)
}, sealed = SEALED)

setMethod("map_values", c("data.frame", "missing"), function(object,
    coerce = character()) {
  if (isTRUE(coerce))
    result <- unlist(lapply(object, class))
  else {
    coerce <- prepare_class_names(coerce)
    if (!"ANY" %in% coerce)
      object <- object[, sapply(object, class) %in% coerce, drop = FALSE]
    result <- unlist(lapply(object, as.character))
  }
  map_values(result)
}, sealed = SEALED)

#-------------------------------------------------------------------------------
                                                               
setMethod("map_values", c(MOA, "character"), function(object, mapping,
    coerce = TRUE) {
  if (isTRUE(coerce)) {
    storage.mode(object) <- map_values(storage.mode(object), mapping)
    object
  } else {
    coerce <- prepare_class_names(coerce)
    if (!identical("ANY", coerce) && !storage.mode(object) %in% coerce)
      stop("storage mode of 'object' not contained in 'coerce'")
    result <- map_values(as.character(object), mapping)
    attributes(result) <- attributes(object)
    result
  }
}, sealed = SEALED)

setMethod("map_values", c(MOA, "missing"), function(object, coerce = TRUE) {
  if (isTRUE(coerce))
    result <- storage.mode(object)
  else {
    coerce <- prepare_class_names(coerce)
    if (!identical("ANY", coerce) && !storage.mode(object) %in% coerce)
      stop("storage mode of 'object' not contained in 'coerce'")
    result <- as.character(object)
  }
  map_values(result)
}, sealed = SEALED)

setMethod("map_values", c(MOA, "function"), function(object, mapping, ...) {
  result <- mapping(as.vector(object), ...)
  mostattributes(result) <- attributes(object)
  result
}, sealed = SEALED)

#-------------------------------------------------------------------------------
                                                               
setMethod("map_values", c("character", "function"), function(object, mapping,
    ...) {
  result <- mapping(object, ...)
  mostattributes(result) <- attributes(object)
  result
}, sealed = SEALED)

setMethod("map_values", c("character", "character"), function(object, mapping) {
  mapped <- match(object, names(mapping))
  object[found] <- mapping[mapped[found <- !is.na(mapped)]]
  object
}, sealed = SEALED)

setMethod("map_values", c("character", "missing"), function(object) {
  object <- sort(unique(object))
  structure(.Data = object, .Names = object)
}, sealed = SEALED)

#-------------------------------------------------------------------------------                                                               
                                                               
setMethod("map_values", c("factor", "function"), function(object, mapping,
    ...) {
  levels(object) <- map_values(levels(object), mapping, ...)
  object
}, sealed = SEALED)

setMethod("map_values", c("factor", "character"), function(object, mapping) {
  levels(object) <- map_values(levels(object), mapping)
  object
}, sealed = SEALED)

setMethod("map_values", c("factor", "missing"), function(object) {
  map_values(levels(object))
}, sealed = SEALED)

#-------------------------------------------------------------------------------
                                                               
setMethod("map_values", c("NULL", "function"), function(object, mapping, ...) {
  NULL
}, sealed = SEALED)

setMethod("map_values", c("NULL", "character"), function(object, mapping) {
  NULL
}, sealed = SEALED)

setMethod("map_values", c("NULL", "missing"), function(object, mapping) {
  map_values(character())
}, sealed = SEALED)


################################################################################


setGeneric("map_names",
  function(object, mapping, ...) standardGeneric("map_names"))
#' Map names
#'
#' Use a character vector or a function for recursively mapping list names, or
#' mapping the \sQuote{colnames} and \sQuote{rownames} attributes of a
#' data frame. In the case of lists, the
#' function is not applied to list elements which are not themselves lists,
#' even if they have a \sQuote{names} attribute. Such elements and their
#' names, if any, are returned unchanged. If a \sQuote{names}, \sQuote{colnames}
#' or \sQuote{rownames} attribute is \code{NULL}, it is ignored.
#' Alternatively, instead of mapping the names,
#' collect them and return them as a single character vector, sorted and
#' with duplicates removed. The collected names are added as their own
#' \code{names} attribute; this might be useful if the result is later on used
#' for some mapping (using this function or \code{\link{map_values}}).
#'
#' @param object Any R object. The default method applies the mapping to the
#'   \sQuote{names} attribute. The behaviour is special for lists, which are
#'   traversed recursively to also consider sublists with names. Data frames
#'   and \code{\link{MOA}} objects (that is, including matrices and arrays)
#'   are also treated specially because the \sQuote{dimnames} attribute, not
#'   the \sQuote{names} attribute is considered.
#' @param mapping Mapping function that takes a character vector as first
#'   argument, or character vector used for mapping from its names to its
#'   values, or missing. It is guaranteed that \code{NULL} input remains
#'   \code{NULL}, irrespective of the value of \code{mapping}.
#' @param ... Optional further arguments to \code{mapping} (if it is a
#'   function).
#' @return Character vector if \code{mapping} is missing, otherwise an R object
#'   of the same class than \code{object}.
#' @export
#' @family list-functions
#' @seealso base::rapply base::list base::as.list
#' @keywords manip list
#' @note This function is not normally directly called by an \pkg{opm} user
#'   because \code{\link{map_metadata}} is available.
#' @examples
#'
#' # List/function method
#' x <- list(a = 1:8, c = 9, d = 'x')
#' map <- function(x) sprintf("%s%s", x, x)
#' (y <- map_names(x, map))
#' stopifnot(identical(as.character(x), as.character(y)))
#' stopifnot(!identical(names(x), names(y)))
#'
#' # List/character method
#' x <- list(a = 1:8, c = 9, d = 'x')
#' map <- c(a = "b", e = "f", x = "y")
#' (y <- map_names(x, map))
#' stopifnot(identical(as.character(x), as.character(y)))
#' stopifnot(!identical(names(x), names(y)))
#' # compare with the map_values() example
#'
#' # List/missing method
#' x <- list(a = 1:8, c = 9, d = 'x')
#' (y <- map_names(x))
#' stopifnot(identical(as.vector(y), names(x)))
#' stopifnot(identical(names(y), names(x)))
#' # Now a recursive list
#' x <- list(a = 1:8, c = 9, d = list(d1 = 'x', d2 = 'y'))
#' (y <- map_names(x))
#' stopifnot(length(y) > length(names(x)))
#'
#' # Data frame/function method
#' x <- data.frame(a = 1:3, b = letters[1:3])
#' (y <- map_names(x, toupper))
#' stopifnot(identical(y[[1]], x[[1]]), identical(y[[2]], x[[2]]))
#' stopifnot(identical(names(y), c("A", "B")))
#'
#' # Data frame/character method
#' (y <- map_names(x, c(a = "b", b = "a")))
#' stopifnot(x == y, names(y) == c("b", "a"))
#'
#' # Data frame/missing method
#' (y <- map_names(x))
#' stopifnot(is.character(y), y == names(y), length(y) == 5)
#'
#' # Matrix/function method
#' x <- as.matrix(x)
#' (y <- map_names(x, toupper))
#' stopifnot(x == y, toupper(colnames(x)) == colnames(y))
#'
#' # Matrix/character method
#' (y <- map_names(x, c(a = "b", b = "a")))
#' stopifnot(x == y, colnames(y) == c("b", "a"))
#'
#' # Matrix/missing method
#' (y <- map_names(x))
#' stopifnot(y == c("a", "b"), names(y) == y)
#'
setMethod("map_names", c("list", "function"), function(object, mapping, ...) {
  map_names_recursively <- function(item) {
    if (is.list(item)) {
      names(item) <- map_values(names(item), mapping, ...)
      lapply(item, FUN = map_names_recursively)
    } else
      item
  }
  map_names_recursively(object)
}, sealed = SEALED)

setMethod("map_names", c("list", "character"), function(object, mapping) {
  map_names_recursively <- function(item) {
    if (is.list(item)) {
      names(item) <- map_values(names(item), mapping)
      lapply(item, FUN = map_names_recursively)
    } else
      item
  }
  map_names_recursively(object)
}, sealed = SEALED)

setMethod("map_names", c("list", "missing"), function(object) {
  get_names_recursively <- function(item) {
    if (is.list(item))
      c(names(item), unlist(lapply(item, FUN = get_names_recursively)))
    else
      character()
  }
  map_values(get_names_recursively(object))
}, sealed = SEALED)

#-------------------------------------------------------------------------------

setMethod("map_names", c("data.frame", "function"), function(object, mapping,
    ...) {
  dimnames(object) <- map_values(dimnames(object), mapping, ...)
  object
}, sealed = SEALED)

setMethod("map_names", c("data.frame", "character"), function(object, mapping) {
  dimnames(object) <- map_values(dimnames(object), mapping)
  object
}, sealed = SEALED)

setMethod("map_names", c("data.frame", "missing"), function(object) {
  map_values(dimnames(object))
}, sealed = SEALED)

#-------------------------------------------------------------------------------

setMethod("map_names", c(MOA, "function"), function(object, mapping, ...) {
  dimnames(object) <- map_values(dimnames(object), mapping, ...)
  object
}, sealed = SEALED)

setMethod("map_names", c(MOA, "character"), function(object, mapping) {
  dimnames(object) <- map_values(dimnames(object), mapping)
  object
}, sealed = SEALED)

setMethod("map_names", c(MOA, "missing"), function(object) {
  map_values(dimnames(object))
}, sealed = SEALED)

#-------------------------------------------------------------------------------

setMethod("map_names", c("ANY", "function"), function(object, mapping, ...) {
  names(object) <- map_values(names(object), mapping, ...)
  object
}, sealed = SEALED)

setMethod("map_names", c("ANY", "character"), function(object, mapping) {
  names(object) <- map_values(names(object), mapping)
  object
}, sealed = SEALED)

setMethod("map_names", c("ANY", "missing"), function(object) {
  map_values(names(object))
}, sealed = SEALED)


################################################################################
################################################################################
#
# Search functions
#


setGeneric("contains",
  function(object, other, ...) standardGeneric("contains"))
#' Query a list with a list
#'
#' Test whether all names of a query list occur as names in a data list and
#' optionally also whether they point to the same elements; apply this principle
#' recursively to all sublists. Non-list elements are ignored if \code{values}
#' is \code{FALSE}. Otherwise the comparison is done using \code{identical} if
#' \code{exact} is \code{TRUE}. If \code{exact} is \code{FALSE}, the value(s) in
#' the data list can be any of the values at the corresponding position in the
#' query list, and the comparison is done by coercion to character vectors.
#' An empty query list results in \code{TRUE}. Missing names in a non-empty
#' query list result in \code{FALSE}. There is also an \code{\link{OPMS}}
#' method, which tests whether an \code{\link{OPM}} object is contained.
#'
#' @param object List containing the data, or \code{\link{OPMS}} object.
#' @param other For the list method, a list used as query; for the 
#'   \code{\link{OPMS}} method, an \code{\link{OPM}} object used as query.
#' @param values Logical scalar. Compare also the values or only the keys? If
#'   \code{FALSE}, \code{exact} is ignored.
#' @param exact Logical scalar. If \code{FALSE}, the data value(s) might by
#'   any of the query value(s), and some coercion is done before comparing (see
#'   \code{match} for details. If \code{TRUE}, the data value(s) must exactly
#'   correspond to the query value(s), and no coercion is done (see
#'   \code{identical}) for details). This might be too strict for most
#'   applications.
#' @param ... Optional arguments passed to \code{identical} from the 
#'   \pkg{base} package, allowing for fine-control of identity. Has no effect
#'   unless \code{exact} is \code{TRUE}.
#' @export
#' @return Logical scalar.
#' @family list-functions
#' @seealso base::list base::as.list base::`[` base::`[[` base::match
#' @seealso base::identity
#' @keywords attribute list
#' @note This function is not normally directly called by an \pkg{opm} user but
#'   might be useful in other contexts. It forms the basis of a number of
#'   metadata query functions.
#' @examples
#'
#' # List/list method
#' x <- list(a = 1:8, c = 9, d = list(d1 = 'x', d2 = 'y'))
#' y <- list(a = 1:10, c = "9", d = list(d1 = "x"))
#' stopifnot(contains(x, y))
#' stopifnot(!contains(x, y, exact = TRUE))
#' stopifnot(contains(x, y, exact = TRUE, values = FALSE))
#' # see particularly infix-q and infix-k for more examples
#'
#' # OPMS/OPM method
#' data(vaas_4)
#' data(vaas_1)
#' stopifnot(contains(vaas_4, vaas_1))
#'
setMethod("contains", c("list", "list"), function(object, other,
    values = TRUE, exact = FALSE, ...) {
  query.keys <- names(other)
  if (length(query.keys) == 0L && length(other) > 0L)
    return(FALSE)
  found <- match(query.keys, names(object), incomparables = "")
  if (any(is.na(found)))
    return(FALSE)
  for (idx in seq_along(query.keys)) {
    query.subset <- other[[idx]]
    data.subset <- object[[found[idx]]]
    result <- if (is.list(query.subset)) {
      if (is.list(data.subset))
        Recall(object = data.subset, other = query.subset, values = values,
          exact = exact, ...)
      else if (values)
        FALSE
      else
        is.null(names(query.subset))
    } else if (values) {
      if (exact)
        identical(x = data.subset, y = query.subset, ...)
      else
        all(data.subset %in% query.subset)
    } else
      TRUE
    if (!result)
      return(FALSE)
  }
  TRUE
}, sealed = SEALED)

setMethod("contains", c(OPMS, OPM), function(object, other, ...) {
  for (plate in object@plates)
    if (identical(x = plate, y = other, ...))
      return(TRUE)
  FALSE
}, sealed = SEALED)


################################################################################
################################################################################
#
# YAML reparation
#


setGeneric("repair_na_strings",
  function(object, ...) standardGeneric("repair_na_strings"))
#' Repair NAs
#'
#' Replace \sQuote{NA} by \code{NA_character_}.
#' When reading YAML input previously output by R, \sQuote{NA} values cause
#' numeric vectors to be interpreted as character. This function fixes this
#' problem and also takes care of misinterpreted numbers in exponential
#' notation.
#'
#' @param object Character vector or list.
#' @param type Character scalar denoting the type to which input character
#'   vectors shall be tried to be converted.
#' @return Character vector or list.
#' @seealso utils::type.convert
#' @keywords internal
#' @references \url{http://www.yaml.org/}
#'
setMethod("repair_na_strings", "character", function(object) {
  object[grepl("^\\s*NA$", object, perl = TRUE)] <- NA_character_
  object
}, sealed = SEALED)

setMethod("repair_na_strings", "list", function(object,
    type = c("numeric", "integer", "complex")) {
  type <- match.arg(type)
  rapply(object, f = function(item) {
    tryCatch(as(repair_na_strings(item), type), warning = function(w) item)
  }, classes = "character", how = "replace")
}, sealed = SEALED)


################################################################################


setGeneric("repair_names",
  function(object, ...) standardGeneric("repair_names"))
#' Repair names
#'
#' Recursively repair all names of a list, i.e. ensure that \code{names}
#' applied to the list and its sublists, if any, returns either \code{NULL}
#' or a character vector not containing any empty string. This is useful when
#' outputting lists in YAML format because otherwise maps with empty keys
#' might be created, resulting in invalidly formatted, unparseable data.
#'
#' @param object List.
#' @param fill Logical scalar. If \code{TRUE}, fill empty-string names by list
#'   positions coerced to character mode. If \code{FALSE}, delete all names if
#'   at least one of them is the empty string.
#' @return List.
#' @keywords internal
#'
setMethod("repair_names", "list", function(object, fill = TRUE) {
  repair_fun <- if (fill)
    function(keys) {
      if (is.null(keys))
        return(keys)
      keys[empty] <- seq_along(keys)[empty <- !nzchar(keys)]
      keys
    }
  else
    function(keys) {
      if (is.null(keys) || any(!nzchar(keys)))
        NULL
      else
        keys
    }
  map_names(object, mapping = repair_fun)
}, sealed = SEALED)


################################################################################
################################################################################
#
# List traversal
#


setGeneric("traverse",
  function(object, func, ...) standardGeneric("traverse"))
#' Traverse a list with a function
#'
#' Apply a function to all list elements in turn, optionally in parallel using
#' the \pkg{multicore} package.
#'
#' @param object List.
#' @param func Function to apply to each element of the list.
#' @param cores Integer scalar. Number of cores to use. If more than one, a
#'   warning is issued if the \pkg{multicore} package is not available, and
#'   the number of cores is set back to 1.
#' @param ... Optional arguments to \code{lapply} or \code{mclapply} (can be
#'   arguments passed to \code{func}).
#' @return List.
#' @keywords internal
#'
setMethod("traverse", c("list", "function"), function(object, func, cores,
    ...) {
  assert_length(cores)
  if (cores > 1L &&
      !require(multicore, quietly = TRUE, warn.conflicts = FALSE)) {
    warning("'multicore' not available -- switching back to 1 core")
    cores <- 1L
  }
  if (cores > 1L)
    multicore::mclapply(X = object, FUN = func, mc.cores = cores, ...)
  else
    lapply(X = object, FUN = func, ...)
}, sealed = SEALED)


################################################################################
################################################################################
#
# List insertion
#


setGeneric("insert", function(object, ...) standardGeneric("insert"))
#' Insert a list in a list
#'
#' Insert all values from another list in a list, either by overwriting the
#' previously present data or by only setting the missing ones. Note that this
#' comparison is based on the names. It does not matter whether the values are
#' \code{NULL}.
#'
#' @param object List.
#' @param other R object to insert. List.
#' @param ... Optional other items to insert.
#' @param .force Logical scalar. Overwite items that are already there?
#' @return List.
#' @seealso utils::modifyList
#' @keywords internal
#'
setMethod("insert", "list", function(object, other, ..., .force = FALSE) {
  other <- if (missing(other))
    list(...)
  else if (is.list(other))
    c(other, list(...))
  else
    list(other, ...)
  keys <- names(other)
  if (!.force)
    keys <- setdiff(keys, names(object))
  object[keys] <- other[keys]
  object
}, sealed = SEALED)


################################################################################



