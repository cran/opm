

################################################################################
################################################################################
#
# Conversions with as()
#


setAs(from = "list", to = OPM, function(from) {
  convert_measurements <- function(mat) {
    mat <- do.call(cbind, lapply(mat, as.numeric))
    hour.pos <- which(colnames(mat) == HOUR)
    if (length(hour.pos) != 1L)
      stop(
        "uninterpretable column names in 'measurements' entry of input list")
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
    warning("Unknown aggregation program '", program, 
      "' -- you might experience problems.")
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
#' @param List the objects that can be passed to \code{\link{opms}}
#' @param precomputed Logical scalar. See \code{\link{opms}}.
#' @param skip Logical scalar. See \code{\link{opms}}.
#' @return List.
#' @keywords internal
#'
setMethod("to_opm_list", "list", function(object, precomputed = TRUE,
    skip = FALSE) {
  opm.slots <- slotNames(OPM)
  opma.slots <- setdiff(slotNames(OPMA), opm.slots)
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
    if (inherits(item, OPM))
      item
    else if (inherits(item, OPMS))
      plates(item)
    else if (skip)
      NULL
    else
      stop("need object derived from ", OPM, " or ", OPMS)
  }
  if (precomputed)
    rapply(object, f = get_plates, how = "unlist")
  else
    c(convert_recursively(object), recursive = TRUE)
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
    skip = skip)), error = function(e) object)
}, sealed = SEALED)


################################################################################


## Not an S4 method for flexibility regarding its first argument

#' OPMS constructor
#'
#' Easily build an \code{\link{OPMS}} object.
#'
#' @rdname opms-function
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
#' @export
#' @return \code{\link{OPMS}} object.
#' @family combination-functions
#' @keywords manip
#' @note While otherwise rather flexible, this function will fail to construct
#'   an \code{\link{OPMS}} object if the plate types do not match (simply
#'   because such \code{\link{OPMS}} objects are disallowed).
#' @examples
#' # Testing distinct OPM/OPMS combinations -- all should work
#' data(vaas_1, vaas_4)
#' x <- opms(vaas_1, vaas_1)
#' stopifnot(is(x, "OPMS"), length(x) == 2L)
#' x <- opms(vaas_4, vaas_1)
#' stopifnot(is(x, "OPMS"), length(x) == 5L)
#' x <- opms(vaas_1, vaas_4)
#' stopifnot(is(x, "OPMS"), length(x) == 5L)
#' x <- opms(vaas_4, vaas_4)
#' stopifnot(is(x, "OPMS"), length(x) == 8L)
#'
opms <- function(..., precomputed = TRUE, skip = FALSE) {
  new(OPMS, plates = to_opm_list(list(...), precomputed = precomputed,
    skip = skip))
}


################################################################################
################################################################################
#
# Mapping functions
#
  

setAs(from = "ANY", to = "factor", function(from) as.factor(from))
setAs(from = "ANY", to = "ordered", function(from) as.ordered(from))


################################################################################


setGeneric("map_values",
  function(object, mapping, ...) standardGeneric("map_values"))
#' Map values
#'
#' Map \sQuote{character} data using another \sQuote{character} vector, or
#' recursively apply a mapping function to all \sQuote{character} values within
#' a list, or non-recursively to a dataframe. Optionally coerce other data types
#' to \sQuote{character}; return remaining ones unchanged. It is also possible
#' to map between classes using coercion functions. For convenience in 
#' programming, methods for the \sQuote{NULL} class are also available.
#'
#' @param object List (may be nested), dataframe or character vector. If it has
#'   names, they are preserved. \code{NULL} can also be given and yields
#'   \code{NULL} or an empty named character vector (if \code{mapping} is 
#'   missing).
#' @param mapping Character vector used as a mapping from its names to its 
#'   values. Values from \code{object} are searched for in the \code{names}
#'   attribute of \code{mapping}; those found are replaced by the
#'   corresponding values of \code{mapping}. If \code{mapping} is missing, a 
#'   character vector is returned (sorted and with duplicates removed) whose
#'   names are identical to the values. This eases the construction of mapping
#'   vectors specific for \code{object}. If \code{mapping} is missing, the
#'   \code{coerce} argument must be named. \code{mapping} changes its usage
#'   if \code{coerce} is \code{TRUE}.
#' @param coerce Character vector with the names of classes that are coerced to
#'   \sQuote{character} to allow the mapping. Other classes are returned 
#'   unchanged. Note that the coerced data are \strong{not} converted back to
#'   their original data type. \sQuote{ANY} can be used to indicate that all
#'   classes will be considered.
#'   Alternatively, \code{coerce} can be \code{TRUE}. \code{mapping} is then
#'   interpreted as a mapping between the names of classes, and \code{as} from
#'   the \pkg{methods} package is used for conducting the requested coercions.
#'   Attempting an undefined coercion will result in an error.
#' @export
#' @return List, dataframe, character vector or \code{NULL}.
#' @seealso base::rapply base::list base::as.list methods::as
#' @family list-functions
#' @keywords manip list
#' @note This function is not normally directly called by an \pkg{opm} user 
#'   because \code{\link{map_metadata}} is available.
#' @examples
#'
#' # Character+character method
#' map <- letters
#' names(map) <- rev(LETTERS)
#' (x <- map_values(LETTERS, map))
#' stopifnot(rev(x) == letters)
#'
#' # Character+missing method
#' (x <- map_values(letters))
#' stopifnot(x == letters, names(x) == letters)
#'
#' # List+character method
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
#' # List+missing method
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
#' # Dataframe+character method
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
#' # Dataframe+missing method
#' (y <- map_values(x))
#' stopifnot(is.character(y), length(y) == 0)
#' (y <- map_values(x, coerce = "factor"))
#' stopifnot(is.character(y), y == letters[1:3], names(y) == y)
#' (y <- map_values(x, coerce = "ANY"))
#' stopifnot(is.character(y), length(y) == 6, names(y) == y)
#' (y <- map_values(x, coerce = TRUE))
#' stopifnot(is.character(y), y == c("factor", "integer"), names(y) == y)
#'
setMethod("map_values", c("list", "character"), function(object, mapping,
    coerce = character()) {
  if (isTRUE(coerce)) {
    if (is.null(classes <- names(mapping)))
      classes <- character()
    mapfun <- function(item, mapping) as(item, mapping[class(item)])
  } else {
    classes <- unique(c("character", coerce))
    if ("ANY" %in% classes)
      classes <- "ANY"
    mapfun <- if (length(classes) > 1L)
      function(item, mapping) { 
        # as.character() drops the names, hence structure()
        map_values(structure(as.character(item), names = names(item)), mapping)
      }
    else
      map_values
  }
  rapply(object, mapfun, classes = classes, how = "replace", mapping = mapping)
}, sealed = SEALED)

#' @export
#'    
setMethod("map_values", c("list", "missing"), function(object, 
    coerce = character()) {
  if (isTRUE(coerce)) {
    classes <- "ANY"
    mapfun <- class
  } else {
    classes <- unique(c("character", coerce))
    if ("ANY" %in% classes)
      classes <- "ANY"
    mapfun <- as.character
  }
  map_values(rapply(object, mapfun, classes = classes))
}, sealed = SEALED)

#' @export
#'    
setMethod("map_values", c("data.frame", "character"), function(object, mapping,
    coerce = character()) {
  if (isTRUE(coerce)) {
    if (is.null(classes <- names(mapping)))
      classes <- character()
    mapfun <- function(item, mapping) as(item, mapping[class(item)])
  } else {
    classes <- unique(c("character", coerce))
    if ("ANY" %in% classes)
      classes <- sapply(object, class)
    mapfun <- function(item, mapping) map_values(as.character(item), mapping)
  }
  for (i in which(sapply(object, class) %in% classes))
    object[[i]] <- mapfun(object[[i]], mapping)
  object
}, sealed = SEALED)
    
#' @export
#'    
setMethod("map_values", c("data.frame", "missing"), function(object, 
    coerce = character()) {
  if (isTRUE(coerce)) {
    result <- unlist(lapply(object, class))
  } else {
    classes <- unique(c("character", coerce))
    if (!"ANY" %in% classes)
      object <- object[, sapply(object, class) %in% classes, drop = FALSE]
    result <- unlist(lapply(object, as.character))
  }
  map_values(result)
}, sealed = SEALED)

#' @export
#'
setMethod("map_values", c("character", "character"), function(object, mapping) {
  mapped <- match(object, names(mapping))
  object[found] <- mapping[mapped[found <- !is.na(mapped)]]
  object  
}, sealed = SEALED)

#' @export
#'
setMethod("map_values", c("character", "missing"), function(object) {
  object <- sort(unique(object))
  structure(.Data = object, .Names = object)
}, sealed = SEALED)

#' @export
#'
setMethod("map_values", c("NULL", "character"), function(object, mapping) {
  NULL
}, sealed = SEALED)

#' @export
#'
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
#' dataframe. In the case of lists, the
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
#' @param object List.
#' @param mapping Mapping function that takes a character vector as first 
#'   argument, or character vector used for mapping from its names to its 
#'   values, or missing.
#' @param ... Optional further arguments to \code{mapping} (if it is a 
#'   function).
#' @return Character vector (if \code{mapping} is missing), or list, or 
#'   dataframe.
#' @export
#' @family list-functions
#' @seealso base::rapply base::list base::as.list
#' @keywords manip list
#' @note This function is not normally directly called by an \pkg{opm} user 
#'   because \code{\link{map_metadata}} is available.
#' @examples
#'
#' # List+function method
#' x <- list(a = 1:8, c = 9, d = 'x')
#' map <- function(x) sprintf("%s%s", x, x)
#' (y <- map_names(x, map))
#' stopifnot(identical(as.character(x), as.character(y)))
#' stopifnot(!identical(names(x), names(y)))
#'
#' # List+character method
#' x <- list(a = 1:8, c = 9, d = 'x')
#' map <- c(a = "b", e = "f", x = "y")
#' (y <- map_names(x, map))
#' stopifnot(identical(as.character(x), as.character(y)))
#' stopifnot(!identical(names(x), names(y)))
#' # compare with the map_values() example
#'
#' # List+missing method
#' x <- list(a = 1:8, c = 9, d = 'x')
#' (y <- map_names(x))
#' stopifnot(identical(as.vector(y), names(x)))
#' stopifnot(identical(names(y), names(x)))
#' # Now a recursive list
#' x <- list(a = 1:8, c = 9, d = list(d1 = 'x', d2 = 'y'))
#' (y <- map_names(x))
#' stopifnot(length(y) > length(names(x)))
#'
#' # Dataframe+function method
#' x <- data.frame(a = 1:3, b = letters[1:3])
#' (y <- map_names(x, toupper))
#' stopifnot(identical(y[[1]], x[[1]]), identical(y[[2]], x[[2]]))
#' stopifnot(identical(names(y), c("A", "B")))
#'
#' # Dataframe+character method
#' (y <- map_names(x, c(a = "b", b = "a")))
#' stopifnot(identical(y[[1]], x[[1]]), identical(y[[2]], x[[2]]))
#' stopifnot(identical(names(y), c("b", "a")))
#'
#' # Dataframe+missing method
#' (y <- map_names(x))
#' stopifnot(is.character(y), y == names(y), length(y) == 5)
#'
setMethod("map_names", c("list", "function"), function(object, mapping, ...) {
  map_names_recursively <- function(item) {
    if (is.list(item)) {
      if (!is.null(n <- names(item)))
        names(item) <- mapping(n, ...)
      lapply(item, FUN = map_names_recursively)
    } else
      item
  }
  map_names_recursively(object)
}, sealed = SEALED)

#' @export
#'
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

#' @export
#'
setMethod("map_names", c("list", "missing"), function(object) {
  get_names_recursively <- function(item) {
    if (is.list(item))
      c(names(item), unlist(lapply(item, FUN = get_names_recursively)))
    else
      character()
  }
  map_values(get_names_recursively(object))
}, sealed = SEALED)

#' @export
#'
setMethod("map_names", c("data.frame", "function"), function(object, mapping, 
    ...) {
  if (!is.null(n <- colnames(object)))
    colnames(object) <- mapping(n, ...)
  if (!is.null(n <- rownames(object)))
    rownames(object) <- mapping(n, ...)
  object
}, sealed = SEALED)

#' @export
#'
setMethod("map_names", c("data.frame", "character"), function(object, mapping) {
  colnames(object) <- map_values(colnames(object), mapping)
  rownames(object) <- map_values(rownames(object), mapping)
  object
}, sealed = SEALED)

#' @export
#'
setMethod("map_names", c("data.frame", "missing"), function(object) {
  map_values(c(colnames(object), rownames(object)))
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
#' query list result in \code{FALSE}.
#'
#' @param object List containing the data.
#' @param other List used as query.
#' @param values Logical scalar. Compare also the values or only the keys? If
#'   \code{FALSE}, \code{exact} is ignored.
#' @param exact Logical scalar. If \code{FALSE}, the data value(s) might by 
#'   any of the query value(s), and some coercion is done before comparing (see
#'   \code{match} for details. If \code{TRUE}, the data value(s) must exactly
#'   correspond to the query value(s), and no coercion is done (see 
#'   \code{identical}) for details). This might be too strict for most 
#'   applications.
#' @export
#' @return Logical scalar.
#' @family list-functions
#' @seealso base::list base::as.list base::`[` base::`[[` base::match
#' @keywords attribute list
#' @note This function is not normally directly called by an \pkg{opm} user but
#'   might be useful in other contexts. It forms the basis of a number of
#'   metadata query functions.  
#' @examples
#' x <- list(a = 1:8, c = 9, d = list(d1 = 'x', d2 = 'y'))
#' y <- list(a = 1:10, c = "9", d = list(d1 = "x"))
#' stopifnot(contains(x, y))
#' stopifnot(!contains(x, y, exact = TRUE))
#' stopifnot(contains(x, y, exact = TRUE, values = FALSE))
#' # see particularly infix-q and infix-k for more examples
#'
setMethod("contains", c("list", "list"), function(object, other,
    values = TRUE, exact = FALSE) {
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
        Recall(data.subset, query.subset, values = values, exact = exact)
      else if (values)
        FALSE
      else
        is.null(names(query.subset))
    } else if (values) {
      if (exact)
        identical(data.subset, query.subset)
      else
        all(data.subset %in% query.subset)
    } else
      TRUE
    if (!result)
      return(FALSE)
  }
  TRUE
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


setGeneric("insert",
  function(object, ...) standardGeneric("insert"))
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
#' @param .force Logical scalar. Overwite items that are not there?
#' @return List.
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


  
