

################################################################################
################################################################################
#
# Miscellaneous utilities
#


#' Modified switch function
#'
#' An altered switch statement for flow control.
#'
#' @param EXPR A scalar based on which a decision is made. If of mode 
#'   \sQuote{logical}, exactly two other elements must be passed, the first
#'   of which is chosen if \code{EXPR} is \code{TRUE}, the second if it is
#'   \code{FALSE}. If \code{EXPR} is a character scalar, the behaviour is like
#'   the one of \code{switch} with the exception that unmatched values within 
#'   \code{...} cause an error. If \code{EXPR} is of mode \sQuote{numeric},
#'   the behaviour is like \code{switch} but counting starts at 0 and values
#'   larger than the number of elements within \code{...} select the last 
#'   element. It is an error if \code{EXPR} is negative.
#' @param x Chosen if \code{EXPR} is \code{TRUE}.
#' @param y Chosen if \code{EXPR} is \code{FALSE}.
#' @param ... Additional arguments from which to select an alternative.
#' @return Selected value of \code{...}, \code{x} or \code{y}.
#' @seealso base::switch
#' @keywords internal
#'
setGeneric("case", function(EXPR, ...) standardGeneric("case"))

setMethod("case", "logical", function(EXPR, x, y) {
  stopifnot(!is.na(EXPR))
  switch(EXPR = 2L - EXPR, x, y)  
}, sealed = SEALED)

setMethod("case", "numeric", function(EXPR, ...) {
  stopifnot(EXPR >= 0L) # this also catches NA values
  switch(EXPR = pmin(EXPR, nargs() - 2L) + 1L, ...)
}, sealed = SEALED)

setMethod("case", "character", function(EXPR, ...) {
  switch(EXPR = EXPR, ..., stop("unmatched 'EXPR' value"))
}, sealed = SEALED)


################################################################################


#' Assert class
#'
#' Raise an error if the classes of two R objects are different.
#'
#' @param x Object to test.
#' @param y Object with target class.
#' @return \code{x}.
#' @keywords internal
#'
assert_class <- function(x, y) {
  if (!identical(cx <- class(x), cy <- class(y))) {
    join <- function(x) paste(x, collapse = "->")
    stop(sprintf("expected class '%s', got class '%s'", join(cy), join(cx)))
  }
  x
}


################################################################################


#' Assert a length
#'
#' Raise an error if a given R object does not have the specified length. This
#' is mainly used to generate more meaningful error messages related to 
#' function arguments.
#'
#' @param x R objects to test.
#' @param wanted Integer scalar giving the desired length. Note that this can
#'   \strong{not} be given as scalar with \code{storage.mode} evaluating to
#'   \sQuote{double}.
#' @param msg Error message passed to \code{sprintf}, receiving the name of
#'   the calling function, the name of \code{x} and the value of \code{wanted} 
#'   as additional arguments.
#' @return If successful, \code{x}, but an error message is raised if 
#'   \code{length(x)} is not identical to \code{wanted}.
#' @keywords internal
#'
L <- function(x, wanted = 1L, msg = "need object '%s' of length %i") {
  if (identical(length(x), wanted))
    return(x)
  stop(sprintf(msg, as.character(match.call()[2L]), wanted), call. = FALSE)
}


################################################################################


#' Assert length
#'
#' Raise an error if a given R object does not have the specified length. This
#' is mainly used to generate meaningful error messages for function arguments.
#'
#' @param ... Any R objects to test.
#' @param .wanted Integer scalar giving the desired length.
#' @return The names of the arguments contained in \code{...}, returned
#'   invisibly.
#' @keywords internal
#'
LL <- function(..., .wanted = 1L) {
  arg.names <- as.character(match.call())[-1L][seq_along(items <- list(...))]
  mapply(function(item, name) {
    if (!identical(length(item), .wanted))
      stop(sprintf("need object '%s' of length %i", name, .wanted), 
        call. = FALSE)
  }, items, arg.names, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  invisible(arg.names)
}


################################################################################


#' Convert warnings to errors
#'
#' Raise an error if a warning occurs. Useful for making certain tests more
#' strict.
#'
#' @param expr R expression to evaluate.
#' @param ... Optional further arguments to \code{tryCatch}.
#' @return The result of \code{expr} (if no error occurs).
#' @keywords internal
#'
must <- function(expr, ...) {
  # For some reason, using stop() directly resulted in errors that could not be
  # catched with tryCatch() any more.
  tryCatch(expr = expr, warning = function(w) stop(conditionMessage(w)), ...)
}


################################################################################


#' Convert errors to their messages
#'
#' If an error occurs, return its message. The expression used has to ensure
#' that its normal result can be distinguished from the error message.
#'
#' @param expr R expression to evaluate.
#' @param ... Optional further arguments to \code{tryCatch}.
#' @return The result of \code{expr} if no error occurs, otherwise the error
#'   message as character scalar.
#' @keywords internal
#'
taste <- function(expr, ...) {
  tryCatch(expr = expr, error = conditionMessage, ...)
}


################################################################################


## NOTE: not an S4 method because applicable to any subsettable objects

#' Fetch the last elements
#'
#' Fetch the last element(s) from a subsettable objject.
#'
#' @param x An R object to which \code{[} can be applied.
#' @param i Integer scalar. Number of elements to fetch.
#' @return Object of the same class than \code{x}.
#' @keywords internal
#'
last <- function(x, i = 1L) {
  if ((len <- length(x)) < L(i))
    stop("more elements requested than available")
  x[seq.int(len - i + 1L, len)]
}


################################################################################


## NOTE: not an S4 method because applicable to any objects

#' Check for uniformity
#'
#' Assess whether all elements in a collection are identical.
#'
#' @param x An R object to which \code{duplicated} can be applied.
#' @param na.rm Logical scalar. Remove \code{NA} elements before determining
#'   uniformity?
#' @return Either \code{TRUE} or a vector of the class of \code{x} containing
#'   all deviating elements.
#' @keywords internal
#'
is_uniform <- function(x, na.rm = FALSE) {
  if (na.rm)
    x <- na.exclude(x)
  if (length(x) < 2L || all((dup <- duplicated(x))[-1L]))
    return(TRUE)
  x[!dup]
}


################################################################################


#' Check for constantness
#'
#' Assess whether all elements in a collection are identical. This uses
#' \code{duplicated} by default, but there is also an \sQuote{extended} mode
#' for list-like objects.
#'
#' @param x An R object to which \code{duplicated} can be applied.
#' @param set.like Logical scalar. Consider objects as identical if their
#'   intersection is non-empty?
#' @param na.rm Logical scalar. Remove \code{NA} elements before determining
#'   constantness?
#' @return Logical scalar.
#' @keywords internal
#'
setGeneric("is_constant", function(x, ...) standardGeneric("is_constant"))

setMethod("is_constant", "vector", function(x, na.rm = TRUE) {
  if (na.rm)
    x <- na.exclude(x)
  length(x) < 2L || all(duplicated(x)[-1L])
}, sealed = SEALED)

setMethod("is_constant", "list", function(x, set.like = FALSE, na.rm = TRUE) {
  if (na.rm)
    x <- lapply(x, na.exclude)
  dup_fun <- if (set.like)
    function(x) {
      for (i in seq_along(x)[-1L]) {
        v1 <- x[[i]]
        for (j in seq.int(1L, i - 1L))
          if (length(intersect(v1, x[[j]])) == 0L)
            return(FALSE)
      }
      TRUE
    }
  else
    function(x) all(duplicated(x)[-1L])
  length(x) < 2L || dup_fun(x)
}, sealed = SEALED)
  
setMethod("is_constant", "MOA", function(x, margin = 1L, na.rm = TRUE) {
  if (margin == 0L)
    return(is_constant(as.vector(x), na.rm = na.rm))
  apply(X = x, MARGIN = margin, FUN = is_constant, na.rm = na.rm)
}, sealed = SEALED)

          
################################################################################


## NOTE: not an S4 method because conversion is done

#' Nicer message listings
#'
#' Create a nice-looking message listing. This is not normally directly called
#' by an \pkg{opm} user but by, e.g., the scripts accompanying the package; see
#' \code{\link{opm_files}} for details.
#'
#' @param x Object convertible via \code{unlist} to a vector. Afterwards its
#'   \sQuote{names} attribute is used as the first column of the resulting
#'   listing; if it is \code{NULL} or if \code{force.numbers} is \code{TRUE},
#'   numbers are inserted.
#' @param header \code{NULL} or character vector. Prepended to the result.
#' @param footer \code{NULL} or character vector. Appended to the result.
#' @param begin \code{NULL} or numeric or character vector. Prepended to each
#'   line except \code{header} and \code{footer}. If a numeric vector, the
#'   number of spaces. Otherwise converted to \sQuote{character} mode and used
#'   directly.
#' @param collapse Character scalar. How to join the resulting vector elements.
#' @param style Character scalar. If \sQuote{table} or \sQuote{list}, passed
#'   to \code{formatDL}. Otherwise, a pattern for \code{sprintf} is assumed
#'   taking two arguments, the names of \code{x} and the values \code{x} (after
#'   conversion with \code{unlist}).
#' @param force.numbers Logical scalar. Always use numbers instead of the
#'   \sQuote{names} attribute?
#' @param digits Numeric scalar. Ignored unless \code{x} is numeric.
#' @param ... Optional other arguments passed to \code{formatDL}.
#' @return Character vector.
#' @export
#' @seealso base::message base::warning base::stop base::formatDL
#' @family auxiliary-functions
#' @keywords utilities
#' @examples
#'
#' x <- letters[1:5]
#' names(x) <- LETTERS[1:5]
#' message(y <- listing(x, header = "Five letters:", footer = "...end here",
#'   begin = 3))
#' stopifnot(is.character(y), length(y) == 1)
#'
#' x <- c("CTMT", "Chryseobacterium soli DSM 19298T",
#'   "Chryseobacterium soldanellicola DSM 17072T")
#' message(y <- listing(x, style = "%s, %s", collapse = "; "))
#' stopifnot(is.character(y), length(y) == 1)
#'
listing <- function(x, header = NULL, footer = NULL, begin = NULL,
    collapse = "\n", style = "list", force.numbers = FALSE,
    digits = opm_opt("digits"), ...) {
  LL(style, collapse, digits, force.numbers)
  if (is.double(x))
    x <- signif(x, digits)
  if (is.null(names(x <- unlist(x))) || force.numbers)
    names(x) <- seq_along(x)
  x <- if (style %in% c("table", "list"))
    formatDL(x, style = style, ...)
  else
    sprintf(style, names(x), x)
  if (length(begin))
    x <- if (is.numeric(begin))
      paste(paste(rep.int(" ", begin), collapse = ""), x, sep = "")
    else
      paste(begin, x, sep = "")
  paste(c(header, x, footer), collapse = collapse)
}


################################################################################


#' Turn the head of a formula into a vector
#'
#' If a formula has length 3, the second element represents the left part (the
#' first element is the tilde). Once extracted using \code{[[}, the left part
#' can be a call, a name or a vector. These methods convert it to a vector,
#' aiming at generating a valid key for indexing a list.
#'
#' @param object An object of class \sQuote{call}, \sQuote{name} or 
#'   \sQuote{vector} (S4-based).
#' @return Vector.
#' @keywords internal
#'
setGeneric("parse_formula_head", 
  function(object) standardGeneric("parse_formula_head"))

setMethod("parse_formula_head", "vector", function(object) {
  object
}, sealed = SEALED)

setMethod("parse_formula_head", "name", function(object) {
  as.character(object)
}, sealed = SEALED)

setMethod("parse_formula_head", "call", function(object) {
  if (identical(object[[1L]], as.name("$")))
    all.names(object, functions = FALSE)
  else
    eval(object)
}, sealed = SEALED)


################################################################################


#' OPM options
#'
#' Get and set global \pkg{opm} options.
#'
#' @param x Character scalar or list. If not given, all current settings are
#'   returned (as a named list). If a list, it is expected to contain key-value
#'   pairs that can be set. In that case, it is an error if a key is unknown or
#'   if the value's class(es) is/are not compatible with the previously stored 
#'   value's class(es). If \code{x} is a character scalar, it is used for
#'   querying for a value. 
#' @param ... Optional arguments. If \code{x} is missing, these arguments are
#'   concatenated into a list and used as if \code{x} was given as a list (see 
#'   above). That is, the argument names are used as the keys for setting 
#'   values.
#' @return List or atomic vector.
#' @family auxiliary-functions
#' @details The following keys can be used with the following kinds of values:
#'   \describe{
#'     \item{color.borders}{Character vector with default color borders between
#'       which \code{\link{level_plot}} interpolates to obtain a colour 
#'       palette.}
#'     \item{csv.keys}{Character vector with names of entries of
#'       \code{\link{csv_data}} be used by \code{\link{include_metadata}}.
#'       Should be kept a subset of \code{opm_opt("csv.selection")}.}
#'     \item{csv.selection}{Character vector with names of entries of
#'       \code{\link{csv_data}} (must be a valid \sQuote{keys} argument) to be
#'       extracted by \code{\link{collect_template}}.}
#'     \item{digits}{Integer scalar. Number of digits used by some functions 
#'       generating output text.}
#'     \item{gen.iii}{Logical scalar indicating whether \code{\link{read_opm}}
#'       and other IO functions based on it automatically convert to Generation
#'       III as plate type.}
#'     \item{phylo.fmt}{Character scalar indicating the default output format
#'       used by \code{\link{phylo_data}}.}
#'     \item{split}{Character scalar indicating the default spliiting characters
#'       used by \code{\link{separate}}.}
#'     \item{time.zone}{Character scalar indicating the time zone to be used
#'       when parsing \code{\link{setup_time}} entries. This is relevant for 
#'       \code{\link{merge}}, which by default attempts to sort by parsed setup
#'       times}
#'     \item{time.fmt}{Character vector indicating the time formats used for
#'       parsing the \code{\link{setup_time}} entries (in the given order).
#'       Also relevant for \code{\link{merge}} by default.}
#'     \item{xy.colors}{Default color set used by the \code{\link{OPMS}} method
#'       of \code{\link{xy_plot}}.}
#'   }
#'   It is an error to set novel values whose classes are not identical to, or
#'   derived from, the classes of the old value. It is also an error to use a
#'   name that is not already contained (\pkg{opm} would never query for it
#'   anyway).
#' @keywords utilities
#' @seealso base::options base::getOption
#' @export
#' @examples
#'
#' # fetching a value
#' (gen.3 <- opm_opt("gen.iii"))
#' stopifnot(identical(gen.3, FALSE))
#'
#' # setting a value; previous value is returned as list
#' (old.opts <- opm_opt(gen.iii = TRUE))
#' stopifnot(is.list(old.opts), length(old.opts) == 1L)
#' stopifnot(identical(old.opts$gen.iii, FALSE))
#'
#' # fetching the value again: should now be changed
#' (gen.3 <- opm_opt("gen.iii"))
#' stopifnot(isTRUE(gen.3))
#'
#' # resetting the value
#' (old.opts <- opm_opt(old.opts))
#' stopifnot(is.list(old.opts), length(old.opts) == 1L)
#' stopifnot(isTRUE(old.opts$gen.iii))
#' (gen.3 <- opm_opt("gen.iii"))
#' stopifnot(identical(gen.3, FALSE))
#'
setGeneric("opm_opt", function(x, ...) standardGeneric("opm_opt"))

setMethod("opm_opt", "list", function(x) {
  old <- mget(keys <- names(x), envir = OPM_OPTIONS)
  for (i in seq_along(x))
    if (!all(inherits(x[[i]], class(old[[i]]))))
      stop("new and old value have conflicting class(es) for key ", keys[[i]])
  list2env(x, envir = OPM_OPTIONS)
  old  
}, sealed = SEALED)
 
setMethod("opm_opt", "missing", function(x, ...) {
  if (nargs())
    opm_opt(list(...))
  else
    as.list(OPM_OPTIONS)
}, sealed = SEALED)
 
setMethod("opm_opt", "character", function(x) {
  OPM_OPTIONS[[x]]
}, sealed = SEALED)


################################################################################


#' Collect factor indexes
#'
#' Convert a factor to a list with its levels as keys and the positions of
#' each level within the factor as values.
#'
#' @param x Factor.
#' @return List of integer vectors
#' @keywords internal
#'
setGeneric("indexes", function(x, ...) standardGeneric("indexes"))

setMethod("indexes", "factor", function(x) {
  y <- as.character(x)
  sapply(levels(x), function(level) which(y == level), simplify = FALSE)
}, sealed = SEALED)


################################################################################


#' Parse time strings
#'
#' Parse time strings by trying potentially many formats in turn. Each 
#' subsequent format is only applied to the \code{NA} values created in the
#' previous attempt, if any. A warning is issued if final \code{NA} values
#' remain.
#'
#' @param object Character vector.
#' @return Object of class \sQuote{POSIXlt}.
#' @seealso base::strptime
#' @keywords internal
#'
setGeneric("parse_time", 
  function(object, format, ...) standardGeneric("parse_time"))

setMethod("parse_time", c("character", "missing"), function(object, format,
    tz = opm_opt("time.zone")) {
  parse_time(object, format = opm_opt("time.fmt"), tz = tz)
}, sealed = SEALED)

setMethod("parse_time", c("character", "character"), function(object, format, 
    tz = "") {
  if (!length(format))
    stop("need non-empty object 'format'")   
  result <- strptime(object, format[1L], tz = tz)
  for (fmt in format[-1L])
    result[isna] <- strptime(object[isna <- is.na(result)], fmt, tz = tz)
  if (any(is.na(result)))
    warning("parsing time strings resulted in NA values")
  result  
}, sealed = SEALED)

  
################################################################################


#' Regularly split character vectors if possible
#'
#' From a given set of splitting characters select the ones that split a
#' character vector in a regular way, yielding the same number of parts for all
#' vector elements. Then apply these splitting characters to create a matrix.
#' The data frame method applies this to all character vectors (and
#' optionally also all factors) within a data frame.
#'
#' @param object Character vector to be split, or data frame in which character
#'   vectors (or factors) shall be attempted to be split, or factor.
#' @param split Character vector or \code{TRUE}. If a character vector, used as
#'   container of the splitting characters and converted to a vector containing
#'   only non-duplicated single-character strings. For instance, the default
#'   \code{split} argument \code{".-_"} yields \code{c(".", "-", "_")}. If
#'   a vector of only empty strings or
#'   \code{TRUE}, strings with substrings representing fixed-width fields are
#'   assumed, and splitting is done at whitespace-only columns. Beforehand,
#'   equal-length strings are created by padding with spaces at the right.
#'   After splitting in fixed-width mode, whitespace characters are trimmed
#'   from both ends of the resulting strings.
#' @param simplify Logical scalar indicating whether a resulting matrix with
#'   one column should be simplified to a vector (or such a data frame to a
#'   factor).
#' @param keep.const Logical scalar indicating whether constant columns
#'   should be kept or removed. Ignored if only a single column is present.
#' @param coerce Logical scalar indicating whether factors should be coerced
#'   to \sQuote{character} mode and then also be attempted to be split. The
#'   resulting columns will be coerced back to factors.
#' @param name.sep Character scalar to be inserted in the constructed column
#'   names. If more than one column results from splitting, the names will
#'   contain (i) the original column name, (ii) \code{name.sep} and (iii)
#'   their index, thus creating unique column names (if the original ones
#'   were unique).
#' @param list.wise Logical scalar. Ignored if \code{split} is \code{TRUE}.
#'   Otherwise, \code{object} is assumed to contains word lists separated by
#'   \code{split}. The result is a logical matrix in which the columns represent
#'   these words and the fields indicate whether or not a word was present in
#'   a certain item contained in \code{object}.
#' @param strip.white Logical scalar. Remove whitespace from the ends of each
#'   resulting character scalar after splitting? Has an effect on the removal
#'   of constant columns. Whitespace is always removed if \code{split} is
#'   \code{TRUE}.
#' @param ... Optional arguments passed between the methods.
#' @export
#' @return Character matrix, its number of rows being equal to the length of
#'   \code{object}, or data frame with the same number of rows as \code{object}
#'   but potentially more columns. May be character vector of factor with
#'   character or factor input and \code{simplify} set to \code{TRUE}.
#' @family auxiliary-functions
#' @keywords character manip
#' @seealso base::strsplit utils::read.fwf
#' @examples
#'
#' # Splitting by characters
#' x <- c("a-b-cc", "d-ff-g")
#' (y <- separate(x, ".")) # a split character that does not occur
#' stopifnot(is.matrix(y), y[, 1L] == x)
#' (y <- separate(x, "-")) # a split character that does occur
#' stopifnot(is.matrix(y), dim(y) == c(2, 3))
#'
#' # Fixed-with splitting
#' x <- c("  abd  efgh", " ABCD EFGH ", " xyz")
#' (y <- separate(x, TRUE))
#' stopifnot(is.matrix(y), dim(y) == c(3, 2))
#'
#' # Applied to factors
#' xx <- as.factor(x)
#' (yy <- separate(xx, TRUE))
#' stopifnot(identical(yy, as.data.frame(y)))
#'
#' # List-wise splitting
#' x <- c("a,b", "c,b", "a,c")
#' (y <- separate(x, ",", list.wise = TRUE))
#' stopifnot(is.matrix(y), dim(y) == c(3, 3), is.logical(y))
#'
#' # Data-frame method
#' x <- data.frame(a = 1:2, b = c("a-b-cc", "a-ff-g"))
#' (y <- separate(x, coerce = FALSE))
#' stopifnot(identical(x, y))
#' (y <- separate(x))
#' stopifnot(is.data.frame(y), dim(y) == c(2, 4))
#' stopifnot(sapply(y, class) == c("integer", "factor", "factor", "factor"))
#' (y <- separate(x, keep.const = FALSE))
#' stopifnot(is.data.frame(y), dim(y) == c(2, 3))
#' stopifnot(sapply(y, class) == c("integer", "factor", "factor"))
#'
setGeneric("separate", function(object, ...) standardGeneric("separate"))

setMethod("separate", "character", function(object, split = opm_opt("split"),
    simplify = FALSE, keep.const = TRUE, list.wise = FALSE, 
    strip.white = list.wise) {
  
  strip_white <- function(x) {
    for (pat in c("^\\s+", "\\s+$"))
      x <- sub(pattern = pat, replacement = "", x = x, perl = TRUE)
    x
  }
      
  simple_if <- function(x) {
    LL(simplify, keep.const)
    if (is.matrix(x)) {
      if (!keep.const && ncol(x) > 1L) {
        if (all(const <- is_constant(x, 2L)))
          x <- x[, 1L, drop = FALSE]
        else
          x <- x[, !const, drop = FALSE]
      }
      if (simplify && ncol(x) == 1L)
        x[, 1L]
      else
        x
    } else  if (simplify)
      x
    else if (length(x))
      matrix(x)
    else
      matrix(ncol = 0L, nrow = 0L, data = NA_character_)
  }

  p0 <- function(x) paste(x, collapse = "")
    
  char_group <- function(single, multiple) {
    if (length(single)) {
      if (length(multiple))
        sprintf("([%s]|[%s]+)", p0(single), p0(multiple))
      else
        sprintf("[%s]", p0(single))
    } else 
      sprintf("[%s]+", p0(multiple))
  }
  
  split_fixed <- function(x) {
    ws <- c(" ", "\t", "\v", "\r", "\n", "\b", "\a", "\f")
    x <- strsplit(x, split = "", fixed = TRUE)
    max.len <- max(vapply(x, length, integer(1L)))
    x <- lapply(x, function(y) c(y, rep.int(" ", max.len - length(y))))
    x <- do.call(rbind, x)
    groups <- group_by_sep(apply(x, 2L, function(y) all(y %in% ws)))
    x <- apply(x, 1L, split, f = groups)
    x <- lapply(x, function(y) strip_white(vapply(y, p0, character(1L))))
    do.call(rbind, x)
  }
  
  yields_constant <- function(chars) {
    splits_constant <- function(char, ...) {
      is_constant(lapply(strsplit(object, char, ...), length))  
    }
    vapply(chars, function(char) {
      if (splits_constant(sprintf("[%s]+", char), perl = TRUE))
        "multiple"
      else if (splits_constant(char, fixed = TRUE))
        "single"
      else
        "no"
    }, character(1L))
  }
  
  lists_to_matrix <- function(x, split, strip.white) {
    x <- strsplit(x, split = sprintf("[%s]", p0(split)), perl = TRUE)
    if (strip.white)
      x <- lapply(x, strip_white)
    chars <- unique(na.exclude(unlist(x)))
    result <- matrix(nrow = length(x), ncol = length(chars), data = FALSE)
    colnames(result) <- sort(chars)
    rownames(result) <- names(x)
    for (i in seq_along(x)) {
      if (identical(entries <- x[[i]], NA_character_))
        result[i, ] <- NA
      else
        result[i, entries] <- TRUE
    }
    result
  }

  LL(list.wise, strip.white)    
    
  # Fixed-width splitting mode      
  if (isTRUE(split) || all(!nzchar(split <- na.exclude(as.character(split)))))
    return(simple_if(split_fixed(object)))
  
  # Prepare split characters
  split <- unique(unlist(strsplit(x = split, split = "", fixed = TRUE)))
  if (length(split) == 0L)
    return(simple_if(object))
  split <- c(setdiff(split, "-"), intersect(split, "-"))
  
  # List-wise splitting
  if (list.wise)
    return(simple_if(lists_to_matrix(object, split, strip.white)))
    
  # Check and apply split characters
  yields.constant <- vapply(split, yields_constant, character(1L))
  if (all(yields.constant == "no"))
    return(simple_if(object))
  split <- char_group(split[yields.constant == "single"], 
    split[yields.constant == "multiple"])
  object <- do.call(rbind, strsplit(object, split, perl = TRUE))
  if (strip.white)
    object <- strip_white(object)
  simple_if(object)
  
}, sealed = SEALED)

setMethod("separate", "factor", function(object, split = "/.-_",
    simplify = FALSE, keep.const = TRUE, ...) {
  result <- separate(as.character(object), split = split,
    keep.const = keep.const, simplify = FALSE, ...)
  if (simplify && ncol(result) == 1L)
    as.factor(result[, 1L])
  else
    as.data.frame(result, stringsAsFactors = TRUE)
}, sealed = SEALED)

setMethod("separate", "data.frame", function(object, split = "/.-_",
    keep.const = TRUE, coerce = TRUE, name.sep = ".", ...) {
  LL(coerce, name.sep)
  do.call(cbind, mapply(function(x, name) {
    result <- if (is.character(x))
      as.data.frame(separate(x, split = split, keep.const = keep.const,
        simplify = FALSE, ...), stringsAsFactors = FALSE)
    else if (coerce && is.factor(x))
      separate(x, split = split, keep.const = keep.const,
        simplify = FALSE, ...)
    else
      as.data.frame(x)
    names(result) <- if ((nc <- ncol(result)) == 1L)
      name
    else
      paste(name, seq_len(nc), sep = name.sep)
    result
  }, object, names(object), SIMPLIFY = FALSE, USE.NAMES = FALSE))
}, sealed = SEALED)


################################################################################


#' Convert wildcard to regular expression
#'
#' Change a shell globbing wildcard into a regular expression. This is just a
#' slightly extended version of \code{glob2rx} from the \pkg{utils} package,
#' but more conversion steps might need to be added here in the future.
#'
#' @param object Character vector or factor.
#' @export
#' @return Character vector (or factor).
#' @family auxiliary-functions
#' @keywords character
#' @seealso utils::glob2rx base::regex
#' @note This is not normally directly called by an \pkg{opm} user because
#'   particularly \code{\link{explode_dir}} and the IO functions calling that
#'   function internally use \code{glob_to_regex} anyway.
#' @details The here used globbing search patterns contain only two special
#'   characters, \sQuote{?} and \sQuote{*}, and are thus more easy to master
#'   than regular expressions. \sQuote{?} matches a single arbitrary character,
#'   whereas \sQuote{*} matches zero to an arbitrary number of arbitrary
#'   characters. Some examples:
#'   \describe{
#'     \item{a?c}{Matches \sQuote{abc}, \sQuote{axc}, \sQuote{a c} etc. but not
#'       \sQuote{abbc}, \sQuote{abbbc}, \sQuote{ac} etc.}
#'     \item{a*c}{Matches \sQuote{abc}, \sQuote{abbc}, \sQuote{ac} etc. but not
#'       \sQuote{abd} etc.}
#'     \item{ab*}{Matches \sQuote{abc}, \sQuote{abcdefg}, \sQuote{abXYZ} etc.
#'       but not \sQuote{acdefg} etc.}
#'     \item{?bc}{Matches \sQuote{abc}, \sQuote{Xbc}, \sQuote{ bc} etc.
#'       but not \sQuote{aabc}, \sQuote{abbc}, \sQuote{bc} etc.}
#'   }
#'   Despite their simplicity, globbing patterns are often sufficient for
#'   selecting filenames.
#' @examples
#' x <- "*what glob2rx() can't handle because a '+' is included*"
#' (y <- glob_to_regex(x))
#' (z <- glob2rx(x))
#' stopifnot(!identical(y, z))
#' # Factor method
#' (z <- glob_to_regex(as.factor(x)))
#' stopifnot(identical(as.factor(y), z))
#'
setGeneric("glob_to_regex", 
  function(object, ...) standardGeneric("glob_to_regex"))

setMethod("glob_to_regex", "character", function(object) {
  x <- glob2rx(gsub("[+^$]", "\\\\1", object, perl = TRUE))
  #x <- gsub("+", "\\+", glob2rx(object), fixed = TRUE)
  attributes(x) <- attributes(object)
  x
}, sealed = SEALED)

setMethod("glob_to_regex", "factor", function(object) {
  levels(object) <- glob_to_regex(levels(object))
  object
}, sealed = SEALED)


################################################################################
################################################################################
#
# Creating strings
#


## NOTE: not an S4 method because conversion is done

#' Trim string
#'
#' Trim a string to a given length, but by default append an indicator of
#' whether something has been trimmed.
#'
#' @param str Character vector or convertible to such.
#' @param max Numeric scalar. Maximum allowed length.
#' @param append Character scalar. To be appended to strings that needed to be
#'   trimmed, for indicating just that.
#' @param clean Logical scalar. If \code{TRUE}, clean trimmed end from non-word
#'   characters, and return empty string if only \code{append} remains.
#' @param word.wise Logical scalar. If \code{TRUE}, abbreviate words
#'   separately, deleting vowels first.
#' @return Character vector.
#' @keywords internal
#'
trim_string <- function(str, max, append = ".", clean = TRUE,
    word.wise = FALSE) {
  do_trim <- function(x) {
    trim.len <- max(0L, max - nchar(append))
    if (word.wise) {
      if (clean)
        x <- gsub("\\W", "", x, perl = TRUE)
      result <- abbreviate(x, minlength = trim.len, strict = TRUE)
    } else {
      result <- strtrim(x, trim.len)
      if (clean)
        result <- sub("\\W+$", "", result, perl = TRUE)
    }
    result
  }
  long <- nchar(str) > max
  str[long] <- do_trim(str[long])
  if (clean)
    long <- long & nzchar(str)
  str[long] <- paste(str[long], append, sep = "")
  str
}


################################################################################


## NOTE: not an S4 method because conversion is done

#' Add note in parentheses
#'
#' Append an annotation in parentheses to a string; trim it if necessary.
#'
#' @inheritParams trim_string
#' @param str.1 Character vector or convertible to such.
#' @param str.2 Character vector or convertible to such, to be added in
#'   parentheses. Trimming only affects \code{str.2}, and not the parentheses.
#' @param brackets Logical scalar. Should brackets instead of parentheses be
#'   used?
#' @param word.wise Logical scalar. Do abbreviation per word?
#' @param paren.sep Character scalar. What to insert before the opening
#'   parenthesis (or bracket).
#' @return Character vector.
#' @keywords internal
#'
add_in_parens <- function(str.1, str.2, max = 1000L, append = ".",
    clean = TRUE, brackets = FALSE, word.wise = FALSE, paren.sep = " ") {
  max <- max - nchar(str.1) - 3L
  str.2 <- trim_string(str.2, max, append = append, clean = clean,
    word.wise = word.wise)
  if (brackets) {
    template <- "%s%s[%s]"
    str.2 <- gsub("[", "(", gsub("]", ")", str.2, fixed = TRUE), fixed = TRUE)
    remove <- " \\[\\]$"
  } else {
    template <- "%s%s(%s)"
    str.2 <- gsub("(", "[", gsub(")", "]", str.2, fixed = TRUE), fixed = TRUE)
    remove <- " \\(\\)$"
  }
  sub(remove, "", sprintf(template, str.1, paren.sep, str.2))
}


################################################################################
################################################################################
#
# Data selection
#


#' Pick rows
#'
#' Pick rows from a data frame if selected columns are identical to keys.
#'
#' @param object Dataframe. At least two rows are needed.
#' @param selection Named list, keys should correspond to column names of
#'   \code{object}, values to one to several alternative values that should
#'   occur in the respective data-frame column.
#' @return Dataframe.
#' @keywords internal
#'
setGeneric("pick_from", function(object, ...) standardGeneric("pick_from"))

setMethod("pick_from", "data.frame", function(object, selection) {
  matches <- lapply(names(selection), FUN = function(name) {
    m <- lapply(selection[[name]], `==`, y = object[, name])
    apply(do.call(cbind, m), 1L, any)
  })
  matches <- apply(do.call(cbind, matches), 1L, all)
  matches[is.na(matches)] <- FALSE # we get NA from all-NA rows
  object[matches, , drop = FALSE]
}, sealed = SEALED)


################################################################################
################################################################################
#
# Easter eggs
#


## NOTE: not an S4 method because conversion is done

#' Stanley Kubrick
#'
#' Stanley Kubrick memorial function. Prints a quote from one of his movies.
#'
#' @param movie Character scalar (name of the movie) or convertible to such. If
#'   empty, the quotation is chosen randomly. Otherwise, partial matching
#'   is allowed.
#' @return Character scalar (quotation), returned invisibly.
#' @export
#' @family auxiliary-functions
#' @keywords utilities
#' @examples
#' x <- kubrick()
#' stopifnot(is.character(x), length(x) == 1L)
#'
kubrick <- function(movie = character()) {
  data <- c(
    `Paths Of Glory` =
      "You see, George, those men know that I would never let them down.",
    Spartacus = "I am Spartacus!",
    Lolita = "The wedding was a quiet affair.",
    `Dr. Strangelove` =
      "Gentlemen, you can't fight in here! This is the War Room!",
    `2001: A Space Odyssey` = "My God, it's full of stars.",
    `A Clockwork Orange` = paste("It's a sin! Using Ludwig van like that.",
      "He did no harm to anyone. Beethoven just wrote music."),
    `Barry Lyndon` =
      "I'm under arrest? Captain Potzdorf, sir! I'm a British officer.",
    `The Shining` = "All work and no play makes Jack a dull boy.",
    `Full Metal Jacket` = "Sir, yes, sir!",
    `Eyes Wide Shut` = "If you men only knew..."
  )
  idx <- if (length(movie) == 0L)
    as.integer(runif(1L, max = length(data))) + 1L
  else
    as.character(movie)
  message(msg <- data[[idx, exact = FALSE]])
  invisible(msg)
}


################################################################################
################################################################################
#
# Grouping utilities
#


#' Grouping using a separator
#'
#' For the \sQuote{logical} method,
#' treat a logical vector by regarding \code{TRUE} as indicating separating.
#' Create a factor that could be used with \code{split} to split the logical
#' vector, or any equal-length object from which it was created, into according
#' groups. For the character method,
#' grep for a pattern in a character vector, thus creating a logical vector
#' indicating the matches. Then use this to construct a factor with the
#' \sQuote{logical} method.
#'
#' @param object Logical or character vector, or factor.
#' @param include Logical scalar indicating whether the separator positions
#'   should also be included in the factor levels instead of being coded as
#'   \code{NA}.
#' @param pattern Character scalar passed to \code{grepl}.
#' @param invert Logical scalar. Invert the result of pattern matching with
#'   \code{grepl}? If so, unmatched lines are treated as separators.
#' @param ... Optional arguments passed to \code{grepl} or between the methods.
#'
#' @return Factor, its length being the one of \code{object}. The levels
#'   correspond to a groups whose indices correspond to the index of a
#'   \code{TRUE} value in \code{object} plus the indices of the \code{FALSE}
#'   values immediately following it. The positions of \code{TRUE} values that
#'   are followed by \code{TRUE} values are set to \code{NA} (irrespective of
#'   \code{include}).
#' @seealso base::split base::grepl
#' @keywords internal
#'
setGeneric("group_by_sep",
  function(object, ...) standardGeneric("group_by_sep"))

setMethod("group_by_sep", "logical", function(object, include = TRUE) {
  prepare_clean_part <- function(x) {
    if (prepend <- !x[1L])
      x <- c(TRUE, x)
    if (append <- x[len <- length(x)])
      x <- c(x, FALSE)
    pos <- matrix(cumsum(rle(x)$lengths), ncol = 2L, byrow = TRUE)
    result <- unlist(lapply(seq.int(1L, nrow(pos)), function(i) {
      rep.int(i, pos[i, 2L] - pos[i, 1L] + 1L)
    }))
    if (prepend)
      result <- result[-1L]
    if (append)
      result <- result[-len]
    result
  }
  if (!(len <- length(object)))
    return(factor())
  result <- integer(len)
  true.runs <- object & c(object[-1L] == object[-len], FALSE)
  result[!true.runs] <- prepare_clean_part(object[!true.runs])
  if (include)
    result[true.runs] <- NA_integer_
  else
    result[object] <- NA_integer_
  as.factor(result)
}, sealed = SEALED)

setMethod("group_by_sep", "character", function(object, pattern,
    invert = FALSE, include = TRUE, ...) {
  matches <- grepl(pattern = pattern, x = object, ...)
  if (invert)
    matches <- !matches
  group_by_sep(matches, include)
}, sealed = SEALED)

setMethod("group_by_sep", "factor", function(object, ...) {
  group_by_sep(object = as.character(object), ...)
}, sealed = SEALED)


################################################################################


#' Check HTML using the Tidy program
#'
#' Run the HTML Tidy program for check or converting HTML character vectors.
#'
#' @param object Query character vector, or list of such vectors, or missing. 
#'   If missing, the location of the tidy executable is returned
#' @param check Logical scalar. If \code{TRUE}, the Tidy checking results,
#'   potentially including warnings and error messages, are captured in a
#'   character vector. Otherwise the converted HTML is returned.
#' @param args Character vector with arguments passed to HTML Tidy. Is is
#'   currently an error to set any of its \sQuote{File manipulation} options.
#' @param ... Optional arguments passed between the methods.
#' @return Character vector, or list of such vectors. If \code{object} is
#'   missing, the method returns the location of the HTML Tidy executable but
#'  \code{NULL} if it cannot be found.
#' @keywords internal
#'
setGeneric("tidy",  function(object, ...) standardGeneric("tidy"))

setMethod("tidy", "missing", function() {
  if (nzchar(result <- Sys.which("tidy")))
    result
  else
    NULL
}, sealed = SEALED)

setMethod("tidy", "character", function(object, check = TRUE, 
    args = c("-u", "-i")) {
  LL(check, program <- tidy())
  bad <- c("-o", "-output", "-config", "-file", "-f", "-modify", "-m")
  if (any(bad %in% (args <- as.character(args))))
    stop("you cannot set any of the 'File manipulation' options")
  if (stderr <- check)
    args <- c(args, "-e")
  else
    args <- setdiff(args, "-e")
  # The combination of stderr = TRUE and stdout = FALSE/"" is impossible.
  # '-e' turns the output of converted HTML off within Tidy.
  suppressWarnings(system2(command = program, args = unique(args),
     input = object, stderr = stderr, stdout = TRUE))
}, sealed = SEALED)

setMethod("tidy", "list", function(object, ...) {
  lapply(X = object, FUN = tidy, ...)
}, sealed = SEALED)


################################################################################
################################################################################
#
# OPM class: object construction functions
#


setGeneric("opm_problems",
  function(object, ...) standardGeneric("opm_problems"))
#' Check OPM
#'
#' Check whether a matrix fulfils the requirements for \code{\link{OPM}}
#' measurements, or
#' check whether a character vector fulfils the requirements for
#' \code{\link{OPM}} CSV data. Called when constructing an object of the class.
#'
#' @param object Matrix or character vector.
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
  if (length(bad <- grep(pattern, col.names, invert = TRUE, value = TRUE)))
    errs <- c(errs, paste("invalid entry in header:", bad[1L]))
  if (bad <- anyDuplicated(col.names))
    errs <- c(errs, paste("duplicated entry in header:", col.names[bad]))
  if (col.names[1L] != HOUR)
    errs <- c(errs, paste("first entry in header must be", HOUR))
  if (is.unsorted(col.names[-1L]))
    errs <- c(errs, "names of wells must be sorted")

  errs
}, sealed = SEALED)

setMethod("opm_problems", "character", function(object) {
  errs <- character()
  wanted <- c(FILE, PLATE_TYPE, POS, SETUP)
  missing <- !wanted %in% names(object)
  if (any(missing))
    errs <- c(errs, sprintf("need '%s' in CSV data", wanted[missing]))
  errs
}, sealed = SEALED)


################################################################################


#' Initialize
#'
#' Initialize methods for the \code{\link{OPM}} and \code{\link{OPMS}}
#' classes.
#'
#' @param .Object \code{\link{OPM}} or \code{\link{OPMS}} object.
#' @param ... Additional arguments.
#' @return \code{\link{OPM}} or \code{\link{OPMS}} object.
#' @keywords internal
#'
setMethod("initialize", OPM, function(.Object, ...) {
  .Object <- callNextMethod()
  .Object@csv_data[PLATE_TYPE] <- plate_type(.Object@csv_data[PLATE_TYPE])
  .Object
}, sealed = SEALED)


################################################################################
################################################################################
#
# Conversion functions: OPM <=> lists. Detailed comments are given close
# to the class definition.
## These are deliberately not defined for OPMS:
## * other_slots()
## * attach_attr()
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


################################################################################


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


################################################################################
################################################################################
#
# OPMA class
#


setGeneric("opma_problems",
  function(object, ...) standardGeneric("opma_problems"))
#' Check OPMA
#'
#' Check whether a matrix fulfils the requirements for  \code{\link{OPMA}}
#' aggregated data, or
#' check whether a list fulfils the requirements for \code{\link{OPMA}}
#' aggregation settings. Called when constructing an object of the class.
#'
#' @param object Matrix of aggregated data or list describing the aggregation
#'   settings.
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


################################################################################
################################################################################
#
# OPMS class: object construction functions
#


setGeneric("opms_problems",
  function(object, ...) standardGeneric("opms_problems"))
#' Check OPMS list
#'
#' Check whether a list fulfils the requirements for \code{\link{OPMS}}
#' \code{\link{plates}}. Called when constructing an object of that class.
#'
#' @param object List to be checked.
#' @return Character vector with description of problems, empty if there
#'   are none.
#' @keywords internal
#'
setMethod("opms_problems", "list", function(object) {
  errs <- character()
  if (length(object) < 2L) {
    errs <- c(errs, "less than two plates submitted")
    return(errs) # further checks are useless in that case
  }
  if (length(no.opm <- which(!vapply(object, is, logical(1L), OPM))) > 0L) {
    bad.classes <- unlist(lapply(object[no.opm], class))
    errs <- c(errs, paste("wrong class:", bad.classes))
    return(errs) # further checks are impossible in that case
  }
  if (!isTRUE(isuni <- is_uniform(vapply(object, plate_type, character(1L)))))
    errs <- c(errs, paste("plate types are not uniform:",
      paste(isuni, collapse = " <=> ")))
  if (!isTRUE(is_uniform(lapply(object, wells))))
    errs <- c(errs, "wells are not uniform")
  if (length(errs) == 0L &&
      !isTRUE(is_uniform(lapply(object, FUN = hours, what = "all"))))
    warning("running times are not uniform")
  errs
}, sealed = SEALED)


################################################################################


setMethod("initialize", OPMS, function(.Object, ...) {
  .Object <- callNextMethod()
  names(.Object@plates) <- NULL
  .Object
}, sealed = SEALED)


################################################################################


setGeneric("flattened_to_factor",
  function(object, ...) standardGeneric("flattened_to_factor"))
#' Factor from flattened data
#'
#' Extract all plate-specifying information from a data frame as created by
#' \code{\link{flatten}}. If metadata have been included, these will be joined
#' together; otherwise the plate identifiers (basically numbers) themselves are
#' used.
#'
#' @param object Object as returned by \code{\link{flatten}}.
#' @param sep Character scalar. Separator used for joining the columns
#'   together.
#' @return Factor with one entry per plate.
#' @keywords internal
#'
setMethod("flattened_to_factor", "data.frame", function(object, sep = " ") {
  LL(plate.pos <- which(colnames(object) == "Plate"), sep)
  if (plate.pos == 1L)
    return(unique(object$Plate))
  result <- aggregate(object[, seq.int(1L, plate.pos)],
    by = list(object$Plate), FUN = `[[`, i = 1L)
  result <- as.list(result[, seq.int(2L, ncol(result) - 1L), drop = FALSE])
  as.factor(do.call(paste, c(result, sep = sep)))
}, sealed = SEALED)


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
  LL(precomputed, skip, group)
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
    result <- split(result, vapply(result, plate_type, character(1L)))
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
#' @family auxiliary-functions
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
    coerce <- unique(unlist((lapply(object, class))))
  for (i in which(vapply(object, function(x) any(class(x) %in% coerce), 
      logical(1L))))
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
    if (!"ANY" %in% coerce) {
      wanted <- vapply(object, function(x) any(class(x) %in% coerce),
        logical(1L))
      object <- object[, wanted, drop = FALSE]
    }
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
#' @family auxiliary-functions
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
#' @family auxiliary-functions
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
  if (L(cores) > 1L &&
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
#' @param .strict Logical scalar. If \code{TRUE}, has precedence over 
#'   \code{.force} and causes some restrictions: Only names that are already
#'   present are allowed, and the classes must match the classes of the
#'   already contained values.
#' @return List.
#' @seealso utils::modifyList
#' @keywords internal
#'
setMethod("insert", "list", function(object, other, ..., .force = FALSE, 
    .strict = FALSE) {
  insert_carefully <- function(x, y) {
    if (length(bad <- setdiff(nn <- names(y), names(x))))
      stop("unknown key: ", bad[1L])
    for (name in nn) {
      novel <- y[[name]]
      if (!identical(class(novel), wanted <- class(x[[name]])))
        stop(sprintf("value of key '%s' must have class '%s'", name,
           paste(wanted, collapse = " -> ")))
      x[[name]] <- novel
    }
    x  
  }  
  other <- if (missing(other))
    list(...)
  else if (is.list(other))
    c(other, list(...))
  else
    list(other, ...)
  if (.strict)
    return(insert_carefully(object, other))
  keys <- names(other)
  if (!.force)
    keys <- setdiff(keys, names(object))
  object[keys] <- other[keys]
  object
}, sealed = SEALED)









