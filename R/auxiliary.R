

################################################################################
################################################################################
#
# Miscellaneous helper functions
#


## NOTE: not an S4 method because trivial

#' Name of this package
#'
#' Generate character string describing this package, optionally with its
#' version.
#'
#' @param version Logical scalar indicating whether or not to append version
#'   information.
#' @details The version might be wrong if this function is called after loading
#'   the files with \code{source} instead of \code{library}. If it is
#'   unavailable, it is silently ignored.
#' @return One- or two-element character scalar.
#' @keywords internal
#'
opm_string <- function(version = FALSE) {
  x <- "opm"
  if (version) {
    if (exists("opm.version", MEMOIZED))
      y <- MEMOIZED$opm.version
    else
      MEMOIZED$opm.version <- y <- tryCatch(
        as.character(packageVersion(x)), error = function(e) {
          warning(sprintf("cannot find %s version", x))
          UNKNOWN_VERSION
        })
    c(x, y)
  } else
    x
}


################################################################################


## NOTE: not an S4 method because applicable to any subsettable objects

#' Fetch the last elements
#'
#' Fetch the last element(s) from a subsettable object.
#'
#' @param x An \R object to which \code{[} can be applied.
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

#' Reduce an object
#'
#' Reduce a countable object to the most frequent element(s).
#'
#' @param x An \R object to which \code{table} can be applied.
#' @param cutoff Numeric scalar. Relative frequency below which elements are
#'   discarded.
#' @param use.na Logical scalar indicating whether ambiguous results should be
#'   converted to \code{NA}.
#' @return Vector of the same storage mode than \code{x}.
#' @keywords internal
#'
reduce_to_mode <- function(x, cutoff, use.na = TRUE) {
  counts <- table(x, useNA = "always")
  counts <- counts[counts > length(x) * cutoff]
  result <- case(length(counts), NA_character_, names(counts), if (use.na)
    NA_character_
  else
    names(counts))
  storage.mode(result) <- storage.mode(x)
  result
}


################################################################################


## NOTE: not an S4 method because applicable to any objects

#' Check for uniformity
#'
#' Assess whether all elements in a collection are identical.
#'
#' @param x An \R object to which \code{duplicated} can be applied.
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
#' \code{duplicated} by default, but there is also an \sQuote{extended} mode for
#' list-like objects.
#'
#' @param x Vector, matrix, array or \sQuote{CMAT} object.
#' @param strict Logical scalar. Has no effect unless \code{x} is a list. If its
#'   elements are integers, \code{FALSE} implies that objects are considered as
#'   identical if their intersection is non-empty. If the elements are floats,
#'   they are compared using \code{mean} and \code{sd}. They are identical if
#'   there ranges, defined as \code{n} standard deviations around the mean,
#'   overlap, with \code{n} being either \code{1} or \code{2}, depending on
#'   \code{strict}.
#' @param na.rm Logical scalar. Remove \code{NA} elements before determining
#'   constantness?
#' @return Logical scalar.
#' @keywords internal
#'
setGeneric("is_constant", function(x, ...) standardGeneric("is_constant"))

setMethod("is_constant", "vector", function(x, na.rm = TRUE) {
  if (na.rm)
    x <- x[!is.na(x)]
  length(x) < 2L || all(duplicated(x)[-1L])
}, sealed = SEALED)

setMethod("is_constant", "list", function(x, na.rm = TRUE) {
  if (length(x) < 2L)
    return(TRUE)
  if (na.rm)
    x <- lapply(x, na.exclude)
  all(duplicated(x)[-1L])
}, sealed = SEALED)

setMethod("is_constant", MOA, function(x, margin = 1L, na.rm = TRUE) {
  if (!margin)
    return(is_constant(as.vector(x), na.rm = na.rm))
  apply(X = x, MARGIN = margin, FUN = is_constant, na.rm = na.rm)
}, sealed = SEALED)

setMethod("is_constant", CMAT, function(x, strict, digits = opm_opt("digits"),
    na.rm = TRUE) {
  no_dup <- function(y) all(duplicated(if (na.rm)
    y[!is.na(y)]
  else
    y)[-1L])
  zero_sd <- function(y) !identical(!sd(y, na.rm = na.rm), FALSE)
  list_remove_na <- function(y) {
    y <- lapply(y, na.exclude)
    y[!!vapply(y, length, integer(1L))]
  }
  uniq_list_const <- function(y) {
    if (na.rm)
      y <- list_remove_na(y)
    all(duplicated(lapply(y, unique.default))[-1L])
  }
  no_set_overlap <- function(y) {
    if (na.rm)
      y <- list_remove_na(y)
    for (i in seq_along(y)[-1L]) {
      v1 <- y[[i]]
      for (j in seq.int(1L, i - 1L))
        if (!length(intersect(v1, y[[j]])))
          return(FALSE)
    }
    TRUE
  }
  all_distrib_overlap <- function(x, fac) {
    x <- cbind(vapply(x, mean, numeric(1L), na.rm = na.rm),
      vapply(x, sd, numeric(1L), na.rm = na.rm))
    x[, 2L] <- fac * x[, 2L]
    x <- cbind(x[, 1L] - x[, 2L], x[, 1L] + x[, 2L])
    for (i in seq.int(nrow(x)))
      if (any(x[i, 2L] < x[-i, 1L] | x[i, 1L] > x[-i, 2L], na.rm = TRUE))
        return(FALSE)
    TRUE
  }
  if (!length(x))
    return(logical(0L))
  if (nrow(x) < 2L)
    return(!logical(ncol(x)))
  case(typeof(x),
    integer = apply(x, 2L, no_dup),
    double = if (strict)
      apply(x, 2L, no_dup)
    else
      apply(round(x, digits), 2L, zero_sd),
    list = case(typeof(x[[1L]]),
      integer = apply(x, 2L, if (strict)
        uniq_list_const
      else
        no_set_overlap),
      double = apply(x, 2L, all_distrib_overlap, fac = 2L - strict)
    )
  )
}, sealed = SEALED)


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
# String processing
#


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
#' The data frame method applies this to all character vectors (and optionally
#' also all factors) within a data frame.
#'
#' @param object Character vector to be split, or data frame in which character
#'   vectors (or factors) shall be attempted to be split, or factor.
#' @param split Character vector or \code{TRUE}. If a character vector, used as
#'   container of the splitting characters and converted to a vector containing
#'   only non-duplicated single-character strings. For instance, the default
#'   \code{split} argument \code{".-_"} yields \code{c(".", "-", "_")}. If a
#'   vector of only empty strings or \code{TRUE}, strings with substrings
#'   representing fixed-width fields are assumed, and splitting is done at
#'   whitespace-only columns. Beforehand, equal-length strings are created by
#'   padding with spaces at the right. After splitting in fixed-width mode,
#'   whitespace characters are trimmed from both ends of the resulting strings.
#' @param simplify Logical scalar indicating whether a resulting matrix with one
#'   column should be simplified to a vector (or such a data frame to a factor).
#' @param keep.const Logical scalar indicating whether constant columns should
#'   be kept or removed. Ignored if only a single column is present.
#' @param coerce Logical scalar indicating whether factors should be coerced to
#'   \sQuote{character} mode and then also be attempted to be split. The
#'   resulting columns will be coerced back to factors.
#' @param name.sep Character scalar to be inserted in the constructed column
#'   names. If more than one column results from splitting, the names will
#'   contain (i) the original column name, (ii) \code{name.sep} and (iii) their
#'   index, thus creating unique column names (if the original ones were
#'   unique).
#' @param list.wise Logical scalar. Ignored if \code{split} is \code{TRUE}.
#'   Otherwise, \code{object} is assumed to contains word lists separated by
#'   \code{split}. The result is a logical matrix in which the columns represent
#'   these words and the fields indicate whether or not a word was present in a
#'   certain item contained in \code{object}.
#' @param strip.white Logical scalar. Remove whitespace from the ends of each
#'   resulting character scalar after splitting? Has an effect on the removal of
#'   constant columns. Whitespace is always removed if \code{split} is
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
    } else if (simplify)
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
    groups <- sections(apply(x, 2L, function(y) all(y %in% ws)))
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
    colnames(result) <- sort.int(chars)
    rownames(result) <- names(x)
    for (i in seq_along(x)) {
      if (identical(entries <- x[[i]], NA_character_))
        result[i, ] <- NA
      else
        result[i, entries] <- TRUE
    }
    result
  }

  LL(list.wise, strip.white, simplify, keep.const)

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

setMethod("separate", "factor", function(object, split = opm_opt("split"),
    simplify = FALSE, keep.const = TRUE, ...) {
  result <- separate(as.character(object), split = split,
    keep.const = keep.const, simplify = FALSE, ...)
  if (L(simplify) && ncol(result) == 1L)
    as.factor(result[, 1L])
  else
    as.data.frame(result, stringsAsFactors = TRUE)
}, sealed = SEALED)

setMethod("separate", "data.frame", function(object, split = opm_opt("split"),
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
#' slightly extended version of \code{glob2rx} from the \pkg{utils} package, but
#' more conversion steps might need to be added here in the future.
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
#'     \item{?bc}{Matches \sQuote{abc}, \sQuote{Xbc}, \sQuote{ bc} etc. but not
#'     \sQuote{aabc}, \sQuote{abbc}, \sQuote{bc} etc.}
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
  attributes(x) <- attributes(object)
  x
}, sealed = SEALED)

setMethod("glob_to_regex", "factor", function(object) {
  levels(object) <- glob_to_regex(levels(object))
  object
}, sealed = SEALED)


################################################################################


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
#' @param word.wise Logical scalar. If \code{TRUE}, abbreviate words separately,
#'   deleting vowels first.
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
    str.2 <- chartr("[]", "()", str.2)
    remove <- " \\[\\]$"
  } else {
    template <- "%s%s(%s)"
    str.2 <- chartr("()", "[]", str.2)
    remove <- " \\(\\)$"
  }
  sub(remove, "", sprintf(template, str.1, paren.sep, str.2))
}


################################################################################


#' Check HTML using the Tidy program
#'
#' Run the Tidy program for check or converting \acronym{HTML} character
#' vectors.
#'
#' @param object Query character vector, or list of such vectors, or missing. If
#'   missing, the location of the tidy executable is returned
#' @param check Logical scalar. If \code{TRUE}, the Tidy checking results,
#'   potentially including warnings and error messages, are captured in a
#'   character vector. Otherwise the converted \acronym{HTML} is returned.
#' @param args Character vector with arguments passed to Tidy. Is
#'   is currently an error to set any of its \sQuote{File manipulation} options.
#' @param ... Optional arguments passed between the methods.
#' @return Character vector, or list of such vectors. If \code{object} is
#'   missing, the method returns the location of the Tidy executable but
#'   \code{NULL} if it cannot be found.
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
    args <- c(args, "-e") # '-e' turns off the output of converted HTML
  else
    args <- setdiff(args, "-e")
  # NB: the combination of stderr = TRUE and stdout = FALSE/"" is impossible
  suppressWarnings(system2(command = program, args = unique(args),
    input = object, stderr = stderr, stdout = TRUE))
}, sealed = SEALED)

setMethod("tidy", "list", function(object, ...) {
  lapply(X = object, FUN = tidy, ...)
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
#'   empty, the quotation is chosen randomly. Otherwise, partial matching is
#'   allowed.
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
    `Paths Of Glory` = paste(
      "You see, George, those men know that I would never let them down."),
    Spartacus = "I am Spartacus!",
    Lolita = "The wedding was a quiet affair.",
    `Dr. Strangelove` = paste(
      "Gentlemen, you can't fight in here! This is the War Room!"),
    `2001: A Space Odyssey` = "My God, it's full of stars.",
    `A Clockwork Orange` = paste("It's a sin! Using Ludwig van like that.",
      "He did no harm to anyone. Beethoven just wrote music."),
    `Barry Lyndon` = paste(
      "I'm under arrest? Captain Potzdorf, sir! I'm a British officer."),
    `The Shining` = "All work and no play makes Jack a dull boy.",
    `Full Metal Jacket` = "Sir, yes, sir!",
    `Eyes Wide Shut` = "If you men only knew..."
  )
  idx <- if (length(movie))
    as.character(movie)
  else
    as.integer(runif(1L, max = length(data))) + 1L
  message(msg <- data[[idx, exact = FALSE]])
  invisible(msg)
}


################################################################################
################################################################################
#
# Mapping functions
#


setAs(from = "ANY", to = "factor", function(from) as.factor(from))
setAs(from = "ANY", to = "ordered", function(from) as.ordered(from))


################################################################################


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
setGeneric("prepare_class_names",
  function(object) standardGeneric("prepare_class_names"))

setMethod("prepare_class_names", "character", function(object) {
  object <- unique(c("character", object))
  if ("ANY" %in% object)
    "ANY"
  else
    object
}, sealed = SEALED)


################################################################################


#' Map values
#'
#' Map \sQuote{character} data using another \sQuote{character} vector, or
#' recursively apply a mapping function to all \sQuote{character} values within
#' a list, or non-recursively to a data frame. Optionally coerce other data
#' types to \sQuote{character}; return remaining ones unchanged. It is also
#' possible to map between classes using coercion functions. For convenience in
#' programming, methods for the \sQuote{NULL} class are also available.
#'
#' @param object List (may be nested), data frame or character vector. If it has
#'   names, they are preserved. \code{NULL} can also be given and yields
#'   \code{NULL} or an empty named character vector (if \code{mapping} is
#'   missing). \code{object} may also belong to the virtual class
#'   \code{\link{MOA}}, comprising matrices and arrays.
#' @param mapping Character vector, function, formula, or missing. If a
#'   character vector used as a mapping from its names to its values. Values
#'   from \code{object} are searched for in the \code{names} attribute of
#'   \code{mapping}; those found are replaced by the corresponding values of
#'   \code{mapping}. If \code{mapping} is missing, a character vector is
#'   returned (sorted and with duplicates removed) whose names are identical to
#'   the values. This eases the construction of mapping vectors specific for
#'   \code{object}. If \code{mapping} is missing, the \code{coerce} argument
#'   must be named. \code{mapping} changes its usage if \code{coerce} is
#'   \code{TRUE}. For \code{\link{MOA}} objects, if \code{mapping} was a
#'   function, it would be applied to \code{object} after conversion with
#'   \code{as.vector}, and it would be attempted to add the original attributes
#'   (particularly important are \sQuote{dim} and \sQuote{dimnames} back to the
#'   result. For \code{\link{MOA}} objects, if \code{mapping} is the usual
#'   character vector, it then is used for mapping the \code{storage.mode}, not
#'   the \code{class} of \code{object}. \code{mapping} can also be a formula, it
#'   is then used to compute on lists. The see examples below.
#' @param coerce Character vector with the names of classes that are coerced to
#'   \sQuote{character} to allow the mapping. Other classes are returned
#'   unchanged. Note that the coerced data are \strong{not} converted back to
#'   their original data type. \sQuote{ANY} can be used to indicate that all
#'   classes will be considered. Alternatively, \code{coerce} can be
#'   \code{TRUE}. \code{mapping} is then interpreted as a mapping between the
#'   names of classes, and \code{as} from the \pkg{methods} package is used for
#'   conducting the requested coercions. Attempting an undefined coercion will
#'   result in an error.
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
setGeneric("map_values",
  function(object, mapping, ...) standardGeneric("map_values"))

#-------------------------------------------------------------------------------

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
  mostattributes(result) <- c(attributes(result), attributes(object))
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
  object <- sort.int(unique(object))
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


#' Map names
#'
#' Use a character vector or a function for recursively mapping list names, or
#' mapping the \sQuote{colnames} and \sQuote{rownames} attributes of a data
#' frame. In the case of lists, the function is not applied to list elements
#' which are not themselves lists, even if they have a \sQuote{names} attribute.
#' Such elements and their names, if any, are returned unchanged. If a
#' \sQuote{names}, \sQuote{colnames} or \sQuote{rownames} attribute is
#' \code{NULL}, it is ignored. Alternatively, instead of mapping the names,
#' collect them and return them as a single character vector, sorted and with
#' duplicates removed. The collected names are added as their own \code{names}
#' attribute; this might be useful if the result is later on used for some
#' mapping (using this function or \code{\link{map_values}}).
#'
#' @param object Any \R object. The default method applies the mapping to the
#'   \sQuote{names} attribute. The behaviour is special for lists, which are
#'   traversed recursively to also consider sublists with names. Data frames and
#'   \code{\link{MOA}} objects (that is, including matrices and arrays) are also
#'   treated specially because the \sQuote{dimnames} attribute, not the
#'   \sQuote{names} attribute is considered.
#' @param mapping Mapping function that takes a character vector as first
#'   argument, or character vector used for mapping from its names to its
#'   values, or missing. It is guaranteed that \code{NULL} input remains
#'   \code{NULL}, irrespective of the value of \code{mapping}.
#' @param ... Optional further arguments to \code{mapping} (if it is a
#'   function).
#' @return Character vector if \code{mapping} is missing, otherwise an \R object
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
setGeneric("map_names",
  function(object, mapping, ...) standardGeneric("map_names"))

#-------------------------------------------------------------------------------

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
# YAML reparation
#


#' Repair NAs
#'
#' Replace \sQuote{NA} by \code{NA_character_}. When reading \acronym{YAML}
#' input previously output by \R, \sQuote{NA} values cause numeric vectors to be
#' interpreted as character. This function fixes this problem and also takes
#' care of misinterpreted numbers in exponential notation.
#'
#' @param object Character vector or list.
#' @param type Character scalar denoting the type to which input character
#'   vectors shall be tried to be converted.
#' @return Character vector or list.
#' @seealso utils::type.convert
#' @keywords internal
#' @references \url{http://www.yaml.org/}
#'
setGeneric("repair_na_strings",
  function(object, ...) standardGeneric("repair_na_strings"))

setMethod("repair_na_strings", "character", function(object) {
  object[grepl("^\\s*NA$", object, perl = TRUE)] <- NA_character_
  object
}, sealed = SEALED)

setMethod("repair_na_strings", "list", function(object,
    type = c("double", "integer", "complex", "logical")) {
  type <- match.arg(type)
  rapply(object, f = function(x) {
    tryCatch({
      x <- repair_na_strings(x)
      storage.mode(x) <- type
      x
    }, warning = function(w) x)
  }, classes = "character", how = "replace")
}, sealed = SEALED)


################################################################################
################################################################################
#
# Lists
#


#' Traverse a list with a function
#'
#' Apply a function to all list elements in turn, optionally in parallel using
#' the \pkg{multicore} package.
#'
#' @param object List.
#' @param func Function to apply to each element of the list.
#' @param cores Integer scalar. Number of cores to use. If more than one, a
#'   warning is issued if the \pkg{multicore} package is not available, and the
#'   number of cores is set back to 1.
#' @param ... Optional arguments to \code{lapply} or \code{mclapply} (can be
#'   arguments passed to \code{func}).
#' @return List.
#' @keywords internal
#'
setGeneric("traverse",
  function(object, func, ...) standardGeneric("traverse"))

setMethod("traverse", c("list", "function"), function(object, func, cores,
    ...) {
  if (L(cores) > 1L && !suppressWarnings(require(
      multicore, quietly = TRUE, warn.conflicts = FALSE))) {
    warning("'multicore' not available -- switching back to 1 core")
    cores <- 1L
  }
  if (cores > 1L)
    multicore::mclapply(X = object, FUN = func, mc.cores = cores, ...)
  else
    lapply(X = object, FUN = func, ...)
}, sealed = SEALED)


################################################################################


#' Insert a list in a list
#'
#' Insert all values from another list in a list, either by overwriting the
#' previously present data or by only setting the missing ones. Note that this
#' comparison is based on the names. It does not matter whether the values are
#' \code{NULL}.
#'
#' @param object List.
#' @param other \R object to insert. List.
#' @param ... Optional other items to insert.
#' @param .force Logical scalar. Overwite items that are already there?
#' @param .strict Logical scalar. If \code{TRUE}, has precedence over
#'   \code{.force} and causes some restrictions: Only names that are already
#'   present are allowed, and the classes must match the classes of the already
#'   contained values.
#' @return List.
#' @seealso utils::modifyList
#' @keywords internal
#'
setGeneric("insert", function(object, ...) standardGeneric("insert"))

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


################################################################################


#' Query a list with a list
#'
#' Test whether all names of a query list occur as names in a data list and
#' optionally also whether they point to the same elements; apply this principle
#' recursively to all sublists. Non-list elements are ignored if \code{values}
#' is \code{FALSE}. Otherwise the comparison is done using \code{identical} if
#' \code{exact} is \code{TRUE}. If \code{exact} is \code{FALSE}, the value(s) in
#' the data list can be any of the values at the corresponding position in the
#' query list, and the comparison is done by coercion to character vectors. An
#' empty query list results in \code{TRUE}. Missing names in a non-empty query
#' list result in \code{FALSE}. There is also an \code{\link{OPMS}} method,
#' which tests whether an \code{\link{OPM}} object is contained.
#'
#' @param object List containing the data, or \code{\link{OPMS}} object.
#' @param other For the list method, a list used as query; for the
#'   \code{\link{OPMS}} method, an \code{\link{OPM}} object used as query.
#' @param values Logical scalar. Compare also the values or only the keys? If
#'   \code{FALSE}, \code{exact} is ignored.
#' @param exact Logical scalar. If \code{FALSE}, the data value(s) might by any
#'   of the query value(s), and some coercion is done before comparing (see
#'   \code{match} for details. If \code{TRUE}, the data value(s) must exactly
#'   correspond to the query value(s), and no coercion is done (see
#'   \code{identical}) for details). This might be too strict for most
#'   applications.
#' @param ... Optional arguments passed to \code{identical} from the \pkg{base}
#'   package, allowing for fine-control of identity. Has no effect unless
#'   \code{exact} is \code{TRUE}.
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
#' stopifnot(contains(vaas_4, vaas_4[3]))
#'
setGeneric("contains",
  function(object, other, ...) standardGeneric("contains"))

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
# Global options
#


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
#' @return List or atomic vector. If one to several values are set, the previous
#'   entries are returned invisibly.
#' @family auxiliary-functions
#' @details The following keys can be used with the following kinds of values:
#'   \describe{
#'     \item{colors}{Default color set used by the \code{\link{OPMS}} method
#'       of \code{\link{xy_plot}} and other plotting functions.}
#'     \item{color.borders}{Character vector with default color borders between
#'       which \code{\link{level_plot}} interpolates to obtain a colour
#'       palette.}
#'     \item{css.file}{Character scalar. Default \acronym{CSS} file linked by
#'       \code{\link{phylo_data}} when producing \acronym{HTML} output. Ignored
#'       if empty.}
#'     \item{csv.keys}{Character vector with names of entries of
#'       \code{\link{csv_data}} be used by \code{\link{include_metadata}}.
#'       Should be kept a subset of \code{opm_opt("csv.selection")}.}
#'     \item{csv.selection}{Character vector with names of entries of
#'       \code{\link{csv_data}} (must be a valid \sQuote{keys} argument) to be
#'       extracted by \code{\link{collect_template}}.}
#'     \item{curve.param}{Character scalar. Default \sQuote{subset} argument of
#'       \code{\link{extract}} and the plotting functions.}
#'     \item{digits}{Integer scalar. Number of digits used by some functions
#'       generating output text.}
#'     \item{file.encoding}{Character scalar. Character encoding in input files
#'       as assumed by \code{\link{read_opm}}.}
#'     \item{gen.iii}{Character scalar indicating whether \code{\link{read_opm}}
#'       and other IO functions based on it automatically convert to this plate
#'       type. If empty, nothing is changed.}
#'     \item{html.attr}{Used by \code{\link{phylo_data}} for automatically
#'       creating \acronym{HTML} \sQuote{title} and \sQuote{class} attributes.}
#'     \item{phylo.fmt}{Character scalar indicating the default output format
#'       used by \code{\link{phylo_data}}.}
#'     \item{split}{Character scalar indicating the default spliiting characters
#'       used by \code{\link{separate}}.}
#'     \item{time.zone}{Character scalar indicating the time zone to be used
#'       when parsing \code{\link{setup_time}} entries. This is relevant for
#'       \code{\link{merge}}, which by default attempts to sort by parsed setup
#'       times}
#'     \item{time.fmt}{Character vector indicating the time formats used for
#'       parsing the \code{\link{setup_time}} entries (in the given order). Also
#'       relevant for \code{\link{merge}} by default.}
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
#' (digits <- opm_opt("digits"))
#' stopifnot(digits == 4)
#'
#' # setting a value; previous value is returned as list
#' (old.opts <- opm_opt(digits = 5L))
#' stopifnot(is.list(old.opts), length(old.opts) == 1L)
#' stopifnot(old.opts$digits == 4)
#'
#' # fetching the value again: should now be changed
#' (digits <- opm_opt("digits"))
#' stopifnot(digits == 5)
#'
#' # resetting the value
#' (old.opts <- opm_opt(old.opts))
#' stopifnot(is.list(old.opts), length(old.opts) == 1L)
#' stopifnot(old.opts$digits == 5)
#' (digits <- opm_opt("digits"))
#' stopifnot(digits == 4)
#'
setGeneric("opm_opt", function(x, ...) standardGeneric("opm_opt"))

setMethod("opm_opt", "list", function(x) {
  old <- mget(keys <- names(x), envir = OPM_OPTIONS)
  for (i in seq_along(x))
    if (!all(inherits(x[[i]], class(old[[i]]))))
      stop("new and old value have conflicting class(es) for key ", keys[[i]])
  list2env(x, envir = OPM_OPTIONS)
  invisible(old)
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


#' Modify a CMAT object
#'
#' Modify a CMAT object in a specified way.
#'
#' @param object An object of class \sQuote{CMAT}.
#' @param how Characater scalar indicating how \code{object} should be
#'   modified.
#' @param digits Integer scalar indicating the number of decimal points to
#'   consider when comparing \sQuote{double} values.
#' @param na.rm Logical scalar passed to \code{\link{is_constant}} (if
#'   applicable).
#' @return Object of the same class than \code{object}.
#' @keywords internal
#'
setGeneric("update")

setMethod("update", CMAT, function(object,
    how = c("NA2int", "delete.uninf", "delete.constant", "delete.ambig"),
    digits = opm_opt("digits"), na.rm = TRUE) {
  if (!length(object))
    return(object)
  shiftable <- function(x) {
    x <- unique.default(x)
    length(x[!is.na(x)]) == 2L
  }
  shift_int <- function(x) {
    isna <- is.na(x)
    x.max <- max(x[!isna])
    x.min <- min(x[!isna])
    if (x.max == x.min + 1L) {
      x[x == x.max] <- x.max + 1L
      x.max <- x.max + 1L
    }
    x[isna] <- as.integer(mean(c(x.min, x.max)))
    x
  }
  has_ambig <- function(x) {
    if (na.rm)
      x <- lapply(x, na.exclude)
    for (item in x) {
      if (length(unique.default(item)) > 1L)
        return(TRUE)
    }
    FALSE
  }
  has_nonzero_sd <- function(x) {
    isTRUE(sd(x, na.rm = TRUE) > .Machine$double.eps ^ 0.5)
  }
  no.transformation <- "transforming NA impossible: not two non-NA entries"
  switch(how <- match.arg(how),
    NA2int = {
      switch(typeof(object),
        integer = if (shiftable(object))
          object[] <- shift_int(object)
        else
          warning(no.transformation)
        ,
        list = if (typeof(object[[1L]]) == "integer")
          if (shiftable(unlist(object)))
            object[] <- lapply(object, shift_int)
          else
            warning(no.transformation)
      )
    },
    {
      bad <- case(sub("^delete\\.", "", how, perl = TRUE),
        ambig = if (typeof(object) == "list")
          case(typeof(object[[1L]]),
            integer = apply(object, 2L, has_ambig),
            double = apply(object, 2L, has_nonzero_sd))
        else
          FALSE,
        constant = is_constant(object, strict = TRUE, digits = digits,
          na.rm = na.rm),
        uninf = is_constant(object, strict = FALSE, digits = digits,
          na.rm = na.rm)
      )
      if (any(bad))
        object <- as(object[, !bad, drop = FALSE], CMAT)
    }
  )
  object
}, sealed = SEALED)


################################################################################

