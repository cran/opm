



################################################################################
################################################################################
#
# Miscellaneous utilities
#


#' Assert length
#'
#' Raise an error if a given R object does not have the specified length. This
#' is mainly used to generate meaningful error messages for function arguments.
#'
#' @param ... Any R objects to test.
#' @param wanted Integer scalar giving the desired length.
#' @return The names of the arguments contained in \code{...}, returned 
#'   invisibly.
#' @keywords internal
#'
assert_length <- function(..., .wanted = 1L) {
  arg.names <- as.character(match.call())[-1L][seq_along(items <- list(...))]
  mapply(function(item, name) {
    if (!identical(length(item), .wanted))
      stop(sprintf("need object '%s' of length %i", name, .wanted))
  }, items, arg.names, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  invisible(arg.names)
}


################################################################################


## NOTE: not an S4 method because applicable to any objects

#' Check for uniformity.
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


## NOTE: not an S4 method because applicable to any objects

#' Check for constantness.
#'
#' Assess whether all elements in a collection are identical.
#'
#' @param x An R object to which \code{duplicated} can be applied.
#' @param na.rm Logical scalar. Remove \code{NA} elements before determining
#'   constantness?
#' @return Logical scalar.
#' @keywords internal
#'
is_constant <- function(x, na.rm = TRUE) {
  if (na.rm)
    x <- na.exclude(x)
  length(x) < 2L || all(duplicated(x)[-1L])
}


################################################################################


## NOTE: not an S4 method because conversion is done

#' Nicer message listings
#'
#' Create a nice-looking message listing. This is not normally directly called
#' by an \pkg{opm} user but by, e.g., the scripts accompanying the package; see 
#' \code{\link{opm_files}} for details.
#'
#' @param x Object convertible via \code{unlist} to a vector.
#' @param header \code{NULL} or character vector. Prepended to the result.
#' @param footer \code{NULL} or character vector. Appended to the result.
#' @param begin \code{NULL} or numeric or character vector. Prepended to each
#'   line except \code{header} and \code{footer}. If a numeric vector, the
#'   number of spaces. Otherwise converted to \sQuote{character} mode and used
#'   directly.
#' @param collapse Character scalar. How to join the resulting vector elements.
#' @param style Character scalar. Passed to \code{formatDL}.
#' @param ... Optional other arguments passed to \code{formatDL}.
#' @return Character vector.
#' @export
#' @seealso base::message base::warning base::stop base::formatDL
#' @keywords utilities
#' @examples
#' x <- letters[1:5]
#' names(x) <- LETTERS[1:5]
#' message(y <- listing(x, header = "Five letters:", footer = "...end here", 
#'   begin = 3))
#' stopifnot(is.character(y), length(y) == 1)
#'
listing <- function(x, header = NULL, footer = NULL, begin = NULL,
    collapse = "\n", style = "list", ...) {
  result <- formatDL(unlist(x), style = style, ...)
  if (length(begin))
    if (is.numeric(begin))
      result <- paste(paste(rep.int(" ", begin), collapse = ""), result,
        sep = "")
    else
      result <- paste(begin, result, sep = "")
  if (length(header))
    result <- c(header, result)
  if (length(footer))
    result <- c(result, footer)
  paste(result, collapse = collapse)
}


################################################################################


setGeneric("separate", function(object, ...) standardGeneric("separate"))
#' Regularly split character vectors if possible
#'
#' From a given set of splitting characters select the ones that split a
#' character vector in a regular way, yielding the same number of parts for all
#' vector elements. Then apply these splitting characters to create a matrix.
#' The data frame method applies this to all character vectors (and 
#' optionally also all factors) within a data frame.
#'
#' @param object Character vector to be split, or data frame in which character
#'   vectors (or factors) shall be attempted to be split.
#' @param split Character vector or \code{TRUE}. If a character vector, used as
#'   container of the splitting characters and converted to a vector containing 
#'   only non-duplicated single-character strings. For instance, the default 
#'   \code{split} argument \code{".-_"} yields \code{c(".", "-", "_")}. If
#'   \code{TRUE}, strings with substrings representing fixed-width fields are 
#'   assumed, and splitting is done at whitespace-only columns. Beforehand,
#'   equal-length strings are created by padding with spaces at the right.
#'   After splitting in fixed-width mode, whitespace characters are trimmed 
#'   from both ends of the resulting strings.
#' @param coerce Logical scalar indicating whether factors should be coerced
#'   to \sQuote{character} mode and then also be attempted to be split. The
#'   resulting columns will be coerced back to factors.
#' @param name.sep Character scalar to be inserted in the constructed column
#'   names. If more than one column results from splitting, the names will
#'   contain (i) the original column name, (ii) \code{name.sep} and (iii)
#'   their index, thus creating unique column names (if the original ones
#'   were unique).
#' @export
#' @return Character matrix, its number of rows being equal to the length of
#'   \code{object}, or data frame with the same number of rows as \code{object}
#'   but potentially more columns.
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
#' # Data frame method
#' x <- data.frame(a = 1:2, b = c("a-b-cc", "d-ff-g"))
#' (y <- separate(x))
#' stopifnot(is.data.frame(y), dim(y) == c(2, 4))
#' stopifnot(sapply(y, class) == c("integer", "factor", "factor", "factor"))
#'
setMethod("separate", "character", function(object, split = ".-_") {
  char_group <- function(x) sprintf("[%s]", paste(x, collapse = ""))
  split_fixed <- function(x) {
    ws <- c(" ", "\t", "\v", "\r", "\n", "\b", "\a", "\f")
    max.len <- max(sapply(x <- strsplit(x, split = "", fixed = TRUE), length))
    x <- lapply(x, function(y) c(y, rep.int(" ", max.len - length(y))))
    x <- do.call(rbind, x)
    groups <- group_by_sep(apply(x, 2L, function(y) all(y %in% ws)))
    x <- apply(x, 1L, split, f = groups)
    do.call(rbind, lapply(x, function(y, p1, p2) {
      y <- sapply(y, paste, collapse = "")
      sub(p2, "", sub(p1, "", y, perl = TRUE), perl = TRUE)
    }, p1 = sprintf("^%s+", ws <- char_group(ws)), p2 = sprintf("%s+$", ws)))
  }
  if (isTRUE(split))
    return(split_fixed(object))
  split <- unique(unlist(strsplit(x = split, split = "", fixed = TRUE)))
  if (length(split) == 0L)
    return(matrix(object))
  yields.constant <- sapply(split, function(char) {
    is_constant(lapply(strsplit(object, char, fixed = TRUE), length))
  })
  if (length(split <- split[yields.constant]) == 0L)
    return(matrix(object))
  split <- char_group(c(split[!(dash <- split == "-")], split[dash]))
  do.call(rbind, strsplit(object, split, perl = TRUE))
}, sealed = SEALED)

#' @export
#'
setMethod("separate", "data.frame", function(object, split = ".-_",
    coerce = TRUE, name.sep = ".") {
  assert_length(coerce, name.sep)
  do.call(cbind, mapply(function(x, name) {
    result <- if (is.fac <- is.factor(x)) {
      if (coerce)
        separate(as.character(x), split)
      else
        x
    } else if (is.character(x))
      separate(x, split)
    else
      x
    result <- as.data.frame(result, stringsAsFactors = is.fac)
    names(result) <- if ((nc <- ncol(result)) == 1L)
      name
    else
      paste(name, seq_len(nc), sep = name.sep)
    result
  }, object, names(object), SIMPLIFY = FALSE, USE.NAMES = FALSE))
}, sealed = SEALED)


################################################################################


setGeneric("glob_to_regex", function(x, ...) standardGeneric("glob_to_regex"))
#' Convert wildcard to regular expression
#'
#' Change a shell globbing wildcard into a regular expression. This is just a
#' slightly extended version of \code{glob2rx} from the \pkg{utils} package,
#' but more conversion steps might need to be added here in the future.
#'
#' @param x Character vector.
#' @export
#' @return Character vector.
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
#'
setMethod("glob_to_regex", "character", function(x) {
  result <- glob2rx(x)
  gsub("+", "\\+", result, fixed = TRUE)
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
# Plate, substrate, well, and curve parameter names
#


## NOTE: Not an S4 method because check is done with match.arg()

#' Grofit mapping
#'
#' Create a mapping for names of curve parameters.
#'
#' @param subset \code{NULL} or character vector. Use only these values?
#' @param ci Logical scalar. Also return CI names?
#' @param plain Logical scalar. Return the plain basenames only, ignoring
#'   \code{subset} and \code{ci}?
#' @param opm.fast Logical scalar. Produce the mapping gfor the 
#'   \sQuote{opm-fast} method instead?
#' @return Named list with old names as keys, new ones as values.
#' @keywords internal
#'
map_grofit_names <- function(subset = NULL, ci = TRUE, plain = FALSE,
    opm.fast = FALSE) {
  part.1 <- as.list(CURVE_PARAMS)
  names(part.1) <- if (opm.fast)
    c("mu", "lambda", "A", "AUC")
  else
    c("mu", "lambda", "A", "integral")
  if (plain)
    return(part.1)
  if (length(subset) > 0L) {
    subset <- match.arg(subset, part.1, several.ok = TRUE)
    part.1 <- part.1[part.1 %in% subset]
  }
  if (ci) {
    part.2 <- paste(part.1, "CI95 low")
    part.3 <- paste(part.1, "CI95 high")
    if (opm.fast) {
      names(part.2) <- sprintf("%s.ci.low", names(part.1))
      names(part.3) <- sprintf("%s.ci.high", names(part.1))
    } else { 
      names(part.2) <- sprintf("ci95.%s.bt.lo", names(part.1))
      names(part.3) <- sprintf("ci95.%s.bt.up", names(part.1))
    }
  } else {
    part.2 <- NULL
    part.3 <- NULL
  }
  if (opm.fast)
    names(part.1) <- sprintf("%s.point.est", names(part.1))
  else
    names(part.1) <- sprintf("%s.spline", names(part.1))
  c(part.1, part.2, part.3)
}


################################################################################


## NOTE: Not an S4 method because there are no arguments

#' Names of curve parameters
#'
#' Yield the names of the estimated curve parameters used internally and in the
#' output.
#'
#' @return Character vector.
#' @export
#' @family aggregation-functions
#' @keywords utilities
#' @examples
#' (x <- param_names())
#' stopifnot(is.character(x), length(x) == 4L)
#' stopifnot(identical(unique(x), x))
#'
param_names <- function() {
  CURVE_PARAMS  
}


################################################################################


## NOTE: not an S4 method because conversion is done

#' Normalize plate name
#'
#' Normalize the names of OmniLog(R) PM plates to the internally used naming
#' scheme. Unrecognized names are returned unchanged. This needs not normally
#' be called by the \pkg{opm} user but might be of interest.
#'
#' @param plate Character vector of original plate name(s).
#' @param subtype Logical scalar. Keep the plate subtype indicator, if any?
#' @export
#' @return Character vector of the same length than \code{plate}.
#' @family naming-functions
#' @keywords utilities character
#' @seealso base::gsub
#' @examples
#' # Entirely unrecognized strings are returned as-is 
#' x <- normalize_plate_name(letters)
#' stopifnot(identical(x, letters))
#'
#' # Something more realistic
#' (x <- normalize_plate_name(y <- c("PM1", "PM-11C", "PMM04-a"), TRUE))
#' stopifnot(x != y)
#'
normalize_plate_name <- function(plate, subtype = FALSE) {
  normalize_pm <- function(x, subtype)  {
    x <- sub("^PMM", "PM-M", x, perl = TRUE)
    repl <- if (subtype)
      "-\\1"
    else
      ""  
    x <- sub("([A-Z]+)$", repl, x, perl = TRUE)
    sub("([^\\d])(\\d)([^\\d]|$)", "\\10\\2\\3", x, perl = TRUE)
  }
  result <- toupper(gsub("\\W", "", plate, perl = TRUE))
  ok <- grepl("^PMM?\\d+[A-Z]*$", result, perl = TRUE)
  result[ok] <- normalize_pm(result[ok], subtype = subtype)
  result[!ok] <- plate[!ok]
  result
}


################################################################################


## NOTE: not an S4 method because conversion is done

#' Map well names to substrates
#'
#' Translate well names (which are basically their coordinates on the plate) to 
#' substrate names, given the name of the plate.
#'
#' @param wells Character vector of original well names (coordinates on the
#'   plate).
#' @param plate_type Character scalar. The type of the plate. See
#'   \code{\link{plate_type}}.
#' @param in.parens Logical scalar. See \code{\link{wells}}.
#' @param brackets Logical scalar. See \code{\link{wells}}.
#' @param paren.sep Character scalar. See \code{\link{wells}}.
#' @param ... Arguments that can be passed to both 
#'   \code{\link{add_in_parens}} and \code{\link{trim_string}}.
#' @return Character vector.
#' @keywords internal
#' @note The user-level function is \code{\link{well_to_substrate}}.
#'
map_well_names <- function(wells, plate, in.parens = FALSE, brackets = FALSE,
    paren.sep = " ", ...) {
  assert_length(plate)
  pos <- match(plate, colnames(WELL_MAP))
  if (is.na(pos)) {
    warning("cannot find plate type ", plate)
    trim_string(wells, ...)
  } else if (in.parens)
    add_in_parens(wells, WELL_MAP[wells, pos], brackets = brackets,
      paren.sep = paren.sep, ...)
  else
    trim_string(WELL_MAP[wells, pos], ...)
}


################################################################################


## NOTE: not an S4 method because conversion is done
  
#' Map well names to substrates
#'
#' Translate well names (which are basically their coordinates on the plate) to 
#' substrate names, given the name of the plate.
#'
#' @param plate Character vector. The type(s) of the plate(s). See
#'   \code{\link{plate_type}}. \code{\link{normalize_plate_name}} is applied
#'   before searching for the substrate names, and partial matching is allowed.
#' @param well Character vector of original well names (coordinates on the
#'   plate), or integer vector, or convertible to such.
#' @export
#' @return Character vector or matrix (depending on the length of \code{plate}),
#'   containing \code{NA} values for plates and wells that could not be
#'   identified.
#' @family naming-functions
#' @keywords utilities
#' @examples
#' x <- c("A01", "B10")
#' (y <- well_to_substrate("PM1", x))
#' stopifnot(nchar(y) > nchar(x))
#'
well_to_substrate <- function(plate, well = 1L:96L) {
  pos <- pmatch(normalize_plate_name(plate), colnames(WELL_MAP))
  found <- !is.na(pos)
  result <- WELL_MAP[well, pos[found], drop = FALSE]
  others <- matrix(data = NA_character_, nrow = length(well), 
    ncol = length(plate[!found]), dimnames = list(well, plate[!found]))
  cbind(result, others)[]
}


################################################################################


setGeneric("find_substrate", function(x, ...) standardGeneric("find_substrate"))
#' Identify substrates
#'
#' Identify the names of substrates as used in the stored plate annotations.
#' Exact or error-tolerant matching can be used, as well as globbing and
#' regular-expression matching.
#'
#' @param x Query character vector.
#' @param search Character scalar indicating the search mode. If \sQuote{exact},
#'   query names must exactly match (parts of) the well annotations. If
#'   \sQuote{glob}, shell globbing is used. If \sQuote{approx}, approximate
#'   matching is used; the number or proportion of errors allowed is set using
#'   \code{max.dev}, and neither globbing or regular-expression matching is done
#'   in that case. If \sQuote{regex}, regular-expression matching is used.
#'   All matching is case-insensitive except for \sQuote{exact} search mode.
#' @param max.dev Numeric scalar indicating the maximum allowed deviation. If
#'   < 1, the proportion of characters that might deviate, otherwise their
#'   absolute number. It can also be a list; see the \sQuote{max.distance}
#'   argument of \code{agrep} in the \pkg{base} package for details. Has an 
#'   effect only if \sQuote{approx} is chosen as search mode (see the 
#'   \code{search} argument).
#' @export
#' @return List of character vectors (empty if nothing was found), with
#'   duplicates removed and the rest sorted. The names of the list correspond
#'   to \code{names}.
#' @note See \code{\link{glob_to_regex}} for a description of globbing 
#'   patterns.
#' @seealso base::grep base::agrep
#' @family naming-functions
#' @keywords character utilities
#' @examples
#' # Note that 'exact' search uses partial matching, whereas globbing is exact
#' # if there are no wildcards
#' (x <- find_substrate("a-D-Glucose", search = "exact"))
#' (y <- find_substrate("a-D-Glucose", search = "glob"))
#' stopifnot(length(x[[1]]) > length(y[[1]]))
#'
#' # Now allowing mismatches
#' (z <- find_substrate("a-D-Glucose", search = "approx"))
#' stopifnot(length(z[[1]]) > length(x[[1]]))
#'
setMethod("find_substrate", "character", function(x,
    search = c("exact", "glob", "approx", "regex"), max.dev = 0.2) {
  find_name <- function(pattern, ...) {
    grep(pattern = pattern, x = WELL_MAP, value = TRUE, useBytes = TRUE, ...)
  }
  find_approx <- function(pattern, ...) {
    agrep(pattern = pattern, x = WELL_MAP, ignore.case = TRUE, value = TRUE,
      useBytes = TRUE, ...)
  }
  search <- match.arg(search)
  sapply(x, FUN = function(name) {
    sort(unique(switch(search,
      exact = find_name(name, fixed = TRUE),
      glob = find_name(glob_to_regex(name), ignore.case = TRUE, perl = TRUE),
      regex = find_name(name, ignore.case = TRUE, perl = TRUE),
      approx = find_approx(name, max.distance = max.dev),
      stop(BUG_MSG)
    )))
  }, simplify = FALSE)
}, sealed = SEALED)


################################################################################


setGeneric("find_positions", function(x, ...) standardGeneric("find_positions"))
#' Identify positions of substrates
#'
#' Identify the positions of substrates, i.e. the plate(s) and well(s) in which
#' they occur. The query names must be written exactly as used in the stored
#' plate annotations. To determine their spelling, use
#' \code{\link{find_substrate}}.
#'
#' @param x Query character vector or query list.
#' @export
#' @return The character method returns a
#'   list of character matrices (empty if nothing was found), with one row
#'   per position found, the plate name in the first column and the well name in
#'   the second. The names of this list correspond to \code{names}. The list
#'   method returns lists of such lists.
#' @family naming-functions
#' @keywords utilities
#' @examples
#'
#' # Character method; compare correct and misspelled substrate name
#' (x <- find_positions(c("a-D-Glucose", "a-D-Gloucose")))
#' stopifnot(length(x[[1]]) > length(x[[2]]))
#'
#' # List method
#' (x <- find_positions(find_substrate(c("a-D-Glucose", "a-D-Gloucose"))))
#' stopifnot(length(x[[1]]) > length(x[[2]]))
#'
setMethod("find_positions", "character", function(x) {
  plates <- colnames(WELL_MAP)
  sapply(x, FUN = function(name) {
    result <- which(WELL_MAP == name, arr.ind = TRUE)
    matrix(c(plates[result[, 2L]], rownames(result)), ncol = 2L,
      dimnames = list(NULL, c("Plate", "Well")))
  }, simplify = FALSE)
}, sealed = SEALED)

#' @export
#'
setMethod("find_positions", "list", function(x) {
  rapply(x, f = find_positions, classes = "character", how = "list")
}, sealed = SEALED)


################################################################################
################################################################################
#
# Data selection
#


setGeneric("pick_from", function(object, ...) standardGeneric("pick_from"))
#' Pick rows
#'
#' Pick rows from a dataframe if selected columns are identical to keys.
#'
#' @param object Dataframe. At least two rows are needed.
#' @param selection Named list, keys should correspond to column names of
#'   \code{object}, values to one to several alternative values that should 
#'   occur in the respective dataframe column.
#' @return Dataframe.
#' @keywords internal
#'
setMethod("pick_from", "data.frame", function(object, selection) {
  # We do not use sapply() in the following because it might return a vector
  # instead of a matrix; lapply()/cbind() always creates a matrix.
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
# Colors
#


## NOTE: not an S4 method because conversion is done

#' Sort colors
#'
#' A helper function for methods such as \code{\link{xy_plot}}.
#' Arrange colors to achieve that neighboring colors are most distinct with
#' respect to their RGB coordinates. This is done as follows: (1) euclidean
#' distances between the RGB coordinates of the input colors are calculated;
#' (2) the distances are inversed; (3) a principal-coordinate analysis is
#' conducted on these inversed distances; (4) the input colors are sorted
#' according to the first principal coordinate.
#'
#' @param col Vector. Names or hexadecimal codes of the colors to be sorted.
#'   Might also be an integer vector, see \code{col2rgb} from the 
#'   \pkg{grDevices} package for details. Duplicate RGB coordinates and
#'   unknown names will cause an error.
#' @export
#' @return Character vector (rearranged input names).
#' @family plotting-functions
#' @seealso grDevices::col2rgb 
#' @keywords color
#' @note The resulting vector could as well be used in reverse order.
#' @examples
#' (x <- max_rgb_contrast(c("darkred", "darkblue", "blue", "red")))
#' y <- c("darkblue", "red", "blue", "darkred")
#' stopifnot(identical(x, y) || identical(x, rev(y)))
#'
max_rgb_contrast <- function(col) {
  col.rgb <- t(col2rgb(col))
  rownames(col.rgb) <- col
  pco <- cmdscale(1 / log(dist(col.rgb)), k = 1L)
  names(sort(pco[, 1L]))
}


################################################################################


## NOTE: not an S4 method because check is done using match.arg()

#' Select colors
#'
#' Select a set of colors for plotting. See \code{\link{xy_plot}} for usage
#' example. This is not normally directly called
#' by an \pkg{opm} user but could be used for testing before doing some serious
#' plotting.
#'
#' @param set Character scalar. Name of the color vector to use. Color vectors
#'   have been optimized for maximum contrast between adjacent colors, either
#'   manually or using \code{\link{max_rgb_contrast}}. Names
#'   ending in \sQuote{.i} indicate vectors in inverse order (compared to the
#'   vector with the same name except \sQuote{.i}).
#' @export
#' @return Character vector (names of colors).
#' @family plotting-functions
#' @keywords color
#' @seealso grDevices::colors grDevices::rainbow grDevices::grey
#' @references \url{http://www.colorbrewer.org}
#' @examples
#' (x <- select_colors("nora"))
#' (y <- select_colors("nora.i"))
#' stopifnot(is.character(x), length(x) > 0L, identical(x, rev(y)))
#'
select_colors <- function(
    set = c("w3c", "w3c.i", "nora", "nora.i", "roseobacter", "roseobacter.i")) {
  switch(match.arg(set),
    w3c = W3C_COLORS[W3C_NAMES_MAX_CONTRAST],
    w3c.i = rev(W3C_COLORS[W3C_NAMES_MAX_CONTRAST]),
    nora = NORAS_COLORS,
    nora.i = rev(NORAS_COLORS),
    brewer = BREWER_COLORS,
    brewer.i = rev(BREWER_COLORS),
    roseobacter = ROSEOBACTER_COLORS,
    roseobacter.i = rev(ROSEOBACTER_COLORS),
    stop(BUG_MSG)
  )
}


################################################################################


## NOTE: not an S4 method because conversion is done

#' Color regions
#'
#' Create default color regions for use with \code{\link{level_plot}}.
#'
#' @param colors Character or integer vector with at least two distinct colors. 
#'   If \code{NULL} or empty, default colors are chosen.
#' @return Character vector of color codes.
#' @keywords internal
#'
default_color_regions <- function(colors = NULL) {
  if (length(colors) == 0L)
    colors <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")
  brewer.div <- colorRampPalette(unique(colors), space = "Lab", bias = 0.5)
  brewer.div(200)
}


################################################################################
################################################################################
#
# Plotting helper functions
#


setGeneric("draw_ci", function(object, ...) standardGeneric("draw_ci"))
#' Draw CI
#'
#' Draw a confidence interval.
#'
#' @param object Four-element numeric vector containing (i) the left margin of 
#'   the CI; (ii) the point estimate; (iii) the right margin; (iv) the position 
#'   on the y axis. The point estimate can be \code{NA} at any time; whether
#'   the margins can also be \code{NA} depends on \code{na.action}.
#' @param col Character scalar. Name of the color to be used.
#' @param cex Numeric scalar. Magnification for CI margin symbols and point
#'   estimate. Also affects line width, and proportionally so.
#' @param na.action Character scalar. What to do if a margin value is
#'   \code{NA}.
#' @return \code{object}, returned invisibly.
#' @keywords internal
#'
setMethod("draw_ci", "numeric", function(object, col = "blue", cex = 1,
    na.action = c("warn", "error", "ignore")) {
  assert_length(object, .wanted = 4L)
  if (any(is.na(c(left <- object[1L], right <- object[3L])))) {
    msg <- "cannot draw CI because left or right margin is 'NA'"
    switch(match.arg(na.action),
      warn = warning(msg),
      error = stop(msg),
      ignore = NULL,
      stop(BUG_MSG)
    )
  }
  if (is.na(y <- object[4L]))
    stop("position on y axis must be provided")
  segments(left, y, right, y, lwd = cex, col = col)
  text(left, y, "(", col = col, cex = cex)
  text(right, y, ")", col = col, cex = cex)
  if (!is.na(point <- object[2L]))
    points(point, y, col = col, lwd = cex, pch = 19L, cex = cex)
  invisible(object)
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
#' @family easter-egg-functions
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


setGeneric("group_by_sep", 
  function(object, ...) standardGeneric("group_by_sep"))
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
#' @param object Logical vector.
#' @param include Logical scalar indicating whether the sepator positions  
#'   should also be included in the factor levels instead of being coded as 
#'   \code{NA}.
#' @param pattern Character scalar passed to \code{grepl}.
#' @param invert Logical scalar. Invert the result of pattern matching with
#'   \code{grepl}? If so, unmatched lines are treated as separators.
#' @param ... Optional arguments passed to \code{grepl}.
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


################################################################################


