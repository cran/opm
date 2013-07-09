

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


#' Convert to metadata-like data frame
#'
#' A helper function for \code{\link{to_metadata}}.
#'
#' @param object Nested list.
#' @param stringsAsFactors Logical scalar.
#' @param optonal Logical scalar.
#' @return Data frame.
#' @keywords internal
#'
md_data_frame <- function(object, stringsAsFactors, optional, ...) {
  data_frameable <- function(x) {
    oneify <- function(x) {
      x[vapply(x, length, 0L) == 0L] <- NA
      x[bad] <- lapply(x[bad <- vapply(x, length, 0L) != 1L], list)
      x
    }
    if (any(bad <- is.na(names(x)) | !nzchar(names(x)))) {
      warning("skipping elements with invalid names")
      x <- x[!bad]
    }
    oneify(x)
  }
  x <- lapply(object, data_frameable)
  keys <- unique.default(unlist(lapply(x, names), recursive = FALSE))
  result <- matrix(NA, length(x), length(keys), dimnames = list(NULL, keys))
  result <- as.data.frame(x = result, stringsAsFactors = FALSE,
    optional = TRUE, ...)
  for (i in seq_along(x))
    result[i, names(x[[i]])] <- x[[i]]
  if (stringsAsFactors)
    for (i in which(vapply(result, typeof, "") == "character"))
      result[, i] <- as.factor(result[, i])
  if (!optional)
    names(result) <- make.names(names(result))
  result
}


################################################################################


## NOTE: not an S4 method because applicable to any objects

#' Reduce an object
#'
#' Reduce a countable object to the most frequent element(s).
#'
#' @param x An \R object to which \code{table} can be applied. The matrix method
#'   reduces the columns.
#' @param cutoff Numeric scalar. Relative frequency below which elements are
#'   discarded.
#' @param use.na Logical scalar indicating whether ambiguous results should be
#'   converted to \code{NA}.
#' @return Vector of the same storage mode than \code{x}.
#' @keywords internal
#'
reduce_to_mode <- function(x, cutoff, use.na) UseMethod("reduce_to_mode")

#' @rdname reduce_to_mode
#' @method reduce_to_mode default
#'
reduce_to_mode.default <- function(x, cutoff, use.na = TRUE) {
  counts <- table(x, useNA = "always")
  counts <- counts[counts >= length(x) * cutoff]
  result <- case(length(counts), NA_character_, names(counts), if (use.na)
    NA_character_
  else
    names(counts))
  storage.mode(result) <- storage.mode(x)
  result
}

#' @rdname reduce_to_mode
#' @method reduce_to_mode matrix
#'
reduce_to_mode.matrix <- function(x, cutoff, use.na = TRUE) {
  apply(x, 2L, reduce_to_mode.default, cutoff, use.na)
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
  length(x) < 2L || all(duplicated.default(x)[-1L])
}, sealed = SEALED)

setMethod("is_constant", "list", function(x, na.rm = TRUE) {
  if (length(x) < 2L)
    return(TRUE)
  if (na.rm)
    x <- lapply(x, na.exclude)
  all(duplicated.default(x)[-1L])
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
    y[!!vapply(y, length, 0L)]
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
    x <- cbind(vapply(x, mean, 0, na.rm = na.rm),
      vapply(x, sd, 0, na.rm = na.rm))
    x[, 2L] <- fac * x[, 2L]
    x <- cbind(x[, 1L] - x[, 2L], x[, 1L] + x[, 2L])
    for (i in seq_len(nrow(x)))
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


#' Check presence of split column
#'
#' Check whether a certain column is present and not at the end of a data frame
#' or matrix.
#'
#' @param x Data frame, matrix or array.
#' @param split.at Names of columns at which \code{x} should be split.
#' @return Integer scalar indicating the split position. An error is raised
#'   if this is missing or non-unique.
#' @keywords internal
#'
assert_splittable_matrix <- function(x, split.at) {
  pos <- which(colnames(x) == split.at)
  LL(pos, .msg = listing(sprintf("'%s'", split.at), style = "sentence",
    prepend = FALSE, header = "need exactly one column name present among: ",
    last.sep = "comma"))
  if (pos == ncol(x))
    stop("column given by 'split.at' must not be the last one")
  pos
}


################################################################################
################################################################################
#
# String processing
#


#' Create formula
#'
#' Construct a formula from a template.
#'
#' @param fmt Character scalar. The format of the formula; omitting \sQuote{~}
#'   yields an error.
#' @param ... Passed to \code{sprintf} after joining. It is an error to not
#'   pass enough arguments.
#' @param env Passed to \code{formula} as \sQuote{env} argument.
#' @return Formula.
#' @keywords internal
#'
create_formula <- function(fmt, ..., .env = parent.frame()) {
  x <- c(list(fmt = fmt), lapply(list(...), as.list))
  formula(do.call(sprintf, unlist(x, recursive = FALSE)), .env)
}


################################################################################


#' Create metadata key
#'
#' A helper function for \code{\link{metadata}} and the methods that are
#' dependent on it.
#'
#' @param x List, formula, or atomic object.
#' @param to.formula Logical scalar indicating whether conversion to a formula
#'   should be conducted.
#' @param remove Names of elements to be deleted after conversion to list.
#' @param ops Character vector containing the operators to use when converting
#'   a list to a formula. Recycled if necessary.
#' @inheritParams print
#' @param full.eval Logical scalar indicating whether to evaluate the result.
#'   Usually makes no sense for formulas here.
#' @param envir Passed to \code{eval}.
#' @return List or character vector.
#' @keywords internal
#'
metadata_key <- function(x, to.formula, ...) UseMethod("metadata_key")

#' @rdname metadata_key
#' @method metadata_key default
#'
metadata_key.default <- function(x, to.formula = FALSE, remove = NULL, ...) {
  if (!is.atomic(x))
    stop(NOT_YET)
  if (length(x) == 1L && x %in% remove)
    return(NULL)
  if (to.formula) ## TODO check whether this makes sense
    create_formula("~ c(%s)", paste(x, collapse = ", "))
  else
    x
}

#' @rdname metadata_key
#' @method metadata_key factor
#'
metadata_key.factor <- function(x, to.formula = FALSE, remove = NULL, ...) {
  metadata_key.character(as.character(x), to.formula, remove, ...)
}

#' @rdname metadata_key
#' @method metadata_key character
#'
metadata_key.character <- function(x, to.formula = FALSE, remove = NULL, ...) {
  if (length(x) == 1L && x %in% remove)
    return(NULL)
  if (to.formula)
    return(create_formula("~ `%s`",
      paste(x, collapse = get("key.join", OPM_OPTIONS))))
  if (is.null(names(x)))
    names(x) <- x
  x
}

#' @rdname metadata_key
#' @method metadata_key list
#'
metadata_key.list <- function(x, to.formula = FALSE, remove = NULL, ops = "+",
    ...) {
  join <- function(x) vapply(x, paste0, "",
    collapse = get("key.join", OPM_OPTIONS))
  if (is.null(names(x <- flatten(x))))
    names(x) <- join(x)
  else
    names(x)[bad] <- join(x[bad <- !nzchar(names(x)) | is.na(names(x))])
  x <- x[!names(x) %in% remove]
  if (!to.formula)
    return(x)
  fmt <- case(length(x), stop("'x' must not be empty"), "",
    paste(rep(ops, length.out = length(x) - 1L), "`%s`", collapse = " "))
  create_formula(paste("~ `%s`", fmt), names(x))
}

#' @rdname metadata_key
#' @method metadata_key formula
#'
metadata_key.formula <- function(x, to.formula = FALSE, remove = NULL, ...,
    full.eval = !to.formula, envir = parent.frame()) {
  elem_type <- function(name) switch(as.character(name),
    `::` =, `:::` =, `$` =, `@` = 1L, # operators with highest precedence
    `I` = 2L, # protected formula elements
    `J` = 3L, # causing on-the-fly joining of metadata elements
    4L # anything else
  )
  apply_to_tail <- function(x, fun) {
    for (i in seq_along(x)[-1L])
      x[[i]] <- fun(x[[i]])
    x
  }
  combine <- new.env(parent = emptyenv())
  comb_list <- function(...) {
    if (length(keys <- flatten(x <- list(...))) > 1L) {
      keys <- vapply(keys, paste0, "",
        collapse = get("key.join", OPM_OPTIONS))
      combine[[paste0(keys,
        collapse = get("comb.key.join", OPM_OPTIONS))]] <- keys
    }
    x
  }
  comb_names <- function(x) {
    x <- all.vars(x)
    key <- paste0(x, collapse = get("comb.key.join", OPM_OPTIONS))
    if (length(x) > 1L)
      combine[[key]] <- x
    as.name(key)
  }
  final_comb_list <- function(x, remove) {
    x <- as.list(x)
    if (length(remove))
      x <- x[!vapply(x, function(y) any(y %in% remove), NA)]
    if (length(x))
      x
    else
      NULL
  }
  c.name <- as.name("c")
  list.name <- as.name("list")
  comblist.name <- as.name("comb_list")
  rec_listify <- function(x) case(length(x), NULL, if (is.call(x))
      NULL
    else if (is.name(x))
      as.character(x)
    else
      x, switch(
    elem_type(x[[1L]]),
    {
      x[[1L]] <- c.name # tight binding
      apply_to_tail(x, rec_listify)
    },
    {
      x[[1L]] <- c.name # tight binding, no changes
      eval(x, envir)
    },
    {
      x[[1L]] <- comblist.name
      apply_to_tail(x, rec_listify)
    },
    {
      x[[1L]] <- list.name
      apply_to_tail(x, rec_listify)
    }
  ))
  rec_replace <- function(x) case(length(x), x, if (is.character(x))
      as.name(x)
    else
      x, switch(
    elem_type(x[[1L]]),
    as.name(paste0(all.vars(apply_to_tail(x, rec_replace)),
      collapse = get("key.join", OPM_OPTIONS))),
    {
      x[[1L]] <- c.name
      as.name(paste0(eval(x, envir), collapse = get("key.join", OPM_OPTIONS)))
    },
    comb_names(apply_to_tail(x, rec_replace)),
    apply_to_tail(x, rec_replace)
  ))
  result <- if (to.formula)
    rec_replace(x[[length(x)]])
  else
    rec_listify(x[[length(x)]])
  if (full.eval) {
    result <- metadata_key(x = eval(result, enclos = envir), remove = remove,
      ...)
    if (length(result))
      attr(result, "combine") <- final_comb_list(combine, remove)
    result
  } else {
    x[[length(x)]] <- result
    attr(x, "combine") <- final_comb_list(combine, remove)
    x
  }
}


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
#' The data frame method applies this to all character vectors (and optionally
#' also all factors) within a data frame.
#'
#' @param object Character vector to be split, or data frame in which character
#'   vectors (or factors) shall be attempted to be split, or factor.
#' @param split Character vector or \code{TRUE}. \itemize{
#'   \item If a character vector, used as container of the splitting characters
#'   and converted to a vector containing only non-duplicated single-character
#'   strings. For instance, the default \code{split} argument \code{".-_"}
#'   yields \code{c(".", "-", "_")}.
#'   \item If a vector of only empty strings or \code{TRUE}, strings with
#'   substrings representing fixed-width fields are assumed, and splitting is
#'   done at whitespace-only columns. Beforehand, equal-length strings are
#'   created by padding with spaces at the right. After splitting in fixed-width
#'   mode, whitespace characters are trimmed from both ends of the resulting
#'   strings.
#'   }
#' @param simplify Logical scalar indicating whether a resulting matrix with one
#'   column should be simplified to a vector (or such a data frame to a factor).
#'   If so, at least one matrix column is kept, even if \code{keep.const} is
#'   \code{FALSE}.
#' @param keep.const Logical scalar indicating whether constant columns should
#'   be kept or removed.
#' @param coerce Logical scalar indicating whether factors should be coerced to
#'   \sQuote{character} mode and then also be attempted to be split. The
#'   resulting columns will be coerced back to factors.
#' @param name.sep Character scalar to be inserted in the constructed column
#'   names. If more than one column results from splitting, the names will
#'   contain (i) the original column name, (ii) \code{name.sep} and (iii) their
#'   index, thus creating unique column names (if the original ones were
#'   unique).
#' @param list.wise Logical scalar. Ignored if \code{split} is \code{TRUE}.
#'   Otherwise, \code{object} is assumed to contain word lists separated by
#'   \code{split}. The result is a logical matrix in which the columns represent
#'   these words and the fields indicate whether or not a word was present in a
#'   certain item contained in \code{object}.
#' @param strip.white Logical scalar. Remove whitespace from the ends of each
#'   resulting character scalar after splitting? Has an effect on the removal of
#'   constant columns. Whitespace is always removed if \code{split} is
#'   \code{TRUE}.
#' @param ... Optional arguments passed between the methods.
#' @export
#' @details This function is useful if information coded in the elements of a
#'   character vector is to be converted to a matrix or data frame. For
#'   instance, file names created by a batch export conducted by a some software
#'   are usually more or less regularly structured and contain content at
#'   distinct possitions. In such situations, the correct splitting approach can
#'   be recognized by yielding the same number of fields from each vector
#'   element.
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
#' (y <- separate(x)) # only character/factor columns are split
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

  strip_white <- function(x) sub("\\s+$", "", sub("^\\s+", "", x, perl = TRUE),
    perl = TRUE)

  p0 <- function(x) paste(x, collapse = "")

  simple_if <- function(x, keep.const, simplify) {
    if (is.matrix(x)) {
      if (!keep.const) {
        if (all(const <- is_constant(x, 2L)) && simplify)
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
      matrix(NA_character_, 0L, 0L)
  }

  # create regexp for splitting
  char_group <- function(single, multiple) {
    if (length(single))
      if (length(multiple))
        sprintf("([%s]|[%s]+)", p0(single), p0(multiple))
      else
        sprintf("[%s]", p0(single))
    else if (length(multiple))
      sprintf("[%s]+", p0(multiple))
    else
      NA_character_ # does not split at all
  }

  # splitting at positions that contain whitespace in all strings
  split_fixed <- function(x) {
    ws <- c(" ", "\t", "\v", "\r", "\n", "\b", "\a", "\f")
    x <- strsplit(x, split = "", fixed = TRUE)
    max.len <- max(vapply(x, length, 0L))
    x <- lapply(x, function(y) c(y, rep.int(" ", max.len - length(y))))
    x <- do.call(rbind, x)
    groups <- sections(apply(x, 2L, function(y) all(y %in% ws)))
    x <- apply(x, 1L, split.default, groups)
    x <- lapply(x, function(y) strip_white(vapply(y, p0, "")))
    do.call(rbind, x)
  }

  yields_constant <- function(char, x) {
    splits_constant <- function(char, x, ...)
      is_constant(vapply(strsplit(x, char, ...), length, 0L))
    if (splits_constant(sprintf("[%s]+", char), x, perl = TRUE))
      2L
    else if (splits_constant(char, x, fixed = TRUE))
      1L
    else
      0L
  }

  # collect words after splitting and mark their occurrences
  word_occurrences <- function(x, split, strip.white) {
    x <- strsplit(x, sprintf("[%s]", p0(split)), perl = TRUE)
    if (strip.white)
      x <- lapply(x, strip_white)
    chars <- unlist(x, recursive = FALSE)
    chars <- unique.default(chars[!is.na(chars)])
    result <- matrix(FALSE, length(x), length(chars))
    colnames(result) <- sort.int(chars)
    rownames(result) <- names(x)
    for (i in seq_along(x))
      if (identical(x[[i]], NA_character_))
        result[i, ] <- NA
      else
        result[i, x[[i]]] <- TRUE
    result
  }

  LL(list.wise, strip.white, simplify, keep.const)

  # Fixed-width splitting mode
  if (identical(TRUE, split <- c(split)))
    return(simple_if(split_fixed(object), keep.const, simplify))
  split <- as.character(split)
  if (all(!nzchar(split <- split[!is.na(split)])))
    return(simple_if(split_fixed(object), keep.const, simplify))

  # Prepare split characters
  split <- strsplit(split, "", fixed = TRUE)
  split <- unique.default(unlist(split, recursive = FALSE))
  if (!length(split))
    return(simple_if(object, keep.const, simplify))
  split <- c(setdiff(split, "-"), intersect(split, "-"))

  # List-wise splitting
  if (list.wise)
    return(simple_if(word_occurrences(object, split, strip.white),
      keep.const, simplify))

  # Check and apply split characters
  yields.const <- vapply(split, yields_constant, 0L, object)
  split <- char_group(split[yields.const == 1L], split[yields.const == 2L])
  object <- do.call(rbind, strsplit(object, split, perl = TRUE))
  if (strip.white)
    object[] <- strip_white(object)
  simple_if(object, keep.const, simplify)

}, sealed = SEALED)

setMethod("separate", "factor", function(object, split = opm_opt("split"),
    simplify = FALSE, keep.const = TRUE, ...) {
  result <- separate(as.character(object), split = split,
    keep.const = keep.const, simplify = FALSE, ...)
  if (L(simplify) && ncol(result) == 1L)
    as.factor(result[, 1L])
  else
    as.data.frame(result, stringsAsFactors = TRUE, optional = TRUE)
}, sealed = SEALED)

setMethod("separate", "data.frame", function(object, split = opm_opt("split"),
    simplify = FALSE, keep.const = TRUE, coerce = TRUE, name.sep = ".", ...) {
  LL(coerce, name.sep, simplify)
  object <- do.call(cbind, mapply(function(x, name) {
    result <- if (is.character(x))
      as.data.frame(separate(x, split = split, keep.const = keep.const,
        simplify = FALSE, ...), stringsAsFactors = FALSE, optional = TRUE)
    else if (coerce && is.factor(x))
      separate(x, split = split, keep.const = keep.const,
        simplify = FALSE, ...)
    else
      as.data.frame(x, stringsAsFactors = FALSE, optional = TRUE)
    case(ncol(result),
      if (keep.const)
        result[, name] <- x,
      names(result) <- name,
      names(result) <- paste(name, seq_len(ncol(result)), sep = name.sep)
    )
    result
  }, object, names(object), SIMPLIFY = FALSE, USE.NAMES = FALSE))
  if (ncol(object) == 1L && simplify)
    object <- object[, 1L]
  object
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
#' @details This is not normally directly called by an \pkg{opm} user because
#'   particularly \code{\link{explode_dir}} and the IO functions calling that
#'   function internally use \code{glob_to_regex} anyway. But some hints when
#'   using globbing patterns are given in the following.
#'
#'   The here used globbing search patterns contain only two special characters,
#'   \sQuote{?} and \sQuote{*}, and are thus more easy to master than regular
#'   expressions. \sQuote{?} matches a single arbitrary character, whereas
#'   \sQuote{*} matches zero to an arbitrary number of arbitrary characters.
#'   Some examples:
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
  # TODO: one should perhaps also check for '|'
  x <- glob2rx(gsub("([+^$])", "\\\\\\1", object, perl = TRUE))
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
  str[long] <- paste0(str[long], append)
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


#' Convert recursively to HTML
#'
#' This is the helper function used by \code{\link{format}} for converting
#' user-defined additions to \acronym{HTML}.
#'
#' @param x List or other vector.
#' @param level Integer scalar defining the starting level for indentation and
#'   naming of unnamed sections.
#' @param fmt Character scalar used for transforming \code{level} into section
#'   \sQuote{class} and \sQuote{title} attributes.
#' @param fac Integer scalar for inferring the number of spaces used for
#'   indentation from the current \code{level} (recursively incremented).
#' @return Character scalar.
#' @details  If applied to lists, this functions works recursively, generating
#'   \sQuote{div} elements from each list. Names are used as \sQuote{class} and
#'   \sQuote{title} attributes. Where names are missing, \code{level} is used in
#'   conjunction with \code{fmt}. Non-list vectors are converted using
#'   \sQuote{span} tags if names are present, simply joined otherwise.
#' @keywords internal
#'
list2html <- function(x, level = 1L, fmt = opm_opt("html.class"), fac = 2L) {
  indent <- paste(rep.int(" ", fac * (level - 1L)), collapse = "")
  if (is.list(x)) {
    if (is.null(n <- names(x)))
      n <- sprintf(fmt, level)
    else
      n[!nzchar(n)] <- sprintf(fmt, level)
    n <- ifelse(nzchar(n), safe_labels(n, "html"), NA_character_)
    x <- vapply(x, list2html, "", level = level + 1L, fmt = fmt)
    x <- paste0(x, indent)
    x <- hmakeTag("div", x, class = n, title = n, newline = TRUE)
    paste0(indent, x, collapse = "")
  } else {
    if (is.character(x) && !inherits(x, "AsIs"))
      x <- safe_labels(x, "html")
    if (!is.null(n <- names(x))) {
      n <- ifelse(nzchar(n), safe_labels(n, "html"), NA_character_)
      x <- hmakeTag("span", x, class = n, title = n)
    }
    paste0(indent, paste0(x, collapse = " "), "\n")
  }
}


################################################################################


#' Create HTML head
#'
#' This is the helper function used by \code{\link{format}} and other functions
#' to create the \sQuote{head} antry of an \acronym{HTML} strings.
#'
#' @param title Character scalar defining the title of the \acronym{HTML}
#'   document. Must contain an attribute called as returned by
#'   \code{\link{opm_string}}.
#' @param css Character vector containing the names of \acronym{CSS} files to
#'   link.
#' @param meta Character vector defining additional meta tags.
#' @return Character vector.
#' @keywords internal
#'
html_head <- function(title, css, meta) {
  single_tag <- function(x, ...) {
    listing(list(...), c("<", x), ">", style = " %s=\"%s\"", collapse = "")
  }
  html_comment <- function(x) {
    safe_labels(x, "html", comment = TRUE, enclose = FALSE)
  }
  if (length(title)) {
    from.opm <- attr(title, opm_string())
    # Tidy accepts only a single title entry
    title <- hmakeTag("title", data = safe_labels(title[1L], format = "html"))
    if (!from.opm)
      title <- c(html_comment("user-defined title"), title)
  } else
    title <- NULL
  if (length(css <- css[nzchar(css)])) {
    is.abs.path <- grepl("^(/|[a-zA-Z]:)", css, perl = TRUE)
    css[is.abs.path] <- sprintf("file://%s", css[is.abs.path])
    css <- vapply(css, function(y) {
      single_tag("link", rel = "stylesheet", type = "text/css", href = y)
    }, "")
    css <- c(html_comment("user-defined CSS file(s)"), unname(css))
  } else
    css <- NULL
  generator <- single_tag("meta", name = "generator",
    content = paste(opm_string(version = TRUE), collapse = " version "))
  # see http://www.w3.org/TR/NOTE-datetime
  # but %s appears to be affected by a bug in R 2.15.2
  time <- format(Sys.time(), "%Y-%M-%dT%H:%M:%S%z")
  time <- single_tag("meta", name = "date", content = time)
  if (length(meta)) {
    meta <- vapply(meta, function(y) {
      if (is.null(names(y)))
        stop("HTML meta entry without names")
      do.call(single_tag, c(list(x = "meta"), as.list(y)))
    }, "")
    meta <- c(html_comment("user-defined metadata"), unname(meta))
  } else
    meta <- NULL
  c("<head>", title, generator, time, meta, css, "</head>")
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
setGeneric("tidy", function(object, ...) standardGeneric("tidy"))

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
#' @param x Character vector.
#' @return Character vector.
#' @keywords internal
#'
prepare_class_names <- function(x) UseMethod("prepare_class_names")

#' @rdname prepare_class_names
#' @method prepare_class_names character
#'
prepare_class_names.character <- function(x) {
  x <- unique.default(c("character", x))
  if ("ANY" %in% x)
    "ANY"
  else
    x
}


################################################################################


#' Map values
#'
#' Map values using a character vector, a function or a formula. This is not
#' normally directly called by an \pkg{opm} user because
#' \code{\link{map_metadata}} is available.
#'
#' @param object List (may be nested), data frame or character vector. If it has
#'   names, they are preserved. \code{NULL} can also be given and yields
#'   \code{NULL} or an empty named character vector (if \code{mapping} is
#'   missing). \code{object} may also belong to the virtual class
#'   \code{\link{MOA}}, comprising matrices and arrays.
#' @param mapping Character vector, function, formula, expression, \code{NULL}
#'   or missing.
#'   \itemize{
#'   \item If a character vector used as a mapping from its names to its values.
#'   Values from \code{object} are searched for in the \code{names} attribute of
#'   \code{mapping}; those found are replaced by the corresponding values of
#'   \code{mapping}.
#'   \item If \code{mapping} is missing, a character vector is returned (sorted
#'   and with duplicates removed) whose names are identical to the values. This
#'   eases the construction of mapping vectors specific for \code{object}. If
#'   \code{mapping} is missing, the \code{coerce} argument must be named.
#'   \code{mapping} changes its usage if \code{coerce} is \code{TRUE}.
#'   \item For \code{\link{MOA}} objects, if \code{mapping} was a function, it
#'   would be applied to \code{object} after conversion with \code{as.vector},
#'   and it would be attempted to add the original attributes (particularly
#'   important are \sQuote{dim} and \sQuote{dimnames} back to the result.
#'   \item For \code{\link{MOA}} objects, if \code{mapping} is the usual
#'   character vector, it then is used for mapping the \code{storage.mode}, not
#'   the \code{class} of \code{object}.
#'   \item \code{mapping} can also be a formula, it is then used to compute on
#'   lists. The see examples below.
#'   \item If \code{mapping} is an expression, all sub-expressions will be
#'   evualated in \code{object} represented as an environment, which after
#'   conversion back to a list, is returned.
#'   \item If \code{mapping} is \code{NULL} and \code{object} is a list, all
#'   contained objects of zero length are removed recursively.
#' }
#' @param coerce The usage of this argument depends on \code{object}.
#'   \itemize{
#'   \item A character vector with the names of classes that are coerced to
#'   \sQuote{character} to allow the mapping. Other classes are returned
#'   unchanged. Note that the coerced data are \strong{not} converted back to
#'   their original data type. \sQuote{ANY} can be used to indicate that all
#'   classes will be considered.
#'   \item Alternatively, \code{coerce} can be \code{TRUE}. \code{mapping} is
#'   then interpreted as a mapping between the names of classes, and \code{as}
#'   from the \pkg{methods} package is used for conducting the requested
#'   coercions. Attempting an undefined coercion will result in an error.
#'   \item For the formula method, an enclosing environment to look up objects
#'   that are not found in \code{mapping}.
#'   }
#' @param ... Optional further arguments to \code{mapping} (\strong{if} it is a
#'   function).
#' @export
#' @return List, data frame, character vector or \code{NULL}.
#' @seealso base::rapply base::list base::as.list methods::as base::class
#'   base::storage.mode base::as.vector
#' @family auxiliary-functions
#' @keywords manip list
#' @details Mapping of \sQuote{character} data using another \sQuote{character}
#'   vector is possible, as well as recursively applying a mapping function to
#'   all \sQuote{character} values within a list, or non-recursively to a data
#'   frame. Optionally other data types are coerced to \sQuote{character}; the
#'   remaining ones are returned unchanged. It is also possible to map between
#'   classes using coercion functions. For convenience in programming, methods
#'   for the \sQuote{NULL} class are also available.
#'
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
#' # same effect with an expression
#' (z <- map_values(x, expression(c <- a + b)))
#' stopifnot(identical(z, y))
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

setMethod("map_values", c("list", "NULL"), function(object, mapping,
    coerce = character()) {
  clean_recursively <- function(x) {
    if (!is.list(x))
      return(x)
    x <- lapply(x, clean_recursively)
    x[vapply(x, length, 0L) > 0L]
  }
  if (length(coerce))
    object <- rapply(object, as.character, prepare_class_names(coerce), NULL,
      "replace")
  clean_recursively(object)
}, sealed = SEALED)

setMethod("map_values", c("list", "missing"), function(object, mapping,
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

setMethod("map_values", c("list", "formula"), function(object, mapping,
    coerce = parent.frame()) {
  if (length(mapping) > 2L) {
    right <- eval(mapping[[3L]], object, coerce)
    left <- metadata_key.formula(mapping[-3L], FALSE, envir = coerce)
    if (is.list(left)) {
      right <- rep(right, length.out = length(left))
      for (i in seq_along(left))
        object[[left[[i]]]] <- right[[i]]
    } else
      object[[left]] <- right
    object
  } else
    eval(mapping[[2L]], object, coerce)
}, sealed = SEALED)

setMethod("map_values", c("list", "expression"), function(object, mapping,
    coerce = parent.frame()) {
  e <- list2env(object, NULL, coerce)
  for (subexpr in mapping)
    eval(subexpr, e)
  e <- as.list(e) # return 'e' if the order of list elements doesn't matter
  novel <- setdiff(names(e), names(object))
  for (name in setdiff(names(object), names(e)))
    object[[name]] <- NULL
  object[novel] <- e[novel]
  object
}, sealed = SEALED)

#-------------------------------------------------------------------------------

setMethod("map_values", c("data.frame", "function"), function(object, mapping,
    coerce = character(), ...) {
  if (identical("ANY", coerce <- prepare_class_names(coerce)))
    coerce <- unique(unlist((lapply(object, class))))
  for (i in which(vapply(object, inherits, NA, coerce)))
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

setMethod("map_values", c("data.frame", "NULL"), function(object, mapping,
    coerce = character(), ...) {
  if (identical("ANY", coerce <- prepare_class_names(coerce)))
    coerce <- unique(unlist((lapply(object, class))))
  for (i in which(vapply(object, inherits, NA, coerce)))
    object[[i]] <- as.character(object[[i]])
  object
}, sealed = SEALED)

setMethod("map_values", c("data.frame", "missing"), function(object,
    coerce = character()) {
  if (isTRUE(coerce))
    result <- unlist(lapply(object, class))
  else {
    coerce <- prepare_class_names(coerce)
    if (!"ANY" %in% coerce)
      object <- object[, vapply(object, inherits, NA, coerce),
        drop = FALSE]
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
  object <- sort.int(unique.default(object))
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
#' frame. This function is not normally directly called by an \pkg{opm} user
#' because \code{\link{map_metadata}} is available.
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
#' @details In the case of lists, the function is not applied to list elements
#'   which are not themselves lists, even if they have a \sQuote{names}
#'   attribute. Such elements and their names, if any, are returned unchanged.
#'   If a \sQuote{names}, \sQuote{colnames} or \sQuote{rownames} attribute is
#'   \code{NULL}, it is ignored.
#'
#'   Alternatively, instead of mapping the names, collect them and return them
#'   as a single character vector, sorted and with duplicates removed. The
#'   collected names are added as their own \code{names} attribute; this might
#'   be useful if the result is later on used for some mapping (using this
#'   function or \code{\link{map_values}}).
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
#' interpreted as character. This function fixes this problem.
#'
#' @param object Character vector or list.
#' @param type Character scalar denoting the type to which input character
#'   vectors shall be tried to be converted.
#' @return Character vector or list.
#' @seealso utils::type.convert
#' @details This problem does not occur anymore with the \pkg{yaml} package of
#'   at least version 2.1.7, but for legacy \acronym{YAML} (and \acronym{JSON})
#'   files it is necessary to conduct the conversions implemented here.
#' @keywords internal
#' @references \url{http://www.yaml.org/}
#'
repair_na_strings <- function(object, ...) UseMethod("repair_na_strings")

#' @rdname repair_na_strings
#' @method repair_na_strings character
#'
repair_na_strings.character <- function(object, ...) {
  object[grepl("^(\\s*NA|\\.na(\\.(real|integer|character))?)$", object,
    perl = TRUE)] <- NA_character_
  object
}

#' @rdname repair_na_strings
#' @method repair_na_strings list
#'
repair_na_strings.list <- function(object,
    type = c("double", "integer", "complex", "logical", "character"), ...) {
  type <- match.arg(type)
  mapfun <- if (type == "character")
    repair_na_strings.character
  else
    function(x) tryCatch({
      x <- repair_na_strings.character(x)
      storage.mode(x) <- type
      x
    }, warning = function(w) x)
  rapply(object, mapfun, "character", NULL, "replace")
}


################################################################################
################################################################################
#
# Lists
#


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
#' recursively to all sublists.
#'
#' @param object List containing the data, or \code{\link{OPMS}} object.
#' @param other For the list method, a list used as query; for the
#'   \code{\link{OPMS}} method, an \code{\link{OPM}} object used as query.
#' @param values Logical scalar. Compare also the values or only the keys? If
#'   \code{FALSE}, \code{exact} is ignored.
#' @param exact Logical scalar. If \code{FALSE}, the data value(s) might by any
#'   of the query value(s), and some coercion is done before comparing (see
#'   \code{match} for details.
#'
#'   If \code{TRUE}, the data value(s) must exactly correspond to the query
#'   value(s), and no coercion is done (see \code{identical}) for details). This
#'   might be too strict for most applications.
#' @param ... Optional arguments passed to \code{identical} from the \pkg{base}
#'   package, allowing for fine-control of identity. Has no effect unless
#'   \code{exact} is \code{TRUE}.
#' @export
#' @return Logical scalar.
#' @details  Non-list elements are ignored if \code{values} is \code{FALSE}.
#'   Otherwise the comparison is done using \code{identical} if \code{exact} is
#'   \code{TRUE}. If \code{exact} is \code{FALSE}, the value(s) in the data list
#'   can be any of the values at the corresponding position in the query list,
#'   and the comparison is done by coercion to character vectors. An empty query
#'   list results in \code{TRUE}. Missing names in a non-empty query list result
#'   in \code{FALSE}.
#'
#'   There are also \code{\link{OPMS}} and \code{\link{OPM}} methods, which
#'   test, for instance, whether an \code{\link{OPM}} object is contained in an
#'   \code{\link{OPMS}} object.
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
#' stopifnot(contains(vaas_4, vaas_4[3])) # single one contained
#' stopifnot(contains(vaas_4, vaas_4)) # all contained
#' stopifnot(!contains(vaas_4[3], vaas_4)) # OPMS cannot be contained in OPM
#' stopifnot(contains(vaas_4[3], vaas_4[3])) # identical OPM objects
#' stopifnot(!contains(vaas_4[3], vaas_4[2])) # non-identical OPM objects
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

setMethod("contains", c(OPMS, OPMS), function(object, other, ...) {
  single_contained <- function(x) {
    for (plate in object@plates)
      if (identical(x = plate, y = x, ...))
        return(TRUE)
    FALSE
  }
  vapply(other@plates, single_contained, NA)
}, sealed = SEALED)

setMethod("contains", c(OPM, OPMS), function(object, other, ...) {
  FALSE
}, sealed = SEALED)

setMethod("contains", c(OPM, OPM), function(object, other, ...) {
  identical(x = object, y = other, ...)
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
#'     \item{comb.key.join}{Used by functions that support combination of
#'       metadata entries converted to data-frame columns immediately after
#'       their selection. Sets the character string that is used when joining
#'       old names to new name.}
#'     \item{comb.value.join}{Used by functions that support combination of
#'       metadata entries converted to data-frame columns immediately after
#'       their selection. Sets the character string that is used when joining
#'       old values to new values.}
#'     \item{contrast.type}{Character scalar indicating the default type of
#'       contrast used by \code{\link{opm_mcp}}.}
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
#'     \item{disc.param}{Character scalar. Default \sQuote{subset} argument of
#'       \code{\link{do_disc}}. It is usually not advisable to  change it.}
#'     \item{digits}{Integer scalar. Number of digits used by some functions
#'       generating output text.}
#'     \item{file.encoding}{Character scalar. Character encoding in input files
#'       as assumed by \code{\link{read_opm}}.}
#'     \item{file.split.tmpl}{Character scalar. Template used as \sQuote{format}
#'       argument by \code{\link{split_files}}.}
#'     \item{gen.iii}{Character scalar indicating whether \code{\link{read_opm}}
#'       and other IO functions based on it automatically convert to this plate
#'       type. If empty, nothing is changed.}
#'     \item{group.name}{Character scalar used as column name for trivial
#'       groups (either all items in the same group or each item in a group of
#'       its own) created by \code{\link{extract}}.}
#'     \item{heatmap.colors}{Colour palette used by \code{\link{heat_map}}.}
#'     \item{html.attr}{Used by \code{\link{phylo_data}} for automatically
#'       creating \acronym{HTML} \sQuote{title} and \sQuote{class} attributes.}
#'     \item{key.join}{Used by \code{\link{metadata}} and some other functions
#'       that must be in sync with it for joining metadata keys used in nested
#'       queries (because the resulting object is \sQuote{flat}).}
#'     \item{min.mode}{Used when making discretization results uniform within a
#'       group. The minimum proportion the most frequent value much reach to be
#'       used for representing all values (if less, frequent, \code{NA} is
#'       used). Must be a numeric scalar between 0 and 1.}
#'     \item{phylo.fmt}{Character scalar indicating the default output format
#'       used by \code{\link{phylo_data}}.}
#'     \item{split}{Character scalar indicating the default splitting characters
#'       used by \code{\link{separate}}.}
#'     \item{strict.OPMD}{Logical scalar indicating whether \code{\link{OPMD}}
#'       objects can only be created if the discretized data are consistent with
#'       the parameter from which they have been estimated.}
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
  old <- mget(names(x), envir = OPM_OPTIONS)
  for (i in seq_along(x))
    if (!all(inherits(x[[i]], class(old[[i]]), TRUE)))
      stop(sprintf("new and old value have conflicting class(es) for key '%s'",
        names(x)[i]))
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

