

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
  no.opm <- which(!sapply(object, inherits, OPM))
  if (length(no.opm) > 0L) {
    bad.classes <- sapply(object[no.opm], class)
    errs <- c(errs, paste("wrong class:", bad.classes))
    return(errs) # further checks are impossible in that case
  }
  if (!isTRUE(isuni <- is_uniform(sapply(object, plate_type))))
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


#' OPMS class
#'
#' Class for holding multi-plate OmniLog(R) phenotype microarray data with or
#' without aggregated values. The data may have been obtained from distinct
#' organisms and/or replicates, but \strong{must} correspond to the same plate
#' type and \strong{must} contain the same wells. Regarding the name: 
#' \sQuote{OPMS} is just the plural of \sQuote{OPM}.
#'
#' @docType class
#'
#' @export
#' @note As a rule, OPMS has the same methods as the \code{\link{OPM}} class,
#'   but adapted to a collection of more than one \code{\link{OPM}} object.
#'   Only the additional ones and those with special arguments and/or behaviors
#'   are documented in detail. Also, OPMS can hold \code{\link{OPMA}} as well
#'   as \code{\link{OPM}} objects, even though this is not indicated for all its
#'   methods in this manual.
#' @family classes
#' @seealso Methods
#' @keywords methods
#'
setClass(OPMS,
  representation = representation(plates = "list"),
  validity = function(object) {
    if (length(errs <- opms_problems(object@plates)))
      errs
    else
      TRUE
  },
  sealed = SEALED
)


################################################################################


#' Initialize
#'
#' Initialize method for the \code{\link{OPMS}} class.
#'
#' @name initialize,OPMS
#'
#' @param .Object \code{\link{OPMS}} object.
#' @param ... Additional arguments.
#' @return \code{\link{OPMS}} object.
#' @keywords internal
#'
setMethod("initialize", OPMS, function(.Object, ...) {
  .Object <- callNextMethod()
  names(.Object@plates) <- NULL
  .Object
}, sealed = SEALED)


################################################################################
################################################################################
#
# Combination functions
#


#' Addition
#'
#' Combine an \code{\link{OPM}} or \code{\link{OPMS}} object with another
#' \code{\link{OPM}} or \code{\link{OPMS}} object or a list of such objects.
#' Raise an error if the objects are incompatible.
#'
#' @name plus
#' @exportMethod "+"
#'
#' @param e1 \code{\link{OPM}} or \code{\link{OPMS}} object.
#' @param e2 \code{\link{OPM}} or \code{\link{OPMS}} object, or list.
#' @return \code{\link{OPMS}} object that contains the plates from both
#'   \code{e1} and \code{e2}.
#' @family combination-functions
#' @keywords manip
#'
#' @examples 
#'
#' data(vaas_1)
#' data(vaas_4)
#' # the examples do not show particularly useful additions, as the plates
#' # are either entirely or partially identical
#'
#' # OPM+OPM method
#' x <- vaas_1 + vaas_1
#' stopifnot(identical(dim(x), c(2L, dim(vaas_1))))
#'
#' # OPM+OPMS method
#' x <- vaas_1 + vaas_4
#' stopifnot(identical(dim(x), c(5L, dim(vaas_1))))
#'
#' # OPM+list method
#' x <- vaas_1 + list(vaas_1, vaas_1)
#' stopifnot(identical(dim(x), c(3L, dim(vaas_1))))
#'
#' # OPMS+OPMS method
#' x <- vaas_4 + vaas_4
#' stopifnot(identical(dim(x), c(8L, dim(vaas_4)[-1L])))
#'
#' # OPMS+OPM method
#' x <- vaas_4 + vaas_1
#' stopifnot(identical(dim(x), c(5L, dim(vaas_1))))
#'
#' # OPMS+list method
#' x <- vaas_4 + list(vaas_1)
#' stopifnot(identical(dim(x), c(5L, dim(vaas_1))))
#'
setMethod("+", c(OPM, OPM), function(e1, e2) {
  new(OPMS, plates = list(e1, e2))
}, sealed = SEALED)

#' @export
#'
setMethod("+", c(OPM, OPMS), function(e1, e2) {
  new(OPMS, plates = c(list(e1), plates(e2)))
}, sealed = SEALED)

#' @export
#'
setMethod("+", c(OPM, "list"), function(e1, e2) {
  new(OPMS, plates = c(list(e1), e2))
}, sealed = SEALED)

#' @export
#'
setMethod("+", c(OPMS, OPMS), function(e1, e2) {
  new(OPMS, plates = c(plates(e1), plates(e2)))
}, sealed = SEALED)

#' @export
#'
setMethod("+", c(OPMS, OPM), function(e1, e2) {
  new(OPMS, plates = c(plates(e1), e2))
}, sealed = SEALED)

#' @export
#'
setMethod("+", c(OPMS, "list"), function(e1, e2) {
  new(OPMS, plates = c(plates(e1), e2))
}, sealed = SEALED)


################################################################################
################################################################################
#
# Getter functions
#


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


#' @export
#'
setMethod("dim", OPMS, function(x) {
  c(length(x@plates), dim(x@plates[[1L]]))
}, sealed = SEALED)


################################################################################


setGeneric("seq")
#' Sequence of plate indexes
#'
#' Get the indexes of all plates contained in an \code{\link{OPMS}} object. 
#' This is mainly useful for looping over such objects. See \code{\link{[}} for
#' a loop-construct usage example, and note that \code{\link{oapply}} is also
#' available.
#'
#' @param ... \code{\link{OPMS}} objects. Only the first one is used.
#' @return Integer vector (starting with 1 and at least of length 2).
#' @export
#' @family getter-functions
#' @keywords attribute
#' @seealso base::seq
#' @examples 
#' data(vaas_4)
#' (x <- seq(vaas_4))
#' stopifnot(identical(x, 1:4))
#'
setMethod("seq", OPMS, function(...) {
  seq_along(..1@plates)
}, sealed = SEALED)


################################################################################


setGeneric("plates", function(object, ...) standardGeneric("plates"))
#' Get available plates
#'
#' Get all plates contained in an \code{\link{OPMS}} object.
#'
#' @param object \code{\link{OPMS}} object.
#' @return List of \code{\link{OPM}} objects.
#' @export
#' @family conversion-functions
#' @keywords attribute
#' @seealso base::list base::as.list
#' @examples 
#' data(vaas_4)
#' x <- plates(vaas_4)
#' stopifnot(is.list(x), length(x) == 4L)
#'
setMethod("plates", OPMS, function(object) {
  object@plates
}, sealed = SEALED)


################################################################################


#' @export
#'
setMethod("max", OPMS, function(x, ..., na.rm = FALSE) {
  max(sapply(x@plates, FUN = max, ..., na.rm = na.rm), na.rm = na.rm)
}, sealed = SEALED)


################################################################################


#' @export
#'
setMethod("minmax", OPMS, function(x, ..., na.rm = FALSE) {
  min(sapply(x@plates, FUN = minmax, ..., na.rm = na.rm))
}, sealed = SEALED)


################################################################################


#' @export
#'
setMethod("summary", OPMS, function(object, ...) {
  invisible(lapply(seq_along(object@plates), FUN = function(idx) {
    message(idx)
    summary(object@plates[[idx]], ...)
  }))
}, sealed = SEALED)


################################################################################


setGeneric("oapply", function(object, ...) standardGeneric("oapply"))
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
setMethod("oapply", OPMS, function(object, fun, ..., simplify = TRUE) {
  result <- sapply(X = object@plates, FUN = fun, ..., simplify = simplify, 
    USE.NAMES = FALSE)
  if (simplify && is.list(result))
    result <- try_opms(result)
  result
}, sealed = SEALED)


################################################################################


setGeneric("duplicated")
#' Determine duplicated plates
#'
#' Check whether duplicated \code{\link{OPM}} or \code{\link{OPMA}} objects
#' are contained within an \code{\link{OPMS}} object.
#'
#' @param x \code{\link{OPMS}} object.
#' @param incomparables Vector of values that cannot be compared. See 
#'   \code{duplicated} from the \pkg{base} package for details.
#' @param ... Optional other arguments passed to that function.
#' @export
#' @return Logical vector.
#' @family getter-functions
#' @keywords attribute
#' @examples 
#' data(vaas_4)
#' stopifnot(!duplicated(vaas_4))
#' x <- vaas_4[c(1, 1)]
#' stopifnot(c(FALSE, TRUE) == duplicated(x))
#'
setMethod("duplicated", OPMS, function(x, incomparables = FALSE, ...) {
  duplicated(x = x@plates, incomparables = incomparables, ...)
}, sealed = SEALED)


################################################################################

  
setGeneric("anyDuplicated")
#' Determine whether plates are duplicated
#'
#' Check whether duplicated \code{\link{OPM}} or \code{\link{OPMA}} objects
#' are contained within an \code{\link{OPMS}} object.
#'
#' @param x \code{\link{OPMS}} object.
#' @param incomparables Vector of values that cannot be compared. See 
#'   \code{anyDuplicated} from the \pkg{base} package for details.
#' @param fromLast Logical scalar. See  below and
#'   \code{anyDuplicated} from the \pkg{base} package for details.
#' @param ... Optional other arguments passed to that function.
#' @export
#' @return Integer scalar. \code{0} if no values are duplicated, the index of
#'   the first or last (depending on \code{fromLast}) duplicated object 
#'   otherwise.
#' @family getter-functions
#' @keywords attribute
#' @examples 
#' data(vaas_4)
#' stopifnot(!anyDuplicated(vaas_4))
#' x <- vaas_4[c(1, 1)]
#' stopifnot(anyDuplicated(x) == 2)
#'
setMethod("anyDuplicated", OPMS, function(x, incomparables = FALSE, 
    fromLast = FALSE, ...) {
  anyDuplicated(x = x@plates, incomparables = incomparables, 
    fromLast = fromLast, ...)
}, sealed = SEALED)
  

################################################################################


## These are deliberately not defined for OPMS:
## * other_slots()
## * attach_attr()


# Applying OPM methods with function(object, ...) signature to the 1st plate
# only.
#
lapply(c(
    wells,
    plate_type
  ), FUN = function(func) {
  setMethod(func, OPMS, function(object, ...) {
    func(object@plates[[1L]], ...)
  }, sealed = SEALED)
})


# OPM methods with function(object, ...) signature that can conditionally be
# simplified.
#
lapply(c(
    aggregated,
    aggr_settings,
    csv_data,
    has_aggr,
    hours,
    measurements,
    metadata,
    filename,
    position,
    setup_time,
    well
  ), FUN = function(func) {
  setMethod(func, OPMS, function(object, ...) {
    simplify_conditionally <- function(x) { # instead of sapply()
      if (any(sapply(x, is.list)) || any(sapply(x, is.matrix)))
        return(x)
      if (length(len <- unique(sapply(x, length))) > 1L)
        return(x)
      if (len > 1L)
        do.call(rbind, x)
      else
        unlist(x)
    }
    simplify_conditionally(lapply(object@plates, FUN = func, ...))
  }, sealed = SEALED)
})


################################################################################
################################################################################
#
# Setter functions
#


# Based on OPM methods with function(object, ...) signature that return OPM(A) 
# objects.
#
lapply(c(
    gen_iii,
    do_aggr,
    include_metadata,
    thin_out
  ), FUN = function(func) {
  setMethod(func, OPMS, function(object, ...) {
    new(OPMS, plates = lapply(object@plates, FUN = func, ...))
  }, sealed = SEALED)
})


################################################################################
################################################################################
#
# Metadata functions (including the infix operators)
# 


# OPM methods with function(x, table, ...) signature (infix operators).
#
lapply(c("%k%", "%K%", "%q%", "%Q%"), FUN = function(func) {
  lapply(c("list", "character"), FUN = function(klass) {
    setMethod(func, c(klass, OPMS), function(x, table) {
      sapply(table@plates, func, x = x, USE.NAMES = FALSE)
    }, sealed = SEALED)
  })
})


################################################################################


#' @export
#'
setMethod("map_metadata", c(OPMS, "ANY"), function(object, mapping, ...) {
  object@plates <- lapply(object@plates, FUN = map_metadata, mapping = mapping,
    ...)
  object
}, sealed = SEALED)


################################################################################


#' @name metadata.set
#' @export
#'
setMethod("metadata<-", c(OPMS, "missing", "list"), function(object, value) {
  for (i in seq_along(object@plates))
    metadata(object@plates[[i]]) <- value
  object    
}, sealed = SEALED)

#' @name metadata.set
#' @export
#'
setMethod("metadata<-", c(OPMS, "ANY", "ANY"), function(object, key, value) {
  for (i in seq_along(object@plates))
    metadata(object@plates[[i]], key) <- value
  object
}, sealed = SEALED)


################################################################################


#' @export
#'
setMethod("metadata_chars", OPMS, function(object, ...) {
  result <- lapply(object@plates, FUN = metadata_chars, ...)
  result <- sort(unique(unlist(result)))
  structure(.Data = result, names = result) # unique() removes the names
}, sealed = SEALED)


################################################################################
################################################################################
#
# Conversion functions: OPMS => lists.
#


setAs(from = OPMS, to = "list", function(from) {
  lapply(from@plates, as, Class = "list")
})


################################################################################
################################################################################
#
# Thinning out and subsetting
#


## for thin_out() see above


#' @export
#'
setMethod("[", OPMS, function(x, i, j, ..., drop = FALSE) {
  result <- x@plates[i]
  if (missing(j)) {
    if (!missing(...) || drop)
      result <- lapply(result, FUN = function(obj) obj[, ..., drop = drop])
  } else if (is.list(j))
    result <- mapply(FUN = function(obj, jj) obj[jj, ..., drop = drop],
      result, j, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  else
    result <- lapply(result, FUN = function(obj) obj[j, ..., drop = drop])
  if ((len <- length(result)) == 0L)
    NULL
  else if (len == 1L)
    result[[1L]]
  else {
    x@plates <- result
    x
  }
}, sealed = SEALED)


################################################################################


setGeneric("select", function(object, ...) standardGeneric("select"))
#' Select a subset of the plates (or time points)
#'
#' Select a subset of the plates in an \code{\link{OPMS}} object based on the 
#' content of the metadata. Alternatively, select a common subset of time 
#' points from all plates.
#'
#' @param object \code{\link{OPMS}} object.
#' @param query Logical, numeric or character vector, or list (other objects 
#'   can be provided but are coerced to class \sQuote{character}). If a logical
#'   or numeric vector, \code{query} is directly used as the first argument of
#'   \code{\link{[}}, and all following arguments, if any, are ignored.
#'   If a list or a character vector, it is used for conducting a query based
#'   on one of the infix operators as described below.
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
#'   and the object is reduced to a common subset of time point (measurement
#'   hours and minutes).
#' @param use Character scalar. An alternative way to specify the settings. If
#'   \sQuote{i} or \sQuote{I}, ignored. If \sQuote{t} or \sQuote{T}, 
#'   \code{time} is set to \code{TRUE}. Otherwise, \code{use} is taken directly
#'   as the one-latter name of the infix operators to use for plate
#'   selection, overriding \code{values} and \code{exact}.
#' @export
#' @return \code{NULL} or \code{\link{OPM}} or \code{\link{OPMS}} object. This
#'   depends on how many plates are selected; see \code{\link{[}} for details.
#'   
#' @family getter-functions
#' @keywords manip
#' @seealso base::`[` base::`[[` base::subset
#' @examples 
#'
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
setMethod("select", OPMS, function(object, query, values = TRUE,
    invert = FALSE, exact = FALSE, time = FALSE, 
    use = c("i", "I", "k", "K", "q", "Q", "t", "T")) {
  switch(match.arg(use),
    i =, I = NULL,
    k =, K = values <- FALSE,
    q = {
      values <- TRUE
      exact <- FALSE
    },
    Q = {
      values <- TRUE
      exact <- TRUE
    },
    t =, T = time <- TRUE,
    stop(BUG_MSG)
  )
  if (time) {
    tp <- hours(object, what = "all")
    if (is.matrix(tp))
      tp <- lapply(seq.int(nrow(tp)), function(i) tp[i, ])
    if (length(maxs <- unique(sapply(tp, max))) < 2L)
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


################################################################################
################################################################################
#
# Extraction of character matrices
#


setGeneric("extract_columns",
  function(object, ...) standardGeneric("extract_columns"))
#' Create data frame or vector from metadata
#'
#' Extract selected metadata entries for use as additional columns in a  
#' dataframe or (after joining) as character vector with labels. This is not
#' normally directly called by an \pkg{opm} user because
#' \code{\link{extract}} is available, which uses this function, but can be 
#' used for testing the applied metadata selections beforehand.
#'
#' @param object \code{\link{OPMS}} object.
#' @param what List of metadata keys to consider, or single such key; passed
#'   to \code{\link{metadata}}.
#' @param join Logical scalar. Join each row together to yield a character
#'   vector? Otherwise it is just attempted to construct a data frame.
#' @param sep Character scalar. Used as separator between the distinct metadata
#'   entries if these are to be pasted together. Ignored unless \code{join} 
#'   is \code{TRUE}.
#' @param dups Character scalar specifying what to do in the case of duplicate
#'   labels: either \sQuote{warn}, \sQuote{error} or \sQuote{ignore}. Ignored
#'   unless \code{join} is \code{TRUE}.
#' @param exact Logical scalar. Also passed to \code{\link{metadata}}.
#' @param strict Logical scalar. Also passed to \code{\link{metadata}}.
#' @export
#' @return Data frame or character vector, depending on the \code{join} 
#'   argument.
#' @family conversion-functions
#' @family metadata-functions
#' @keywords dplot manip
#' @seealso base::data.frame base::as.data.frame base::cbind
#' @examples 
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
#' stopifnot(is(x, "try-error"))
#' (x <- try(extract_columns(vaas_4, what = list("Species"), join = TRUE,
#'   dups = "warn"), silent = TRUE))
#' stopifnot(is.character(x), length(x) == 4L)
#'
setMethod("extract_columns", OPMS, function(object, what, join = FALSE,
    sep = " ", dups = c("warn", "error", "ignore"), exact = TRUE, 
    strict = TRUE) {
  result <- metadata(object, as.list(what), exact = exact, strict = strict)
  result <- lapply(result, FUN = rapply, f = as.character)
  if (join) {
    labels <- unlist(lapply(result, FUN = paste, collapse = sep))
    msg <- if (is.dup <- anyDuplicated(labels))
      paste("duplicated label:", labels[is.dup])
    else
      NULL
    if (!is.null(msg))
      switch(match.arg(dups),
        ignore = NULL,
        warn = warning(msg),
        error = stop(msg),
        stop(BUG_MSG)
      )
    labels
  } else
    tryCatch(as.data.frame(do.call(rbind, result)),
      warning = function(w) stop(w$message))
}, sealed = SEALED)


################################################################################


setGeneric("extract", function(object, ...) standardGeneric("extract"))
#' Extract aggregated values
#'
#' Extract selected aggregated values into common matrix or dataframe.
#'
#' @param object \code{\link{OPMS}} object.
#' @param as.labels List. Metadata to be joined and used as row names (if
#'   \code{dataframe} is \code{FALSE}) or additional columns (if otherwise).
#'   Ignored if \code{NULL}.
#'
#' @param subset Character vector. The parameter(s) to put in the matrix.
#' @param ci Logical scalar. Also return the CIs?
#' @param trim Character scalar. See \code{\link{aggregated}} for details.
#' @param dataframe Logical scalar. Return dataframe or matrix?
#'
#' @param as.groups List. Metadata to be joined and used as \sQuote{row.groups}
#'   attribute of the output matrix. See \code{\link{heat_map}} for its usage.
#'   Ignored if \code{NULL} and if \code{dataframe} is \code{FALSE}. 
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
#' @export
#' @return Numeric matrix or dataframe.
#' @family conversion-functions
#' @seealso base::data.frame base::as.data.frame base::matrix base::as.matrix
#' @keywords manip dplot
#' @examples 
#' data(vaas_4)
#' # Matrix
#' (x <- extract(vaas_4, as.labels = list("Species", "Strain")))
#' stopifnot(is.matrix(x), identical(dim(x), c(4L, 96L)))
#' # Data frame
#' (x <- extract(vaas_4, as.labels = list("Species", "Strain"), 
#'   dataframe = TRUE))
#' stopifnot(is.data.frame(x), identical(dim(x), c(4L, 99L)))
#' # All parameters in a single dataframe
#' x <- lapply(param_names(), function(name) extract(vaas_4, subset = name,
#'   as.labels = list("Species", "Strain"), dataframe = TRUE))
#' x <- do.call(rbind, x)
#'
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


################################################################################
################################################################################
#
# CI plot methods
#


setGeneric("ci_plot", function(object, ...) standardGeneric("ci_plot"))
#' Plot point estimates with CIs
#'
#' Draw point estimates with their confidence intervals. The dataframe method
#' is not normally directly called by an \pkg{opm} user but via the 
#' \code{\link{OPMS}} method. This one is used for comparing aggregated values
#' together with their confidence intervals between plates. This method can in
#' most cases \strong{not} be applied to entire plates but to selected wells 
#' only.
#'
#' @param object Dataframe or \code{\link{OPMS}} object. If an 
#'   \code{\link{OPMS}} object, it is in most cases necessary to restrict
#'   the plates to at most about one dozen wells. See \code{\link{[}}
#'   for how to achieve this. The dataframe should be as exported by 
#'   \code{\link{extract}} with \code{ci}
#'   set to \code{TRUE}. There must be a column named \sQuote{Parameter}
#'   followed by columns with only numeric values. Columns before the
#'   \sQuote{Parameter} column, if any, are used for grouping. The rows must
#'   entirely comprise triplets representing (i) the point estimate, (ii)
#'   the lower and (iii) the upper confidence interval.
#'
#' @param as.labels List. Metadata to be joined and used to draw a legend.
#'   Ignored if \code{NULL}.
#' @param subset Character scalar. The parameter to plot. Only a single one
#'   can be selected. See \code{\link{param_names}} for the options.
#'
#' @param rowname.sep Character scalar. Used when joining explanatory columns
#'   into row labels of the plots.
#'
#' @param prop.offset Numeric scalar. A proportional offset that is added to 
#'   the vertical range of the panels (after determining the maximum range
#'   among all panels to ensure consistency within the plot).
#' @param align Character scalar. How to apply the offset; one of 
#'   \sQuote{center}, \sQuote{left} and \sQuote{right}.
#'
#' @param col Character scalar. Color to be used.
#'
#' @param na.action Character scalar. What to do if a confidence interval 
#'   contains \code{NA} values; one of \sQuote{ignore}, \sQuote{warn} and 
#'   \sQuote{error}.
#'
#' @param draw.legend Logical scalar. Ignored if there are no explanatory
#'   columns.
#' @param legend.field Two-element numeric vector. Indicates the panel in which
#'   the legend is drawn. Subsequent arguments work then relative to this 
#'   panel. If \code{legend.field} has less then two fields, the number of
#'   panels is set to 1 (the entire plot), and the legend is drawn relative to
#'   that.
#' @param x Legend position, passed to \code{legend} from the \pkg{graphics}
#'   package. Ignored unless \code{draw.legend} is \code{TRUE}.
#' @param xpd Logical scalar. Also passed to that function.
#' @param ... Optional other arguments passed to that function, or arguments
#'   passed from the \code{\link{OPMS}} method to the dataframe method.
#'
#' @note \itemize{
#'  \item The default placement of the legend is currently not necessarily very
#'    useful. 
#'  \item When plotting entire PM plates, the \sQuote{mar} parameter of
#'     \code{par} most likely would need to be set to a lower value, but it
#'     is recommended to plot only subsets of plates, i.e. selected wells.
#'  }
#'
#' @references Vaas LAI, Sikorski J, Michael V, Goeker M, Klenk H-P. 
#'   Visualization and curve parameter estimation strategies for efficient 
#'   exploration of Phenotype Microarray kinetics. PLoS ONE 2012; in press.
#'
#' @return Character vector describing the plot's legend, returned invisibly.
#' @family plotting-functions
#' @seealso graphics::plot
#' @keywords hplot
#' @examples
#'
#' data(vaas_4)
#'
#' # most of the parameters used here are explained under the data.frame
#' # method of ci_plot()
#' x <- ci_plot(vaas_4[, , 1:3], as.labels = list("Species", "Strain"),
#'    subset = "A", x = "bottomright", legend.field = NULL)
#' # note that the values on the y axes are drawn to scale
#' x
#' stopifnot(is.character(x), identical(length(x), 4L))
#' # ... and that the return value contains the legend (even if it is not drawn)
#'
setMethod("ci_plot", "data.frame", function(object, rowname.sep = " ", 
    prop.offset = 0.04, align = "center", col = "blue", na.action = "warn",
    draw.legend = TRUE, legend.field = c(1, 1), x = "topleft", xpd = TRUE, 
    ...) {

  single_plot <- function(col.pos) {
    plot(x = NULL, y = NULL, xlim = ranges[, col.pos], ylim = ylim,
      main = colnames(object)[col.pos], yaxt = "n", xlab = "", ylab = "")
    axis(2L, at = chunk.pos, labels = row.names)
    sapply(chunk.pos, FUN = function(pos) {
      pe <- object[pos, col.pos]
      left <- object[pos + 1L, col.pos]
      right <- object[pos + 2L, col.pos]
      draw_ci(c(left, pe, right, pos), col = col, na.action = na.action)
    })
  }

  # Check the triplet structure and determine all triplet start positions
  if (nrow(object) %% 3L != 0L)
    stop("need dataframe with 3 * n rows")
  chunk.pos <- seq.int(nrow(object))
  chunk.pos <- chunk.pos[chunk.pos %% 3L == 1L]
  row.names <- as.character(seq_along(chunk.pos))

  # Determine the 'Parameter' position, used for splitting the dataframe
  param.pos <- which(colnames(object) == "Parameter")
  if (length(param.pos) != 1L)
    stop("need dataframe with one column called 'Parameter'")
  if (param.pos == ncol(object))
    stop("the data columns are missing")

  # Reorder the matrix and construct the legend if necessary
  if (param.pos > 1L) {
    factor.pos <- seq.int(1L, param.pos - 1L)
    ordering <- do.call(order, as.list(object[, factor.pos, drop = FALSE])) 
    object <- object[ordering, , drop = FALSE]
    legend <- as.matrix(object[chunk.pos, factor.pos, drop = FALSE])
    legend <- apply(legend, 1L, paste, collapse = rowname.sep)
    legend <- paste(row.names, legend, sep = ": ")
  } else
    legend <- NULL

  # Reduce to the numeric part of matrix
  object <- as.matrix(object[, seq.int(param.pos + 1L, ncol(object)), 
    drop = FALSE])

  # Determine field range (which is set to be uniform)
  ranges <- apply(object, 2L, range, na.rm = TRUE)
  max.range <- max(apply(ranges, 2L, FUN = function(x) x[2L] - x[1L]))
  ranges <- apply(ranges, 2L, FUN = best_range, target = max.range,
    align = align, prop.offset = prop.offset)
  ylim <- best_range(chunk.pos, target = NULL, prop.offset = prop.offset)

  # Panel layout and plotting of individual panels
  old.par <- par(mfcol = best_layout(ncol(object)))
  on.exit(par(old.par))              
  sapply(seq.int(ncol(object)), FUN = single_plot)
    
  # Legend
  if (draw.legend && !is.null(legend)) {
    if (length(legend.field) > 1L)
      par(mfg = legend.field[1L:2L])  
    else
      par(mfcol = c(1L, 1L))
    legend(x = x, legend = legend, xpd = xpd, ...)
  }
  invisible(legend)

}, sealed = SEALED)

#' @export
#'
setMethod("ci_plot", OPMS, function(object, as.labels, subset = "A", ...) {
  ci_plot(extract(object, as.labels = as.labels, subset = subset,
    dataframe = TRUE, ci = TRUE), ...)
}, sealed = SEALED)


################################################################################
################################################################################
#
# XY plot and level plot, and their helper functions
#


#' @export
#'
setMethod("flatten", OPMS, function(object, include = NULL, fixed = list(),
    ...) {
  plate.nums <- paste("Plate", seq_along(object@plates))
  do.call(rbind, mapply(FUN = function(plate, plate.num) {
    flatten(plate, include = include, 
      fixed = c(list(Plate = plate.num), fixed), ...)
  }, object@plates, plate.nums, SIMPLIFY = FALSE))
}, sealed = SEALED)


################################################################################


setGeneric("flattened_to_factor", 
  function(object, ...) standardGeneric("flattened_to_factor"))
#' Factor from flattened data
#'
#' Extract all plate-specifying information from a dataframe as created by
#' \code{\link{flatten}}. If metadata have been included, these will be joined
#' together; otherwise the plate identifiers (basically numbers) themselves are
#' used.
#'
#' @param data Object as returned by \code{\link{flatten}}.
#' @param sep Character scalar. Separator used for joining the columns
#'   together.
#' @return Factor with one entry per plate.
#' @keywords internal
#'
setMethod("flattened_to_factor", "data.frame", function(object, sep = " ") {
  assert_length(plate.pos <- which(colnames(object) == "Plate"), sep)
  if (plate.pos == 1L)
    return(unique(object$Plate))
  result <- aggregate(object[, seq.int(1L, plate.pos)],
    by = list(object$Plate), FUN = `[[`, i = 1L)
  result <- as.list(result[, seq.int(2L, ncol(result) - 1L), drop = FALSE])
  as.factor(do.call(paste, c(result, sep = sep)))
}, sealed = SEALED)
  

################################################################################


#' @export
#'
setMethod("xy_plot", OPMS, function(x, col = "nora", lwd = 1,
    neg.ctrl = "A01", base.col = "black", base.lwd = lwd,
    main = list(), xlab = "Time [h]", ylab = "Value [OmniLog units]",
    theor.max = TRUE, draw.grid = TRUE, space = "top",
    strip.fmt = list(), striptext.fmt = list(),
    legend.fmt = list(), legend.sep = " ", draw.legend = TRUE,
    ...) {

  ## BEGIN must be synchronized with xy_plot,OPM
  
  # Setup
  layout <- best_layout(dim(x)[3L])
  y.max <- improved_max(x, theor.max)
  main <- main_title(x, main)
  neg.ctrl <- negative_control(x, neg.ctrl)

  # Adding default to settings lists. insert() is used here: for some reason 
  # the later entries have precedence in striptext.fmt
  strip.fmt <- insert(as.list(strip.fmt), bg = "grey90")
  striptext.fmt <- insert(as.list(striptext.fmt), cex = 1.5 / sqrt(layout[2L]), 
    lines = 1.25)  

  ## END must be synchronized with xy_plot,OPM

  # OPMS-specific addition of defaults
  legend.fmt <- insert(as.list(legend.fmt), space = space)

  # Selection of a color set
  col <- tryCatch(select_colors(col), error = function(e) col)
  
  # Conversion
  data <- flatten(x, ...)

  # Assignment of colors to plates
  param <- flattened_to_factor(object = data, sep = legend.sep)
  key.text <- levels(param)
  if (length(col) < length(key.text))
    stop("color should be by plate or metadata, but there are too few colors")
  key.col <- col[seq_along(key.text)]
  col <- col[param]
  
  # Plot
  lattice::xyplot(
    # Principally unchangeable arguments
    Value ~ Time | Well, data = data, type = "l", layout = layout,
    as.table = TRUE, groups = Plate,
    # Curve colors and panel height
    col = col, ylim = c(0, y.max),
    # Axis annotation
    scales = list(x = list(rot = 90)),
    # Description above each panel
    strip = do.call(lattice::strip.custom, strip.fmt), 
      par.strip.text = striptext.fmt,
    # Main annotation
    main = main, ylab = ylab, xlab = xlab,
    # Legend
    key = if (draw.legend)
      c(list(col = key.col, text = list(key.text)), legend.fmt)
    else
      NULL,
    panel = function(...) {
      if (draw.grid)
        lattice::panel.grid(h = -1, v = -1)
      if (!is.null(neg.ctrl))
        lattice::panel.abline(neg.ctrl, 0, col = base.col, lwd = base.lwd)
      lattice::panel.xyplot(..., lwd = lwd)
    }
  )

}, sealed = SEALED)


################################################################################


#' @export
#'
setMethod("level_plot", OPMS, function(x, main = list(),
    colors = NULL, panel.headers = TRUE, cex = NULL, strip.fmt = list(),
    striptext.fmt = list(), legend.sep = " ", ...) {
  dims <- dim(x)
  if (is.null(cex))
    cex <- guess_cex(dims[3L])
  data <- flatten(x, ...)
  if (is.null(panel.headers) || (is.logical(panel.headers) && !panel.headers))
    strip.fmt <- FALSE
  else {
    if (is.logical(panel.headers))
      panel.headers <- flattened_to_factor(object = data, sep = legend.sep)
    if (!is.expression(panel.headers))
      panel.headers <- as.character(panel.headers)
    strip.fmt <- insert(as.list(strip.fmt), bg = "grey90", 
      factor.levels = panel.headers)
    strip.fmt <- do.call(lattice::strip.custom, strip.fmt)
  }
  main <- main_title(x, main)
  lattice::levelplot(Value ~ Time * Well | Plate, data = data,
    main = main, col.regions = default_color_regions(colors),
    strip = strip.fmt, as.table = TRUE, layout = c(dims[1L], 1L),
    par.strip.text = as.list(striptext.fmt),
    scales = list(cex = cex, lineheight = 10))
}, sealed = SEALED)


################################################################################


