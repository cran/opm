



################################################################################
################################################################################
#
# Conditional conversions to OPMS
#


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
to_opm_list <- function(object, ...) UseMethod("to_opm_list")

#' @method to_opm_list list
#' @rdname to_opm_list
#'
to_opm_list.list <- function(object, precomputed = TRUE, skip = FALSE,
    group = FALSE) {
  LL(precomputed, skip, group)
  opmd.slots <- setdiff(slotNames(OPMD), opma.slots <- slotNames(OPMA))
  opma.slots <- setdiff(opma.slots, opm.slots <- slotNames(OPM))
  convert_recursively <- function(item) {
    if (!is.list(item))
      if (skip)
        return(NULL)
    else
      stop("non-list element encountered")
    keys <- names(item)
    if (all(opm.slots %in% keys))
      as(item, if (all(opma.slots %in% keys))
        if (all(opmd.slots %in% keys))
          OPMD
        else
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
      stop("need object derived from ", OPM, " or ", OPMS, ", got ",
        class(item)[1L])
  }
  result <- if (precomputed)
    rapply(object, f = get_plates, how = "unlist")
  else
    c(convert_recursively(object), recursive = TRUE)
  if (group)
    result <- split(result, vapply(result, plate_type, character(1L)))
  result
}


################################################################################


#' Assign subset
#'
#' Assign subsets of \code{\link{OPMS}} objects.
#'
#' @param x \code{\link{OPMS}} object.
#' @param i One to several plate indexes. Should be compatible with the length
#'   of \code{value}.
#' @param j Must \strong{not} be set. See the examples.
#' @param value Value to be assigned. \code{NULL} causes the selected plates
#'   to be removed. Alternatively, \code{\link{OPM}} or \code{\link{OPMS}}
#'   objects can be assigned, subject to the restrictions explained in the help
#'   entry of the \code{\link{OPMS}} class.
#' @return \code{value}.
#' @family combination-functions
#' @keywords manip
#' @rdname bracket.set
#' @exportMethod "[<-"
#' @export
#' @examples
#' data(vaas_4)
#' copy <- vaas_4
#' copy[5] <- NULL # has no effect
#' stopifnot(identical(vaas_4, copy))
#' length(copy)
#' copy[2:3] <- NULL # removes these plates
#' length(copy)
#' stopifnot(length(vaas_4) == length(copy) + 2)
#' copy[1:4] <- vaas_4 # set all plates to the plates from 'vaas_4'
#' stopifnot(identical(vaas_4, copy))
#' copy[3] <- copy[3] # no change
#' stopifnot(identical(vaas_4, copy))
#' copy[3] <- copy[2] # now assign other plate
#' stopifnot(!identical(vaas_4, copy))
#'
setMethod("[<-", c(OPMS, "ANY", "missing", "NULL"), function(x, i, j, value) {
  x@plates[i] <- NULL
  case(length(x@plates), NULL, x@plates[[1L]], x) # no check necessary here
}, sealed = SEALED)

setMethod("[<-", c(OPMS, "ANY", "missing", OPM), function(x, i, j, value) {
  x@plates[i] <- value
  new(OPMS, plates = x@plates) # check needed
}, sealed = SEALED)

setMethod("[<-", c(OPMS, "ANY", "missing", OPMS), function(x, i, j, value) {
  x@plates[i] <- value@plates
  new(OPMS, plates = x@plates) # check needed
}, sealed = SEALED)


################################################################################


#' Convert list to OPMS
#'
#' Conditionally convert a list to an \code{\link{OPMS}} object. This method is
#' used by \code{\link{c}}.
#'
#' @param object List.
#' @param precomputed Logical scalar. See \code{\link{opms}}.
#' @param skip Logical scalar. See \code{\link{opms}}.
#' @return \code{\link{OPMS}} object (if conversions was successful) or just the
#'   input \code{object} (if conversions was unsuccessful).
#' @keywords internal
#'
try_opms <- function(object, ...) UseMethod("try_opms")

#' @method try_opms list
#' @rdname try_opms
#'
try_opms.list <- function(object, precomputed = TRUE, skip = FALSE) {
  tryCatch(
    new(OPMS, plates = to_opm_list.list(object, precomputed, skip, FALSE)),
    error = function(e) object)
}


################################################################################
################################################################################
#
# Combination functions
#


#' Combination
#'
#' Combine a \code{\link{OPM}} or \code{\link{OPMS}} object with other objects.
#' If possible, create an \code{\link{OPMS}} object, otherwise return a list.
#'
#' @param x \code{\link{OPMX}} object.
#' @param ... Other \R objects.
#' @param recursive Logical scalar. See \code{c} from the \pkg{base} package.
#' @export
#' @return \code{\link{OPMS}} object, list, or \code{\link{OPM}} object (if
#'   \code{\dots} is not given and \code{x} is such an object).
#' @family combination-functions
#' @seealso base::c
#' @keywords manip
#' @examples
#'
#' data(vaas_1, vaas_4)
#'
#' # Adding nothing
#' dim(x <- c(vaas_1))
#' stopifnot(identical(x, vaas_1))
#' dim(x <- c(vaas_4))
#' stopifnot(identical(x, vaas_4))
#'
#' # Not particularly useful: adding identical plates!
#' dim(x <- c(vaas_1, vaas_1)) # yields a two-plate OPMS object
#' stopifnot(identical(dim(x), c(2L, dim(vaas_1))))
#'
#' # Also not particularly useful: adding partially identical plates!
#' dim(x <- c(vaas_4, vaas_1))
#' stopifnot(identical(dim(x), c(5L, dim(vaas_1))))
#'
setMethod("c", OPMX, function(x, ..., recursive = FALSE) {
  if (missing(..1))
    return(x)
  try_opms.list(list(x, ...))
}, sealed = SEALED)


################################################################################


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
#' data(vaas_1, vaas_4)
#' # The examples do not show particularly useful additions, as the plates
#' # are either entirely or partially identical. Note the changes in the
#' # dimensions.
#'
#' # OPM+OPM method
#' dim(x <- vaas_1 + vaas_1)
#' stopifnot(identical(dim(x), c(2L, dim(vaas_1))))
#'
#' # OPM+OPMS method
#' dim(x <- vaas_1 + vaas_4)
#' stopifnot(identical(dim(x), c(5L, dim(vaas_1))))
#'
#' # OPM+list method
#' dim(x <- vaas_1 + list(vaas_1, vaas_1))
#' stopifnot(identical(dim(x), c(3L, dim(vaas_1))))
#'
#' # OPMS+OPMS method
#' dim(x <- vaas_4 + vaas_4)
#' stopifnot(identical(dim(x), c(8L, dim(vaas_4)[-1L])))
#'
#' # OPMS+OPM method
#' dim(x <- vaas_4 + vaas_1)
#' stopifnot(identical(dim(x), c(5L, dim(vaas_1))))
#'
#' # OPMS+list method
#' dim(x <- vaas_4 + list(vaas_1))
#' stopifnot(identical(dim(x), c(5L, dim(vaas_1))))
#'
setMethod("+", c(OPM, OPM), function(e1, e2) {
  new(OPMS, plates = list(e1, e2))
}, sealed = SEALED)

setMethod("+", c(OPM, OPMS), function(e1, e2) {
  new(OPMS, plates = c(list(e1), plates(e2)))
}, sealed = SEALED)

setMethod("+", c(OPM, "list"), function(e1, e2) {
  new(OPMS, plates = c(list(e1), e2))
}, sealed = SEALED)

setMethod("+", c(OPMS, OPMS), function(e1, e2) {
  new(OPMS, plates = c(plates(e1), plates(e2)))
}, sealed = SEALED)

setMethod("+", c(OPMS, OPM), function(e1, e2) {
  new(OPMS, plates = c(plates(e1), e2))
}, sealed = SEALED)

setMethod("+", c(OPMS, "list"), function(e1, e2) {
  new(OPMS, plates = c(plates(e1), e2))
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
#'   \code{FALSE}, silently skip objects that do not belong to the three target
#'   classes? Otherwise, an error is generated if such a list element is
#'   encountered.
#' @param group Logical or character scalar. If \code{TRUE}, split the list of
#'   collected \code{\link{OPM}} objects according to the plate type and convert
#'   the sublists seperately if they contain more than one plate; otherwise just
#'   keep the \code{\link{OPM}} object. \code{FALSE} is the default: all plates
#'   are tried to be forced into a single \code{\link{OPMS}} object. If a
#'   character scalar, the name of the plate type to be extracted.
#' @export
#' @return \code{\link{OPMS}} object, or list of such objects (and/or
#'   \code{\link{OPM}} objects), or \code{\link{OPM}} object, or \code{NULL}.
#' @family combination-functions
#' @keywords manip
#' @details While otherwise rather flexible, this function will fail to return
#'   an \code{\link{OPMS}} object if the plate types do not match (simply
#'   because such \code{\link{OPMS}} objects are disallowed) and \code{group} is
#'   set to \code{FALSE}. But if \code{group} is set to \code{TRUE}, a list, not
#'   a single \code{\link{OPMS}} object will be returned; and if \code{group} is
#'   of mode \sQuote{character}, this extracts the plate type(s) of interest.
#' @note Consider also the plate-type selection options of
#'   \code{\link{read_opm}}.
#' @examples
#'
#' ## Testing distinct OPM/OPMS combinations -- all should work
#' data(vaas_1, vaas_4)
#' # Note the number of contained plates in the generated objects.
#'
#' (x <- opms())
#' stopifnot(is.null(x))
#'
#' dim(x <- opms(vaas_1))
#' stopifnot(identical(x, vaas_1))
#'
#' dim(x <- opms(vaas_4, group = plate_type(vaas_4)))
#' stopifnot(identical(x, vaas_4))
#'
#' dim(x <- opms(vaas_4, group = "PM01"))
#' stopifnot(is.null(x)) # no such plate type => empty object!
#'
#' dim(x <- opms(vaas_1, vaas_1))
#' stopifnot(is(x, "OPMS"), length(x) == 2L)
#'
#' dim(x <- opms(vaas_4, vaas_1))
#' stopifnot(is(x, "OPMS"), length(x) == 5L)
#'
#' dim(x <- opms(vaas_1, vaas_4))
#' stopifnot(is(x, "OPMS"), length(x) == 5L)
#'
#' dim(x <- opms(vaas_4, vaas_4))
#' stopifnot(is(x, "OPMS"), length(x) == 8L)
#'
opms <- function(..., precomputed = TRUE, skip = FALSE, group = FALSE) {
  opms_or_first_or_NULL <- function(x) {
    case(length(x), NULL, x[[1L]], new(OPMS, plates = x))
  }
  if (is.character(group)) {
    wanted <- plate_type(group) # for normalization
    group <- TRUE
  } else {
    wanted <- NULL
    group <- as.logical(group)
  }
  # to_opm_list() checks the argument lengths
  result <- to_opm_list.list(list(...), precomputed, skip, group)
  if (is.null(wanted)) {
    if (group)
      lapply(result, opms_or_first_or_NULL)
    else
      opms_or_first_or_NULL(result)
  } else # group was TRUE in that case, and to_opm_list() split the list
    opms_or_first_or_NULL(result[[wanted]])
}


################################################################################

