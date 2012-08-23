
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


## NOTE: not an S4 method because conversion is done

#' Map well names to substrates
#'
#' Translate well names (which are basically their coordinates on the plate) to
#' substrate names, given the name of the plate.
#'
#' @param wells Character vector of original well names (coordinates on the
#'   plate).
#' @param plate Character scalar. The type of the plate. See
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
  pos <- match(L(plate), colnames(WELL_MAP))
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
#' @param plate Character vector or factor. The type(s) of the plate(s). See
#'   \code{\link{plate_type}}. \code{\link{plate_type}} is applied
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
  pos <- pmatch(plate_type(plate), colnames(WELL_MAP))
  found <- !is.na(pos)
  result <- WELL_MAP[well, pos[found], drop = FALSE]
  others <- matrix(data = NA_character_, nrow = length(well),
    ncol = length(plate[!found]), dimnames = list(well, plate[!found]))
  cbind(result, others)[]
}


################################################################################


#' Identify substrates
#'
#' Identify the names of substrates as used in the stored plate annotations.
#' Exact or error-tolerant matching can be used, as well as globbing and
#' regular-expression matching.
#'
#' @param object Query character vector or factor.
#' @param search Character scalar indicating the search mode. If \sQuote{exact},
#'   query names must exactly match (parts of) the well annotations. If
#'   \sQuote{glob}, shell globbing is used. If \sQuote{approx}, approximate
#'   matching is used; the number or proportion of errors allowed is set using
#'   \code{max.dev}, and neither globbing or regular-expression matching is done
#'   in that case. If \sQuote{regex}, regular-expression matching is used. If
#'   \sQuote{pmatch}, uses \code{pmatch} from the \pkg{base} package. All
#'   matching is case-insensitive except for \sQuote{exact} and \sQuote{pmatch}
#'   search modes.
#' @param max.dev Numeric scalar indicating the maximum allowed deviation. If
#'   < 1, the proportion of characters that might deviate, otherwise their
#'   absolute number. It can also be a list; see the \sQuote{max.distance}
#'   argument of \code{agrep} in the \pkg{base} package for details. Has an
#'   effect only if \sQuote{approx} is chosen as search mode (see the
#'   \code{search} argument).
#' @param ... Optional arguments passed between the methods.
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
#' # Note that 'exact' search matches parts of the names, whereas globbing 
#' # matches entire strings if there are no wildcards
#' (x <- find_substrate("a-D-Glucose", search = "exact"))
#' (y <- find_substrate("a-D-Glucose", search = "glob"))
#' stopifnot(length(x[[1]]) > length(y[[1]]))
#'
#' # 'pmatch' matching matches partially at the beginning and returns at most
#' # one match (the first one)
#' (y <- find_substrate("a-D-Glucose", search = "pmatch"))
#' stopifnot(length(x[[1]]) > length(y[[1]]))
#'
#' # Now allowing mismatches
#' (z <- find_substrate("a-D-Glucose", search = "approx"))
#' stopifnot(length(z[[1]]) > length(x[[1]]))
#'
#' # Factor method
#' (zz <- find_substrate(as.factor("a-D-Glucose"), search = "approx"))
#' stopifnot(identical(z, zz))
#'
setGeneric("find_substrate", 
  function(object, ...) standardGeneric("find_substrate"))

setMethod("find_substrate", "character", function(object,
    search = c("exact", "glob", "approx", "regex", "pmatch"), max.dev = 0.2) {
  find_name <- function(patterns, ...) {
    sapply(X = patterns, FUN = grep, x = WELL_MAP, value = TRUE, 
      useBytes = TRUE, ..., simplify = FALSE)
  }
  find_approx <- function(pattern, ...) {
    sapply(X = pattern, FUN = agrep, x = WELL_MAP, ignore.case = TRUE, 
      value = TRUE, useBytes = TRUE, ..., simplify = FALSE)
  }
  find_partial <- function(pattern) {
    found <- pmatch(x = pattern, table = WELL_MAP, duplicates.ok = TRUE)
    names(found) <- pattern 
    lapply(as.list(WELL_MAP[found]), na.exclude)
  }
  result <- case(match.arg(search),
    exact = find_name(object, fixed = TRUE),
    glob = find_name(structure(glob_to_regex(object), names = object), 
      ignore.case = TRUE, perl = TRUE),
    regex = find_name(object, ignore.case = TRUE, perl = TRUE),
    approx = find_approx(object, max.distance = max.dev),
    pmatch = find_partial(object)
  )
  lapply(result, function(x) sort.int(unique(x)))
}, sealed = SEALED)


################################################################################


#' Identify positions of substrates
#'
#' Identify the positions of substrates, i.e. the plate(s) and well(s) in which
#' they occur. The query names must be written exactly as used in the stored
#' plate annotations. To determine their spelling, use
#' \code{\link{find_substrate}}.
#'
#' @param object Query character vector or query list.
#' @param ... Optional arguments passed between the methods.
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
#' # Factor method
#' (y <- find_positions(as.factor(c("a-D-Glucose", "a-D-Gloucose"))))
#' stopifnot(identical(y, x))
#'
#' # List method
#' (x <- find_positions(find_substrate(c("a-D-Glucose", "a-D-Gloucose"))))
#' stopifnot(length(x[[1]]) > length(x[[2]]))
#'
setGeneric("find_positions", 
  function(object, ...) standardGeneric("find_positions"))

setMethod("find_positions", "character", function(object) {
  plates <- colnames(WELL_MAP)
  sapply(object, FUN = function(name) {
    result <- which(WELL_MAP == name, arr.ind = TRUE)
    matrix(c(plates[result[, 2L]], rownames(result)), ncol = 2L,
      dimnames = list(NULL, c("Plate", "Well")))
  }, simplify = FALSE)
}, sealed = SEALED)

setMethod("find_positions", "list", function(object) {
  rapply(object, f = find_positions, classes = c("character", "factor"),
    how = "list")
}, sealed = SEALED)


################################################################################


#' Provide information on substrates
#'
#' Return information on substrates such as their CAS number or KEGG ID. The 
#' query names must be written exactly as used in the stored plate annotations.
#' To determine their spelling, use \code{\link{find_substrate}}.
#'
#' @param object Query character vector or query list.
#' @param what Character scalar indicating which kind of information to output.
#'   See the references for the background of each possible value.
#' @param ... Optional arguments passed between the methods.
#' @export
#' @return The character method returns a character vector with \code{object}
#'   used as names and either a matched entry or \code{NA} as value. The factor
#'   method does the same, whereas the list method traverses a list and calls
#'   \code{substrate_info} on its elements.
#' @family naming-functions
#' @keywords utilities
#' @references \url{http://en.wikipedia.org/wiki/CAS_registry_number}
#' @references \url{http://www.genome.jp/kegg/}
#' @references \url{http://metacyc.org/}
#' @note Currently the information is incomplete, particularly for the PM-M 
#'   plates.
#' @examples
#'
#' # Character method; compare correct and misspelled substrate name
#' (x <- substrate_info(c("a-D-Glucose", "a-D-Gloucose")))
#'
#' # Factor method
#' (y <- substrate_info(as.factor(c("a-D-Glucose", "a-D-Gloucose"))))
#' stopifnot(identical(x, y))
#'
#' # List method
#' (x <- substrate_info(find_substrate(c("a-D-Glucose", "a-D-Gloucose"))))
#' stopifnot(length(x[[1]]) > length(x[[2]]))
#'
setGeneric("substrate_info", 
  function(object, ...) standardGeneric("substrate_info"))

setMethod("substrate_info", "character", function(object, 
    what = c("cas", "kegg", "metacyc")) {
  result <- SUBSTRATE_INFO[match(object, rownames(SUBSTRATE_INFO)), 
    toupper(match.arg(what))]
  names(result) <- object
  result
}, sealed = SEALED)

setMethod("substrate_info", "list", function(object, ...) {
  sapply(X = object, FUN = substrate_info, simplify = FALSE, ...)
}, sealed = SEALED)


################################################################################
#
# Automatically generated factor methods
#


lapply(c(
    #+
    find_substrate,
    find_positions,
    substrate_info
    #-
  ), FUN = function(func_) {
  setMethod(func_, "factor", function(object, ...) {
    func_(as.character(object), ...)
  }, sealed = SEALED)
})


################################################################################



