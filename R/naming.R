
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
#' @param opm.fast Logical scalar. Produce the mapping for the
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


#' Clean well coordinates
#'
#' Clean well coordinates given as character strings.
#'
#' @param x Character vector.
#' @return Character vector of the length of \code{x}.
#' @keywords internal
#'
clean_coords <- function(x) {
  do_clean <- function(x) {
    x <- sub("\\s+$", "", sub("^\\s+", "", x, perl = TRUE), perl = TRUE)
    sprintf("%s%02i", toupper(substring(x, 1L, 1L)),
      as.integer(sub("^[A-Za-z]+", "", x, perl = TRUE)))
  }
  if (any(bad <- !grepl("^[A-Z]\\d{2,2}$", x, perl = TRUE)))
    x[bad] <- do_clean(x[bad])
  x
}


################################################################################


## NOTE: not an S4 method because manual dispatch


#' Translate well coordinates
#'
#' Translate well coordinates to numeric indexes, or clean well indexes given
#' as character vector.
#'
#' @param x Vector, formula or missing. Basically any \R object.
#' @param names Character vector. Ignored unless \code{x} is a formula.
#' @return Either \code{x}, \code{TRUE} or (if a formula) the result of
#'   evaluating \code{x} in the context of \code{names}, converted to a mapping
#'   from elements to indexes.
#' @keywords internal
#'
well_index <- function(x, names) {
  if (missing(x))
    TRUE
  else if (is.character(x))
    clean_coords(x)
  else if (inherits(x, "formula"))
    eval(x[[length(x)]], structure(as.list(seq_along(names)), names = names))
  else
    x
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
#' @param downcase Logical scalar. See \code{\link{wells}}.
#' @param ... Arguments that can be passed to both \code{\link{add_in_parens}}
#'   and \code{\link{trim_string}}.
#' @return Character vector.
#' @keywords internal
#' @note The user-level function is \code{\link{wells}}.
#'
map_well_names <- function(wells, plate, in.parens = FALSE, brackets = FALSE,
    paren.sep = " ", downcase = FALSE, ...) {
  pos <- match(L(plate), colnames(WELL_MAP))
  if (is.na(pos)) {
    warning("cannot find plate type ", plate)
    return(trim_string(wells, ...))
  }
  res <- WELL_MAP[wells, pos]
  if (downcase)
    res <- substrate_info(res, "downcase")
  if (in.parens)
    add_in_parens(str.1 = wells, str.2 = res, brackets = brackets,
      paren.sep = paren.sep, ...)
  else
    trim_string(str = res, ...)
}


################################################################################


#' Create sentences
#'
#' Create a textual (listing-like) description.
#'
#' @param x Logical vector.
#' @param html Logical scalar.
#' @param ... Optional arguments passed to and from other methods.
#' @return Character vector, one element per sentence.
#' @keywords internal
#'
to_sentence <- function(x, ...) UseMethod("to_sentence")

#' @rdname to_sentence
#' @method to_sentence logical
#'
to_sentence.logical <- function(x, html, ...) {
  sentence <- function(x, what) {
    if (length(x)) {
      if (html)
        x <- substrate_info(x, "html")
      sprintf("%s for %s.", what, listing(x, style = "sentence"))
    } else
      ""
  }
  LL(html)
  isna <- is.na(x)
  n <- c("Positive", "Negative", "Ambiguous")
  result <- c(sentence(names(x)[x & !isna], n[1L]),
    sentence(names(x)[!x & !isna], n[2L]), sentence(names(x)[isna], n[3L]))
  if (html)
    result <- sprintf("<div>%s</div>", result)
  names(result) <- n
  result
}


################################################################################


#' Create textual listing
#'
#' This creates a textual listing of the discretized values. This is useful to
#' describe OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} phenotype
#' microarray results in a scientific manuscript.
#'
#' @param x \code{\link{OPMD}} or \code{\link{OPMS}} object.
#' @param as.groups Vector or \code{NULL}. If non-empty, passed as eponymous
#'   argument to \code{\link{extract}}. Thus \code{TRUE} and \code{FALSE} can be
#'   used, creating either a single group or one per plate. The extracted
#'   metadata define groups for which the discretized data are aggregated.
#'
#'   If \code{x} is an \code{\link{OPMD}} object and \code{as.groups} is not
#'   empty, it is used to create the row name of the single row of the resulting
#'   \sQuote{OPMS_Listing} object. Otherwise an \sQuote{OPMD_Listing} object is
#'   produced. It is currently possible but \strong{deprecated} to not set
#'   \code{as.groups} explicitly.
#'
#' @param cutoff Numeric scalar used if \sQuote{as.groups} is a list. If the
#'   relative frequency of the most frequent entry within the discretized values
#'   to be joined is below that cutoff, \code{NA} is used. Ignored if \code{x}
#'   is an \code{\link{OPMD}} object but added to the result if \code{as.groups}
#'   is non-empty.
#' @param downcase Logical scalar passed to \code{\link{wells}}.
#' @param full Logical scalar passed to \code{\link{wells}}.
#' @param in.parens Logical scalar passed to \code{\link{wells}}.
#' @param html Logical scalar. Convert to \acronym{HTML}? This involves Greek
#'   letters and paragraph (\sQuote{div}) tags.
#' @param sep Character scalar used for joining the \sQuote{as.groups} entries
#'   (if any).
#' @param ... Optional arguments passed between the methods or to
#'   \code{\link{wells}}.
#' @param exact Logical scalar passed to \code{\link{metadata}}.
#' @param strict Logical scalar also passed to \code{\link{metadata}}.
#' @return Character vector or matrix with additional class atribute
#'   \sQuote{OPMD_Listing} or \sQuote{OPMS_Listing}. See the examples for
#'   details.
#' @export
#' @family naming-functions
#' @keywords character category
#' @note  See \code{\link{do_disc}} for generating discretized data.
#' @examples
#'
#' # 'OPMD' method
#' data(vaas_1)
#'
#' # this yields one sentence for each kind of reaction:
#' (x <- listing(vaas_1, NULL))
#' stopifnot(inherits(x, "OPMD_Listing"), is.character(x), length(x) == 3,
#'   !is.null(names(x)))
#'
#' # create an 'OPMS_Listing' object
#' (y <- listing(vaas_1, ~ Species + Strain))
#' stopifnot(inherits(y, "OPMS_Listing"), is.matrix(y), dim(y) == c(1, 3),
#'   y == x, colnames(y) == names(x), !is.null(rownames(y)))
#'
#' # including HTML tags
#' (y <- listing(vaas_1, NULL, html = TRUE))
#' stopifnot(inherits(y, "OPMD_Listing"), is.character(x), nchar(y) > nchar(x),
#'   !is.null(names(x)))
#'
#' # 'OPMS' method
#' data(vaas_4)
#'
#' # no grouping, no names (numbering used instead for row names)
#' (x <- listing(vaas_4[1:2], as.groups = NULL))
#' stopifnot(inherits(x, "OPMS_Listing"), is.matrix(x), dim(x) == c(2, 3))
#' stopifnot(!is.null(rownames(x)), !is.null(colnames(x)))
#' (y <- listing(vaas_4[1:2], as.groups = FALSE)) # alternative
#' stopifnot(identical(x, y))
#'
#' # in effect no grouping, but names
#' (x <- listing(vaas_4[1:2], as.groups = list("Species", "Strain")))
#' stopifnot(inherits(x, "OPMS_Listing"), is.matrix(x), dim(x) == c(2, 3))
#' stopifnot(!is.null(rownames(x)), !is.null(colnames(x)))
#'
#' # only single group for all plates
#' (y <- listing(vaas_4[1:2], as.groups = TRUE))
#' stopifnot(inherits(y, "OPMS_Listing"), is.matrix(y), dim(y) == c(1, 3))
#' stopifnot(!is.null(rownames(x)), !is.null(colnames(x)))
#'
#' # two groups
#' (x <- listing(vaas_4, as.groups = list("Species")))
#' stopifnot(inherits(x, "OPMS_Listing"), is.matrix(x), dim(x) == c(2, 3))
#' stopifnot(!is.null(rownames(x)), !is.null(colnames(x)))
#'
setGeneric("listing")

setMethod("listing", OPMD, function(x, as.groups = NULL,
    cutoff = opm_opt("min.mode"), downcase = TRUE, full = TRUE,
    in.parens = FALSE, html = FALSE, sep = " ", ..., exact = TRUE,
    strict = TRUE) {
  if (missing(as.groups))
    warning("providing 'as.groups' will be mandatory in future versions")
  res <- discretized(x)
  names(res) <- wells(object = x, full = full, in.parens = in.parens,
    downcase = downcase, ...)
  res <- to_sentence(res, html)
  if (length(as.groups)) {
    res <- matrix(res, 1L, length(res), FALSE, list(NULL, names(res)))
    rownames(res) <- paste0(metadata(x, as.groups, exact, strict),
      collapse = L(sep))
    attr(res, "cutoff") <- L(cutoff)
    class(res) <- "OPMS_Listing"
  } else
    class(res) <- "OPMD_Listing"
  attr(res, "html") <- html
  res
}, sealed = SEALED)

setMethod("listing", OPMS, function(x, as.groups, cutoff = opm_opt("min.mode"),
    downcase = TRUE, full = TRUE, in.parens = FALSE, html = FALSE, sep = " ",
    ..., exact = TRUE, strict = TRUE) {
  add_stuff <- function(x, html, cutoff) {
    class(x) <- "OPMS_Listing"
    attr(x, "html") <- html
    attr(x, "cutoff") <- cutoff
    x
  }
  LL(cutoff, sep)
  if (!length(as.groups)) {
    res <- do.call(rbind, lapply(X = x@plates, FUN = listing, html = html,
      downcase = downcase, full = full, in.parens = in.parens,
      as.groups = NULL, ...))
    rownames(res) <- seq_len(nrow(res))
    return(add_stuff(res, html, cutoff))
  }
  res <- extract(object = x, subset = "disc", as.groups = as.groups,
    sep = sep, exact = exact, strict = strict, downcase = downcase,
    full = full, in.parens = in.parens, dataframe = FALSE, as.labels = NULL,
    ...)
  res <- vapply(split.default(seq_len(nrow(res)), attr(res, "row.groups")),
    function(idx) to_sentence(reduce_to_mode.matrix(res[idx, , drop = FALSE],
      cutoff, TRUE), html), character(3L))
  add_stuff(t(res), html, cutoff)
}, sealed = SEALED)


################################################################################


## NOTE: not an S4 method because conversion is done

#' Map well names to substrates
#'
#' Translate well names (which are basically their coordinates on the plate) to
#' substrate names, given the name of the plate. This function is
#' \strong{deprecated}; use \code{\link{wells}} instead.
#'
#' @param plate Character vector or factor. The type(s) of the plate(s). See
#'   \code{\link{plate_type}}. \code{\link{plate_type}} is applied before
#'   searching for the substrate names, and partial matching is allowed.
#' @param well Character vector of original well names (coordinates on the
#'   plate), or integer vector, or convertible to such, or formula. The formula
#'   allows for sequences of well coordinates; see the examples for details.
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
#' # formula yields same result (except for row names)
#' stopifnot(y == well_to_substrate("PM1", ~ c(A01, B10)))
#' # using a sequence of well coordinates
#' stopifnot(nrow(well_to_substrate("PM1", ~ C02:C06)) == 5) # well sequence
#' stopifnot(nrow(well_to_substrate("PM1")) == 96) # all wells by default
#'
well_to_substrate <- function(plate, well = TRUE) {
  warning("well_to_substrate() is deprecated -- use wells() instead")
  wells(object = well, full = TRUE, plate = plate)
}


################################################################################


#' Identify substrates
#'
#' Identify the names of substrates as used in the stored plate annotations.
#' Exact or error-tolerant matching can be used, as well as globbing and
#' regular-expression matching.
#'
#' @param object Query character vector or factor.
#' @param search Character scalar indicating the search mode. \describe{
#'   \item{exact}{Query names must exactly match (parts of) the well
#'   annotations.}
#'   \item{glob}{Shell globbing is used.}
#'   \item{approx}{Approximate matching is used; the number or proportion of
#'   errors allowed is set using \code{max.dev}, and neither globbing or
#'   regular-expression matching is done in that case.}
#'   \item{regex}{Regular-expression matching is used.}
#'   \item{pmatch}{Uses \code{pmatch} from the \pkg{base} package.}
#'   }
#'   All matching is case-insensitive except for \sQuote{exact} and
#'   \sQuote{pmatch} search modes.
#' @param max.dev Numeric scalar indicating the maximum allowed deviation. If <
#'   1, the proportion of characters that might deviate, otherwise their
#'   absolute number. It can also be a list; see the \sQuote{max.distance}
#'   argument of \code{agrep} in the \pkg{base} package for details. Has an
#'   effect only if \sQuote{approx} is chosen as search mode (see the
#'   \code{search} argument).
#' @param ... Optional arguments passed between the methods.
#' @export
#' @return An S3 object of class \sQuote{substrate_match}; basically a list of
#'   character vectors (empty if nothing was found), with duplicates removed and
#'   the rest sorted. The names of the list correspond to \code{names}.
#' @note See \code{\link{glob_to_regex}} for a description of globbing patterns.
#' @seealso base::grep base::agrep
#' @family naming-functions
#' @keywords character utilities
#' @examples
#' # Note that 'exact' search matches parts of the names, whereas globbing
#' # matches entire strings if there are no wildcards (which wouldn't make much
#' # sense)
#' (x <- find_substrate("D-Glucose", search = "exact"))
#' (y <- find_substrate("D-Glucose", search = "glob"))
#' stopifnot(length(x[[1]]) > length(y[[1]]))
#'
#' # 'pmatch' matching matches partially at the beginning and returns at most
#' # one match (the first one)
#' (y <- find_substrate("D-Glucose", search = "pmatch"))
#' stopifnot(length(x[[1]]) > length(y[[1]]))
#'
#' # Now allowing mismatches
#' (z <- find_substrate("D-Glucose", search = "approx"))
#' stopifnot(length(z[[1]]) > length(x[[1]]))
#'
#' # Factor method
#' (zz <- find_substrate(as.factor("D-Glucose"), search = "approx"))
#' stopifnot(identical(z, zz))
#'
setGeneric("find_substrate",
  function(object, ...) standardGeneric("find_substrate"))

setMethod("find_substrate", "character", function(object,
    search = c("exact", "glob", "approx", "regex", "pmatch"), max.dev = 0.2) {
  su <- function(x) lapply(lapply(x, unique.default), sort.int)
  find_name <- function(patterns, ...) {
    su(lapply(X = patterns, FUN = grep, x = WELL_MAP, value = TRUE,
      useBytes = TRUE, ...))
  }
  find_approx <- function(pattern, ...) {
    su(lapply(X = pattern, FUN = agrep, x = WELL_MAP, ignore.case = TRUE,
      value = TRUE, useBytes = TRUE, ...))
  }
  find_partial <- function(pattern) {
    # next step necessary because multiple <partial> matches are never allowed
    table <- unique.default(WELL_MAP)
    found <- table[pmatch(pattern, table, NA_integer_, TRUE)]
    names(found) <- pattern
    lapply(lapply(as.list(found), na.exclude), sort.int)
  }
  result <- case(match.arg(search),
    exact = find_name(object, fixed = TRUE),
    glob = find_name(structure(glob_to_regex(object), names = object),
      ignore.case = TRUE, perl = TRUE),
    regex = find_name(object, ignore.case = TRUE, perl = TRUE),
    approx = find_approx(object, max.distance = max.dev),
    pmatch = find_partial(object)
  )
  names(result) <- object
  class(result) <- c("substrate_match", "print_easy")
  result
}, sealed = SEALED)


setOldClass("substrate_match")


################################################################################


#' Identify positions of substrates
#'
#' Identify the positions of substrates, i.e. the plate(s) and well(s) in which
#' they occur.
#'
#' @param object Query character vector, factor or list, S3 object of class
#'   \sQuote{substrate_match}, \code{\link{OPM}} or \code{\link{OPMS}} object.
#' @param ... Optional arguments passed between the methods.
#' @export
#' @return The character method returns a list of character matrices (empty if
#'   nothing was found), with one row per position found, the plate name in the
#'   first column and the well name in the second. The names of this list
#'   correspond to \code{names}. The \code{\link{OPM}} and \code{\link{OPMS}}
#'   methods do the same, using their own substrates. The list and
#'   \sQuote{substrate_match} methods return lists of such lists.
#' @details  The query names must be written exactly as used in the stored plate
#'   annotations. To determine their spelling, use \code{\link{find_substrate}}.
#'   This spelling is not guaranteed to be stable between distinct \pkg{opm}
#'   releases.
#' @family naming-functions
#' @keywords utilities
#' @examples
#'
#' # Character method; compare correct and misspelled substrate name
#' (x <- find_positions(c("D-Glucose", "D-Gloucose")))
#' stopifnot(length(x[[1]]) > length(x[[2]]))
#'
#' # Factor method
#' (y <- find_positions(as.factor(c("D-Glucose", "D-Gloucose"))))
#' stopifnot(identical(y, x))
#'
#' # List method
#' x <- find_positions(find_substrate(c("D-Glucose", "D-Gloucose")))
#' x[[1]][1:3]
#' x[[2]]
#' stopifnot(length(x[[1]]) > length(x[[2]]))
#'
setGeneric("find_positions",
  function(object, ...) standardGeneric("find_positions"))

setMethod("find_positions", "character", function(object, ...) {
  plates <- colnames(WELL_MAP)
  sapply(object, FUN = function(name) {
    result <- which(WELL_MAP == name, arr.ind = TRUE)
    matrix(c(plates[result[, 2L]], rownames(result)), ncol = 2L,
      dimnames = list(NULL, RESERVED_NAMES[c("plate", "well")]))
  }, simplify = FALSE)
}, sealed = SEALED)

setMethod("find_positions", "substrate_match", function(object, ...) {
  rapply(object, f = find_positions, "character", how = "list", ...)
}, sealed = SEALED)

setMethod("find_positions", "list", function(object, ...) {
  rapply(object, f = find_positions, classes = c("character", "factor"),
    how = "list", ...)
}, sealed = SEALED)

setMethod("find_positions", OPM, function(object, ...) {
  find_positions(wells(object, full = TRUE, in.parens = FALSE), ...)
}, sealed = SEALED)


################################################################################


#' Provide information on substrates
#'
#' Return information on substrates such as their \acronym{CAS} number or
#' other database ID or convert substrate names.
#'
#' @param object Query character vector, factor or list, S3 object of class
#'   \sQuote{substrate_match}, \code{\link{OPM}} or \code{\link{OPMS}} object.
#' @param what Character scalar indicating which kind of information to output.
#'   \describe{
#'     \item{all}{Create object of S3 class \sQuote{substrate_data} containing
#'     all available information and useful for display.}
#'     \item{cas}{\acronym{CAS} registry number, optionally expanded to an
#'     \acronym{URL}.}
#'     \item{chebi}{\acronym{ChEBI} database ID, optionally expanded to an
#'     \acronym{URL}.}
#'     \item{downcase}{Substrate name converted to lower case, protecting
#'     one-letter specifiers, acronyms and chemical symbols, and translating
#'     relevant characters from the Greek alphabet.}
#'     \item{drug}{\acronym{KEGG} drug database ID, optionally expanded to an
#'     \acronym{URL}.}
#'     \item{greek}{Substrate name after translation of relevant characters to
#'     Greek letters.}
#'     \item{html}{Like \sQuote{greek}, but use \acronym{HTML} tags.}
#'     \item{kegg}{\acronym{KEGG} compound database ID, optionally expanded to
#'     an \acronym{URL}.}
#'     \item{mesh}{\acronym{MeSH} database name (useful for conducting
#'     \acronym{PubMed} searches), optionally expanded to an \acronym{URL}.}
#'     \item{metacyc}{\acronym{MetaCyc} database ID, optionally expanded to an
#'     \acronym{URL}.}
#'   }
#'   See the references for information on the databases.
#' @param browse Numeric scalar. If non-zero, an \acronym{URL} is generated from
#'   each \acronym{ID}. If positive, this number of \acronym{URL}s (counted from
#'   the beginning) is also opened in the default web browser; if negative, the
#'   \acronym{URL}s are only returned. It is an error to try this with those
#'   values of \code{what} that do not yield an \acronym{ID}.
#' @param ... Optional other arguments passed between the methods.
#' @export
#' @return The character method returns a character vector with \code{object}
#'   used as names and either a matched entry or \code{NA} as value. The factor
#'   method does the same, whereas the list method traverses a list and calls
#'   \code{substrate_info} on its elements. The \code{\link{OPM}} and
#'   \code{\link{OPMS}} methods work like the character method, using their own
#'   substrates. Depending on the \code{browse} argument, the returned
#'   \acronym{ID}s might have been converted to \acronym{URL}s, and as a side
#'   effect tabs in the default web browser might have been opened.
#' @details The generated \acronym{URL}s should provide plenty of information
#'   on the respective substrate. In the case of \acronym{ChEBI}, \acronym{KEGG}
#'   and \acronym{Metacyc}, much information is directly displayed on the page
#'   itself, whereas the chosen \acronym{CAS} site contains a number of links
#'   providing additional chemical details. The \acronym{MeSH} weg pages
#'   directly link to according \acronym{PubMed} searches.
#' @family naming-functions
#' @keywords utilities
#' @seealso utils::browseURL
#' @references Bochner, B. R., pers. comm.
#' @references \url{http://www.cas.org/content/chemical-substances/faqs}
#' @references \url{http://www.genome.jp/kegg/}
#' @references Kanehisa, M., Goto, S., Furumichi, M., Tanabe, M., and Hirakawa,
#'   M. 2010 KEGG for representation and analysis of molecular networks
#'   involving diseases and drugs. \emph{Nucleic Acids Research} \strong{38}:
#'   D355--D360.
#' @references \url{http://metacyc.org/}
#' @references Caspi, R., Altman, T., Dreher, K., Fulcher, C.A., Subhraveti,
#'   P., Keseler, I.M., Kothari, A., Krummenacker, M., Latendresse, M.,
#'   Mueller, L.A., Ong, Q., Paley, S., Pujar, A., Shearer, A.G., Travers, M.,
#'   Weerasinghe, D., Zhang, P., Karp, P.D. 2012 The MetaCyc database of
#'   metabolic pathways and enzymes and the BioCyc collection of pathway/genome
#'   databases. \emph{Nucleic Acids Research} \strong{40}: D742--D753.
#' @references \url{http://www.ncbi.nlm.nih.gov/mesh}
#' @references Coletti, M.H., Bleich, H.L 2001 Medical subject headings used to
#'   search the biomedical literature. \emph{Journal of the American Medical
#'   Informatics Association} \strong{8}: 317--323.
#' @references \url{http://www.ebi.ac.uk/chebi/}
#' @references Hastings, J., de Matos, P., Dekker, A., Ennis, M., Harsha, B.,
#'   Kale, N., Muthukrishnan, V., Owen, G., Turner, S., Williams, M.,
#'   Steinbeck, C. 2013 The ChEBI reference database and ontology for
#'   biologically relevant chemistry: enhancements for 2013. \emph{Nucleic
#'   Acids Research} \strong{41}: D456--D463.
#' @details The query names must be written exactly as used in the stored plate
#'   annotations. To determine their spelling, use \code{\link{find_substrate}}.
#'
#'   Currently the information is incomplete, particularly for the PM-M plates.
#'   While it should eventually be possible to link all substrates to
#'   \acronym{CAS} numbers, they are not necessarily contained in the other
#'   databases.
#' @examples
#'
#' # Character method; compare correct and misspelled substrate name
#' (x <- substrate_info(c("D-Glucose", "D-Gloucose")))
#'
#' # Factor method
#' (y <- substrate_info(as.factor(c("D-Glucose", "D-Gloucose"))))
#' stopifnot(identical(x, y))
#'
#' # Now with generation of URLs
#' (y <- substrate_info(c("D-Glucose", "D-Gloucose"), browse = -1))
#' stopifnot(is.na(y) | nchar(y) > nchar(x))
#' # NA remains NA (and would not be opened in the web browser)
#'
#' # Character method, safe conversion to lower case
#' (x <- substrate_info(c("a-D-Glucose", "a-D-Gloucose"), "downcase"))
#' stopifnot(nchar(x) > nchar(c("a-D-Glucose", "a-D-Gloucose")))
#' # note the protection of 'D' and the conversion of 'a'
#' # whether ot not substrate names are known does not matter here
#'
#' # List method
#' (x <- substrate_info(find_substrate(c("D-Glucose", "D-Gloucose"))))
#' stopifnot(length(x[[1]]) > length(x[[2]]))
#'
#' # OPM and OPMS methods
#' data(vaas_1)
#' (x <- substrate_info(vaas_1[, 1:3], "all"))
#' stopifnot(inherits(x, "substrate_data"))
#' data(vaas_4)
#' (y <- substrate_info(vaas_4[, , 1:3], "all"))
#' stopifnot(identical(x, y))
#' \dontrun{
#'
#'   # this would open 96 tabs in your browser...
#'   substrate_info(vaas_4, "kegg", browse = 100)
#' }
#'
setGeneric("substrate_info",
  function(object, ...) standardGeneric("substrate_info"))

setMethod("substrate_info", "character", function(object,
    what = c("cas", "kegg", "drug", "metacyc", "chebi", "mesh", "downcase",
      "greek", "html", "all"), browse = 0L, ...) {
  create_url <- function(x, how) {
    base <- URL_BASE[match.arg(how, names(URL_BASE))]
    x <- sub("^CAS\\s+", "", x, perl = TRUE)
    ifelse(is.na(x), NA_character_, paste0(base, vapply(x, URLencode, "")))
  }
  map_words <- function(x, fun, ...) {
    y <- strsplit(x, "\\w+", perl = TRUE)
    x <- strsplit(x, "\\W+", perl = TRUE)
    bad <- !vapply(x, function(value) nzchar(value[1L]), NA)
    x[bad] <- lapply(x[bad], `[`, i = -1L)
    bad <- vapply(x, length, 0L) < vapply(y, length, 0L)
    x[bad] <- lapply(x[bad], function(value) c(value, ""))
    x <- lapply(X = x, FUN = fun, ...) # fun() must keep the length!
    mapply(paste0, y, x, MoreArgs = list(collapse = ""))
  }
  convert_greek <- function(x, how) {
    map_words(x, fun = map_values, mapping = GREEK_LETTERS[, how])
  }
  safe_downcase <- function(x) {
    good_case <- function(x) {
      bad <- nchar(x) > 1L # avoid changing acronyms and chemical elements
      bad[bad] <- !grepl("^(pH|[a-z]?[A-Z][A-Z]+|([A-Z][a-z]?\\d*)+)$", x[bad],
        perl = TRUE)
      x[bad] <- tolower(x[bad])
      x
    }
    map_words(x, function(y) map_values(good_case(y), GREEK_LETTERS[, "plain"]))
  }
  all_information <- function(x) {
    result <- SUBSTRATE_INFO[match(object, rownames(SUBSTRATE_INFO)), ,
      drop = FALSE]
    colnames(result) <- map_values(colnames(result), c(METACYC = "MetaCyc",
      MESH = "MeSH", CHEBI = "ChEBI", KEGG = "KEGG compound",
      DRUG = "KEGG drug"))
    result <- split.data.frame(result, seq_len(nrow(result)))
    result <- lapply(result, function(y) y[, !is.na(y), drop = TRUE])
    class(result) <- c("substrate_data", "print_easy")
    result
  }
  result <- case(what <- match.arg(what),
    all = all_information(object),
    downcase = safe_downcase(object),
    greek = convert_greek(object, "plain"),
    html = convert_greek(object, "html"),
    chebi =, drug =, kegg =, metacyc =, mesh =,
    cas = SUBSTRATE_INFO[match(object, rownames(SUBSTRATE_INFO)), toupper(what)]
  )
  browse <- must(as.integer(L(browse)))
  if (browse != 0L) {
    result <- create_url(result, what)
    if (browse > 0L)
      lapply(head(result[!is.na(result)], browse), browseURL)
  }
  names(result) <- object
  result
}, sealed = SEALED)

setMethod("substrate_info", "substrate_match", function(object, ...) {
  rapply(object = object, f = substrate_info, classes = "character",
    how = "list", ...)
}, sealed = SEALED)

setMethod("substrate_info", "list", function(object, ...) {
  rapply(object = object, f = substrate_info,
    classes = c("character", "factor"), how = "list", ...)
}, sealed = SEALED)

setMethod("substrate_info", OPM, function(object, ...) {
  substrate_info(wells(object, full = TRUE, in.parens = FALSE), ...)
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
#
# Automatically generated OPMS methods
#
lapply(c(
    #+
    find_positions,
    substrate_info
    #-
  ), FUN = function(func_) {
  setMethod(func_, OPMS, function(object, ...) {
    func_(object@plates[[1L]], ...)
  }, sealed = SEALED)
})



