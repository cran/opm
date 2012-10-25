

################################################################################
#
# Constants used only in this file
#

CHARACTER_STATES <- c(0L:9L, LETTERS)[1L:32L]
MISSING_CHAR <- "?"
if (length(MISSING_CHAR) != 1L || MISSING_CHAR %in% CHARACTER_STATES)
  stop(BUG_MSG)
if (any(nchar(c(MISSING_CHAR, CHARACTER_STATES)) != 1L))
  stop(BUG_MSG)


DEFAULT_PHYLO_COMMENT <- "Phenotype microarray data exported by opm"


PHYLO_FORMATS <- c("epf", "nexus", "phylip", "hennig", "html")


PAUP_BLOCK <- c(
  "set torder=left tcompress taxlabels=full outroot=monophyl autoclose;",
  "set maxtrees=1000 increase=auto autoinc=1000 storetreewts storebrlens;",
  "set rootmethod=midpoint warnroot=no;",
  "defaults nj bionj breakties=random;",
  "defaults upgma bionj breakties=random;",
  "defaults hsearch start=stepwise addseq=random randomize=addseq swap=tbr;",
  "default hsearch multrees steepest=no;",
  "defaults bootstrap grpfreq=no;",
  "defaults contree grpfreq=no;",
  "defaults savedist triangle=both;",
  "defaults describetrees labelnode=no;",
  "dset negbrlen=setzero dcollapse missdist=ignore;",
  "pset opt=minf collapse=minbrlen mstaxa=uncertain;"
)


################################################################################


setGeneric("check_discrete",
  function(object, ...) standardGeneric("check_discrete"))
#' Check phylogenetic characters
#'
#' Check discrete characters. Raise an error if the check fails.
#'
#' @param object Character or numeric vector.
#' @param joined Logical scalar. Expect the data to be joined beforehand?
#' @param from.numeric Logical scalar. Ignored unless \code{joined} is
#'   \code{TRUE}.
#' @return \code{NULL}.
#' @keywords internal
#'
setMethod("check_discrete", "character", function(object, joined,
    from.numeric = FALSE) {
  if (length(object) == 0L || any(is.na(object)))
    stop("need > 0 non-NA strings")
  if (joined) {
    if (from.numeric)
      pat <- sprintf("^([%s]|\\s*\\d+\\.\\d+(-\\s*\\d+\\.\\d+)?)$",
        MISSING_CHAR)
    else {
      pat <- paste(c(CHARACTER_STATES, MISSING_CHAR), collapse = "")
      pat <- sprintf("^([%s]|\\([%s]+\\)|\\[[%s]+\\])$", pat, pat, pat)
    }
    bad.chars <- grep(pat, object, ignore.case = TRUE, perl = TRUE,
      invert = TRUE, value = TRUE)
    if (length(bad.chars) > 0L)
      stop("uninterpretable character: ", bad.chars[1L])
  } else {
    if (any(nchar(object) != 1L))
      stop("need one-character strings")
  }
  NULL
}, sealed = SEALED)

setMethod("check_discrete", "numeric", function(object, joined,
    from.numeric = FALSE) {
  if (length(object) == 0L)
    stop("need non-empty numeric vector")
  NULL
}, sealed = SEALED)

setMethod("check_discrete", "matrix", function(object, joined,
    from.numeric = FALSE) {
  check_discrete(as.vector(object), joined, from.numeric)
}, sealed = SEALED)

setMethod("check_discrete", "ANY", function(object, joined,
    from.numeric = FALSE) {
  stop("need numeric or character vector or matrix")
}, sealed = SEALED)


################################################################################


setGeneric("join_discrete",
  function(object, ...) standardGeneric("join_discrete"))
#' Join phylogenetic characters
#'
#' Join discrete characters represented as strings of length 1, considering
#' ambiguities, if any. The outcome is suitable for phylogenetic studies with
#' software such as PAUP*.
#'
#' @param object Character or numeric vector or matrix.
#' @param format Character scalar.
#' @param groups Character vector or factor, determining which rows should
#'   be joined.
#' @param digits Number of digits used for rounding. Has an effect only for
#'   numeric data.
#' @return Character scalar, for the matrix method a vector.
#' @keywords internal
#'
setMethod("join_discrete", "character", function(object, format) {
  check_discrete(object, joined = FALSE)
  object <- sort.int(unique(object))
  if (length(object) > 1L)
    object <- grep(MISSING_CHAR, object, fixed = TRUE, value = TRUE,
      invert = TRUE)
  if (length(object) > 1L)
    case(match.arg(format, PHYLO_FORMATS),
      html = paste(object, collapse = "/"),
      epf =,
      phylip = MISSING_CHAR,
      hennig = sprintf("[%s]", paste(object, collapse = "")),
      nexus = sprintf("(%s)", paste(object, collapse = ""))
    )
  else
    object
}, sealed = SEALED)

setMethod("join_discrete", "numeric", function(object, format, digits = 3L) {
  switch(match.arg(format, PHYLO_FORMATS),
    hennig = {
      check_discrete(object, joined = FALSE)
      zero.fmt <- sprintf(fmt <- sprintf("%%%i.%if", digits + 3L, digits), 0)
      if ((len <- length(object <- na.exclude(object))) == 0L)
        MISSING_CHAR
      else if (len == 1L || (sd.o <- sprintf(fmt, sd(object))) == zero.fmt)
        sprintf(fmt, object[1L])
      else
        paste(sprintf(fmt, mean(object)), sd.o, sep = "-")
    },
    html = join_discrete(as.character(object), format),
    stop("joining is not implemented for data type 'numeric' combined with ",
      sprintf("format '%s'", format))
  )
}, sealed = SEALED)

setMethod("join_discrete", "matrix", function(object, format,
    groups = TRUE, digits = 3L) {
  if (isTRUE(groups))
    groups <- rownames(object)
  else if (identical(groups, FALSE))
    groups <- seq.int(nrow(object))
  if (length(groups <- as.factor(groups)) != nrow(object))
    stop("length of 'groups' not equal to number of rows")
  switch(match.arg(format, PHYLO_FORMATS),
    html = object <- map_gapmode(object),
    hennig = if (mode(object) == "numeric")
      object <- ranging(object, extended = TRUE, zscores = FALSE, fac = 65)
  )
  if (length(levels(groups)) == length(groups)) {
    check_discrete(object, joined = FALSE)
    if (is.numeric(object)) {
      object <- map_values(object, sprintf,
        fmt = sprintf("%%%i.%if", digits + 3L, digits))
      object[object == "NA"] <- MISSING_CHAR
      attr(object, "from.numeric") <- TRUE
    } else
      attr(object, "from.numeric") <- FALSE
    return(object)
  }
  from.numeric <- is.numeric(object)
  object <- as.data.frame(object, stringsAsFactors = FALSE)
  object <- aggregate(object, by = list(groups), FUN = join_discrete,
    format = format, simplify = TRUE)
  object <- as.matrix(object[, -1L, drop = FALSE])
  rownames(object) <- levels(groups)
  attr(object, "from.numeric") <- from.numeric
  object
}, sealed = SEALED)


################################################################################
#
# Build matrices for output
#


## NOTE: not an S4 method because conversion is done

#' Safe labels
#'
#' Convert strings to safe phylogenetic taxon labels: replace disallowed
#' characters or include all labels in single quotes, and double pre-existing
#' single quotes, if any. This is not normally called directly by an \pkg{opm}
#' user but by \code{\link{phylo_data}}; see there for further details.
#'
#' @param chars Character vector or convertible to such.
#' @param format Character scalar. See \code{\link{phylo_data}}.
#' @param enclose Logical scalar. See \code{\link{phylo_data}}.
#' @param pad Logical scalar. Bring labels to the same number of characters by
#'   appending spaces? Has no effect for \sQuote{phylip} and \sQuote{html}
#'   output format.
#' @export
#' @return Character vector.
#' @family phylogeny-functions
#' @keywords character
#' @seealso base::gsub
#' @examples
#' # Some animals you might know
#' x <- c("Elephas maximus", "Loxodonta africana", "Giraffa camelopardalis")
#'
#' (y <- safe_labels(x, "phylip"))
#' stopifnot(nchar(y) == 10L)
#'
#' (y <- safe_labels(x, "epf"))
#' stopifnot(nchar(y) == nchar(x))
#' (y <- safe_labels(x, "epf", pad = TRUE))
#' stopifnot(nchar(y) == 22)
#'
#' (y <- safe_labels(x, "nexus", enclose = TRUE))
#' stopifnot(grepl("^'.*'$", y))
#'
safe_labels <- function(chars, format, enclose = TRUE, pad = FALSE) {
  do_pad <- function(x, pad) {
    if (pad)
      sprintf(sprintf("%%-%is", max(nchar(x))), x)
    else
      x
  }
  nexus_quote <- function(chars) {
    sprintf("'%s'", gsub("'", "''", chars, fixed = TRUE))
  }
  clean_html <- function(x) {
    gsub(">", "&gt;", gsub("<", "&lt;", x, fixed = TRUE), fixed = TRUE)
  }
  clean <- function(pat, chars) {
    chars <- gsub(pat, "_", chars, perl = TRUE)
    chars <- sub(sprintf("^%s", pat), "", chars, perl = TRUE)
    sub(sprintf("%s$", pat), "", chars, perl = TRUE)
  }
  LL(enclose, pad)
  not.newick <- "[\\s,:;()]+"
  not.nexus <- "[\\s()\\[\\]{}\\/\\,;:=*'\"`+<>-]+" # see PAUP* manual
  # see http://tnt.insectmuseum.org/index.php/Basic_format (16/04/2012)
  not.hennig <- "[\\s;/+-]+"
  case(match.arg(format, PHYLO_FORMATS),
    html = clean_html(chars),
    phylip = sprintf("%-10s", substr(clean(not.newick, chars), 1L, 10L)),
    hennig = do_pad(clean(not.hennig, chars), pad),
    epf = do_pad(clean(not.newick, chars), pad),
    nexus = do_pad(if (enclose)
      nexus_quote(chars)
    else
      clean(not.nexus, chars), pad)
  )
}


################################################################################


setGeneric("phylo_header",
  function(object, ...) standardGeneric("phylo_header"))
#' Header for phylogenetic data
#'
#' Create header of file formats for exporting phylogenetic data.
#'
#' @param object Matrix.
#' @param format Output format (character scalar). See
#'   \code{\link{phylo_data}}.
#' @param enclose Logical scalar. See \code{\link{phylo_data}}.
#' @param indent Integer. See \code{\link{phylo_data}}.
#' @return Character vector.
#' @keywords internal
#'
setMethod("phylo_header", "matrix", function(object, format, enclose = TRUE,
    indent = 3L) {

  prepare_nexus_comment <- function(x) {
    x <- gsub("[", "{", x, fixed = TRUE)
    x <- gsub("]", "}", x, fixed = TRUE)
    c("[", x, "]", "")
  }
  d <- dim(object)
  if (is.null(comments <- comment(object)) || all(!nzchar(comments)))
    comments <- DEFAULT_PHYLO_COMMENT

  case(match.arg(format, PHYLO_FORMATS),

    html = {
      result <- sprintf("Strains: %s.", pkgutils::listing(rownames(object),
        style = "%s, %s", collapse = "; ", force.numbers = TRUE))
      result <- paste(result, paste(c(
        "+, positive metabolic response",
        "w, weak metabolic response",
        "-, negative metabolic response."), collapse = "; "))
      c(
        paste('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"',
          '"http://www.w3.org/TR/html4/strict.dtd">', collapse = " "),
        "<html>",
        "",
        "<head>",
        '<meta name="generator" content="opm">',
        hwriter::hmakeTag("title",
          data = safe_labels(comments, format = "html")),
        "</head>",
        "",
        "<body>",
        hwriter::hmakeTag("p", data = result),
        ""
      )
    },

    phylip =,
    epf = paste(d, collapse = " "),

    hennig = {
      datatype <- switch(mode(object),
        numeric = "continuous",
        character = if (attr(object, "from.numeric"))
          "continuous"
        else
          "numeric",
        stop("uninterpretable datatype")
      )
      nstates <- sprintf("nstates %s;", case(datatype,
        numeric = "32",
        continuous = "cont"
      ))
      c(
        nstates,
        "xread",
        "'",
        gsub("'", '"', comments, fixed = TRUE),
        "'",
        paste(rev(d), collapse = " "),
        sprintf("&[%s]", datatype)
      )
    },

    nexus = {
      indent <- paste(rep.int(" ", indent), collapse = "")
      datatype <- switch(mode(object),
        numeric = "continuous",
        character = if (attr(object, "from.numeric"))
          "continuous"
        else
          "standard",
        stop("uninterpretable datatype")
      )
      if (datatype == "continuous")
        warning("continuous data are not supported by PAUP*")
      symbols <- if (datatype == "standard")
        sprintf('%sformat symbols = "%s";', indent,
          paste(CHARACTER_STATES, collapse = ""))
      else
        NULL
      if (length(charlabels <- colnames(object)) > 0L) {
        charlabels <- safe_labels(charlabels, format = "nexus",
          enclose = enclose)
        charlabels <- sprintf("%scharlabels %s;", indent,
          paste(charlabels, collapse = " "))
      } else {
        warning("character labels not found")
        charlabels <- NULL
      }
      c(
        "#NEXUS",
        "",
        prepare_nexus_comment(comments),
        "begin data;",
        sprintf("%sdimensions ntax = %i nchar = %i;", indent, d[1L], d[2L]),
        sprintf("%sformat datatype = %s missing = ?;", indent, datatype),
        symbols,
        charlabels,
        sprintf("%smatrix", indent)
      )
    }

  )
}, sealed = SEALED)


################################################################################


## NOTE: not an S4 method because checked using match.arg()

#' Footer for phylogenetic data
#'
#' Create footer of file formats for exporting phylogenetic data.
#'
#' @param format Character scalar. See \code{\link{phylo_data}} for the
#'   possible values.
#' @param indent Integer scalar. See \code{\link{phylo_data}}.
#' @param paup.block Logical scalar. Append a PAUP* block with selected default
#'   values?
#' @return Character vector or \code{NULL}.
#' @keywords internal
#'
phylo_footer <- function(format, indent = 3L, paup.block = FALSE) {
  case(match.arg(format, PHYLO_FORMATS),
    html = c("</body>", "</html>", ""),
    epf =,
    phylip = NULL,
    hennig = c(";", "ccode - .;", "procedure /;"),
    nexus = {
      indent <- paste(rep.int(" ", indent), collapse = "")
      block <- if (paup.block)
        c("", "begin paup;", paste(indent, PAUP_BLOCK, sep = ""), "end;")
      else
        character()
      c(
        sprintf("%s;", indent),
        "end;",
        "",
        "begin assumptions;",
        sprintf("%stypeset * default = ord : all;", indent),
        "end;",
        block
      )
    }
  )
}


################################################################################


setGeneric("map_gapmode", function(object) standardGeneric("map_gapmode"))
#' Map gap-mode discretized character codes
#'
#' Map character states that have been discretized with \code{\link{discrete}}
#' and \code{gap} set to \code{TRUE}. This is mainly necessary for creating
#' HTML tables for IJSEM. See \code{\link{phylo_data}} for details.
#'
#' @param object Matrix. See \code{\link{phylo_data}}.
#' @return Matrix
#' @keywords internal
#'
setMethod("map_gapmode", "matrix", function(object) {
  members <- function(x) all(object %in% x)
  if (members(c("-", "w", "+")) ||
      all(grepl("^[+w-](/[+w-])*$", object, perl = TRUE)))
    return(object)
  if (members(c("0", "?", "1")))
    mapping <- c(`0` = "-", `?` = "w", `1` = "+")
  else if (members(c("0", "1", "2")))
    mapping <- c(`0` = "-", `1` = "w", `2` = "+")
  else
    stop("character coding not recognized")
  map_values(object, mapping, coerce = "character")
}, sealed = SEALED)


################################################################################


setGeneric("phylo_char_mat",
  function(object, ...) standardGeneric("phylo_char_mat"))
#' Phylogenetic character matrix
#'
#' Create proper character matrix (without header and footer) in a file format
#' suitable for exporting phylogenetic data.
#'
#' @param object Matrix. See \code{\link{phylo_data}}.
#' @param format Output format (character scalar). See \code{\link{phylo_data}}.
#' @param enclose Logical scalar. See \code{\link{phylo_data}}.
#' @param remove.const Logical scalar. See \code{\link{phylo_data}}.
#' @param remove.ambig Logical scalar. See \code{\link{phylo_data}}.
#' @return Character vector.
#' @keywords internal
#'
setMethod("phylo_char_mat", "matrix", function(object, format, enclose = TRUE,
    delete = c("uninf", "constant", "ambig", "none")) {

  LL(enclose, delete)
  format <- match.arg(format, PHYLO_FORMATS)

  if (format == "html") {
    object <- map_gapmode(object)
    rownames(object) <- seq.int(nrow(object))
    if (is.null(colnames(object)))
      stop("missing substrate labels (column names)")
    case(match.arg(delete),
      ambig = {
        has_ambig <- function(x) any(grepl("/", x, fixed = TRUE))
        object <- object[, !apply(object, 2L, has_ambig), drop = FALSE]
      },
      constant = object <- object[, !is_constant(object, 2L), drop = FALSE],
      none = NULL,
      uninf = {
        const_fun <- function(x) {
          is_constant(strsplit(x, "/", fixed = TRUE), set.like = TRUE)
        }
        object <- object[, !apply(object, 2L, const_fun), drop = FALSE]
      }
    )
    return(c(hwriter::hwrite(t(object), table.summary = "PM data"), ""))
  }

  if (length(cl <- rownames(object)) == 0L)
    stop("missing taxon labels (row names)")
  cl <- safe_labels(cl, format = format, enclose = enclose, pad = TRUE)
  if (dups <- anyDuplicated(cl))
    stop("duplicated taxon label (row name): ", cl[dups])
  sep <- switch(mode(object),
    numeric = " ",
    character = {
      check_discrete(object, joined = TRUE,
        from.numeric = from.numeric <- attr(object, "from.numeric"))
      if (from.numeric)
        " "
      else
        ""
    },
    stop("unsupported datatype")
  )
  paste(cl, apply(object, 1L, paste, collapse = sep), sep = "\t")

}, sealed = SEALED)


################################################################################


setGeneric("phylo_data", function(object, ...) standardGeneric("phylo_data"))
#' Export phylogenetic data
#'
#' Create entire character matrix (include header and footer) in a file format
#' suitable for exporting phylogenetic data. Return it or write it to a file.
#' This function can also produce HTML tables suitable for displaying PM data
#' in taxonomic journals such as IJSEM.
#'
#' @param object Data frame, numeric matrix or \sQuote{OPMS} object (with
#'   aggregated values). Currently only \sQuote{integer}, \sQuote{logical},
#'   \sQuote{double} and \sQuote{character} matrix content is supported. The
#'   data-frame and \sQuote{OPMS} methods first call \code{\link{extract}} and
#'   then the matrix method.
#' @param format Character scalar, either \sQuote{epf} (Extended Phylip Format),
#'   \sQuote{nexus}, \sQuote{phylip}, \sQuote{hennig} or \sQuote{html}. If
#'   \sQuote{nexus} or \sQuote{hennig} format are chosen, a non-empty
#'   \code{comment} attribute will be output together with the data (and
#'   appropriately escaped). In case of \sQuote{html} format, a non-empty
#'   \code{comment} yields the title of the HTML document. The main difference
#'   between \sQuote{epf} and \sQuote{phylip} is that the former can use labels
#'   with more than ten characters, but its labels must not contain whitespace.
#'   (These adaptations are done automatically with \code{\link{safe_labels}}.)
#' @param outfile Character scalar. If a non-empty character scalar, resulting
#'   lines are directly written to this file. Otherwise, they
#'   are returned.
#' @param enclose Logical scalar. Shall labels be enclosed in single quotes?
#'   Ignored unless \code{format} is \sQuote{nexus}.
#' @param indent Integer scalar. Indentation of subcommands in NEXUS format.
#'   Ignored unless \code{format} is \sQuote{nexus} (and a matter of taste
#'   anyway).
#' @param paup.block Logical scalar. Append a PAUP* block with selected default
#'   values?
#' @param delete Character scalar, one of \sQuote{uninf}, \sQuote{ambig},
#'   \sQuote{constant} or \sQuote{none}. If \sQuote{uninf}, columns are removed
#'   which are either constant (in the strict sense) or are columns
#'   in which some fields contain polymorphisms, and no pairs of fields
#'   share no character states. If \sQuote{ambig}, columns with ambiguities are
#'   removed. If \sQuote{constant}, columns which are constant in the strict
#'   sense are removed. \code{delete} is currently ignored for formats other
#'   than \sQuote{html}, and note that columns become rows in the final HTML
#'   output.
#'
#' @param join Logical scalar, vector or factor. Unless \code{FALSE}, rows of
#'   \code{object} are joined together, either according to the row names
#'   (if \code{join} is \code{TRUE}), or directly according to \code{join}.
#'   This can be used to deal with measurements repetitions for the same
#'   organism or treatment.
#' @param digits Numeric scalar. Used for rounding, and thus ignored unless
#'   \code{object} is of mode \sQuote{numeric}.
#' @param prefer.char Logical scalar. When dealing with \sQuote{logical} or
#'   \sQuote{integer} input, convert it to character instead of numeric data?
#'   This fails for integer values > 9 or < 0.
#' @param run.tidy Logical scalar. Filter the resulting HTML through the HTML
#'   Tidy program? Ignored unless \code{format} is \sQuote{html}. Otherwise, if
#'   \code{TRUE}, it is an error if the Tidy executable is not found.
#'
#' @param as.labels Vector of data-frame indices or \sQuote{OPMS} metadata
#'   entries. See \code{\link{extract}}.
#' @param what Character scalar. See \code{\link{extract}}.
#' @param sep Character scalar. See \code{\link{extract}}.
#'
#' @param subset Character scalar passed to the \sQuote{OPMS} method of
#'   \code{\link{extract}}.
#' @param extract.args Optional list of arguments passed to that method.
#' @param discrete.args Optional list of arguments passed from the
#'   \sQuote{OPMS} method to \code{\link{discrete}}. If set to \code{NULL},
#'   discretization is turned off. Ignored if precomputed discretized values are
#'   chosen by setting \code{subset} to \sQuote{disc}.
#'
#' @param ... Optional arguments passed between the methods (i.e., from the
#'   other methods to the matrix method).
#'
#' @export
#' @return Character vector, each element representing a line in a potential
#'   output file, returned invisibly if \code{outfile} is given.
#' @family phylogeny-functions
#' @seealso base::comment base::write
#' @note \itemize{
#'   \item Exporting PM data in such formats allows one to either infer trees
#'     from the data under the maximum-likelihood and/or the maximum-parsimony
#'     criterion, or to reconstruct the evolution of PM characters on given
#'     phylogenetic trees.
#'   \item For exporting NEXUS format, the matrix should normally be converted
#'     beforehand by applying \code{\link{discrete}}. Even stricter is the
#'     \sQuote{html} setting, which requires the data to be discretized with
#'     \code{gap} set to \code{TRUE}. The \sQuote{hennig} (Hennig86) format is
#'     the one used by TNT; it allows continuous characters to be analysed as
#'     such.
#'   \item \sQuote{epf} or \sQuote{extended PHYLIP} is sometimes called
#'     \sQuote{relaxed PHYLIP}.
#'   \item The generated HTML is guaranteed to produce neither errors nor
#'     warnings if checked using HTML Tidy.
#' }
#'
#' @keywords character cluster IO
#'
#' @references Berger, S. A., Stamatakis, A. 2010 Accuracy of morphology-based
#'   phylogenetic fossil placement under maximum likelihood. \emph{8th ACS/IEEE
#'   International Conference on Computer Systems and Applications
#'   (AICCSA-10).} Hammamet, Tunisia [analysis of phenotypic data wih RAxML].
#' @references Felsenstein, J. 2005 PHYLIP (Phylogeny Inference Package)
#'   version 3.6. Distributed by the author. Seattle: University
#'   of Washington, Department of Genome Sciences [the PHYLIP program].
#' @references Goloboff, P.A., Farris, J.S., Nixon, K.C. 2008 TNT, a free
#'   program for phylogenetic analysis. \emph{Cladistics} \strong{24}, 774--786
#'   [the TNT program].
#' @references Goloboff, P.A., Mattoni, C., Quinteros, S. 2005 Continuous
#'   characters analyzed as such. \emph{Cladistics} \strong{22}, 589--601.
#' @references Maddison, D. R., Swofford, D. L., Maddison, W. P. 1997 Nexus: An
#'   extensible file format for systematic information. \emph{Syst Biol}
#'   \strong{46}, 590--621 [the NEXUS format].
#' @references Stamatakis, A. 2006 RAxML-VI-HPC: Maximum likelihood-based
#'   phylogenetic analyses with thousands of taxa and mixed models
#'   \emph{Bioinformatics} \strong{22}, 2688--2690. [the RAxML program].
#' @references Swofford, D. L. 2002 PAUP*: Phylogenetic Analysis Using
#'   Parsimony (*and Other Methods), Version 4.0 b10. Sunderland, Mass.:
#'   Sinauer Associates, [the PAUP* program].
#' @references \url{http://ijs.sgmjournals.org/} [IJSEM journal]
#' @references \url{http://tidy.sourceforge.net/} [HTML Tidy]
#'
#' @examples
#'
#' x <- matrix(c(0:9, letters[1:22]), nrow = 2)
#' colnames(x) <- LETTERS[1:16]
#' rownames(x) <- c("Ahoernchen", "Behoernchen") # Chip and Dale in German
#'
#' (y.epf <- phylo_data(x, format = "epf"))
#' stopifnot(is.character(y.epf), length(y.epf) == 3)
#' stopifnot(identical(y.epf, phylo_data(as.data.frame(x), what = "factor",
#'   format = "epf")))
#'
#' (y.phylip <- phylo_data(x, format = "phylip"))
#' stopifnot((y.epf == y.phylip) == c(TRUE, FALSE, FALSE))
#'
#' (y.nexus <- phylo_data(x, format = "nexus"))
#' nexus.len.1 <- length(y.nexus)
#' stopifnot(is.character(y.nexus), nexus.len.1 > 10)
#'
#' comment(x) <- c("This", "is", "a", "test")
#' (y.nexus <- phylo_data(x, format = "nexus"))
#' stopifnot(identical(length(y.nexus), nexus.len.1 + 3L))
#'
#' (y.hennig <- phylo_data(x, format = "hennig"))
#' hennig.len.1 <- length(y.hennig)
#' stopifnot(is.character(y.hennig), hennig.len.1 > 10)
#'
#' comment(x) <- NULL
#' (y.hennig <- phylo_data(x, format = "hennig"))
#' stopifnot(identical(length(y.hennig), hennig.len.1 - 3L))
#'
#' # Example with real data; see discrete() for the conversion
#' data(vaas_4)
#' x <- extract(vaas_4, as.labels = list("Species", "Strain"),
#'   in.parens = FALSE)
#' x <- discrete(x, range = TRUE, gap = TRUE)
#' (y <- phylo_data(x, format = "html"))
#' stopifnot(is.character(y), length(y) == 16)
#' stopifnot(c("</html>", "</head>", "</body>", "") %in% y)
#'
#' # Example with real data, joining the results per species
#' x <- extract(vaas_4, as.labels = list("Species"), in.parens = FALSE)
#' x <- discrete(x, range = TRUE, gap = TRUE)
#' (y <- phylo_data(x, format = "html", join = TRUE))
#' stopifnot(is.character(y), length(y) == 16)
#' stopifnot(c("</html>", "</head>", "</body>", "") %in% y)
#'
#' # 'OPMS' method
#' (yy <- phylo_data(vaas_4, as.labels = "Species", format = "html",
#'   join = TRUE, extract.args = list(in.parens = FALSE)))
#' stopifnot(identical(y, yy))
#'
#' # Effect of deletion
#' (y <- phylo_data(x, "html", delete = "none", join = FALSE))
#' (y.noambig <- phylo_data(x, "html", delete = "ambig", join = FALSE))
#' stopifnot(identical(y, y.noambig))
#' # ambiguities are created by joining only
#' longer <- function(x, y) {
#'   any(nchar(x) > nchar(y)) && !any(nchar(x) < nchar(y))
#' }
#' (y <- phylo_data(x, "html", delete = "none", join = TRUE))
#' (y.noambig <- phylo_data(x, "html", delete = "ambig", join = TRUE))
#' stopifnot(longer(y, y.noambig))
#' (y.nouninf <- phylo_data(x, "html", delete = "uninf", join = TRUE))
#' stopifnot(longer(y.noambig, y.nouninf))
#' (y.noconst <- phylo_data(x, "html", delete = "const", join = TRUE))
#' stopifnot(longer(y.noconst, y.nouninf))
#'
setMethod("phylo_data", "matrix", function(object,
    format = opm_opt("phylo.fmt"), outfile = "", enclose = TRUE, indent = 3L,
    paup.block = FALSE, delete = "none", join = FALSE,
    digits = opm_opt("digits"),
    prefer.char = identical(format, "html") || !identical(join, FALSE),
    run.tidy = FALSE) {
  LL(outfile, prefer.char, run.tidy)
  if (is.logical(object))
    mode(object) <- "integer"
  if (is.integer(object))
    if (prefer.char) {
      mode(object) <- "character"
      object[is.na(object)] <- MISSING_CHAR
    } else
      mode(object) <- "numeric"
  object <- join_discrete(object, format = format, groups = join,
    digits = digits)
  lines <- c(
    phylo_header(object, format = format, enclose = enclose, indent = indent),
    phylo_char_mat(object, format = format, enclose = enclose,
      delete = delete),
    phylo_footer(format = format, indent = indent, paup.block = paup.block)
  )
  if (run.tidy && match.arg(format, PHYLO_FORMATS) == "html")
    lines <- tidy(lines, check = FALSE)
  if (nzchar(outfile)) {
    write(lines, outfile)
    invisible(lines)
  } else
    lines
}, sealed = SEALED)

setMethod("phylo_data", "data.frame", function(object, as.labels = NULL,
    what = "numeric", sep = " ", ...) {
  object <- extract(object, as.labels = as.labels, what = what, sep = sep)
  phylo_data(object, ...)
}, sealed = SEALED)

setMethod("phylo_data", OPMS, function(object, as.labels, subset = "disc",
    sep = " ", extract.args = list(), join = TRUE,
    discrete.args = list(range = TRUE, gap = TRUE), ...) {
  extract.args <- insert(as.list(extract.args), list(object = object,
    as.labels = as.labels, as.groups = NULL, subset = subset,
    dups = if (isTRUE(join))
      "ignore"
    else
      "warn", dataframe = FALSE, ci = FALSE, sep = sep), .force = TRUE)
  object <- do.call(extract, extract.args)
  if (!is.null(discrete.args) && !is.logical(object)) {
    discrete.args <- as.list(discrete.args)
    discrete.args$x <- object
    object <- do.call(discrete, discrete.args)
  }
  phylo_data(object = object, join = join, ...)
}, sealed = SEALED)


################################################################################


