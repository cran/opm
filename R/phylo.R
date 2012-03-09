

################################################################################
#
# Constants used only in this file
#

CHARACTER_STATES <- c(0L:9L, LETTERS)[1L:32L]
MISSING_CHAR <- "?"

if (MISSING_CHAR %in% CHARACTER_STATES)
  stop(BUG_MSG)

PHYLO_FORMATS <- c("epf", "nexus", "phylip")


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
#
# Character discretization
#


setGeneric("discrete", function(x, ...) standardGeneric("discrete"))
#' Convert to discrete characters
#'
#' Convert a vector of continuous characters to discrete ones. One of the uses
#' of this functions is to create character data suitable for phylogenetic
#' studies with programs such as PAUP* and RAxML. These accept only discrete
#' characters with at most 32 states, coded as 0 to 9 followed by A to V. For
#' the full export one additionally needs \code{\link{phylo_data}}. The matrix
#' method is just a wrapper that takes care of the matrix dimensions.
#'
#' @param x Numeric vector or matrix.
#'
#' @param range In non-\code{gap} mode (see next argument) the assumed real 
#'   range of the data; must contain all elements of \code{x}, but can be much 
#'   wider. In \code{gap} mode, it must, in contrast, lie within the range of 
#'   \code{x}. If \code{range} is set to \code{TRUE}, the empirical range of 
#'   \code{x} is used. This makes not much sense in \code{gap} mode, and a 
#'   warning is issued in that case.
#'
#' @param gap Logical scalar. If \code{TRUE}, always convert to binary or 
#'   ternary
#'   characters, ignoring \code{states}. \code{range} then indicates a subrange
#'   of \code{x} within which character conversion is ambiguous and has to be 
#'   treated as either missing information or intermediate character state, 
#'   depending on \code{middle.na}. If \code{FALSE} (the default), apply an 
#'   equal-width-intervals discretization with the widths determined from the
#'   number of requested \code{states} and \code{range}.
#'
#' @param output String determining the output mode: \sQuote{character},
#'   \sQuote{integer}, \sQuote{logical}, \sQuote{factor}, or \sQuote{numeric}.
#'   \sQuote{numeric} simply returns \code{x}, but performs the range checks.
#'   One cannot combine \sQuote{logical} with \code{TRUE} values for both
#'   \sQuote{gap} and \sQuote{middle.na}.
#'
#' @param middle.na Logical scalar. Only relevant in \code{gap} mode: if 
#'   \code{TRUE}, the middle value yields \code{NA} (uncertain whether negative 
#'   or positive). If \code{FALSE}, the middle value lies between the left and
#'   the right one (i.e., a third character state meaning \sQuote{weak}). This
#'   is simply coded as 0-1-2 and thus cannot be combined with \sQuote{logical}
#'   as \code{output} setting.
#'
#' @param states Integer or character vector. Ignored in \code{gap} mode and if
#'   \code{output} is not \sQuote{character}. Otherwise, (i) a single-element
#'   character vector, which is split into its elements, (ii) a multi-element 
#'   character vector which is used directly, or (iii) an integer vector 
#'   indicating the elements to pick from the default character states. In the
#'   latter case, a single integer is interpreted as the upper bound of an 
#'   integer vector starting at 1.
#'
#' @param ... Arguments passed between the methods.
#'
#' @export
#' @return Double, integer, character or logical vector or factor, depending on 
#'   \code{output}. For the matrix method, a matrix composed of a vector as
#'   produced by the numeric method, the original \code{dimensions} and the
#'   original \code{dimnames} attributes of \code{x}.
#' @family phylogeny-functions
#' @seealso base::cut
#' @keywords character category
#' @references Dougherty J, Kohavi R, Sahami M. Supervised and unsupervised
#'   discretization of continuous features. In: Prieditis A, Russell S (eds) 
#'   Machine Learning: Proceedings of the fifth international conference. 1995.
#' @references Ventura D, Martinez TR. An empirical comparison of 
#'   discretization methods. Proceedings of the Tenth International Symposium 
#'   on Computer and Information Sciences 1995; 443-450.
#' @references Bunuel L. Le charme discret de la bourgeoisie. 1972;
#'   France/Spain, 96 min.
#'
#' @examples
#' # Treat everything between 3.4 and 4.5 as ambiguous
#' (x <- discrete(1:5, range = c(3.5, 4.5), gap = TRUE))
#' stopifnot(identical(x, c("0", "0", "0", "?", "1")))
#' 
#' # Treat everything between 3.4 and 4.5 as intermediate
#' (x <- discrete(1:5, range = c(3.5, 4.5), gap = TRUE, middle.na = FALSE))
#' stopifnot(identical(x, c("0", "0", "0", "1", "2")))
#'
#' # Boring example: real and possible range as well as the number of states
#' # to code the data have a 1:1 relationship
#' (x <- discrete(1:5, range = c(1, 5), states = 5))
#' stopifnot(identical(x, as.character(0:4)))
#'
#' # Now fit the data into a potential range twice as large, and at the 
#' # beginning of it
#' (x <- discrete(1:5, range = c(1, 10), states = 5))
#' stopifnot(identical(x, as.character(c(0, 0, 1, 1, 2))))
#'
#' # Matrix method
#' x <- matrix(1:10, ncol = 2)
#' (y <- discrete(x, range = c(3.4, 4.5), gap = TRUE))
#' stopifnot(identical(dim(x), dim(y)))
#'
setMethod("discrete", "numeric", function(x, range, gap = FALSE,
    output = c("character", "integer", "logical", "factor", "numeric"),
    middle.na = TRUE, states = 32L) {

  convert_states <- function(states) {
    if (length(states) == 0L)
      CHARACTER_STATES
    else if (is.numeric(states))
      if (length(states) > 1L)
        CHARACTER_STATES[states]
      else
        CHARACTER_STATES[seq(states)]
    else if (is.character(states))
      if (length(states) == 1L) {
        if (!nzchar(states))
          stop("'states' cannot be the empty string")
        unlist(strsplit(states, "", fixed = TRUE))
      } else if (any(nchar(states) != 1L))
        stop("'states' cannot contain strings of length other than one")
      else
        states
    else
      stop("'states' must be empty or character or numeric vector")
  }

  output <- match.arg(output)
  
  if (is.logical(range) && range) {
    if (gap)
      warning("using the empirical range of the data in 'gap' mode")
    range <- range(x)
  } else {
    assert_length(range, .wanted = 2L)
    range <- sort(range[1L:2L])
  }

  if (gap) { # binary-state mode with a gap due to ambiguity

    x.range <- range(x)
    if (range[1L] < x.range[1L] || range[2L] > x.range[2L])
      stop("in 'gap' mode, 'range' must be within the range of 'x'")
    if (output == "numeric")
      return(x)
    tol <- .Machine$double.eps^0.5
    breaks <- c(x.range[1L], c(range[1L] + tol, range[2L] - tol),
      x.range[2L] + tol)
    ints <- cut(x, breaks, labels = FALSE, right = FALSE)
    map <- if (middle.na)
      switch(output,
        character = c("0", MISSING_CHAR, "1"),
        integer = c(0L, NA_integer_, 1L),
        logical = c(FALSE, NA, TRUE),
        factor = ordered(c(0L, NA_integer_, 1L)),
        stop(BUG_MSG)
      )
    else
      switch(output,
        character = c("0", "1", "2"),
        integer = c(0L, 1L, 2L),
        logical = stop("one cannot combine 'logical' and 'middle.na'"),
        factor = ordered(c(0L, 1L, 2L)),
        stop(BUG_MSG)
      )
    map[ints]

  } else { # binary- to multi-state mode without a gap

    if (any(x > range[2L] | x < range[1L]))
      stop("if not in 'gap' mode, all values must be between ", range[1L],
        " and ", range[2L])
    if (output == "numeric")
      return(x)
    states <- convert_states(states)
    ints <- if ((nstates <- length(states)) > 1L)
      cut(x = c(range[1L:2L], x), breaks = nstates, right = FALSE,
        labels = FALSE)[-1L:-2L]
    else
      rep.int(1L, length(x))
    switch(output,
      character = states[ints],
      integer = ints,
      logical = as.logical(ints - 1L),
      factor = ordered(ints),
      stop(BUG_MSG)
    )

  }
}, sealed = SEALED)

#' @export
#'
setMethod("discrete", "matrix", function(x, ...) {
  structure(.Data = discrete(as.numeric(x), ...), dim = dim(x),
    dimnames = dimnames(x))
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
#' @param object Character vector or convertible to such, or matrix.
#' @param margin Integer scalar. See \code{apply} from package \pkg{base} and 
#'   \code{\link{join_discrete}}, which is applied to either rows or columns,
#'   depending on this argument.
#' @return Character scalar, for the matrix method a vector.
#' @keywords internal
#'
setMethod("join_discrete", "ANY", function(object) {
  object <- as.character(object)
  if (length(object) == 0L || any(nchar(object) != 1L))
    stop("need strings of length 1")
  object <- sort(unique(object))
  if (length(object) > 1L)
    object <- grep(MISSING_CHAR, object, fixed = TRUE, value = TRUE,
      invert = TRUE)
  if (length(object) > 1L)
    sprintf("(%s)", paste(object, collapse = ""))
  else
    object
}, sealed = SEALED)

setMethod("join_discrete", "matrix", function(object, margin = 2L) {
  apply(object, margin, join_discrete)
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
#'
#' (y <- safe_labels(x, "nexus", enclose = TRUE))
#' stopifnot(grepl("^'.*'$", y))
#'
safe_labels <- function(chars, format, enclose = TRUE) {
  nexus_quote <- function(chars) {
    sprintf("'%s'", gsub("'", "''", chars, fixed = TRUE))
  }
  clean <- function(pat, chars) {
    chars <- gsub(pat, "_", chars, perl = TRUE)
    chars <- sub(sprintf("^%s", pat), "", chars, perl = TRUE)
    sub(sprintf("%s$", pat), "", chars, perl = TRUE)
  }
  not.newick <- "[\\s,:;()]+"
  not.nexus <- "[\\s()\\[\\]{}\\/\\,;:=*'\"`+<>-]+"
  switch(match.arg(format, PHYLO_FORMATS),
    phylip = sprintf("%-10s", substr(clean(not.newick, chars), 1L, 10L)),
    epf = clean(not.newick, chars),
    nexus = if (enclose)
      nexus_quote(chars)
    else
      clean(not.nexus, chars),
    stop(BUG_MSG)
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
  d <- dim(object)
  switch(match.arg(format, PHYLO_FORMATS),
    phylip =,
    epf = paste(d, collapse = " "),
    nexus = {
      
      indent <- paste(rep.int(" ", indent), collapse = "")
      
      datatype <- switch(class(object[1L]),
        integer =,
        numeric = {
          warning("continuous data are not supported by PAUP*")
          "continuous"
        },
        character = "standard",
        stop("uninterpretable datatype")
      )
      
      symbols <- if (datatype == "standard")
        sprintf('%sformat symbols = "%s";', indent,
          paste(CHARACTER_STATES, collapse = ""))
      else
        NULL
      
      charlabels <- colnames(object)
      if (length(charlabels) > 0L) {
        charlabels <- safe_labels(charlabels, format = "nexus", 
          enclose = enclose)
        charlabels <- sprintf("%scharlabels %s;", indent, 
          paste(charlabels, collapse = " "))
      } else {
        warning("character labels not found")
        charlabels <- NULL
      }
      
      comments <- comment(object)
      if (!is.null(comments) && any(nzchar(comments))) {
        comments <- gsub("[", "{", comments, fixed = TRUE)
        comments <- gsub("]", "}", comments, fixed = TRUE)
        comments <- c("[", comments, "]", "")
      }
         
      c(
        "#NEXUS",
        "",
        comments,
        "begin data;",
        sprintf("%sdimensions ntax = %i nchar = %i;", indent, d[1L], d[2L]),
        sprintf("%sformat datatype = %s missing = ?;", indent, datatype),
        symbols,
        charlabels,
        sprintf("%smatrix", indent)
      )

    },
    stop(BUG_MSG)
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
  switch(match.arg(format, PHYLO_FORMATS),
    epf =,
    phylip = NULL,
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
    },
    stop(BUG_MSG)
  )
}


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
#' @return Character vector.
#' @keywords internal
#'
setMethod("phylo_char_mat", "matrix", function(object, format, enclose = TRUE) {
  format <- match.arg(format, PHYLO_FORMATS)
  cl <- rownames(object)
  if (length(cl) == 0L)
    stop("missing taxon labels (row names)")
  cl <- safe_labels(cl, format = format, enclose = enclose)
  if (dups <- anyDuplicated(cl))
    stop("duplicated taxon label (row name): ", cl[dups])
  if (is.logical(object))
    object <- structure(as.integer(object), dim = dim(object), 
      dimnames = dimnames(object))
  sep <- switch(class(object[1L]),
    integer =,
    numeric = " ",
    character = {
      pat <- paste(c(CHARACTER_STATES, MISSING_CHAR), collapse = "")
      pat <- sprintf("^([%s]|\\([%s]+\\))$", pat, pat)
      bad.chars <- grep(pat, object, ignore.case = TRUE, perl = TRUE,
        invert = TRUE, value = TRUE)
      if (length(bad.chars) > 0L)
        stop("uninterpretable character: ", bad.chars[1L])
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
#'
#' @param object Matrix. Currently only \sQuote{integer}, \sQuote{logical},
#'   \sQuote{numeric} and \sQuote{character} content is supported.
#' @param format Character scalar, either \sQuote{epf} (Extended Phylip Format),
#'   \sQuote{nexus} or \sQuote{phylip}. The main difference between 
#'   \sQuote{epf} and \sQuote{phylip} is that the former can use labels with
#'   more than ten characters, but its labels must not contain whitespace. If
#'   \sQuote{nexus} format is chosen, a non-empty \code{comment} attribute will
#'   be output together with the data (and appropriately escaped).
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
#' @export
#' @return Character vector, each element representing a line in a potential
#'   output file, returned invisibly if \code{outfile} is given.
#' @family phylogeny-functions
#' @family IO-functions
#' @seealso base::comment base::write
#' @note For exporting NEXUS format, the matrix should normally be converted
#'   beforehand by applying \code{\link{discrete}}.
#' @keywords character cluster IO
#'
#' @references Berger SA, Stamatakis A. Accuracy of morphology-based 
#'   phylogenetic fossil placement under maximum likelihood. 2010; 8th ACS/IEEE 
#'   International Conference on Computer Systems and Applications (AICCSA-10), 
#'   Hammamet, Tunisia [analysis of phenotypic data wih RAxML].
#' @references Felsenstein J. PHYLIP (Phylogeny Inference Package) version 3.6. 
#'   Distributed by the author. 2005; Department of Genome Sciences, University
#'   of Washington, Seattle [the PHYLIP program].
#' @references Maddison DR, Swofford DL, Maddison WP. Nexus: An extensible file 
#'   format for systematic information. Syst Biol 1997; 46:590-621 [the NEXUS
#'   format].
#' @references Stamatakis A. RAxML-VI-HPC: Maximum likelihood-based 
#'   phylogenetic analyses with thousands of taxa and mixed models
#'   Bioinformatics 2006; 22:2688-2690. [the RAxML program].
#' @references Swofford DL. PAUP*: Phylogenetic Analysis Using Parsimony (*and 
#'   Other Methods), Version 4.0 b10. 2002; Sinauer Associates, Sunderland
#'   [the PAUP* program].
#' 
#' @examples
#' 
#' x <- matrix(c(0:9, letters[1:22]), nrow = 2)
#' colnames(x) <- LETTERS[1:16]
#' rownames(x) <- c("Ahoernchen", "Behoernchen") # Chip and Dale in German
#'
#' (y.epf <- phylo_data(x, format = "epf"))
#' stopifnot(is.character(y.epf), length(y.epf) == 3)
#'
#' (y.phylip <- phylo_data(x, format = "phylip"))
#' stopifnot((y.epf == y.phylip) == c(TRUE, TRUE, FALSE))
#'
#' (y.nexus <- phylo_data(x, format = "nexus"))
#' nexus.len.1 <- length(y.nexus)
#' stopifnot(is.character(y.nexus), nexus.len.1 > 10)
#'
#' comment(x) <- c("This", "is", "a", "test")
#' (y.nexus <- phylo_data(x, format = "nexus"))
#' stopifnot(identical(length(y.nexus), nexus.len.1 + 7L))
#'
setMethod("phylo_data", "matrix", function(object, format = "epf",
    outfile = "", enclose = TRUE, indent = 3L, paup.block = FALSE) {
  assert_length(outfile)
  lines <- c(
    phylo_header(object, format = format, enclose = enclose, indent = indent),
    phylo_char_mat(object, format = format, enclose = enclose),
    phylo_footer(format = format, indent = indent, paup.block = paup.block)
  )
  if (nzchar(outfile)) {
    write(lines, outfile)
    invisible(lines)
  } else
    lines
}, sealed = SEALED)


