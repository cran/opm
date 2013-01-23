

################################################################################
################################################################################
#
# Phylogeny- related methods
#


## NOTE: not an S4 method because conversion is done

#' Safe labels
#'
#' Convert strings to safe phylogenetic taxon labels: replace disallowed
#' characters or include all labels in single quotes, and double pre-existing
#' single quotes, if any. This is not normally called directly by an \pkg{opm}
#' user but by \code{\link{phylo_data}}; see there for further details.
#'
#' @param x Character vector or convertible to such.
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
safe_labels <- function(x, format, enclose = TRUE, pad = FALSE) {
  do_pad <- function(x, pad) {
    if (pad)
      sprintf(sprintf("%%-%is", max(nchar(x))), x)
    else
      x
  }
  nexus_quote <- function(x) sprintf("'%s'", gsub("'", "''", x, fixed = TRUE))
  clean_html <- function(x) {
    x <- gsub("&(?!([A-Za-z]+|#\\d+);)", "&amp;", x, perl = TRUE)
    x <- gsub(">", "&gt;", gsub("<", "&lt;", x, fixed = TRUE), fixed = TRUE)
    gsub("\"", "&quot;", x, fixed = TRUE)
  }
  clean_from <- function(x, pat) {
    pat <- sprintf("[%s]+", pat)
    x <- sub(sprintf("^%s", pat), "", x, perl = TRUE)
    x <- sub(sprintf("%s$", pat), "", x, perl = TRUE)
    gsub(pat, "_", x, perl = TRUE)
  }
  LL(enclose, pad)
  not.newick <- "\\s,:;()" # spaces and Newick-format special characters
  not.nexus <- "\\s()\\[\\]{}/\\,;:=*'\"`+<>-" # see PAUP* manual
  # see http://tnt.insectmuseum.org/index.php/Basic_format (16/04/2012)
  not.hennig <- "\\s;/+-"
  case(match.arg(format, PHYLO_FORMATS),
    html = clean_html(x),
    phylip = sprintf("%-10s", substr(clean_from(x, not.newick), 1L, 10L)),
    hennig = do_pad(clean_from(x, not.hennig), pad),
    epf = do_pad(clean_from(x, not.newick), pad),
    nexus = do_pad(if (enclose)
      nexus_quote(x)
    else
      clean_from(x, not.nexus), pad)
  )
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
    x <- vapply(x, list2html, character(1L), level = level + 1L, fmt = fmt)
    x <- paste(x, indent, sep = "")
    x <- hmakeTag("div", x, class = n, title = n, newline = TRUE)
    paste(indent, x, sep = "", collapse = "")
  } else {
    if (is.character(x) && !inherits(x, "AsIs"))
      x <- safe_labels(x, "html")
    if (!is.null(n <- names(x))) {
      n <- ifelse(nzchar(n), safe_labels(n, "html"), NA_character_)
      x <- hmakeTag("span", x, class = n, title = n)
    }
    paste(indent, paste(x, collapse = " "), "\n", sep = "")
  }
}


################################################################################


#' Format phylogenetic data
#'
#' This is the helper function used by \code{\link{phylo_data}} for converting
#' the data to a character vector.
#'
#' @param x Object of class \sQuote{CMAT}.
#' @param how Character scalar. See \code{\link{phylo_data}} for the possible
#'   values (argument \sQuote{format}).
#' @param enclose Logical scalar. See \code{\link{phylo_data}}.
#' @param digits Integer scalar. See \code{\link{phylo_data}}.
#' @param indent Integer scalar. See \code{\link{phylo_data}}.
#' @param paup.block Logical scalar. See \code{\link{phylo_data}}.
#' @param comments Character vector. See \code{\link{phylo_data}}.
#' @param html.args List. See \code{\link{phylo_data}}.
#' @param ... Optional arguments. See \code{\link{phylo_data}}.
#' @return Character vector or \code{NULL}.
#' @keywords internal
#'
setGeneric("format")

setMethod("format", CMAT, function(x, how, enclose, digits, indent,
    paup.block, comments, html.args, ...) {

  # HTML helper methods.
  #
  unhtml <- function(x) safe_labels(x, format = "html")
  span_class <- function(x, klass, title = klass) {
    hmakeTag("span", unhtml(x), class = klass, title = title)
  }
  div_class <- function(x, klass, title = klass) {
    hmakeTag("div", x, class = klass, title = title)
  }
  single_tag <- function(x, ...) {
    listing(list(...), c("<", x), ">", style = " %s=\"%s\"", collapse = "")
  }
  html_comment <- function(x) {
    sprintf("<!-- %s -->", gsub("--", "==", x, fixed = TRUE))
  }

  # Header for all formats except HTML.
  #
  phylo_header <- function(how, dims, datatype, comments, charlabels,
      enclose, indent) {
    hennig86_header <- function(dims, datatype, comments) {
      case(datatype, standard = {
          nstates <- "32"
          datatype <- "numeric"
        }, continuous = nstates <- "cont")
      nstates <- sprintf("nstates %s;", nstates)
      datatype <- sprintf("&[%s]", datatype)
      comments <- chartr("'", '"', comments)
      if (dims[1L] < 4L)
        warning("TNT will not accept less than 4 organisms")
      dims <- paste(rev(dims), collapse = " ")
      c(nstates, "xread", "'", comments, "'", dims, datatype)
    }
    nexus_header <- function(dims, datatype, comments, labels, enclose,
        indent) {
      case(datatype,
        standard = {
          symbols <- paste(CHARACTER_STATES, collapse = "")
          symbols <- sprintf('%sformat symbols = "%s";', indent, symbols)
        },
        continuous = {
          symbols <- NULL
          warning("continuous data are not supported by PAUP*")
        }
      )
      if (length(labels)) {
        labels <- safe_labels(labels, format = "nexus", enclose = enclose)
        labels <- paste(labels, collapse = " ")
        labels <- sprintf("%scharlabels %s;", indent, labels)
      } else
        warning("character labels not found")
      comments <- chartr("[]", "{}", comments)
      dims <- sprintf("%sdimensions ntax = %i nchar = %i;", indent, dims[1L],
        dims[2L])
      datatype <- sprintf("%sformat datatype = %s missing = ?;", indent,
        datatype)
      c("#NEXUS", "", "[", comments, "]", "",
        "begin data;", dims, datatype, symbols, labels,
        sprintf("%smatrix", indent))
    }
    case(how,
      phylip =, epf = paste(dims, collapse = " "),
      hennig = hennig86_header(dims, datatype, comments),
      nexus = nexus_header(dims, datatype, comments, charlabels, enclose,
        indent)
    )
  }

  # Convert the matrix to character storage mode (without footer and header).
  # This function must leave row and column names untouched. It adds a
  # 'variability' attribute if necessary.
  #
  to_strings <- function(x, how, number.format, html.args, digits) {

    # functions needed for floating-point data
    format_float <- function(x) sprintf(number.format, x)
    formatted.0 <- format_float(0)
    join_floats_for_hennig86 <- function(x) {
      x <- x[!is.na(x)]
      case(length(x), MISSING_CHAR, format_float(x), {
        sd.x <- format_float(sd(x))
        if (sd.x == formatted.0) # i.e., standard deviation practically zero
          format_float(x[1L])
        else
          sprintf(" %s-%s", format_float(mean(x)), sd.x)
      })
    }
    floats2html <- function(x) {
      x <- x[!is.na(x)]
      case(length(x), MISSING_CHAR, span_class(format_float(x), "single-real"),
        sprintf("%s &plusmn; %s",
          span_class(format_float(mean(x)), "mean-real"),
          span_class(format_float(sd(x)), "sd-real")))
    }

    # Functions needed for integer data.
    int2states <- function(states, mapping) {
      if (isTRUE(any(states < 1L)))
        stop("only positive integers allowed here")
      states <- mapping[states]
      states[is.na(states)] <- MISSING_CHAR
      states
    }
    uniform <- function(x) {
      if (length(x <- unique.default(x)) > 1L)
        x <- x[!is.na(x)]
      x
    }
    select_html_mapping <- function(mapping) {
      if (length(mapping)) {
        if (!is.character(mapping))
          stop("'mapping' non-empty but not of mode 'character'")
        unhtml(mapping)
      } else
        CHARACTER_STATES
    }
    convert_html_states <- function(x, mapping) {
      span_class(int2states(x, mapping), sprintf("state-%i", x))
    }

    variability <- NULL

    case(typeof(x),
      list = {
        case(typeof(x[[1L]]),
          integer = {
            from.integer <- TRUE
            x[] <- lapply(x, uniform)
            if (how == "html") {
              mapping <- select_html_mapping(html.args$character.states)
              variability <- ifelse(is_constant(x, digits = digits,
                  strict = FALSE),
                ifelse(is_constant(x, digits = digits, strict = TRUE),
                  "constant", "uninformative"), "informative")
              x[] <- lapply(x, convert_html_states, mapping)
              x[] <- lapply(x, paste, collapse = L(html.args$multiple.sep))
            } else {
              x[] <- lapply(x, int2states, CHARACTER_STATES)
              convert <- case(how,
                epf =,
                phylip = function(x) if (length(x) > 1L)
                  MISSING_CHAR
                else
                  x,
                nexus = function(x) if (length(x) > 1L)
                  sprintf("(%s)", paste(x, collapse = ""))
                else
                  x,
                hennig = function(x) if (length(x) > 1L)
                  sprintf("[%s]", paste(x, collapse = ""))
                else
                  x
              )
              x[] <- lapply(x, convert)
            }
          },
          double = {
            case(how,
              hennig = {
                x[] <- ranging(c(x), fac = 65)
                x[] <- lapply(x, join_floats_for_hennig86)
              },
              html = {
                variability <- ifelse(is_constant(x, digits = digits,
                  strict = FALSE), ifelse(is_constant(x, digits = digits,
                  strict = TRUE), "constant", "uninformative"), "informative")
                x[] <- lapply(x, floats2html)
              }
            )
            from.integer <- FALSE
          }
        )
        storage.mode(x) <- "character"
      },
      integer = {
        if (how == "html") {
          mapping <- select_html_mapping(html.args$character.states)
          variability <- is_constant(x, strict = TRUE, digits = digits)
          variability <- ifelse(variability, "constant", "informative")
          x[] <- convert_html_states(x, mapping)
        } else
          x[] <- int2states(x, CHARACTER_STATES)
        from.integer <- TRUE
      },
      double = {
        from.integer <- FALSE
        switch(how,
          hennig = x <- ranging(x, fac = 65),
          html = {
            variability <- is_constant(x, strict = TRUE, digits = digits)
            variability <- ifelse(variability, "constant", "informative")
          }
        )
        x[] <- ifelse(is.na(x), MISSING_CHAR, format_float(x))
        if (how != "html")
          x[] <- sprintf(" %s", x)
      }
    )

    x <- as(x, "matrix")
    attr(x, "variability") <- variability
    attr(x, "from.integer") <- from.integer
    x
  }

  # Useful default settings for PAUP*.
  #
  paup_cmds <- function() c(
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

  # Footer for all formats except HTML.
  #
  phylo_footer <- function(how, indent, paup.block) {
    nexus_footer <- function(indent, paup.block) {
      if (paup.block) {
        block <- paste(indent, paup_cmds(), sep = "")
        block <- c("begin paup;", block, "end;", "")
      } else
        block <- NULL
      typeset <- sprintf("%stypeset * default = ord : all;", indent)
      typeset <- c("begin assumptions;", typeset, "end;", "")
      c(sprintf("%s;", indent), "end;", "", typeset, block)
    }
    case(how,
      epf =, phylip = NULL,
      hennig = c(";", "ccode - .;", "procedure /;"),
      nexus = nexus_footer(indent, paup.block)
    )
  }

  # HTML construction after generation of a matrix in character mode (which
  # also contains HTML tags)
  #
  make_html <- function(x, title, html.args, ...) {

    convert_greek <- function(x) {
      x <- strsplit(x, sep <- "-", fixed = TRUE)
      x <- map_values(x, GREEK_LETTERS[, "html"])
      vapply(x, paste, character(1L), collapse = sep)
    }

    html_head <- function(title, css, meta) {
      if (length(title)) { # Tidy accepts only a single title entry
        from.opm <- attr(title, opm_string())
        title <- hmakeTag("title", data = unhtml(title[1L]))
        if (!from.opm)
          title <- c(html_comment("user-defined title"), title)
      } else
        title <- NULL
      if (length(css <- css[nzchar(css)])) {
        is.abs.path <- grepl("^(/|[a-zA-Z]:)", css, perl = TRUE)
        css[is.abs.path] <- sprintf("file://%s", css[is.abs.path])
        css <- vapply(css, function(y) {
          single_tag("link", rel = "stylesheet", type = "text/css", href = y)
        }, character(1L))
        css <- c(html_comment("user-defined CSS file(s)"), css)
      } else
        css <- NULL
      generator <- single_tag("meta", name = "generator",
        content = paste(opm_string(version = TRUE), collapse = " version "))
      # see http://www.w3.org/TR/NOTE-datetime
      # but %s appears to by affected by a bug in R 2.15.2
      time <- format(Sys.time(), "%Y-%M-%dT%H:%M:%S%z")
      time <- single_tag("meta", name = "date", content = time)
      if (length(meta)) {
        meta <- vapply(meta, function(y) {
          if (is.null(names(y)))
            stop("HTML meta entry without names")
          do.call(single_tag, c(list(x = "meta"), as.list(y)))
        }, character(1L))
        meta <- c(html_comment("user-defined metadata"), unname(meta))
      } else
        meta <- NULL
      c("<head>", title, generator, time, meta, css, "</head>")
    }

    headline <- function(headline, title) {
      headline <- unlist(headline)
      if (length(headline <- headline[nzchar(headline)]))
        from.user <- TRUE
      else {
        headline <- title
        from.user <- !attr(headline, opm_string())
      }
      if (length(headline)) {
        headline <- div_class(unhtml(headline), "headline")
        if (from.user)
          headline <- c(html_comment("user-defined headline(s)"), headline)
      }
      headline
    }

    user_sections <- function(x) {
      if (!length(x))
        return(NULL)
      names(x) <- sprintf("user-%sed-section", names(x))
      c(html_comment(sprintf("%ss", names(x)[1L])), list2html(x))
    }

    table_legend <- function(organisms, html.args, from.integer) {

      style <- sprintf("%%s%s%%s", unhtml(L(html.args$legend.sep.1)))
      collapse <- unhtml(L(html.args$legend.sep.2))

      legend <- span_class(organisms, "organism-name")
      names(legend) <- span_class(seq_along(organisms), "organism-index")
      legend <- listing(legend, style = style, collapse = collapse)

      if (length(html.args$organisms.start))
        legend <- paste(span_class(html.args$organisms.start,
          "organism-list-head"), legend, collapse = "")

      if (from.integer)
        if (length(m <- html.args$character.states)) {
          m <- span_class(m, sprintf("state-%i", seq_along(m)))
          m <- structure(span_class(names(html.args$character.states),
            "character-state-name"), .Names = m)
          m <- listing(m, style = style, collapse = collapse)
          start <- html.args$states.start
          if (length(start <- start[nzchar(start)]))
            m <- paste(span_class(start, "character-state-list-head"), m,
              collapse = "")
          legend <- c(legend, m)
        } else
          warning("character states not indicated")

      if (L(html.args$legend.dot))
        legend <- paste(legend, ".", sep = "")

      div_class(legend, "table-legend")
    }

    html_table <- function(x, html.args, ...) {
      if (is.null(colnames(x)))
        stop("missing character labels (column names)")
      if (length(variability <- attr(x, "variability")) != ncol(x))
        stop(BUG_MSG)
      colnames(x) <- unhtml(colnames(x))
      if (L(html.args$greek.letters))
        colnames(x) <- convert_greek(colnames(x))
      colnames(x) <- div_class(colnames(x), variability)
      colnames(x) <- div_class(colnames(x), "character-name")
      x[] <- t(apply(x, 1L, div_class, variability))
      x[] <- div_class(x, "measured-character-states")
      rownames(x) <- span_class(seq.int(nrow(x)), "organism-index")
      c("<div class=\"main-table\">", hwrite(x = t(x), page = NULL,
        table.summary = html.args$table.summary, div = FALSE, ...), "</div>")
    }

    c(
      paste('<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"',
        '"http://www.w3.org/TR/html4/strict.dtd">', collapse = " "),
      "<html>",
      html_head(title, html.args$css.file,
        html.args[names(html.args) == "meta"]),
      "<body>",
      headline(html.args[names(html.args) == "headline"], title),
      user_sections(html.args[names(html.args) == "prepend"]),
      table_legend(rownames(x), html.args, attr(x, "from.integer")),
      user_sections(html.args[names(html.args) == "insert"]),
      html_table(x, html.args, ...),
      user_sections(html.args[names(html.args) == "append"]),
      "</body>",
      "</html>"
    )
  }

  LL(enclose, digits, indent, paup.block)
  how <- match.arg(how, PHYLO_FORMATS)
  indent <- paste(rep.int(" ", indent), collapse = "")

  x <- to_strings(x, how, sprintf("%%%i.%if", digits + 3L, digits), html.args)

  if (!length(comments <- comments[nzchar(comments)])) {
    comments <- sprintf("Characters exported by %s",
      paste(opm_string(version = TRUE), collapse = " version "))
    attr(comments, opm_string()) <- TRUE
  } else
    attr(comments, opm_string()) <- FALSE

  if (how == "html")
    return(make_html(x = x, title = comments, html.args = html.args, ...))

  if (is.null(labels <- rownames(x)))
    stop("missing organism labels (row names)")
  labels <- safe_labels(labels, format = how, enclose = enclose, pad = TRUE)
  if (dups <- anyDuplicated(labels))
    stop(sprintf("duplicated organism label (row name) '%s'", labels[dups]))
  datatype <- if (grepl("^\\s+", x[1L], perl = TRUE))
    "continuous"
  else
    "standard"
  c(
    phylo_header(how, dim(x), datatype, comments, colnames(x),
      enclose, indent),
    paste(labels, apply(x, 1L, paste, collapse = ""), sep = "\t"),
    phylo_footer(how, indent, paup.block)
  )

}, sealed = SEALED)


################################################################################


#' Default HTML formatting arguments
#'
#' A helper function for \code{\link{phylo_data}} allowing for the easy
#' fine-tuning of the generated \acronym{HTML} output.
#'
#' @param character.states Character vector used for mapping integers to the
#'   elements in the corresponding position. It is also used in conjunction
#'   with its names to create the table legend. The default value is useful
#'   for data of mode \sQuote{logical}, mapping \code{FALSE}, \code{NA} and
#'   \code{TRUE}, in this order. Data of this kind are by default internally
#'   converted to an according integer vector.
#' @param multiple.sep Character scalar used for joining multi-state characters
#'   together.
#' @param organisms.start Character scalar prepended to the organism part of the
#'   table legend. Ignored if empty.
#' @param states.start Character scalar prepended to the character-states part
#'   of the table legend. Ignored if empty.
#' @param legend.dot Logical scalar indicating whether or not a dot shall be
#'   appended to the table-legend entries.
#' @param legend.sep.1 Character scalar used for the first pass of joining
#'   the table-legend entries together.
#' @param legend.sep.2 Character scalar used for the second pass of joining
#'   the table-legend entries together.
#' @param table.summary Character scalar inserted as \sQuote{summary} attribute
#'   of the resulting \code{HTML} table.
#' @param greek.letters Logical scalar indicating whether or not letters between
#'   \sQuote{a} and \sQuote{a} within substrate names should be converted to the
#'   corresponding Greek letters.
#' @param css.file Character vector indicating the name of one to several
#'   \acronym{CSS} files to link. Emptry strings and empty vectors are ignored.
#'   It is no error if the file does not exist, but the page will then probably
#'   not be displayed as intended. Under Windows it is recommended to convert a
#'   filename \code{f} beforehand using \code{normalizePath(f, winslash = "/")}.
#' @param ... Optional other arguments available for inserting user-defined
#'   \acronym{HTML} content. Currently the following ones (in their order of
#'   insertion) are not ignored, and can even be provided several times:
#'   \describe{
#'     \item{meta}{Used as (additional) \sQuote{meta} entries within the
#'       \acronym{HTML} head.}
#'     \item{headline}{Override the use of the \code{title} argument as headline
#'       (placed above the table legend). An empty argument would turn it off.}
#'     \item{prepend}{List or character vector to be inserted before the table
#'       legend. Lists are converted recursively. List names will be converted
#'       to \sQuote{title} and \sQuote{class} attributes (if missing, names are
#'       inferred from the nesting level; see \code{\link{opm_opt}}, entry
#'       \sQuote{html.class}). Names of other vectors, if any, are converted to
#'       \sQuote{title} and \sQuote{span} attributes. Character vectors are
#'       converted using \code{\link{safe_labels}} unless they inherit from
#'       \sQuote{AsIs} (see \code{I} from the \pkg{base} package).}
#'     \item{insert}{As above, but inserted between the legend and the table.}
#'     \item{append}{As above, but inserted after the table.}
#'   }
#' @return List.
#' @keywords character cluster IO
#' @seealso base::normalizePath
#' @export
#' @family phylogeny-functions
#' @examples
#' # see phylo_data()
#'
html_args <- function(
    character.states = c(`negative reaction` = "-",
    `weak reaction` = "w", `positive reaction` = "+"),
    multiple.sep = "/", organisms.start = "Organisms: ",
    states.start = "Symbols: ", legend.dot = TRUE,
    legend.sep.1 = ", ", legend.sep.2 = "; ",
    table.summary = "character matrix", greek.letters = TRUE,
    css.file = opm_opt("css.file"), ...) {
  args <- as.list(match.call())[-1L]
  defaults <- formals()[setdiff(names(formals()), c(names(args), "..."))]
  lapply(c(defaults, args), eval)
}


################################################################################


#' Export phylogenetic data
#'
#' Create entire character matrix (include header and footer) in a file format
#' suitable for exporting phylogenetic data. Return it or write it to a file.
#' This function can also produce \acronym{HTML} tables suitable for displaying
#' PM data in taxonomic journals such as \acronym{IJSEM}.
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
#'   lines are directly written to this file. Otherwise, they are returned.
#' @param enclose Logical scalar. Shall labels be enclosed in single quotes?
#'   Ignored unless \code{format} is \sQuote{nexus}.
#' @param indent Integer scalar. Indentation of subcommands in NEXUS format.
#'   Ignored unless \code{format} is \sQuote{nexus} (and a matter of taste
#'   anyway).
#' @param paup.block Logical scalar. Append a \acronym{PAUP*} block with
#'   selected (recommended) default values? Has no effect unless \sQuote{nexus}
#'   is selected as \sQuote{format}.
#' @param delete Character scalar. If \sQuote{uninf}, columns are removed which
#'   are either constant (in the strict sense) or are columns in which some
#'   fields contain polymorphisms, and no pairs of fields share no character
#'   states. If \sQuote{ambig}, columns with ambiguities (multiple states in at
#'   least one single field) are removed. If \sQuote{constant}, columns which
#'   are constant in the strict sense are removed. \code{delete} is currently
#'   ignored for formats other than \sQuote{html}, and note that columns become
#'   rows in the final \acronym{HTML} output.
#'
#' @param join Logical scalar, vector or factor. Unless \code{FALSE}, rows of
#'   \code{object} are joined together, either according to the row names (if
#'   \code{join} is \code{TRUE}), or directly according to \code{join}. This can
#'   be used to deal with measurements repetitions for the same organism or
#'   treatment.
#' @param cutoff Numeric scalar. If joining results in multi-state characters,
#'   they can be filtered by removing all entries with a relative frequency less
#'   than \sQuote{cutoff}. Makes not much sense for non-integer numeric data.
#' @param digits Numeric scalar. Used for rounding, and thus ignored unless
#'   \code{object} is of mode \sQuote{numeric}.
#' @param comments Character vector. Comments to be added to the output (as
#'   title if \acronym{HTML} is chosen). Ignored if the output format does not
#'   allow for comments. If empty, a default comment is chosen.
#' @param html.args List of arguments used to modify the generated
#'   \acronym{HTML}. See \code{\link{html_args}} for the supported list
#'   elements and their meaning.
#' @param prefer.char Logical scalar indicating whether or not to use \code{NA}
#'   as intermediary character state. Has only an effect for \sQuote{logical}
#'   and \sQuote{integer} characters. A warning is issued if integers are not
#'   within the necessary range, i.e. either \code{0} or \code{1}.
#' @param run.tidy Logical scalar. Filter the resulting \acronym{HTML} through
#'   the Tidy program? Ignored unless \code{format} is \sQuote{html}. Otherwise,
#'   if \code{TRUE}, it is an error if the Tidy executable is not found.
#'
#' @param as.labels Vector of data-frame indices or \sQuote{OPMS} metadata
#'   entries. See \code{\link{extract}}.
#' @param what Character scalar. See \code{\link{extract}}.
#' @param sep Character scalar. See \code{\link{extract}}.
#'
#' @param subset Character scalar passed to the \sQuote{OPMS} method of
#'   \code{\link{extract}}.
#' @param extract.args Optional list of arguments passed to that method.
#' @param discrete.args Optional list of arguments passed from the \sQuote{OPMS}
#'   method to \code{\link{discrete}}. If set to \code{NULL}, discretization is
#'   turned off. Ignored if precomputed discretized values are chosen by setting
#'   \code{subset} to \sQuote{disc}.
#'
#' @param ... Optional arguments passed between the methods (i.e., from the
#'   other methods to the matrix method) or to \code{hwrite} from the
#'   \pkg{hwriter} package. Note that \sQuote{table.summary} is set via
#'   \code{html.args} and that \sQuote{page}, \sQuote{x} and \sQuote{div}
#'   cannot be used.
#'
#' @export
#' @return Character vector, each element representing a line in a potential
#'   output file, returned invisibly if \code{outfile} is given.
#' @family phylogeny-functions
#' @seealso base::comment base::write hwriter::hwrite
#' @details \itemize{
#'   \item Exporting PM data in such formats allows one to either infer trees
#'     from the data under the maximum-likelihood and/or the maximum-parsimony
#'     criterion, or to reconstruct the evolution of PM characters on given
#'     phylogenetic trees, or to nicely display the data in \acronym{HTML}
#'     format.
#'   \item For exporting NEXUS format, the matrix should normally be converted
#'     beforehand by applying \code{\link{discrete}}. Exporting \acronym{HTML}
#'     is optimized for data discretized with \code{gap} set to \code{TRUE}. For
#'     other data, the \code{character.states} argument should be modified, see
#'     \code{\link{html_args}}. The \sQuote{hennig} (Hennig86) format is
#'     the one used by \acronym{TNT}; it allows continuous characters to be
#'     analysed as such.
#'   \item \sQuote{epf} or \sQuote{extended PHYLIP} is sometimes called
#'     \sQuote{relaxed PHYLIP}.
#'   \item The generated \acronym{HTML} is guaranteed to produce neither errors
#'     nor warnings if checked using the Tidy program. It deliberately contains
#'     no formatting instructions but a rich annotation with \sQuote{class}
#'     attributes which allows for \acronym{CSS}-based formatting. This
#'     annotation includes the naming of all sections and all kinds of textual
#'     content. Whether the characters show differences between at least one
#'     organism and the others is also indicated. For the \acronym{CSS} files
#'     that come with the package, see the examples below and
#'     \code{\link{opm_files}}.
#' }
#' @note Regarding the meaning of \sQuote{character} as used here, see the
#'   \sQuote{details} section of \code{\link{discrete}}.
#'
#' @keywords character cluster IO
#'
#' @references Berger, S. A., Stamatakis, A. 2010 Accuracy of morphology-based
#'   phylogenetic fossil placement under maximum likelihood. \emph{8th ACS/IEEE
#'   International Conference on Computer Systems and Applications (AICCSA-10).}
#'   Hammamet, Tunisia [analysis of phenotypic data with RAxML].
#' @references Felsenstein, J. 2005 PHYLIP (Phylogeny Inference Package) version
#'   3.6. Distributed by the author. Seattle: University of Washington,
#'   Department of Genome Sciences [the PHYLIP program].
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
#' @references Swofford, D. L. 2002 \emph{PAUP*: Phylogenetic Analysis Using
#'   Parsimony (*and Other Methods), Version 4.0 b10}. Sunderland, Mass.:
#'   Sinauer Associates [the PAUP* program].
#' @references \url{http://ijs.sgmjournals.org/} [IJSEM journal]
#' @references \url{http://tidy.sourceforge.net/} [HTML Tidy]
#'
#' @examples
#'
#' # simple helper functions
#' echo <- function(x) write(x, file = "")
#' is_html <- function(x) is.character(x) &&
#'   c("<html>", "<head>", "<body>", "</html>", "</head>", "</body>") %in% x
#' longer <- function(x, y) any(nchar(x) > nchar(y)) &&
#'   !any(nchar(x) < nchar(y))
#'
#' # dummy data set
#' x <- matrix(c(0:9, letters[1:22]), nrow = 2)
#' colnames(x) <- LETTERS[1:16]
#' rownames(x) <- c("Ahoernchen", "Behoernchen") # Chip and Dale in German
#'
#' # EPF is a comparatively restricted format
#' echo(y.epf <- phylo_data(x, format = "epf"))
#' stopifnot(is.character(y.epf), length(y.epf) == 3)
#' stopifnot(identical(y.epf, phylo_data(as.data.frame(x), what = "factor",
#'   format = "epf")))
#'
#' # PHYLIP is even more restricted (shorter labels!)
#' echo(y.phylip <- phylo_data(x, format = "phylip"))
#' stopifnot((y.epf == y.phylip) == c(TRUE, FALSE, FALSE))
#'
#' # NEXUS allows for more content; note the comment and the character labels
#' echo(y.nexus <- phylo_data(x, format = "nexus"))
#' nexus.len.1 <- length(y.nexus)
#' stopifnot(is.character(y.nexus), nexus.len.1 > 10)
#'
#' # adding a PAUP* block with (hopefully useful) default settings
#' echo(y.nexus <- phylo_data(x, format = "nexus", paup.block = TRUE))
#' stopifnot(is.character(y.nexus), length(y.nexus) > nexus.len.1)
#'
#' # adding our own comment
#' comment(x) <- c("This is", "a test") # yields two lines
#' echo(y.nexus <- phylo_data(x, format = "nexus"))
#' stopifnot(identical(length(y.nexus), nexus.len.1 + 1L))
#'
#' # Hennig86/TNT also includes the comment
#' echo(y.hennig <- phylo_data(x, format = "hennig"))
#' hennig.len.1 <- length(y.hennig)
#' stopifnot(is.character(y.hennig), hennig.len.1 > 10)
#'
#' # without an explicit comment, the default one will be used
#' comment(x) <- NULL
#' echo(y.hennig <- phylo_data(x, format = "hennig"))
#' stopifnot(identical(length(y.hennig), hennig.len.1 - 1L))
#'
#' # examples with real data and HTML
#' data(vaas_4)
#'
#' # setting the CSS file that comes with opm as default
#' opm_opt(css.file = grep("[.]css$", opm_files("auxiliary"), value = TRUE))
#'
#' # see discrete() for the conversion and note the OPMS example below: one
#' # could also get the results directly from OPMS objects
#' x <- extract(vaas_4, as.labels = list("Species", "Strain"),
#'   in.parens = FALSE)
#' x <- discrete(x, range = TRUE, gap = TRUE)
#' echo(y <- phylo_data(x, format = "html",
#'   html.args = html_args(organisms.start = "Strains: ")))
#' # this yields HTML with the usual tags, a table legend, and the table itself
#' # in a single line; the default 'organisms.start' could also be used
#' stopifnot(is_html(y))
#'
#' # now with joining of the results per species (and changing the organism
#' # description accordingly)
#' x <- extract(vaas_4, as.labels = list("Species"), in.parens = FALSE)
#' x <- discrete(x, range = TRUE, gap = TRUE)
#' echo(y <- phylo_data(x, format = "html", join = TRUE,
#'   html.args = html_args(organisms.start = "Species: ")))
#' stopifnot(is_html(y))
#' # Here and in the following examples note the highlighting of the variable
#' # (uninformative or informative) characters. The uninformative ones are those
#' # that are not constant but show overlap regarding the sets of character
#' # states between all organisms. The informative ones are those that are fully
#' # distinct between all organisms.
#'
#' # 'OPMS' method, yielding the same results than above but directly
#' echo(yy <- phylo_data(vaas_4, as.labels = "Species", format = "html",
#'   join = TRUE, extract.args = list(in.parens = FALSE),
#'   html.args = html_args(organisms.start = "Species: ")))
#' # the timestamps might differ, but otherwise the result is as above
#' stopifnot(length(y) == length(yy) && length(which(y != yy)) < 2)
#'
#' # appending user-defined sections
#' echo(yy <- phylo_data(vaas_4, as.labels = "Species", format = "html",
#'   join = TRUE, extract.args = list(in.parens = FALSE),
#'   html.args = html_args(organisms.start = "Species: ",
#'   append = list(section.1 = "additional text", section.2 = "more text"))))
#' stopifnot(length(y) < length(yy), length(which(!y %in% yy)) < 2)
#' # note the position -- there are also 'prepend' and 'insert' arguments
#'
#' # effect of deletion
#' echo(y <- phylo_data(x, "html", delete = "none", join = FALSE))
#' echo(y.noambig <- phylo_data(x, "html", delete = "ambig", join = FALSE))
#' stopifnot(length(which(y != y.noambig)) < 2) # timestamps might differ
#' # ambiguities are created only by joining
#' echo(y <- phylo_data(x, "html", delete = "none", join = TRUE))
#' echo(y.noambig <- phylo_data(x, "html", delete = "ambig", join = TRUE))
#' stopifnot(longer(y, y.noambig))
#' echo(y.nouninf <- phylo_data(x, "html", delete = "uninf", join = TRUE))
#' stopifnot(longer(y, y.nouninf))
#' echo(y.noconst <- phylo_data(x, "html", delete = "const", join = TRUE))
#' stopifnot(longer(y.noconst, y.nouninf))
#'
#' # getting real numbers, not discretized ones
#' echo(yy <- phylo_data(vaas_4, as.labels = "Species", format = "html",
#'   join = TRUE, extract.args = list(in.parens = FALSE), subset = "A",
#'   discrete.args = NULL,
#'   html.args = html_args(organisms.start = "Species: ")))
#' stopifnot(is_html(yy), length(yy) == length(y) - 1) # no symbols list
#' # the highlighting is also used here, based on the following heuristic:
#' # if mean+/-2*sd does not overlap, the character is informative; else
#' # if mean+/-sd does not overlap, the character is uninformative; otherwise
#' # is is constant
#'
#' # this can also be used for formats other than HTML (but not all make sense)
#' echo(yy <- phylo_data(vaas_4, as.labels = "Species", format = "hennig",
#'   join = TRUE, extract.args = list(in.parens = FALSE), subset = "A",
#'   discrete.args = NULL))
#' stopifnot(is.character(yy), length(yy) > 10)
#'
setGeneric("phylo_data", function(object, ...) standardGeneric("phylo_data"))

setMethod("phylo_data", "matrix", function(object,
    format = opm_opt("phylo.fmt"), outfile = "", enclose = TRUE, indent = 3L,
    paup.block = FALSE, delete = c("none", "uninf", "constant", "ambig"),
    join = FALSE, cutoff = 0, digits = opm_opt("digits"),
    comments = comment(object), html.args = html_args(),
    prefer.char = format == "html", run.tidy = FALSE, ...) {
  format <- match.arg(format, PHYLO_FORMATS)
  delete <- match.arg(delete)
  comments <- comments
  object <- new(CMAT, object)
  if (L(prefer.char))
    object <- update(object, how = "NA2int")
  object <- merge(x = object, y = join)
  if (is.list(object) && L(cutoff) > 0)
    object[] <- lapply(object, reduce_to_mode, cutoff = cutoff, use.na = FALSE)
  switch(delete, none = NULL,
    object <- update(object, how = sprintf("delete.%s", delete)))
  result <- format(x = object, how = format, enclose = enclose, digits = digits,
    indent = indent, paup.block = paup.block, comments = comments,
    html.args = html.args, ...)
  if (L(run.tidy) && format == "html")
    result <- tidy(result, check = FALSE)
  if (nzchar(L(outfile))) {
    write(result, outfile)
    return(invisible(result))
  }
  result
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
    dups = if (is.logical(join) && L(join))
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




