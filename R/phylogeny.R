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

safe_labels <- function(x, format, enclose = TRUE, pad = FALSE,
    comment = FALSE) {
  do_pad <- function(x, pad) {
    if (pad)
      sprintf(sprintf("%%-%is", max(nchar(x))), x)
    else
      x
  }
  nexus_quote <- function(x) {
    sprintf("'%s'", gsub("'", "''", x, FALSE, FALSE, TRUE))
  }
  clean_html <- function(x) {
    x <- gsub("&(?!([A-Za-z]+|#\\d+);)", "&amp;", x, FALSE, TRUE)
    x <- gsub("<", "&lt;", x, FALSE, FALSE, TRUE)
    x <- gsub(">", "&gt;", x, FALSE, FALSE, TRUE)
    gsub("\"", "&quot;", x, FALSE, FALSE, TRUE)
  }
  clean_from <- function(x, pat) {
    pat <- sprintf("[%s]+", pat)
    x <- sub(sprintf("^%s", pat), "", x, FALSE, TRUE)
    x <- sub(sprintf("%s$", pat), "", x, FALSE, TRUE)
    gsub(pat, "_", x, FALSE, TRUE)
  }
  surround <- function(x, start, end, enclose) {
    if (enclose)
      c(start, x, end)
    else
      sprintf("%s%s%s", start, x, end)
  }
  LL(enclose, pad, comment)
  format <- match.arg(format, PHYLO_FORMATS)
  if (comment) {
    case(format,
      html = surround( # the replacement used is the one favoured by HTML Tidy
        gsub("--", "==", x, FALSE, FALSE, TRUE), "<!-- ", " -->", enclose),
      hennig = surround(chartr("'", '"', x), "'", "'", enclose),
      nexus = surround(chartr("[]", "{}", x), "[", "]", enclose),
      epf =,
      phylip = stop("comments are not defined for format ", format)
    )
  } else {
    not.newick <- "\\s,:;()" # spaces and Newick-format special characters
    not.nexus <- "\\s()\\[\\]{}/\\,;:=*'\"`+<>-" # see PAUP* manual
    # see http://tnt.insectmuseum.org/index.php/Basic_format (16/04/2012)
    not.hennig <- "\\s;/+-"
    case(format,
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
}

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
  html_comment <- function(x) {
    safe_labels(x, "html", comment = TRUE, enclose = FALSE)
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
      comments <- safe_labels(comments, "hennig", comment = TRUE)
      if (dims[1L] < 4L)
        warning("TNT will not accept less than 4 organisms")
      dims <- paste0(rev(dims), collapse = " ")
      c(nstates, "xread", comments, dims, datatype)
    }
    nexus_header <- function(dims, datatype, comments, labels, enclose,
        indent) {
      case(datatype,
        standard = {
          symbols <- paste0(CHARACTER_STATES, collapse = "")
          symbols <- sprintf('%sformat symbols = "%s";', indent, symbols)
        },
        continuous = {
          symbols <- NULL
          warning("continuous data are not supported by PAUP*")
        }
      )
      if (length(labels)) {
        labels <- safe_labels(labels, format = "nexus", enclose = enclose)
        labels <- paste0(labels, collapse = " ")
        labels <- sprintf("%scharlabels %s;", indent, labels)
      } else
        warning("character labels not found")
      dims <- sprintf("%sdimensions ntax = %i nchar = %i;", indent, dims[1L],
        dims[2L])
      datatype <- sprintf("%sformat datatype = %s missing = ?;", indent,
        datatype)
      c("#NEXUS", "", safe_labels(comments, "nexus", comment = TRUE), "",
        "begin data;", dims, datatype, symbols, labels,
        sprintf("%smatrix", indent))
    }
    case(how,
      phylip =, epf = paste0(dims, collapse = " "),
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
                  sprintf("(%s)", paste0(x, collapse = ""))
                else
                  x,
                hennig = function(x) if (length(x) > 1L)
                  sprintf("[%s]", paste0(x, collapse = ""))
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
        block <- paste0(indent, paup_cmds())
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
            "character-state-name"), names = m)
          m <- listing(m, style = style, collapse = collapse)
          start <- html.args$states.start
          if (length(start <- start[nzchar(start)]))
            m <- paste(span_class(start, "character-state-list-head"), m,
              collapse = "")
          legend <- c(legend, m)
        } else
          warning("character states not indicated")

      if (L(html.args$legend.dot))
        legend <- paste0(legend, ".")

      div_class(legend, "table-legend")
    }

    html_table <- function(x, html.args, ...) {
      if (is.null(colnames(x)))
        stop("missing character labels (column names)")
      if (length(variability <- attr(x, "variability")) != ncol(x))
        stop(BUG_MSG)
      colnames(x) <- unhtml(colnames(x))
      if (L(html.args$greek.letters))
        colnames(x) <- substrate_info(colnames(x), "html")
      colnames(x) <- div_class(colnames(x), variability)
      colnames(x) <- div_class(colnames(x), "character-name")
      x[] <- t(apply(x, 1L, div_class, variability))
      x[] <- div_class(x, "measured-character-states")
      rownames(x) <- span_class(seq_len(nrow(x)), "organism-index")
      c("<div class=\"main-table\">", hwrite(x = t(x), page = NULL,
        table.summary = html.args$table.summary, div = FALSE, ...), "</div>")
    }

    c(
      HTML_DOCTYPE,
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
  indent <- paste0(rep_len(" ", indent), collapse = "")

  x <- to_strings(x, how, sprintf("%%%i.%if", digits + 3L, digits), html.args)

  if (!length(comments <- comments[nzchar(comments)])) {
    comments <- sprintf("Characters exported by %s",
      paste0(opm_string(version = TRUE), collapse = " version "))
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
  datatype <- if (grepl("^\\s+", x[1L], FALSE, TRUE))
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
    object[] <- lapply(object, reduce_to_mode.default, cutoff, FALSE)
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
    subset = what, sep = " ", what = "numeric", ...) {
  if (!missing(what))
    warning("'what' is deprecated, use 'subset'")
  object <- extract_columns(object, as.labels = as.labels, what = subset,
    direct = FALSE, sep = sep)
  phylo_data(object, ...)
}, sealed = SEALED)

setMethod("phylo_data", OPMS, function(object, as.labels,
    subset = param_names("disc.name"), sep = " ", extract.args = list(),
    join = TRUE, discrete.args = list(range = TRUE, gap = TRUE), ...) {
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

setOldClass("OPMD_Listing")

setMethod("phylo_data", "OPMD_Listing", function(object,
    html.args = html_args(), run.tidy = FALSE) {
  if (!attr(object, "html"))
    return(paste(object, collapse = " "))
  head <- sprintf("Character listing exported by %s",
    paste0(opm_string(version = TRUE), collapse = " version "))
  attr(head, opm_string()) <- TRUE
  head <- html_head(head, html.args$css.file,
    html.args[names(html.args) == "meta"])
  x <- c(HTML_DOCTYPE, "<html>", head, "<body>", unname(object),
    "</body>", "</html>")
  if (L(run.tidy))
    x <- tidy(x, check = FALSE)
  x
}, sealed = SEALED)

setOldClass("OPMS_Listing")

setMethod("phylo_data", "OPMS_Listing", function(object,
    html.args = html_args(), run.tidy = FALSE) {
  prepare_headlines <- function(x) {
    x <- safe_labels(x, format = "html")
    x <- hmakeTag("span", data = x, class = "organism-name",
      title = "organism-name")
    hmakeTag("div", data = x, class = "headline", title = "headline")
  }
  if (!attr(object, "html"))
    return(apply(object, 1L, paste, collapse = " "))
  head <- sprintf("Character listings exported by %s",
    paste0(opm_string(version = TRUE), collapse = " version "))
  attr(head, opm_string()) <- TRUE
  head <- html_head(head, html.args$css.file,
    html.args[names(html.args) == "meta"])
  x <- apply(object, 1L, paste, collapse = "\n")
  x <- as.vector(rbind(prepare_headlines(names(x)), x))
  x <- c(HTML_DOCTYPE, "<html>", head, "<body>", x, "</body>", "</html>")
  if (L(run.tidy))
    x <- tidy(x, check = FALSE)
  x
}, sealed = SEALED)

