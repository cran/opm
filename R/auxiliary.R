get_and_remember <- function(x, prefix, default, getfun, single = FALSE, ...) {
  do_get <- function(x, envir, prefix, default, getfun, single, ...) {
    do_query <- function(x, single, getfun, ...) {
      if (single)
        return(lapply(X = x, FUN = getfun, ...))
      if (!is.list(result <- getfun(x, ...)))
        stop("'getfun' did not return a list")
      if (length(result) != length(x))
        stop("length discrepancy between 'getfun' result and query")
      if (is.null(names(result)))
        result
      else if (all(names(result) %in% x))
        result[x]
      else
        stop("naming discrepancy between 'getfun' result and query")
    }
    result <- vector("list", length(x))
    need <- !vapply(keys <- paste0(prefix, x), exists, NA, envir)
    result[!need] <- mget(keys[!need], envir)
    if (!any(need))
      return(result)
    result[need] <- do_query(x[need], single, getfun, ...)
    if (any(bad <- vapply(result[need], is.null, NA))) {
      warning(listing(x[need][bad], "could not find ", style = "sentence"))
      result[need][bad] <- rep.int(list(default), sum(bad))
    }
    list2env(structure(result[need][!bad], names = keys[need][!bad]), envir)
    result
  }
  if (!is.character(x))
    stop("'x' must be a character vector (of query IDs)")
  result <- vector("list", length(x))
  ok <- !is.na(x) & nzchar(x)
  result[!ok] <- rep.int(list(default), sum(!ok))
  result[ok] <- do_get(x[ok], MEMOIZED, prefix, default, getfun, single, ...)
  names(result) <- x
  result
}

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

reduce_to_mode <- function(x, cutoff, use.na) UseMethod("reduce_to_mode")

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

reduce_to_mode.matrix <- function(x, cutoff, use.na = TRUE) {
  apply(x, 2L, reduce_to_mode.default, cutoff, use.na)
}

list2matrix <- function(x, how = c("yaml", "json", "rcode")) {
  unlist_matrix <- function(x, fun, ...) {
    x <- do.call(rbind, x)
    if (typeof(x) != "list")
      return(x)
    if (!missing(fun)) {
      max.len <- apply(x, 2L, vapply, length, 0L)
      if (is.matrix(max.len))
        max.len <- apply(max.len, 2L, max)
      for (i in which(max.len > 1L))
        x[, i] <- vapply(X = x[, i], FUN = fun, FUN.VALUE = "", ...)
    }
    storage.mode(x) <- "character"
    x
  }
  how <- tryCatch(match.arg(how), error = function(e) how)
  switch(how,
    yaml = unlist_matrix(x, to_yaml, json = FALSE, listify = TRUE),
    json = unlist_matrix(x, to_yaml, json = TRUE, listify = TRUE),
    rcode = unlist_matrix(x),
    collect(x = x, what = how, dataframe = TRUE, stringsAsFactors = FALSE,
      optional = TRUE, keep.unnamed = TRUE, min.cov = 1L)
  )
}

sub_indexes <- function(x) {
  x <- vapply(x, length, 0L)
  add <- c(0L, cumsum(x))
  x <- lapply(x, seq_len)
  for (i in seq_along(x)[-1L])
    x[[i]] <- x[[i]] + add[[i]]
  attr(x, "total") <- add[[length(add)]]
  x
}

simplify_conditionally <- function(x) {
  if (any(vapply(x, is.list, NA)) || any(vapply(x, is.matrix, NA)))
    return(x)
  if (length(n <- unique.default(vapply(x, length, 0L))) > 1L)
    return(x)
  if (n > 1L)
    do.call(rbind, x)
  else
    unlist(x, FALSE, TRUE)
}

close_index_gaps <- function(x) {
  if (any(bad <- vapply(x, is.null, NA))) {
    warning("closing gaps in indexes")
    return(x[!bad])
  }
  x
}

fix_names <- function(x, y) {
  if (any(bad <- !nzchar(x)[i <- seq_along(y)] & nzchar(y)))
    x[i][bad] <- y[bad]
  x
}

is_uniform <- function(x, na.rm = FALSE) {
  if (na.rm)
    x <- na.exclude(x)
  if (length(x) < 2L || all((dup <- duplicated(x))[-1L]))
    return(TRUE)
  x[!dup]
}

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
      for (j in seq_len(i - 1L))
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

assert_splittable_matrix <- function(x, split.at) {
  pos <- which(colnames(x) == split.at)
  LL(pos, .msg = listing(sprintf("'%s'", split.at), style = "sentence",
    prepend = FALSE, header = "need exactly one column name present among: ",
    last.sep = "comma"))
  if (pos == ncol(x))
    stop("column given by 'split.at' must not be the last one")
  pos
}

strip_whitespace <- function(x) {
  strip <- function(x) sub("^\\s+", "", sub("\\s+$", "", x, FALSE, TRUE),
    FALSE, TRUE)
  for (i in which(vapply(x, is.character, NA)))
    x[, i] <- strip(x[, i])
  for (i in which(vapply(x, is.factor, NA)))
    levels(x[, i]) <- strip(levels(x[, i]))
  x
}

vector2row <- function(x) matrix(x, 1L, length(x), FALSE, list(NULL, names(x)))

metadata_key <- function(x, to.formula, ...) UseMethod("metadata_key")

metadata_key.default <- function(x, to.formula = FALSE, remove = NULL, ...) {
  if (!is.atomic(x))
    stop(NOT_YET)
  if (length(x) == 1L && x %in% remove)
    return(NULL)
  if (to.formula) ## TODO check whether this makes sense
    create_formula("~ c(%s)", paste0(x, collapse = ", "))
  else
    x
}

metadata_key.factor <- function(x, ...) {
  metadata_key.character(as.character(x), ...)
}

metadata_key.character <- function(x, to.formula = FALSE, remove = NULL,
    syntactic = FALSE, ...) {
  if (length(x) == 1L && x %in% remove)
    return(NULL)
  if (to.formula) {
    if (syntactic)
      x <- make.names(x)
    return(create_formula("~ `%s`",
      paste0(x, collapse = get("key.join", OPM_OPTIONS))))
  }
  if (is.null(names(x)))
    names(x) <- x
  x
}

metadata_key.list <- function(x, to.formula = FALSE, remove = NULL,
    syntactic = FALSE, ops = "+", ...) {
  join <- function(x) vapply(x, paste0, "",
    collapse = get("key.join", OPM_OPTIONS))
  if (is.null(names(x <- flatten(x))))
    names(x) <- join(x)
  else
    names(x)[bad] <- join(x[bad <- !nzchar(names(x)) | is.na(names(x))])
  x <- x[!names(x) %in% remove]
  if (syntactic) {
    names(x) <- make.names(names(x))
    x <- lapply(x, make.names)
  }
  if (!to.formula)
    return(x)
  fmt <- case(length(x), stop("'x' must not be empty"), "",
    paste(rep_len(ops, length(x) - 1L), "`%s`", collapse = " "))
  create_formula(paste("~ `%s`", fmt), names(x))
}

metadata_key.formula <- function(x, to.formula = FALSE, remove = NULL,
    syntactic = FALSE, ..., full.eval = !to.formula, envir = parent.frame()) {
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
  rec_make_names <- function(x) {
    if (is.name(x))
      as.name(make.names(x)) # make.names() converts to character mode
    else
      apply_to_tail(x, rec_make_names)
  }
  result <- if (to.formula)
    rec_replace(x[[length(x)]])
  else
    rec_listify(x[[length(x)]])
  if (full.eval) {
    result <- metadata_key(x = eval(result, enclos = envir), remove = remove,
      syntactic = syntactic, ...)
    if (length(result))
      attr(result, "combine") <- final_comb_list(combine, remove)
    result
  } else {
    # 'result' is a formula at this stage
    if (syntactic)
      result <- rec_make_names(result)
    x[[length(x)]] <- result
    attr(x, "combine") <- final_comb_list(combine, remove)
    x
  }
}

create_formula <- function(fmt, ..., .env = parent.frame()) {
  x <- c(list(fmt = fmt), lapply(list(...), as.list))
  formula(do.call(sprintf, unlist(x, FALSE, FALSE)), .env)
}

setGeneric("parse_time",
  function(object, format, ...) standardGeneric("parse_time"))

setMethod("parse_time", c("character", "missing"), function(object, format,
    tz = opm_opt("time.zone")) {
  parse_time(object, opm_opt("time.fmt"), tz)
}, sealed = SEALED)

setMethod("parse_time", c("character", "character"), function(object, format,
    tz = opm_opt("time.zone")) {
  if (!length(format))
    stop("need non-empty object 'format'")
  result <- strptime(object, format[1L], tz)
  for (fmt in format[-1L])
    result[isna] <- strptime(object[isna <- is.na(result)], fmt, tz)
  if (any(is.na(result)))
    warning("parsing time strings resulted in NA values")
  result
}, sealed = SEALED)

setGeneric("separate", function(object, ...) standardGeneric("separate"))

setMethod("separate", "character", function(object, split = opm_opt("split"),
    simplify = FALSE, keep.const = TRUE, list.wise = FALSE,
    strip.white = list.wise) {

  strip_white <- function(x) sub("\\s+$", "", sub("^\\s+", "", x, FALSE, TRUE),
    FALSE, TRUE)

  p0 <- function(x) paste0(x, collapse = "")

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
    x <- strsplit(x, "", TRUE)
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
    if (splits_constant(sprintf("[%s]+", char), x, FALSE, TRUE))
      2L
    else if (splits_constant(char, x, TRUE))
      1L
    else
      0L
  }

  # collect words after splitting and mark their occurrences
  word_occurrences <- function(x, split, strip.white) {
    x <- strsplit(x, sprintf("[%s]", p0(split)), FALSE, TRUE)
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
  split <- unique.default(unlist(strsplit(split, "", TRUE), FALSE, FALSE))
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
  object <- do.call(rbind, strsplit(object, split, FALSE, TRUE))
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

trim_string <- function(str, max, append = ".", clean = TRUE,
    word.wise = FALSE) {
  do_trim <- function(x) {
    trim.len <- max(0L, max - nchar(append))
    if (word.wise) {
      if (clean)
        x <- gsub("\\W", "", x, FALSE, TRUE)
      result <- abbreviate(x, minlength = trim.len, strict = TRUE)
    } else {
      result <- strtrim(x, trim.len)
      if (clean)
        result <- sub("\\W+$", "", result, FALSE, TRUE)
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

list2html <- function(x, level = 1L, fmt = opm_opt("html.class"), fac = 2L) {
  indent <- paste0(rep.int(" ", fac * (level - 1L)), collapse = "")
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
    is.abs.path <- grepl("^(/|[a-zA-Z]:)", css, FALSE, TRUE)
    css[is.abs.path] <- sprintf("file://%s", css[is.abs.path])
    css <- vapply(css, function(y) {
      single_tag("link", rel = "stylesheet", type = "text/css", href = y)
    }, "")
    css <- c(html_comment("user-defined CSS file(s)"), unname(css))
  } else
    css <- NULL
  generator <- single_tag("meta", name = "generator",
    content = paste0(opm_string(version = TRUE), collapse = " version "))
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

setAs(from = "ANY", to = "factor", function(from) as.factor(from))
setAs(from = "ANY", to = "ordered", function(from) as.ordered(from))

prepare_class_names <- function(x) UseMethod("prepare_class_names")

prepare_class_names.character <- function(x) {
  x <- unique.default(c("character", x))
  if ("ANY" %in% x)
    "ANY"
  else
    x
}

setGeneric("map_values",
  function(object, mapping, ...) standardGeneric("map_values"))

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
  structure(object, names = object)
}, sealed = SEALED)

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

setMethod("map_values", c("logical", "function"), function(object, mapping,
    ...) {
  result <- mapping(object, ...)
  mostattributes(result) <- attributes(object)
  result
}, sealed = SEALED)

setMethod("map_values", c("logical", "vector"), function(object, mapping) {
  result <- ifelse(object, mapping[[3L]], mapping[[1L]])
  result[is.na(result)] <- mapping[[2L]]
  attributes(result) <- attributes(object)
  result
}, sealed = SEALED)

setMethod("map_values", c("logical", "NULL"), function(object, mapping) {
  object
}, sealed = SEALED)

setMethod("map_values", c("logical", "missing"), function(object) {
  result <- object * 2L + 1L
  result[is.na(result)] <- 2L
  attributes(result) <- attributes(object)
  result
}, sealed = SEALED)

setMethod("map_values", c("NULL", "function"), function(object, mapping, ...) {
  NULL
}, sealed = SEALED)

setMethod("map_values", c("NULL", "character"), function(object, mapping) {
  NULL
}, sealed = SEALED)

setMethod("map_values", c("NULL", "missing"), function(object, mapping) {
  map_values(character())
}, sealed = SEALED)

setGeneric("map_names",
  function(object, mapping, ...) standardGeneric("map_names"))

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

repair_na_strings <- function(object, ...) UseMethod("repair_na_strings")

repair_na_strings.character <- function(object, ...) {
  object[grepl("^(\\s*NA|\\.na(\\.(real|integer|character))?)$", object,
    FALSE, TRUE)] <- NA_character_
  object
}

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

rescue_dots <- function(x) {
  if (is.character(x) && any(bad <- grepl("^_[^_]*_", x, FALSE, TRUE)))
    x[bad] <- chartr("_", ".", substr(x[bad], 2L, nchar(x[bad])))
  x
}

insert <- function(object, ...) UseMethod("insert")

insert.list <- function(object, other, ..., .force = FALSE, .strict = FALSE) {
  insert_carefully <- function(x, y) {
    if (length(bad <- setdiff(nn <- names(y), names(x))))
      stop("unknown key: ", bad[1L])
    for (name in nn) {
      novel <- y[[name]]
      if (!identical(class(novel), wanted <- class(x[[name]])))
        stop(sprintf("value of key '%s' must have class '%s'", name,
          paste0(wanted, collapse = " -> ")))
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
}

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

setGeneric("opm_opt", function(x, ...) standardGeneric("opm_opt"))

setMethod("opm_opt", "list", function(x) {
  old <- mget(names(x), OPM_OPTIONS) # fails if names are missing
  for (i in seq_along(x)) {
    if (!length(value <- x[[i]]))
      stop("empty value provided for key '%s'", names(x)[i])
    if (!all(inherits(value, class(old[[i]]), TRUE)))
      stop(sprintf("new and old value have conflicting class(es) for key '%s'",
        names(x)[i]))
  }
  list2env(x, OPM_OPTIONS)
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
      bad <- case(sub("^delete\\.", "", how, FALSE, TRUE),
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

