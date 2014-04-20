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
  #result[ok] <- reassign_duplicates(x[ok], do_get, MEMOIZED, prefix,
  #  default, getfun, single, ...)
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

setGeneric("common_times", function(x) standardGeneric("common_times"))

setMethod("common_times", OPM, function(x) {
  x
}, sealed = SEALED)

setMethod("common_times", OPMS, function(x) {
  tp <- hours(x, what = "all")
  if (is.matrix(tp))
    tp <- lapply(seq_len(nrow(tp)), function(i) tp[i, ])
  if (length(maxs <- unique.default(vapply(tp, max, 1))) < 2L)
    return(x)
  min.max <- min(maxs)
  tp <- lapply(tp, function(x) which(x <= min.max))
  x[, tp]
}, sealed = SEALED)

setGeneric("select_by_disc", function(x, ...) standardGeneric("select_by_disc"))

setMethod("select_by_disc", OPMD, function(x, invert.1, invert.2, comb.fun) {
  y <- discretized(x)
  if (invert.1)
    y <- !y
  y[is.na(y)] <- FALSE
  if (invert.2)
    y <- !y
  x[, y]
}, sealed = SEALED)

setMethod("select_by_disc", OPMS, function(x, invert.1, invert.2, comb.fun) {
  y <- discretized(x)
  if (invert.1)
    y <- !y
  y[is.na(y)] <- FALSE
  y <- apply(y, 2L, comb.fun)
  if (invert.2)
    y <- !y
  x[, , y]
}, sealed = SEALED)

setGeneric("do_select", function(x, query) standardGeneric("do_select"))

setMethod("do_select", OPM, function(x, query) {
  if (query)
    x
  else
    NULL
}, sealed = SEALED)

setMethod("do_select", OPMS, function(x, query) {
  x[query]
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
  if (!length(x))
    return(NULL)
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
    warning("closing gaps in indexes", call. = FALSE)
    return(x[!bad])
  }
  x
}

metadata2factorlist <- function(x, f) {
  replace_null <- function(x) {
    x[vapply(x, is.null, NA)] <- NA
    x
  }
  f <- metadata(x, f)
  f[simple] <- lapply(f[simple <- vapply(x, is, NA, OPM)], list)
  f <- lapply(lapply(f, replace_null), lapply, replace_null)
  lapply(lapply(f, vapply, paste0, "", collapse = " "), as.factor)
}

is_uniform <- function(x, na.rm = FALSE) {
  if (na.rm)
    x <- na.exclude(x)
  if (length(x) < 2L || all((dup <- duplicated(x))[-1L]))
    return(TRUE)
  x[!dup]
}

reassign_duplicates <- function(x, FUN, ...) {
  # this requires non-NA values (and non-empty values in the case of strings)
  if (!any(dup <- duplicated.default(x)))
    return(FUN(x, ...))
  FUN(x[!dup], ...)[match(x, x[!dup])]
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

setMethod("is_constant", "array", function(x, margin = 1L, na.rm = TRUE) {
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

collect_rows <- function(x) {
  #sortable_indexes <- function(x) {
  #  n <- seq_along(x)
  #  sprintf(sprintf("%%0%ii", ceiling(log(n[length(n)], 10))), n)
  #}
  add_cols <- function(x, cols) {
    if (length(cols <- setdiff(cols, colnames(x))))
      cbind(x, matrix(NA, nrow(x), length(cols), FALSE, list(NULL, cols)))
    else
      x
  }
  cn <- unique.default(unlist(lapply(x, colnames), FALSE, FALSE))
  do.call(rbind, lapply(x, add_cols, cn))
}

metadata_key <- function(x, to.formula, ...) UseMethod("metadata_key")

metadata_key.default <- function(x, to.formula = FALSE, remove = NULL, ...) {
  if (!is.atomic(x))
    stop(NOT_YET)
  if (length(x) == 1L && x %in% remove)
    return(NULL)
  if (to.formula) # no 'syntactic' argument here -- should always be syntactic
    create_formula("~ c(%s)", paste0(x, collapse = ", "))
  else
    x
}

metadata_key.factor <- function(x, ...) {
  metadata_key.character(structure(as.character(x), names = names(x)), ...)
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

formula2infix <- function(f) {
  if (length(f) > 2L)
    sprintf("%%%s%%", all.vars(f[[2L]]))
  else
    "%q%"
}

reassign_args_using <- function(use) {
  case(use,
    i =, I = NULL,
    k =, K = assign("values", FALSE, parent.frame()),
    n = assign("negative", "any", parent.frame()),
    N = assign("negative", "all", parent.frame()),
    p = assign("positive", "any", parent.frame()),
    P = assign("positive", "all", parent.frame()),
    q = {
      assign("values", TRUE, parent.frame())
      assign("exact", FALSE, parent.frame())
    },
    Q = {
      assign("values", TRUE, parent.frame())
      assign("exact", TRUE, parent.frame())
    },
    t =, T = assign("time", TRUE, parent.frame())
  )
  invisible(NULL)
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
  if (!grepl("^\\s*$", paren.sep))
    stop("'paren.sep' must only contain whitespace characters")
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
  sub(remove, "", sprintf(template, str.1, paren.sep, str.2), FALSE, TRUE)
}

remove_concentration <- function(x) {
  sub("\\s*#\\s*\\d+\\s*$", "", x, FALSE, TRUE)
}

get_partial_match <- function(i, m, string) {
  start <- attr(m, "capture.start")[, i]
  substr(string, start, start + attr(m, "capture.length")[, i] - 1L)
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

single_tag <- function(x, ...) {
  listing(list(...), c("<", x), ">", style = " %s=\"%s\"", collapse = "")
}

html_head <- function(title, css, meta, embed) {

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

  if (length(css <- css[nzchar(css)]))
    if (embed) {
      x <- lapply(css, readLines, warn = FALSE)
      css <- html_comment(paste("CSS from user-defined file", css))
      css <- mapply(c, css, single_tag("style", type = "text/css"), x,
        MoreArgs = list("</style>", ""), SIMPLIFY = FALSE, USE.NAMES = FALSE)
      css <- unlist(css, FALSE, FALSE)
    } else {
      is.abs.path <- grepl("^(/|[a-zA-Z]:)", css, FALSE, TRUE)
      css[is.abs.path] <- sprintf("file://%s", css[is.abs.path])
      css <- vapply(css, function(y) {
        single_tag("link", rel = "stylesheet", type = "text/css", href = y)
      }, "")
      css <- c(html_comment("user-defined CSS file(s)"), unname(css))
    }
  else
    css <- NULL

  generator <- single_tag("meta", name = "generator",
    content = paste0(opm_string(version = TRUE), collapse = " version "))

  # see http://www.w3.org/TR/NOTE-datetime
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

