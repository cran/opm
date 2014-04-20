setGeneric("merge")

setMethod("merge", c(OPM, "missing"), function(x, y, sort.first = TRUE,
    parse = TRUE) {
  x
}, sealed = SEALED)

setMethod("merge", c(OPM, "numeric"), function(x, y, sort.first = TRUE,
    parse = TRUE) {
  x
}, sealed = SEALED)

setMethod("merge", c(OPM, OPM), function(x, y, sort.first = TRUE,
    parse = TRUE) {
  merge(new(OPMS, plates = list(x, y)), 0.25, sort.first, parse)
}, sealed = SEALED)

setMethod("merge", c(OPMS, "numeric"), function(x, y, sort.first = TRUE,
    parse = TRUE) {
  if (any(y <= 0))
    stop("'y' must be positive throughout")
  if (L(sort.first))
    x <- sort(x, by = "setup_time", parse = parse, na.last = TRUE)
  m <- do.call(rbind, measurements(x))
  if (is.matrix(tp <- hours(x, what = "all"))) {
    to.add <- c(0, must(cumsum(tp[-nrow(tp), ncol(tp), drop = FALSE]) + y))
    m[, 1L] <- as.vector(t(tp + to.add))
  } else if (is.list(tp)) {
    to.add <- c(0, must(cumsum(vapply(tp[-length(tp)], tail, 1, 1L)) + y))
    m[, 1L] <- unlist(mapply(`+`, tp, to.add, SIMPLIFY = FALSE,
      USE.NAMES = FALSE))
  } else
    stop(BUG_MSG)
  new(OPM, measurements = m, csv_data = csv_data(x[1L]),
    metadata = metadata(x[1L]))
}, sealed = SEALED)

setMethod("merge", c(OPMS, "missing"), function(x, y, sort.first = TRUE,
    parse = TRUE) {
  merge(x, 0.25, sort.first, parse)
}, sealed = SEALED)

setMethod("merge", c(MOPMX, "missing"), function(x, y) {
  combine <- function(x) if (length(x <- plates(x)) > 1L)
      new(OPMS, plates = x)
    else
      x[[1L]]
  if (!anyDuplicated.default(pt <- plate_type(x)))
    return(x)
  x@.Data <- lapply(split.default(x@.Data, as.factor(pt)), combine)
  x
}, sealed = SEALED)

setMethod("merge", c(MOPMX, "ANY"), function(x, y) {
  merge(x + y)
}, sealed = SEALED)

setMethod("merge", c(CMAT, "logical"), function(x, y) {
  merge(x, if (L(y))
      as.factor(rownames(x))
    else
      as.factor(seq_len(nrow(x))))
}, sealed = SEALED)

setMethod("merge", c(CMAT, "ANY"), function(x, y) {
  merge(x, as.factor(y))
}, sealed = SEALED)

setMethod("merge", c(CMAT, "factor"), function(x, y) {
  if (length(y) != nrow(x)) # this also covers NULL row names
    stop("length of 'y' not equal to number of rows")
  if (any(is.na(y)))
    stop("'y' must not contain NA values")
  if (length(levels(y)) == length(y))
    return(x)
  cn <- colnames(x) # later put back, avoiding correction of duplicate names
  x <- aggregate(as.data.frame(x, stringsAsFactors = FALSE), by = list(y),
    FUN = c, recursive = TRUE, simplify = FALSE)
  x <- as.matrix(x[, -1L, drop = FALSE])
  x[] <- lapply(x, sort.int, na.last = TRUE)
  rownames(x) <- levels(y)
  colnames(x) <- cn
  new(CMAT, x)
}, sealed = SEALED)

setGeneric("split")

setMethod("split", c(OPM, "missing", "missing"), function(x, f, drop) {
  split(x, drop = FALSE)
}, sealed = SEALED)

setMethod("split", c(OPMS, "missing", "missing"), function(x, f, drop) {
  split(x, drop = FALSE)
}, sealed = SEALED)

setMethod("split", c(OPM, "missing", "ANY"), function(x, f, drop) {
  extract_concentration <- function(x) {
    m <- regexpr("(?<=#)\\s*\\d+\\s*$", x, FALSE, TRUE)
    conc <- as.integer(substr(x, m, m + attr(m, "match.length") - 1L))
    regmatches(x, m) <- "1"
    list(Concentration = conc, Standardized = structure(names(x), names = x))
  }
  regular_size <- function(x) {
    counts <- tabulate(x$Concentration)
    length(counts) > 1L || all(duplicated.default(counts)[-1L])
  }
  regular_composition <- function(x) {
    for (i in seq_along(x)[-1L])
      if (!setequal(names(x[[1L]]), names(x[[i]])))
        return(FALSE)
    TRUE
  }
  get_and_rename <- function(x, w1, w2, conc, drop, key) {
    x <- rename_wells(x[, w1, drop = drop], w2)
    x@metadata[[key]] <- conc
    x
  }
  w <- extract_concentration(wells(x, TRUE, FALSE))
  if (!regular_size(w) || !regular_composition(
      w <- split.default(w$Standardized, w$Concentration))) {
    warning("no regular concentration structure found")
    return(x)
  }
  for (i in seq_along(w)[-1L])
    w[[i]] <- w[[i]][names(w[[1L]])]
  new(OPMS, plates = mapply(get_and_rename, conc = as.integer(names(w)),
    w1 = w, SIMPLIFY = FALSE, USE.NAMES = FALSE, MoreArgs = list(x = x,
      w2 = w[[1L]], drop = drop, key = get("series.key", OPM_OPTIONS))))
}, sealed = SEALED)

setMethod("split", c(OPMS, "missing", "ANY"), function(x, f, drop) {
  x@plates <- lapply(x@plates, split, drop = drop)
  x@plates <- unlist(lapply(x@plates, slot, "plates"), FALSE, FALSE)
  x
}, sealed = FALSE)

setMethod("split", c(OPM, "ANY", "missing"), function(x, f, drop) {
  split(x, f, FALSE)
}, sealed = SEALED)

setMethod("split", c(OPMS, "ANY", "missing"), function(x, f, drop) {
  split(x, f, FALSE)
}, sealed = SEALED)

setMethod("split", c(OPM, "factor", "missing"), function(x, f, drop) {
  split(x, f, FALSE)
}, sealed = SEALED)

setMethod("split", c(OPM, "factor", "ANY"), function(x, f, drop) {
  object <- split.default(0L, f, FALSE) # to get the warnings/errors
  object[[1L]] <- x[drop = drop]
  new(MOPMX, object)
}, sealed = SEALED)

setMethod("split", c(OPMS, "factor", "missing"), function(x, f, drop) {
  split(x, f, FALSE)
}, sealed = SEALED)

setMethod("split", c(OPMS, "factor", "ANY"), function(x, f, drop) {
  new(MOPMX, lapply(split.default(x, f, FALSE), `[`, drop = drop))
}, sealed = SEALED)

setMethod("split", c(OPMX, "ANY", "ANY"), function(x, f, drop) {
  split(x, as.factor(extract_columns(x, f, TRUE, " ", "ignore")), drop)
}, sealed = SEALED)

setMethod("split", c(MOPMX, "factor", "missing"), function(x, f, drop) {
  split.default(x, f, FALSE)
}, sealed = SEALED)

setMethod("split", c(MOPMX, "factor", "ANY"), function(x, f, drop) {
  split.default(x, f, drop)
}, sealed = SEALED)

setMethod("split", c(MOPMX, "list", "missing"), function(x, f, drop) {
  split(x, f, FALSE)
}, sealed = SEALED)

setMethod("split", c(MOPMX, "list", "ANY"), function(x, f, drop) {
  if (!all(vapply(f, is.factor, NA)))
    f <- metadata2factorlist(x, f)
  x <- mapply(split, x = x, f = f, MoreArgs = list(drop = drop),
    SIMPLIFY = FALSE)
  f <- sort.int(unique.default(unlist(lapply(f, levels), FALSE, FALSE)))
  result <- structure(vector("list", length(f)), names = f)
  for (level in f)
    result[[level]] <- lapply(x, `[[`, i = level)
  lapply(lapply(result, close_index_gaps), as, MOPMX)
}, sealed = SEALED)

setMethod("split", c(MOPMX, "ANY", "missing"), function(x, f, drop) {
  split(x, f, FALSE)
}, sealed = SEALED)

setMethod("split", c(MOPMX, "ANY", "ANY"), function(x, f, drop) {
  split(x, metadata2factorlist(x, f), drop)
}, sealed = SEALED)

setGeneric("plates", function(object, ...) standardGeneric("plates"))

setMethod("plates", WMDS, function(object) {
  object@plates
}, sealed = SEALED)

setMethod("plates", WMD, function(object) {
  list(object)
}, sealed = SEALED)

setMethod("plates", "list", function(object) {
  to_opm_list.list(object, TRUE, TRUE, FALSE)
}, sealed = SEALED)

setMethod("plates", MOPMX, function(object) {
  unlist(lapply(object@.Data, plates), FALSE)
}, sealed = SEALED)

setGeneric("oapply", function(object, fun, ...) standardGeneric("oapply"))

setMethod("oapply", OPM, function(object, fun, ...,
    simplify = TRUE) {
  fun(object, ...)
}, sealed = SEALED)

setMethod("oapply", OPMS, function(object, fun, ...,
    simplify = TRUE) {
  result <- sapply(X = object@plates, FUN = fun, ..., simplify = simplify,
    USE.NAMES = FALSE)
  if (simplify && is.list(result))
    result <- try_opms.list(result)
  result
}, sealed = SEALED)

setMethod("oapply", MOPMX, function(object, fun, ...,
    simplify = TRUE) {
  result <- sapply(X = object, FUN = fun, ..., simplify = simplify,
    USE.NAMES = TRUE) # using object@.Data would lose the names
  if (simplify && is.list(result))
    tryCatch(new(class(object), result[!vapply(result, is.null, NA)]),
      error = function(e) result)
  else
    result
}, sealed = SEALED)

setGeneric("flattened_to_factor",
  function(object, ...) standardGeneric("flattened_to_factor"))

setMethod("flattened_to_factor", "data.frame", function(object, sep = " ") {
  LL(plate.pos <- which(colnames(object) == RESERVED_NAMES[["plate"]]), sep)
  if (plate.pos == 1L)
    return(unique(object[, plate.pos]))
  result <- aggregate(object[, seq_len(plate.pos)],
    by = list(object[, plate.pos]), FUN = `[[`, i = 1L)
  result <- as.list(result[, seq.int(2L, ncol(result) - 1L), drop = FALSE])
  as.factor(do.call(paste, c(result, sep = sep)))
}, sealed = SEALED)

setGeneric("sort")

setMethod("sort", c(OPM, "missing"), function(x, decreasing, ...) {
  x
}, sealed = SEALED)

setMethod("sort", c(OPM, "ANY"), function(x, decreasing, ...) {
  x
}, sealed = SEALED)

setMethod("sort", c(OPMS, "missing"), function(x, decreasing, ...) {
  sort(x = x, decreasing = FALSE, ...)
}, sealed = SEALED)

setMethod("sort", c(OPMS, "ANY"), function(x, decreasing, by = "setup_time",
    parse = by == "setup_time", exact = TRUE, strict = TRUE, na.last = TRUE) {
  if (is.list(by)) {
    keys <- lapply(X = by, FUN = metadata, object = x, exact = exact,
      strict = strict)
    if (!strict)
      if (!length(keys <- keys[!vapply(keys, is.null, NA)]))
        return(x)
  } else if (is.character(by))
    case(length(by),
      stop("if a character scalar, 'by' must not be empty"),
      {
        keys <- csv_data(x, what = by)
        if (L(parse))
          keys <- must(parse_time(keys))
        keys <- list(keys)
      },
      keys <- lapply(X = by, FUN = csv_data, object = x)
    )
  else
    stop("'by' must be a list or a character vector")
  keys <- insert(keys, decreasing = decreasing, na.last = na.last,
    .force = TRUE)
  x@plates <- x@plates[do.call(order, keys)]
  x
}, sealed = SEALED)

setMethod("sort", c(MOPMX, "missing"), function(x, decreasing, ...) {
  sort(x = x, decreasing = FALSE, ...)
}, sealed = SEALED)

setMethod("sort", c(MOPMX, "ANY"), function(x, decreasing,
    by = c("plate.type", "length"), exact = TRUE, strict = TRUE,
    na.last = TRUE, ...) {
  if (length(x) < 2L)
    return(x)
  selection <- tryCatch(match.arg(by), error = function(e) "other")
  case(selection,
    length = criterion <- vapply(x, length, 0L),
    plate.type = criterion <- plate_type(x),
    other = {
      m <- metadata(object = x, key = by, exact = exact, strict = strict)
      criterion <- sapply(m, max, na.rm = TRUE, USE.NAMES = FALSE)
    }
  )
  x[sort.list(x = criterion, decreasing = decreasing, na.last = na.last, ...)]
}, sealed = SEALED)

setGeneric("unique")

setMethod("unique", c(OPM, "ANY"), function(x, incomparables, ...) {
  x
}, sealed = SEALED)

setMethod("unique", c(OPM, "missing"), function(x, incomparables, ...) {
  x
}, sealed = SEALED)

setMethod("unique", c(OPMS, "missing"), function(x, incomparables, ...) {
  unique(x = x, incomparables = FALSE, ...)
}, sealed = SEALED)

setMethod("unique", c(OPMS, "ANY"), function(x, incomparables, ...) {
  x[!duplicated(x = x, incomparables = incomparables, ...)]
}, sealed = SEALED)

setMethod("unique", c(MOPMX, "missing"), function(x, incomparables, ...) {
  unique(x = x, incomparables = FALSE, ...)
}, sealed = SEALED)

setMethod("unique", c(MOPMX, "ANY"), function(x, incomparables, ...) {
  x[!duplicated(x = x, incomparables = incomparables, ...)]
}, sealed = SEALED)

setGeneric("rev")

setMethod("rev", OPM, function(x) {
  x
}, sealed = SEALED)

setMethod("rev", OPMS, function(x) {
  x@plates <- x@plates[seq.int(length(x), 1L)]
  x
}, sealed = SEALED)

setGeneric("rep")

setMethod("rep", OPM, function(x, ...) {
  x <- rep(list(x), ...)
  case(length(x), NULL, x[[1L]], new(OPMS, plates = x))
}, sealed = SEALED)

setMethod("rep", OPMS, function(x, ...) {
  x <- rep(x@plates, ...)
  case(length(x), NULL, x[[1L]], new(OPMS, plates = x))
}, sealed = SEALED)

setGeneric("extract", function(object, ...) standardGeneric("extract"))

setMethod("extract", MOPMX, function(object, as.labels,
    subset = opm_opt("curve.param"), ci = FALSE, trim = "full",
    dataframe = FALSE, as.groups = NULL, ...) {

  convert_row_groups <- function(x) { # for generated matrices only
    result <- unlist(lapply(x, rownames), FALSE, FALSE)
    result <- sort.int(unique.default(result))
    result <- structure(character(length(result)), names = result)
    for (mat in x) # last one wins, as in collect()
      result[rownames(mat)] <- as.character(attr(mat, "row.groups"))
    as.factor(unname(result))
  }

  protected <- function(x) x[seq_len(match(RESERVED_NAMES[["parameter"]], x))]

  group_columns <- function(x, other) { # for generated data frames only
    x <- metadata_key(x)
    setdiff(c(unlist(x, FALSE, FALSE), names(attr(x, "combine"))), other)
  }

  x <- lapply(X = object, FUN = extract, as.labels = as.labels,
    subset = subset, ci = ci, trim = trim, dataframe = dataframe,
    as.groups = as.groups, ...)

  if (!dataframe) {
    if (!length(as.labels)) { # create potentially unique row names
      if (is.null(base <- names(object)))
        base <- plate_type(object)
      for (i in seq_along(x))
        rownames(x[[i]]) <- paste(base[[i]], seq_len(nrow(x[[i]])), sep = ".")
    }
    return(structure(collect(x, "datasets"), row.groups = if (length(as.groups))
        convert_row_groups(x)
      else
        NULL))
  }

  x <- collect_rows(x)
  rownames(x) <- NULL
  if (!length(as.groups))
    return(x)
  p.col <- protected(colnames(x))
  g.col <- group_columns(as.groups, p.col)
  x[, c(p.col, setdiff(colnames(x), c(p.col, g.col)), g.col), drop = FALSE]

}, sealed = SEALED)

setMethod("extract", OPMS, function(object, as.labels,
    subset = opm_opt("curve.param"), ci = FALSE, trim = "full",
    dataframe = FALSE, as.groups = NULL, sep = " ", dups = "warn",
    exact = TRUE, strict = TRUE, full = TRUE, max = 10000L, ...) {

  do_extract <- function(what, join, dups = "ignore") {
    extract_columns(object, what = what, join = join, sep = sep, dups = dups,
      exact = exact, strict = strict)
  }
  create_groups <- function(x, join, ci) {
    result <- do_extract(x, join)
    if (join) {
      result <- as.factor(result)
      if (ci)
        result <- rep(result, each = 3L)
    } else if (ci)
      result <- result[rep(seq_len(nrow(result)), each = 3L), , drop = FALSE]
    result
  }

  # Collect parameters in a matrix
  subset <- match.arg(subset,
    unlist(map_param_names(plain = TRUE, disc = TRUE)))
  if (subset == DISC_PARAM) {
    ci <- FALSE
    result <- discretized(object, full = full, max = max, ...)
  } else {
    result <- do.call(rbind, lapply(object@plates, FUN = aggregated,
      subset = subset, ci = ci, trim = trim, full = full, max = max, ...))
  }

  if (dataframe) {

    result <- as.data.frame(result)
    if (length(as.labels)) {
      columns <- do_extract(as.labels, join = FALSE)
      if (ci)
        columns <- columns[rep(seq_len(nrow(columns)), each = 3L), ,
          drop = FALSE]
      columns <- cbind(columns, rownames(result))
      colnames(columns)[ncol(columns)] <- RESERVED_NAMES[["parameter"]]
      rownames(result) <- rownames(columns) # otherwise a warning is likely
      result <- cbind(columns, result)
    } else {
      params <- rownames(result)
      rownames(result) <- seq_len(nrow(result))
      result <- cbind(params, result)
      colnames(result)[1L] <- RESERVED_NAMES[["parameter"]]
    }
    if (length(as.groups))
      result <- cbind(result, create_groups(as.groups, FALSE, ci))

  } else {

    if (length(as.labels)) {
      labels <- do_extract(as.labels, join = TRUE, dups = dups)
      rownames(result) <- if (ci)
        paste(rep(labels, each = 3L), rownames(result))
      else
        labels
    } else {
      rownames(result) <- if (ci)
        paste(rownames(result), rep(seq_len(nrow(result) / 3L), each = 3L),
          sep = sep)
      else
        seq_len(nrow(result))
    }
    if (length(as.groups))
      attr(result, "row.groups") <- create_groups(as.groups, TRUE, ci)
  }

  result

}, sealed = SEALED)

setMethod("extract", "data.frame", function(object, as.groups = TRUE,
    norm.per = c("row", "column", "none"), norm.by = TRUE, subtract = TRUE,
    direct = inherits(norm.by, "AsIs"), dups = c("warn", "error", "ignore"),
    split.at = param_names("split.at")) {

  do_norm <- function(x, row, by, direct, subtract) sweep(x, 2L - row,
    if (direct)
      by
    else if (row)
      rowMeans(x[, by, drop = FALSE])
    else
      colMeans(x[by, , drop = FALSE]), if (subtract)
      "-"
    else
      "/"
  )

  LL(subtract, direct)
  param.pos <- assert_splittable_matrix(object, split.at)

  num.pos <- seq.int(param.pos + 1L, ncol(object))
  case(match.arg(norm.per), # compute the normalisation if requested
    none = NULL,
    row = object[, num.pos] <- do_norm(object[, num.pos, drop = FALSE],
      TRUE, norm.by, direct, subtract),
    column = object[, num.pos] <- do_norm(object[, num.pos, drop = FALSE],
      FALSE, norm.by, direct, subtract)
  )

  if (!length(as.groups) || identical(c(as.groups), FALSE))
    return(object)

  # make list or vector from the grouping columns and note its length
  # metadata_key() enables lists to be passed as used for selecting metadata
  as.groups <- metadata_key(as.groups, FALSE)
  if (!is.logical(as.groups) && anyDuplicated(as.groups))
    case(match.arg(dups), ignore = as.null, warn = warning, error = stop)(
      "duplicated grouping values")
  as.groups <- unclass(object[, seq_len(param.pos - 1L), drop = FALSE][,
    as.groups, drop = FALSE])
  gl <- length(as.groups)

  # compute the means and CIs with respect to the stated grouping
  aggr.mean <- aggregate(object[, num.pos, drop = FALSE], by = as.groups,
    FUN = mean)
  aggr.CI <- aggregate(object[, num.pos, drop = FALSE], by = as.groups,
    FUN = var) # first the variances

  # The output has to be organized in a certain structure, three rows per group:
  # first the mean, second the lower CI limit third the upper CI limit. This
  # step creates the factor-data part up to the parameter column.
  result <- as.data.frame(sapply(aggr.mean[, seq_len(gl), drop = FALSE],
    rep, each = 3L))
  colnames(result) <- names(as.groups)
  result[, RESERVED_NAMES[["parameter"]]] <- as.factor(unlist(map_param_names(
    subset = as.character(object[1L, param.pos]), ci = TRUE)))

  # Reduce to numeric part and get CIs from means and variances.
  aggr.mean <- as.matrix(aggr.mean[, seq.int(gl + 1L, ncol(aggr.mean)),
    drop = FALSE])
  aggr.CI <- norm.ci(t0 = aggr.mean,
    var.t0 = aggr.CI[, seq.int(gl + 1L, ncol(aggr.CI)), drop = FALSE])
  aggr.CI <- as.matrix(aggr.CI[, -1L, drop = FALSE]) # remove the 'conf' column

  # Prepare the numerical part of the results.
  output <- matrix(ncol = 3L * nrow(aggr.mean), nrow = ncol(aggr.mean))
  pos.1 <- ncol(aggr.CI)
  pos.2 <- seq.int(pos.1 / 2L + 1L, pos.1)
  pos.1 <- seq_len(pos.1 / 2L)
  for (i in seq_len(nrow(aggr.mean)))
    output[, seq.int(i * 3L - 2L, 3L * i)] <- c(aggr.mean[i, , drop = TRUE],
      aggr.CI[i, pos.1, drop = TRUE], aggr.CI[i, pos.2, drop = TRUE])
  output <- t(output)
  colnames(output) <- colnames(aggr.mean)

  # Done.
  cbind(result, output)
}, sealed = SEALED)

setGeneric("extract_columns",
  function(object, ...) standardGeneric("extract_columns"))

setMethod("extract_columns", WMD, function(object, what, join = FALSE,
    sep = " ", dups = c("warn", "error", "ignore"), factors = TRUE,
    exact = TRUE, strict = TRUE) {
  what <- metadata_key(what, FALSE, NULL)
  if (is.logical(what)) {
    result <- 1L
    if (!L(join)) {
      result <- as.data.frame(result)
      colnames(result) <- get("group.name", OPM_OPTIONS)
    }
    return(result)
  }
  result <- metadata(object, what, exact, strict)
  result <- if (is.list(result))
    rapply(result, as.character)
  else
    as.character(result)
  if (L(join)) {
    result <- paste0(result, collapse = sep)
  } else {
    result <- as.list(result)
    if (is.null(names(result)))
      names(result) <- paste0(what, collapse = get("key.join", OPM_OPTIONS))
    result <- as.data.frame(result, optional = TRUE, stringsAsFactors = factors)
    if (ncol(result) > length(colnames(result)))
      colnames(result) <- paste0(what, collapse = get("key.join", OPM_OPTIONS))
    if (is.list(attr(what, "combine")))
      result <- extract_columns(result, attr(what, "combine"),
        factors = factors, direct = TRUE)
  }
  result
}, sealed = SEALED)

setMethod("extract_columns", WMDS, function(object, what, join = FALSE,
    sep = " ", dups = c("warn", "error", "ignore"), factors = TRUE,
    exact = TRUE, strict = TRUE) {
  what <- metadata_key(what, FALSE, NULL)
  if (is.logical(what)) {
    result <- if (L(what))
        rep.int(1L, length(object))
      else
        seq_len(length(object))
    if (!L(join)) {
      result <- as.data.frame(result)
      colnames(result) <- get("group.name", OPM_OPTIONS)
    }
    return(result)
  }
  result <- metadata(object, what, exact, strict)
  result <- if (is.list(result))
    lapply(result, rapply, f = as.character)
  else
    as.list(as.character(result))
  if (L(join)) {
    result <- unlist(lapply(result, FUN = paste0, collapse = sep))
    msg <- if (is.dup <- anyDuplicated(result))
      paste("duplicated label:", result[is.dup])
    else
      NULL
    if (length(msg))
      case(match.arg(dups), ignore = as.null, warn = warning, error = stop)(msg)
  } else {
    result <- must(do.call(rbind, result))
    result <- as.data.frame(result, optional = TRUE, stringsAsFactors = factors)
    if (ncol(result) > length(colnames(result)))
      colnames(result) <- paste0(what, collapse = get("key.join", OPM_OPTIONS))
    if (is.list(attr(what, "combine")))
      result <- extract_columns(result, attr(what, "combine"),
        factors = factors, direct = TRUE)
  }
  result
}, sealed = SEALED)

setMethod("extract_columns", "data.frame", function(object, what,
    as.labels = NULL, as.groups = NULL, sep = opm_opt("comb.value.join"),
    factors = is.list(what), direct = is.list(what) || inherits(what, "AsIs")) {
  join <- function(x, what, sep)
    do.call(paste, c(x[, what, drop = FALSE], list(sep = sep)))
  find_stuff <- function(x, what) {
    x <- x[, vapply(x, inherits, NA, what), drop = FALSE]
    if (!ncol(x))
      stop("no data of class(es) ", paste0(what, collapse = "/"), " found")
    as.matrix(x)
  }
  LL(direct, factors)
  if (direct) {
    if (is.list(what)) {
      if (is.null(names(what)))
        names(what) <- vapply(what, paste0, "",
          collapse = get("comb.key.join", OPM_OPTIONS))
      result <- object
      what <- what[!match(names(what), colnames(result), 0L)]
      if (factors)
        for (i in seq_along(what))
          result[, names(what)[i]] <- as.factor(join(object, what[[i]], sep))
      else
        for (i in seq_along(what))
          result[, names(what)[i]] <- join(object, what[[i]], sep)
      if (length(as.labels))
        rownames(result) <- join(object, as.labels, sep)
      attr(result, "joined.columns") <- c(attr(result, "joined.columns"), what)
    } else {
      result <- join(object, what, sep)
      if (length(as.labels))
        names(result) <- join(object, as.labels, sep)
      if (factors)
        result <- as.factor(result)
    }
  } else {
    result <- find_stuff(object, what)
    if (length(as.labels))
      rownames(result) <- join(object, as.labels, sep)
  }
  if (length(as.groups))
    attr(result, "row.groups") <- as.factor(join(object, as.groups, sep))
  result
}, sealed = SEALED)

setGeneric("as.data.frame")

setMethod("as.data.frame", OPM, function(x, row.names = NULL,
    optional = FALSE, sep = "_", csv.data = TRUE, settings = TRUE,
    include = FALSE, ..., stringsAsFactors = default.stringsAsFactors()) {
  result <- as.data.frame(wells(x), NULL, optional, ...,
    stringsAsFactors = stringsAsFactors)
  colnames(result) <- RESERVED_NAMES[["well"]]
  if (L(csv.data))
    result <- data.frame(as.data.frame(as.list(x@csv_data[CSV_NAMES]), NULL,
      optional, ..., stringsAsFactors = stringsAsFactors), result,
      check.names = FALSE, stringsAsFactors = FALSE)
  if (is.logical(include)) {
    if (L(include))
      result <- data.frame(result, to_metadata(x, stringsAsFactors, optional),
        check.names = FALSE, stringsAsFactors = FALSE)
  } else if (length(include)) {
    result <- data.frame(result, extract_columns(object = x, what = include,
      factors = stringsAsFactors), check.names = FALSE,
      stringsAsFactors = FALSE)
  }
  rownames(result) <- row.names
  if (length(sep))
    colnames(result) <- gsub("\\W+", sep, colnames(result), FALSE, TRUE)
  result
}, sealed = SEALED)

setMethod("as.data.frame", OPMA, function(x, row.names = NULL,
    optional = FALSE, sep = "_", csv.data = TRUE, settings = TRUE,
    include = FALSE, ..., stringsAsFactors = default.stringsAsFactors()) {
  result <- as.data.frame(t(x@aggregated), NULL, optional, ...,
    stringsAsFactors = stringsAsFactors)
  if (length(sep))
    colnames(result) <- gsub("\\W+", sep, colnames(result), FALSE, TRUE)
  result <- data.frame(callNextMethod(x, row.names, optional, sep, csv.data,
    settings, include, ..., stringsAsFactors = stringsAsFactors), result,
    check.names = FALSE, stringsAsFactors = FALSE)
  if (L(settings)) {
    settings <- x@aggr_settings[c(SOFTWARE, VERSION, METHOD)]
    if (length(sep)) {
      names(settings) <- gsub("\\W+", sep, names(settings), FALSE, TRUE)
      names(settings) <- paste("Aggr", names(settings), sep = sep)
    } else {
      names(settings) <- paste("Aggr", names(settings),
        sep = get("comb.key.join", OPM_OPTIONS))
    }
    result <- data.frame(result, as.data.frame(settings, NULL, optional, ...,
      stringsAsFactors = stringsAsFactors), check.names = FALSE,
      stringsAsFactors = FALSE)
  }
  result
}, sealed = SEALED)

setMethod("as.data.frame", OPMD, function(x, row.names = NULL,
    optional = FALSE, sep = "_", csv.data = TRUE, settings = TRUE,
    include = FALSE, ..., stringsAsFactors = default.stringsAsFactors()) {
  result <- callNextMethod(x, row.names, optional, sep, csv.data, settings,
    include, ..., stringsAsFactors = stringsAsFactors)
  result$Discretized <- x@discretized
  if (settings) {
    settings <- x@disc_settings[c(SOFTWARE, VERSION, METHOD)]
    if (length(sep)) {
      names(settings) <- gsub("\\W+", sep, names(settings), FALSE, TRUE)
      names(settings) <- paste("Disc", names(settings), sep = sep)
    } else
      names(settings) <- paste("Disc", names(settings),
        sep = get("comb.key.join", OPM_OPTIONS))
    result <- data.frame(result, as.data.frame(settings, NULL, optional, ...,
      stringsAsFactors = stringsAsFactors), check.names = FALSE,
      stringsAsFactors = FALSE)
  }
  result
}, sealed = SEALED)

setMethod("as.data.frame", OPMS, function(x, row.names = NULL,
    optional = FALSE, sep = "_", csv.data = TRUE, settings = TRUE,
    include = FALSE, ..., stringsAsFactors = default.stringsAsFactors()) {
  if (!length(row.names))
    row.names <- vector("list", length(x@plates))
  do.call(rbind, mapply(as.data.frame, x = x@plates, row.names = row.names,
    MoreArgs = list(optional = optional, sep = sep, csv.data = csv.data,
      settings = settings, include = include, ...,
      stringsAsFactors = stringsAsFactors),
    SIMPLIFY = FALSE, USE.NAMES = FALSE))
}, sealed = SEALED)

setMethod("as.data.frame", MOPMX, function(x, row.names = NULL,
    optional = FALSE, sep = "_", csv.data = TRUE, settings = TRUE,
    include = FALSE, ..., stringsAsFactors = default.stringsAsFactors()) {
  if (!length(row.names))
    row.names <- vector("list", length(x@.Data))
  do.call(rbind, mapply(as.data.frame, x = x@.Data, row.names = row.names,
    MoreArgs = list(optional = optional, sep = sep, csv.data = csv.data,
      settings = settings, include = include, ...,
      stringsAsFactors = stringsAsFactors),
    SIMPLIFY = FALSE, USE.NAMES = FALSE))
}, sealed = SEALED)

setOldClass("kegg_compounds")

setMethod("as.data.frame", "kegg_compounds", function(x, row.names = NULL,
    optional = TRUE, ..., stringsAsFactors = FALSE) {
  result <- lapply(x, as.data.frame, row.names, optional, ...,
    stringsAsFactors = stringsAsFactors)
  do.call(rbind, structure(result, names = names(x)))
}, sealed = SEALED)

setOldClass("kegg_compound")

setMethod("as.data.frame", "kegg_compound", function(x, row.names = NULL,
    optional = TRUE, ..., stringsAsFactors = FALSE) {
  # store database links for later
  links <- strsplit(as.character(x$DBLINKS), "\\s*:\\s*", FALSE, TRUE)
  links <- do.call(rbind, links)
  links <- structure(links[, 2L], names = links[, 1L])
  # get non-link components
  wanted <- c("ENTRY", "NAME", "FORMULA", "SEEALSO", "BRITE", "ACTIVITY",
    "EXACT_MASS")
  x <- structure(x[wanted], names = wanted)
  x$EXACT_MASS <- must(as.numeric(x$EXACT_MASS))
  # 'ACTIVITY' is actually only present in KEGG 'drug' descriptions
  x$ACTIVITY <- paste0(x$ACTIVITY, collapse = " ")
  x$NAME <- sub("\\s*;\\s*$", "", x$NAME, FALSE, TRUE)
  x$SEEALSO <- grep(pat <- "^Same\\s+as:\\s*", x$SEEALSO, FALSE, TRUE, TRUE)
  x$SEEALSO <- sub(pat, "", x$SEEALSO, FALSE, TRUE)
  x$SEEALSO <- gsub("\\s+", "||", x$SEEALSO, FALSE, TRUE)
  ## Note that several hierarchies may be present.
  ## Maybe we can use YAML to better represent this, either directly or after
  ## conversion to nested list.
  x$BRITE <- paste0(x$BRITE, collapse = "\n")
  x <- lapply(x, paste0, collapse = "||")
  # add database-link components
  x$CAS <- if (pos <- match("CAS", names(links), 0L))
      sub("\\s+", "||", links[[pos]], FALSE, TRUE)
    else
      NA_character_
  x$ChEBI <- if (pos <- match("ChEBI", names(links), 0L))
      sub("\\s+", "||", links[[pos]], FALSE, TRUE)
    else
      NA_character_
  # done
  x[!nzchar(x)] <- NA_character_
  as.data.frame(x, row.names, optional, ...,
    stringsAsFactors = stringsAsFactors)
}, sealed = SEALED)

setGeneric("flatten")

setMethod("flatten", OPM, function(object, include = NULL, fixed = list(),
    factors = TRUE, exact = TRUE, strict = TRUE, full = TRUE,
    numbers = FALSE, ...) {

  # Convert to flat data frame
  well.names <- if (L(numbers))
      seq_len(ncol(object@measurements) - 1L)
    else
      well.names <- wells(object, full = full, ...)
  ## the home-brewn solution was much faster than reshape():
  # if (factors)
  #   well.names <- as.factor(well.names)
  # result <- reshape(as.data.frame(object@measurements,
  #   stringsAsFactors = factors), direction = "long", idvar = "Hour",
  #   varying = wells(object), v.names = "Value", timevar = "Well",
  #   times = well.names)
  # colnames(result)[1L] <- "Time"
  times <- hours(object, "all")
  rep.times <- rep.int(times, length(well.names))
  rep.wells <- rep(well.names, each = length(times))
  result <- data.frame(time = rep.times, well = rep.wells,
    value = as.vector(object@measurements[, -1L]), check.names = FALSE,
    stringsAsFactors = factors)
  colnames(result) <- RESERVED_NAMES[colnames(result)]

  if (length(fixed)) # Include fixed stuff
    result <- data.frame(as.list(fixed), result, check.names = FALSE,
      stringsAsFactors = factors)

  if (length(include)) # Pick metadata and include them in the data frame
    result <- data.frame(metadata(object, include, exact = exact,
      strict = strict), result, stringsAsFactors = factors,
      check.names = FALSE)

  result

}, sealed = SEALED)

setMethod("flatten", OPMS, function(object, include = NULL, fixed = list(),
    ...) {
  nums <- paste(RESERVED_NAMES[["plate"]], seq_along(object@plates))
  nums <- lapply(as.list(nums), `names<-`, value = RESERVED_NAMES[["plate"]])
  nums <- lapply(nums, c, fixed, recursive = FALSE)
  do.call(rbind, mapply(flatten, object = object@plates, fixed = nums,
    MoreArgs = list(include = include, ...), SIMPLIFY = FALSE))
}, sealed = SEALED)

setMethod("flatten", MOPMX, function(object, include = NULL, fixed = list(),
    factors = FALSE, ...) {
  pt <- vapply(object@.Data, plate_type, "")
  pt <- lapply(as.list(pt), `names<-`, value = CSV_NAMES[["PLATE_TYPE"]])
  pt <- lapply(pt, c, fixed, recursive = FALSE)
  x <- mapply(flatten, object = object@.Data, fixed = pt,
    MoreArgs = list(include = include, factors = factors, ...),
    SIMPLIFY = FALSE, USE.NAMES = FALSE)
  nr <- vapply(x, ncol, 0L)
  if (any(bad <- nr < max(nr))) {
    pn <- RESERVED_NAMES[["plate"]]
    pn <- structure(list(paste(pn, 1L)), names = pn)
    for (i in seq_along(which(bad)))
      x[[i]] <- data.frame(x[[i]], pn, stringsAsFactors = factors,
        check.names = FALSE)
  }
  do.call(rbind, x)
}, sealed = SEALED)

setGeneric("to_yaml", function(object, ...) standardGeneric("to_yaml"))

setMethod("to_yaml", "list", function(object, sep = TRUE,
    line.sep = "\n", json = FALSE, listify = nodots, nodots = FALSE, ...) {
  replace_dots <- function(x) {
    if (any(bad <- grepl(".", x, FALSE, FALSE, TRUE)))
      x[bad] <- paste0("_", chartr(".", "_", x[bad]))
    x
  }
  to_map <- function(items) if (is.null(names(items)))
    items
  else
    as.list(items)
  LL(sep, line.sep, json, listify, nodots)
  if (listify)
    object <- rapply(object, to_map, "ANY", NULL, "replace")
  if (nodots)
    object <- map_names(object, replace_dots)
  if (json) {
    result <- toJSON(object, "C")
  } else {
    result <- as.yaml(x = object, line.sep = line.sep, ...)
    if (sep)
      result <- sprintf(sprintf("---%s%%s%s", line.sep, line.sep), result)
  }
  result
}, sealed = SEALED)

setMethod("to_yaml", YAML_VIA_LIST, function(object, ...) {
  n <- names(object)
  object <- as(object, "list")
  if (is.null(names(object)) && length(object) == length(n))
    names(object) <- n
  to_yaml(object, ...)
}, sealed = SEALED)

setMethod("to_yaml", MOPMX, function(object, ...) {
  to_yaml(lapply(object, as, "list"), ...)
}, sealed = SEALED)

setGeneric("opmx", function(object, ...) standardGeneric("opmx"))

setMethod("opmx", "data.frame", function(object,
    format = c("horizontal", "rectangular", "vertical"), plate.type = NULL,
    position = NULL, well = NULL, prefix = "T_", sep = "<>", full.name = NULL,
    setup.time = date(), filename = "", interval = NULL) {

  # Create a matrix acceptable as 'measurements' entry.
  #
  convert_rectangular_matrix <- function(x, sep, interval) {
    convert_time_point <- function(x) {
      n <- as.integer(x[1L, -1L, drop = TRUE])
      n <- vapply(x[-1L, 1L], sprintf, character(length(n)), fmt = "%s%02i", n)
      x <- t(as.matrix(x[-1L, -1L]))
      converted <- tryCatch({
          storage.mode(x) <- "numeric"
          TRUE
        }, warning = function(w) FALSE)
      if (converted)
        structure(c(x), names = toupper(c(n)))
      else
        NULL
    }
    for (i in which(vapply(x, is.factor, NA)))
      x[, i] <- as.character(x[, i])
    pos <- logical(nrow(x))
    for (i in seq_along(x))
      if (any(pos <- x[, i] == sep)) {
        x <- x[, c(i, setdiff(seq_along(x), i)), drop = FALSE]
        break
      }
    if (!any(pos))
      stop("'sep' neither found in some column nor in the row names")
    x <- split.data.frame(x, sections(pos, TRUE))
    x <- do.call(rbind, lapply(x, convert_time_point))
    times <- as.double(seq_len(nrow(x)) - 1L)
    if (length(interval) == 1L)
      times <- interval * times
    else if (length(interval) == nrow(x))
      times <- must(as.double(interval))
    else if (length(interval))
      stop("length of 'interval' must be 0, 1, or nrow(x)")
    x <- cbind(times, x)
    colnames(x)[1L] <- HOUR
    rownames(x) <- NULL
    x
  }

  # Create a matrix acceptable as 'measurements' entry.
  #
  convert_vertical_matrix <- function(x, interval) {
    select_columns <- function(x) {
      n <- clean_coords(colnames(x))
      if (any(ok <- grepl("^[A-H]\\d{2}$", n, FALSE, TRUE))) {
        colnames(x)[ok] <- n[ok]
      } else if (any(ok <- grepl("^\\d{3}$", n, FALSE, TRUE))) {
        colnames(x)[ok] <- rownames(WELL_MAP)[as.integer(colnames(x)[ok])]
      } else if (any(ok <- grepl("^V\\d{2}$", n, FALSE, TRUE))) {
        colnames(x)[ok] <- rownames(WELL_MAP)[
          as.integer(chartr("V", " ", colnames(x)[ok]))]
      } else {
        ok <- !logical(ncol(x))
        if (!length(interval) && is.integer(attr(x, "row.names")))
          ok[1L] <- FALSE # first column contains time points
        colnames(x)[ok] <- rownames(WELL_MAP)[seq_along(which(ok))]
      }
      if (length(interval))
        if (length(interval) == 1L)
          hour <- interval * (seq_len(nrow(x)) - 1L)
        else if (length(interval) == nrow(x))
          hour <- must(as.double(interval))
        else
          stop("length of 'interval' must be 0, 1, or nrow(x)")
      else if (any(!ok))
        hour <- x[, !ok, drop = FALSE][, 1L]
      else
        hour <- rownames(x)
      cbind(hour, x[, ok, drop = FALSE])
    }
    x <- as.matrix(select_columns(x))
    must(storage.mode(x) <- "double")
    rownames(x) <- NULL
    colnames(x)[1L] <- HOUR
    x
  }

  # At this stage, 'x' must be a matrix acceptable as 'measurements' entry.
  #
  create_opm_object <- function(x, position, plate.type, full.name, setup.time,
      filename) {
    L(plate.type, .msg = "plate type missing or non-unique")
    L(position, .msg = "'position' missing or non-unique")
    plate.type <- custom_plate_normalize_all(plate.type)
    custom_plate_assert(plate.type, colnames(x)[-1L])
    if (!is.na(full <- full.name[plate.type]))
      custom_plate_set_full(plate.type, full)
    y <- c(L(filename), plate.type, position, L(setup.time))
    names(y) <- CSV_NAMES
    new(OPM, measurements = x, csv_data = y, metadata = list())
  }

  # 'plate.type' and 'full.name' must already be normalized at this stage.
  #
  register_substrates <- function(wells, plate.type, full.name) {
    wn <- unique.default(wells) # already sorted at this stage
    if (all(grepl("^\\s*[A-Za-z]\\s*\\d+\\s*$", wn, FALSE, TRUE))) {
      map <- structure(clean_coords(wn), names = wn)
    } else if (custom_plate_exists(plate.type)) {
      map <- custom_plate_get(plate.type)
      if (any(bad <- !wn %in% map))
        stop("plate type '", plate.type, "' already exists but lacks ",
          "substrate '", wn[bad][1L], "'")
      map <- structure(names(map), names = map)
    } else {
      map <- structure(rownames(WELL_MAP)[seq_along(wn)], names = wn)
      custom_plate_set(plate.type, structure(names(map), names = map))
    }
    if (!is.na(full <- full.name[plate.type]))
      custom_plate_set_full(plate.type, full)
    map_values(wells, map)
  }

  # A mapping of the column names of 'x' must already have been conducted at
  # this stage.
  #
  convert_horizontal_format <- function(x, prefix, full.name, setup.time,
      filename) {
    repair_csv_data <- function(x, full.name, setup.time, filename) {
      map <- c(CSV_NAMES, RESERVED_NAMES[["well"]])
      map <- structure(map, names = chartr(" ", ".", map))
      names(x) <- map_values(names(x), map)
      n <- CSV_NAMES[["PLATE_TYPE"]]
      if (pos <- match(n, colnames(x), 0L))
        x[, pos] <- custom_plate_normalize_all(x[, pos])
      else
        x[, n] <- L(names(full.name),
          .msg = "plate type neither in 'object' nor (uniquely) in 'full.name'")
      n <- CSV_NAMES[["SETUP"]]
      if (!n %in% names(x))
        x[, n] <- setup.time
      n <- CSV_NAMES[["FILE"]]
      if (!n %in% names(x))
        x[, n] <- filename
      x
    }
    csv_positions <- function(x) {
      pos <- get("csv.selection", OPM_OPTIONS)
      pos <- unique.default(c(pos, CSV_NAMES[["PLATE_TYPE"]]))
      match(pos, colnames(x))
    }
    time_point_columns <- function(x, prefix) {
      first <- substring(x, 1L, nchar(prefix))
      x <- substring(x, nchar(prefix) + 1L, nchar(x))
      x <- suppressWarnings(as.numeric(x))
      x[first != prefix] <- NA_real_
      if (all(is.na(x)))
        stop("no columns with time points found -- wrong prefix?")
      x
    }
    per_plate_type <- function(cd, tp, x, md, full.name) {
      pos <- match(RESERVED_NAMES[["well"]], colnames(md))
      colnames(x) <- register_substrates(md[, pos],
        cd[1L, CSV_NAMES[["PLATE_TYPE"]]], full.name)
      md <- md[, -pos, drop = FALSE]
      indexes <- cd[, get("csv.keys", OPM_OPTIONS), drop = FALSE]
      indexes <- apply(indexes, 1L, paste0, collapse = " ")
      indexes <- split.default(seq_len(ncol(x)), indexes)
      result <- vector("list", length(indexes))
      for (i in seq_along(indexes)) {
        val <- x[, idx <- indexes[[i]], drop = FALSE]
        result[[i]] <- new("OPM", csv_data = cd[idx[1L], ],
          metadata = lapply(md[idx, , drop = FALSE], unique.default),
          measurements = cbind(tp, val[, order(colnames(val)), drop = FALSE]))
      }
      case(length(result), NULL, result[[1L]], new("OPMS", plates = result))
    }
    traverse_plate_types <- function(cd, tp, x, md, full.name) {
      indexes <- split.default(seq_len(ncol(x)),
        cd[, CSV_NAMES[["PLATE_TYPE"]]])
      result <- vector("list", length(indexes))
      for (i in seq_along(indexes)) {
        idx <- indexes[[i]]
        result[[i]] <- per_plate_type(cd[idx, , drop = FALSE], tp,
          x[, idx, drop = FALSE], md[idx, , drop = FALSE], full.name)
      }
      names(result) <- names(indexes)
      result
    }
    x <- x[order(x[, RESERVED_NAMES[["well"]]]), , drop = FALSE]
    x <- repair_csv_data(x, full.name, setup.time, filename)
    pos <- csv_positions(x)
    cd <- as.matrix(x[, pos, drop = FALSE])
    x <- x[, -pos, drop = FALSE]
    tp <- time_point_columns(names(x), prefix)
    md <- x[, is.na(tp), drop = FALSE]
    x <- t(as.matrix(x[, !is.na(tp), drop = FALSE]))
    rownames(x) <- NULL
    tp <- matrix(tp[!is.na(tp)], nrow(x), 1L, FALSE, list(NULL, HOUR))
    result <- traverse_plate_types(cd, tp, x, md, full.name)
    case(length(result), NULL, result[[1L]], as(result, "MOPMX"))
  }

  # Only for the 'horizontal' format.
  #
  map_colnames <- function(x, plate.type, position, well) {
    to_positions <- function(x) {
      if (is.factor(x) || is.double(x))
        x <- as.integer(x)
      else if (!is.integer(x))
        x <- as.integer(as.factor(x))
      clean_plate_positions(paste(x, "A"))
    }
    map <- list()
    map[[CSV_NAMES[["PLATE_TYPE"]]]] <- plate.type
    map[[RESERVED_NAMES[["well"]]]] <- well
    if (length(map)) {
      map <- structure(names(map), names = unlist(map, TRUE, FALSE))
      names(x) <- map_values(names(x), map)
    }
    if (length(position)) {
      if (length(map))
        position <- map_values(position, map)
      wanted <- list(position)
      names(wanted) <- pos <- CSV_NAMES[["POS"]]
      x <- extract_columns(x, wanted)
      x[, pos] <- to_positions(x[, pos])
    }
    x
  }

  prepare_full_name <- function(x) {
    if (!length(x))
      return(structure(character(), names = character()))
    names(x) <- custom_plate_normalize_all(names(x))
    x
  }

  for (i in which(vapply(object, is.factor, NA)))
    object[, i] <- as.character(object[, i])

  full.name <- prepare_full_name(full.name)

  case(match.arg(format),

    horizontal = convert_horizontal_format(map_colnames(object,
      plate.type, position, well), prefix, full.name, setup.time, filename),

    rectangular = create_opm_object(convert_rectangular_matrix(object, sep,
      interval), position, plate.type, full.name, setup.time, filename),

    vertical = create_opm_object(convert_vertical_matrix(object, interval),
      position, plate.type, full.name, setup.time, filename)

  )
}, sealed = SEALED)

