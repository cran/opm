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
  x <- new(OPMS, plates = list(x, y))
  merge(x = x, y = 0.25, sort.first = sort.first, parse = parse)
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
  merge(x, 0.25, sort.first = sort.first, parse = parse)
}, sealed = SEALED)

setMethod("merge", c(CMAT, "ANY"), function(x, y) {
  if (is.logical(y))
    if (L(y))
      groups <- as.factor(rownames(x))
    else
      groups <- as.factor(seq_len(nrow(x)))
  else
    groups <- as.factor(y)
  if (length(groups) != nrow(x)) # this also covers NULL row names
    stop("length of 'groups' not equal to number of rows")
  if (any(is.na(groups)))
    stop("'groups' must not contain NA values")
  if (length(levels(groups)) == length(groups))
    return(x)
  cn <- colnames(x) # later put back, avoiding correction of duplicate names
  x <- aggregate(as.data.frame(x, stringsAsFactors = FALSE), by = list(groups),
    FUN = c, recursive = TRUE, simplify = FALSE)
  x <- as.matrix(x[, -1L, drop = FALSE])
  x[] <- lapply(x, sort.int, na.last = TRUE)
  rownames(x) <- levels(groups)
  colnames(x) <- cn
  new(CMAT, x)
}, sealed = SEALED)

setGeneric("plates", function(object, ...) standardGeneric("plates"))

setMethod("plates", OPMS, function(object) {
  object@plates
}, sealed = SEALED)

setMethod("plates", OPM, function(object) {
  list(object)
}, sealed = SEALED)

setMethod("plates", "list", function(object) {
  to_opm_list.list(object, TRUE, TRUE, FALSE)
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
  result <- sapply(X = object@.Data, FUN = fun, ..., simplify = simplify,
    USE.NAMES = TRUE)
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

setMethod("sort", c(OPMX, "missing"), function(x, decreasing, ...) {
  sort(x = x, decreasing = FALSE, ...)
}, sealed = SEALED)

setMethod("sort", c(OPM, "logical"), function(x, decreasing, ...) {
  x
}, sealed = SEALED)

setMethod("sort", c(OPMS, "logical"), function(x, decreasing, by = "setup_time",
    parse = by == "setup_time", exact = TRUE, strict = TRUE, na.last = TRUE) {
  if (is.list(by)) {
    keys <- lapply(X = by, FUN = metadata, object = x, exact = exact,
      strict = strict)
    if (!strict)
      if (!length(keys <- keys[!vapply(keys, is.null, NA)]))
        return(x)
  } else if (is.character(by)) {
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
  } else
    stop("'by' must be a list or a character vector")
  keys <- insert(keys, decreasing = decreasing, na.last = na.last,
    .force = TRUE)
  x@plates <- x@plates[do.call(order, keys)]
  x
}, sealed = SEALED)

setGeneric("unique")

setMethod("unique", c(OPM, "ANY"), function(x, incomparables, ...) {
  x
}, sealed = SEALED)

setMethod("unique", c(OPMS, "missing"), function(x, incomparables, ...) {
  unique(x = x, incomparables = FALSE, ...)
}, sealed = SEALED)

setMethod("unique", c(OPMS, "ANY"), function(x, incomparables, ...) {
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

setMethod("extract", OPMS, function(object, as.labels,
    subset = opm_opt("curve.param"), ci = FALSE, trim = "full",
    dataframe = FALSE, as.groups = NULL, sep = " ", dups = "warn",
    exact = TRUE, strict = TRUE, full = TRUE, max = 10000L, ...) {

  do_extract <- function(what, join, dups = "ignore") {
    extract_columns(object, what = what, join = join, sep = sep, dups = dups,
      exact = exact, strict = strict)
  }
  create_groups <- function(x, join, ci) {
    numeric_groups <- function(how) {
      if (L(how))
        rep.int(1L, length(object))
      else
        seq_len(length(object))
    }
    if (join) {
      result <- if (is.logical(x))
        numeric_groups(x)
      else
        do_extract(x, join = TRUE)
      result <- as.factor(result)
      if (ci)
        result <- rep(result, each = 3L)
    } else {
      if (is.logical(x)) {
        result <- as.data.frame(numeric_groups(x))
        rownames(result) <- get("group.name", OPM_OPTIONS)
      } else
        result <- do_extract(x, join = FALSE)
      if (ci)
        result <- result[rep(seq_len(nrow(result)), each = 3L), , drop = FALSE]
    }
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

setMethod("extract_columns", OPMS, function(object, what, join = FALSE,
    sep = " ", dups = c("warn", "error", "ignore"), factors = TRUE,
    exact = TRUE, strict = TRUE) {
  what <- metadata_key(what, FALSE, NULL)
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
    factors = is.list(what), direct = inherits(what, "AsIs")) {
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
        stop("if 'what' is a list, it must have names")
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
    optional = FALSE, sep = "_", ...,
    stringsAsFactors = default.stringsAsFactors()) {
  result <- cbind(as.data.frame(as.list(x@csv_data[CSV_NAMES]), NULL, optional,
    ..., stringsAsFactors = stringsAsFactors), Well = wells(x))
  rownames(result) <- row.names
  colnames(result) <- gsub("\\W+", sep, colnames(result), FALSE, TRUE)
  result
}, sealed = SEALED)

setMethod("as.data.frame", OPMA, function(x, row.names = NULL,
    optional = FALSE, sep = "_", ...,
    stringsAsFactors = default.stringsAsFactors()) {
  result <- as.data.frame(t(x@aggregated), NULL, optional, ...,
    stringsAsFactors = stringsAsFactors)
  colnames(result) <- gsub("\\W+", sep, colnames(result), FALSE, TRUE)
  result <- cbind(callNextMethod(x, row.names, optional, sep, ...,
    stringsAsFactors = stringsAsFactors), result)
  settings <- x@aggr_settings[c(SOFTWARE, VERSION, METHOD)]
  names(settings) <- paste("Aggr", names(settings), sep = sep)
  cbind(result, as.data.frame(settings, NULL, optional, ...,
    stringsAsFactors = stringsAsFactors))
}, sealed = SEALED)

setMethod("as.data.frame", OPMD, function(x, row.names = NULL,
    optional = FALSE, sep = "_", ...,
    stringsAsFactors = default.stringsAsFactors()) {
  result <- callNextMethod(x, row.names, optional, sep, ...,
    stringsAsFactors = stringsAsFactors)
  result$Discretized <- x@discretized
  settings <- x@disc_settings[c(SOFTWARE, VERSION, METHOD)]
  names(settings) <- paste("Disc", names(settings), sep = sep)
  cbind(result, as.data.frame(settings, NULL, optional, ...,
    stringsAsFactors = stringsAsFactors))
}, sealed = SEALED)

setMethod("as.data.frame", OPMS, function(x, row.names = NULL,
    optional = FALSE, sep = "_", ...,
    stringsAsFactors = default.stringsAsFactors()) {
  if (!length(row.names))
    row.names <- vector("list", length(x@plates))
  do.call(rbind, mapply(as.data.frame, x@plates, row.names, SIMPLIFY = FALSE,
    MoreArgs = list(optional = optional, sep = sep, ...,
    stringsAsFactors = stringsAsFactors), USE.NAMES = FALSE))
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

setMethod("flatten", OPM, function(object, include = NULL, fixed = NULL,
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
    result <- cbind(as.data.frame(as.list(fixed), stringsAsFactors = factors),
      result)

  if (length(include)) # Pick metadata and include them in the data frame
    result <- cbind(as.data.frame(metadata(object, include,
      exact = exact, strict = strict), stringsAsFactors = factors), result)

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

setGeneric("to_yaml", function(object, ...) standardGeneric("to_yaml"))

setMethod("to_yaml", YAML_VIA_LIST, function(object, sep = TRUE,
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
  object <- as(object, "list")
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

setMethod("to_yaml", MOPMX, function(object, ...) {
  to_yaml(lapply(object, as, "list"), ...)
})

