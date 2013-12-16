setGeneric("measurements",
  function(object, ...) standardGeneric("measurements"))

setMethod("measurements", OPM, function(object, i) {
  if (missing(i))
    object@measurements
  else
    cbind(object@measurements[, 1L, drop = FALSE],
      object@measurements[, -1L, drop = FALSE][,
        well_index(i, colnames(object@measurements)[-1L]), drop = FALSE])
}, sealed = SEALED)

setGeneric("well", function(object, ...) standardGeneric("well"))

setMethod("well", OPM, function(object, i, drop = TRUE, use.names = TRUE) {
  x <- object@measurements[, -1L, drop = FALSE]
  if (L(use.names))
    rownames(x) <- object@measurements[, 1L]
  x[, well_index(i, colnames(object@measurements)[-1L]), drop = drop]
}, sealed = SEALED)

setGeneric("hours", function(object, ...) standardGeneric("hours"))

setMethod("hours", OPM, function(object,
    what = c("max", "all", "size", "summary", "interval", "minmax")) {
  tp <- object@measurements[, HOUR]
  case(match.arg(what),
    all = tp,
    interval = {
      if (length(tp) < 2L)
        NA_real_
      else {
        diffs <- unique(tp[-1L] - tp[-length(tp)])
        if (length(diffs) > 1L)
          NA_real_
        else
          diffs[1L]
      }
    },
    minmax =,
    max = max(tp),
    size = length(tp),
    summary = summary(tp)
  )
}, sealed = SEALED)

setMethod("[", c(OPM, "ANY", "ANY", "ANY"), function(x, i, j, ...,
    drop = FALSE) {
  mat <- x@measurements[, -1L, drop = FALSE]
  mat <- mat[i, well_index(j, colnames(mat)), ..., drop = FALSE]
  if (!all(dim(mat)))
    stop("selection resulted in empty matrix")
  mat <- cbind(x@measurements[i, 1L, drop = FALSE], mat)
  names(dimnames(mat)) <- names(dimnames(x@measurements))
  x@measurements <- mat
  x
}, sealed = SEALED)

setMethod("[", c(OPMA, "ANY", "ANY", "ANY"), function(x, i, j, ...,
    drop = FALSE) {
  x <- callNextMethod(x, i, j, ..., drop = drop)
  if (drop)
    return(as(x, OPM))
  if (!missing(j))
    x@aggregated <- x@aggregated[, well_index(j, colnames(x@aggregated)), ...,
      drop = FALSE]
  x
}, sealed = SEALED)

setMethod("[", c(OPMD, "ANY", "ANY", "ANY"), function(x, i, j, ...,
    drop = FALSE) {
  x <- callNextMethod(x, i, j, ..., drop = drop)
  if (drop)
    return(x) # ... which is an OPM object in that case
  if (!missing(j))
    x@discretized <- x@discretized[well_index(j, names(x@discretized))]
  x
}, sealed = SEALED)

setMethod("[", c(OPMS, "ANY", "ANY", "ANY"), function(x, i, j, k, ...,
    drop = FALSE) {
  if (!missing(...))
    stop("incorrect number of dimensions")
  if (missing(i) || identical(i, TRUE))
    y <- x@plates
  else {
    y <- x@plates[i]
    if (any(bad <- vapply(y, is.null, NA))) {
      warning("plate indexes partially out of range")
      y <- y[!bad]
    }
    if (!length(y))
      return(NULL)
  }
  k <- well_index(k, colnames(y[[1L]]@measurements)[-1L])
  if (missing(j) || identical(j, TRUE)) {
    # no call of OPM method if j and k are missing/TRUE and drop is FALSE
    if (!identical(k, TRUE) || drop)
      y <- mapply(`[`, x = y, MoreArgs = list(j = k, drop = drop),
        SIMPLIFY = FALSE, USE.NAMES = FALSE)
  } else if (is.list(j)) {
    y <- mapply(`[`, x = y, i = j, MoreArgs = list(j = k, drop = drop),
      SIMPLIFY = FALSE, USE.NAMES = FALSE)
  } else
    y <- mapply(`[`, x = y, MoreArgs = list(i = j, j = k, drop = drop),
      SIMPLIFY = FALSE, USE.NAMES = FALSE)
  if (length(y) == 1L)
    return(y[[1L]])
  x@plates <- y
  x
}, sealed = SEALED)

setMethod("[", c(MOPMX, "character", "missing", "ANY"), function(x, i, j,
    drop = FALSE) {
  if (drop) # remove the class, return a list
    return(x@.Data[match(i, names(x))])
  x@.Data <- close_index_gaps(x@.Data[match(i, names(x))]) # keeps the class
  x
})

setMethod("[", c(MOPMX, "ANY", "missing", "ANY"), function(x, i, j,
    drop = FALSE) {
  if (drop) # remove the class, return a list
    return(x@.Data[i])
  x@.Data <- x@.Data[i] # keeps the class
  x
})

setMethod("max", OPM, function(x, ..., na.rm = FALSE) {
  if (missing(...))
    max(x@measurements[, -1L, drop = FALSE], na.rm = na.rm)
  else
    max(well(x, ...), na.rm = na.rm)
}, sealed = SEALED)

setMethod("max", OPMS, function(x, ..., na.rm = FALSE) {
  max(vapply(x@plates, FUN = max, 1, ..., na.rm = na.rm),
    na.rm = na.rm)
}, sealed = SEALED)

setGeneric("minmax", function(x, ...) standardGeneric("minmax"))

setMethod("minmax", OPM, function(x, ..., na.rm = FALSE) {
  min(apply(x@measurements[, -1L, drop = FALSE][, ..., drop = FALSE], 2L,
    FUN = max, na.rm = na.rm))
}, sealed = SEALED)

setMethod("minmax", OPMS, function(x, ..., na.rm = FALSE) {
  min(vapply(x@plates, FUN = minmax, 1, ..., na.rm = na.rm))
}, sealed = SEALED)

setMethod("dim", OPM, function(x) {
  dim(measurements(x)[, -1L, drop = FALSE])
}, sealed = SEALED)

setMethod("dim", OPMS, function(x) {
  c(length(x@plates), dim(x@plates[[1L]]))
}, sealed = SEALED)

setMethod("length", OPM, function(x) {
  1L
}, sealed = SEALED)

setMethod("length", OPMS, function(x) {
  length(x@plates)
}, sealed = SEALED)

setGeneric("seq")

setMethod("seq", OPM, function(...) {
  stop("one cannot loop over an object of class ", class(..1))
}, sealed = SEALED)

setMethod("seq", OPMS, function(...) {
  seq_along(..1@plates)
}, sealed = SEALED)

setGeneric("csv_data", function(object, ...) standardGeneric("csv_data"))

setMethod("csv_data", OPM, function(object,
    keys = character(), strict = TRUE,
    what = c("select", "filename", "setup_time", "position", "other"),
    normalize = FALSE) {
  LL(strict, normalize)
  result <- case(match.arg(what),
      select = NULL,
      filename = object@csv_data[[CSV_NAMES[["FILE"]]]],
      setup_time = if (normalize)
          as.character(parse_time(object@csv_data[[CSV_NAMES[["SETUP"]]]]))
        else
          object@csv_data[[CSV_NAMES[["SETUP"]]]],
      position = if (normalize)
          clean_plate_positions(object@csv_data[[CSV_NAMES[["POS"]]]])
        else
          object@csv_data[[CSV_NAMES[["POS"]]]],
      other = object@csv_data[!names(object@csv_data) %in% CSV_NAMES]
    )
  if (length(result))
    return(result)
  if (!length(keys) || all(is.na(keys) | !nzchar(keys))) {
    result <- object@csv_data
  } else {
    result <- object@csv_data[keys]
    if (any(isna <- is.na(result)))
      if (strict)
        stop("could not find key ", keys[isna][1L])
      else
        names(result)[isna] <- keys[isna]
  }
  if (normalize) {
    pos <- match(CSV_NAMES[c("SETUP", "POS")], names(result), 0L)
    if (pos[1L])
      result[pos[1L]] <- as.character(parse_time(result[pos[1L]]))
    if (pos[2L])
      result[pos[2L]] <- clean_plate_positions(result[pos[2L]])
  }
  result
}, sealed = SEALED)

setMethod("csv_data", "OPMS", function(object, ...) {
  x <- lapply(X = object@plates, FUN = csv_data, ...)
  if (all(vapply(x, length, 0L) == 1L))
    return(unlist(x, FALSE, TRUE))
  x <- lapply(x, vector2row)
  for (i in seq_along(x)) # next step necessary to keep all rows
    rownames(x[[i]]) <- i # TODO: this should go into collect()
  collect(x, "datasets", 1L, TRUE)
}, sealed = SEALED)

setMethod("csv_data", "MOPMX", function(object, ...) {
  x <- lapply(X = object, FUN = csv_data, ...)
  if (all(is.vec <- !vapply(x, is.matrix, 0L)))
    return(unlist(x, FALSE, TRUE))
  x[is.vec] <- lapply(x[is.vec], vector2row)
  for (i in seq_along(x)) # next step necessary to keep all rows
    rownames(x[[i]]) <- paste(i, seq_len(nrow(x[[i]])), sep = ".")
  collect(x, "datasets", 1L, TRUE) # TODO: the above should go into collect()
}, sealed = SEALED)

setGeneric("filename", function(object, ...) standardGeneric("filename"))

setMethod("filename", OPM, function(object) {
  warning("this function is deprecated -- use csv_data() instead")
  csv_data(object, what = "filename")
}, sealed = SEALED)

setGeneric("setup_time", function(object, ...) standardGeneric("setup_time"))

setMethod("setup_time", OPM, function(object) {
  warning("this function is deprecated -- use csv_data() instead")
  csv_data(object, what = "setup_time")
}, sealed = SEALED)

setGeneric("position", function(object, ...) standardGeneric("position"))

setMethod("position", OPM, function(object) {
  warning("this function is deprecated -- use csv_data() instead")
  csv_data(object, what = "position")
}, sealed = SEALED)

setGeneric("has_aggr", function(object, ...) standardGeneric("has_aggr"))

setMethod("has_aggr", OPM, function(object) {
  .hasSlot(object, "aggregated")
}, sealed = SEALED)

setGeneric("has_disc", function(object, ...) standardGeneric("has_disc"))

setMethod("has_disc", OPM, function(object) {
  .hasSlot(object, "discretized")
}, sealed = SEALED)

setGeneric("aggregated", function(object, ...) standardGeneric("aggregated"))

setMethod("aggregated", OPMA, function(object, subset = NULL, ci = TRUE,
    trim = c("no", "full", "medium"), full = FALSE, in.parens = TRUE,
    max = opm_opt("max.chars"), ...) {

  # lambda trimming functions
  trim_into_hours <- function(x, hour, trim) {
    if (trim == "no")
      return(x)
    ok <- !is.na(x)
    x[ok & x > hour] <- hour
    case(trim,
      full = x[ok & x < 0] <- 0,
      medium = x[ok & x < -hour] <- -hour
    )
    x
  }
  trim_mat_into_hours <- function(x, hours, trim) {
    structure(trim_into_hours(x, hours, trim), dim = dim(x),
      dimnames = dimnames(x))
  }
  trim_lambda <- function(x, hours, trim) {
    is.lambda <- grepl(CURVE_PARAMS[2L], rownames(x), FALSE, FALSE, TRUE)
    x[is.lambda, ] <- trim_mat_into_hours(x[is.lambda, , drop = FALSE],
      hours, trim = trim)
    x
  }

  trim <- match.arg(trim)

  # NULL as software entry is allowed to increase backwards compatibility
  if (is.null(software <- object@aggr_settings[[SOFTWARE]]))
    warning(sprintf("object has no '%s' entry", SOFTWARE))
  else if (software != opm_string())
    warning(sprintf("unknown '%s' entry '%s': subset creation may not work",
      SOFTWARE, software))

  # generate subset
  wanted <- unlist(map_param_names(subset, ci))
  result <- object@aggregated[wanted, , drop = FALSE]
  if (CURVE_PARAMS[2L] %in% subset)
    result <- trim_lambda(result, hours(object), trim = trim)

  if (L(full))
    colnames(result) <- map_well_names(wells = colnames(result),
      plate = plate_type(object), in.parens = in.parens, max = max, ...)

  result

}, sealed = SEALED)

setGeneric("aggr_settings",
  function(object, ...) standardGeneric("aggr_settings"))

setMethod("aggr_settings", OPMA, function(object, join = NULL) {
  if (length(join))
    list2matrix(list(object@aggr_settings), join)
  else
    object@aggr_settings
}, sealed = SEALED)

setMethod("aggr_settings", OPMS, function(object, join = NULL) {
  result <- lapply(object@plates, slot, "aggr_settings")
  if (length(join))
    list2matrix(result, join)
  else
    result
}, sealed = SEALED)

setMethod("aggr_settings", MOPMX, function(object, join = NULL) {
  result <- lapply(object@.Data, aggr_settings, join)
  if (length(join))
    do.call(rbind, result)
  else
    result
}, sealed = SEALED)

setGeneric("discretized", function(object, ...) standardGeneric("discretized"))

setMethod("discretized", OPMD, function(object, full = FALSE, in.parens = TRUE,
    max = opm_opt("max.chars"), ...) {
  result <- object@discretized
  if (L(full)) # => currently not very efficient for the OPMS methods
    names(result) <- map_well_names(wells = names(result),
      plate = plate_type(object), in.parens = in.parens, max = max, ...)
  result
}, sealed = SEALED)

setGeneric("disc_settings",
  function(object, ...) standardGeneric("disc_settings"))

setMethod("disc_settings", OPMD, function(object, join = NULL) {
  if (length(join))
    list2matrix(list(object@disc_settings), join)
  else
    object@disc_settings
}, sealed = SEALED)

setMethod("disc_settings", OPMS, function(object, join = NULL) {
  result <- lapply(object@plates, slot, "disc_settings")
  if (length(join))
    list2matrix(result, join)
  else
    result
}, sealed = SEALED)

setMethod("disc_settings", MOPMX, function(object, join = NULL) {
  result <- lapply(object@.Data, disc_settings, join)
  if (length(join))
    do.call(rbind, result)
  else
    result
}, sealed = SEALED)

setGeneric("subset")

setMethod("subset", OPMS, function(x, query, values = TRUE,
    invert = FALSE, exact = FALSE, time = FALSE,
    positive = c("ignore", "any", "all"),
    negative = c("ignore", "any", "all"),
    use = c("i", "I", "k", "K", "n", "N", "p", "P", "q", "Q", "t", "T")) {
  select_binary <- function(x, invert.1, combine, invert.2) {
    y <- discretized(x)
    if (invert.1)
      y <- !y
    y[is.na(y)] <- FALSE
    y <- apply(y, 2L, combine)
    if (invert.2)
      y <- !y
    x[, , y]
  }
  case(match.arg(use),
    i =, I = NULL,
    k =, K = values <- FALSE,
    n = negative <- "any",
    N = negative <- "all",
    p = positive <- "any",
    P = positive <- "all",
    q = {
      values <- TRUE
      exact <- FALSE
    },
    Q = {
      values <- TRUE
      exact <- TRUE
    },
    t =, T = time <- TRUE
  )
  LL(values, invert, exact, time)
  case(negative <- match.arg(negative),
    ignore = NULL,
    any =,
    all = return(select_binary(x, TRUE, negative, invert))
  )
  case(positive <- match.arg(positive),
    ignore = NULL,
    any =,
    all = return(select_binary(x, FALSE, positive, invert))
  )
  if (time) {
    tp <- hours(x, what = "all")
    if (is.matrix(tp))
      tp <- lapply(seq_len(nrow(tp)), function(i) tp[i, ])
    if (length(maxs <- unique.default(vapply(tp, max, 1))) < 2L)
      return(x)
    min.max <- min(maxs)
    tp <- lapply(tp, function(x) which(x <= min.max))
    return(x[, tp])
  }
  if (is.logical(query) || is.numeric(query))
    return(x[query, , ])
  pos <- if (values) {
    if (exact)
      query %Q% x
    else
      query %q% x
  } else if (exact)
    query %K% x
  else
    query %k% x
  if (invert)
    pos <- !pos
  x[pos, , ]
}, sealed = SEALED)

setGeneric("thin_out", function(object, ...) standardGeneric("thin_out"))

setMethod("thin_out", OPM, function(object, factor, drop = FALSE) {
  if (L(factor) < 1)
    stop("'factor' must be >= 1")
  idx <- seq_len(dim(object)[1L])
  idx <- idx[idx %% factor == 0L]
  object[idx, , drop = drop]
}, sealed = SEALED)

setMethod("thin_out", OPMS, function(object, ...) {
  new(OPMS, plates = lapply(X = object@plates, FUN = thin_out, ...))
}, sealed = SEALED)

setGeneric("duplicated")

setMethod("duplicated", c(OPM, "ANY"), function(x, incomparables, ...) {
  FALSE
}, sealed = SEALED)

setMethod("duplicated", c(OPMS, "missing"), function(x, incomparables, ...) {
  duplicated(x = x, incomparables = FALSE, ...)
}, sealed = SEALED)

setMethod("duplicated", c(OPMS, "ANY"), function(x, incomparables,
    what = c("all", "csv", "metadata"), ...) {
  selection <- tryCatch(match.arg(what), error = function(e) "other")
  duplicated(x = case(selection,
    all = x@plates,
    csv = cbind(csv_data(x, what = "setup_time"),
      csv_data(x, what = "position")),
    metadata = metadata(x),
    other = metadata(object = x, key = what)
  ), incomparables = incomparables, ...)
}, sealed = SEALED)

setGeneric("anyDuplicated")

setMethod("anyDuplicated", c(OPM, "ANY"), function(x, incomparables, ...) {
  0L
}, sealed = SEALED)

setMethod("anyDuplicated", c(OPMS, "missing"), function(x, incomparables, ...) {
  anyDuplicated(x = x, incomparables = FALSE, ...)
}, sealed = SEALED)

setMethod("anyDuplicated", c(OPMS, "ANY"), function(x, incomparables, ...) {
  dups <- which(duplicated(x = x, incomparables = incomparables, ...))
  case(length(dups), 0L, dups[1L])
}, sealed = SEALED)

lapply(c(
    #+
    aggregated,
    discretized,
    has_aggr,
    has_disc,
    hours,
    measurements,
    metadata,
    filename, # deprecated
    position, # deprecated
    setup_time, # deprecated
    well
    #-
  ), FUN = function(func_) {
  setMethod(func_, OPMS, function(object, ...) {
    simplify_conditionally(lapply(object@plates, FUN = func_, ...))
  }, sealed = SEALED)
})

lapply(c(
    #+
    aggregated,
    discretized,
    has_aggr,
    has_disc,
    hours,
    measurements,
    metadata,
    plate_type,
    well
    #-
  ), FUN = function(func_) {
  setMethod(func_, MOPMX, function(object, ...) {
    simplify_conditionally(lapply(object@.Data, FUN = func_, ...))
  }, sealed = SEALED)
})

setGeneric("%k%", function(x, table) standardGeneric("%k%"))

setMethod("%k%", c("character", WMD), function(x, table) {
  all(x %in% names(table@metadata))
}, sealed = SEALED)

setMethod("%k%", c("list", WMD), function(x, table) {
  contains(table@metadata, x, values = FALSE)
}, sealed = SEALED)

setMethod("%k%", c(WMD, WMD), function(x, table) {
  contains(table@metadata, x@metadata, values = FALSE)
}, sealed = SEALED)

setMethod("%k%", c("formula", WMD), function(x, table) {
  tryCatch({
    eval(x[[length(x)]], table@metadata, parent.frame())
    TRUE
  }, error = function(e) FALSE)
}, sealed = SEALED)

setMethod("%k%", c("expression", WMD), function(x, table) {
  tryCatch({
    eval(x, table@metadata, parent.frame())
    TRUE
  }, error = function(e) FALSE)
}, sealed = SEALED)

setGeneric("%K%", function(x, table) standardGeneric("%K%"))

setMethod("%K%", c("character", WMD), function(x, table) {
  if (!length(x))
    return(TRUE) # for consistency with %k%
  tryCatch(!is.null(table@metadata[[x]]), error = function(e) FALSE)
}, sealed = SEALED)

setMethod("%K%", c("list", WMD), function(x, table) {
  contains(table@metadata, x, values = FALSE)
}, sealed = SEALED)

setMethod("%K%", c(WMD, WMD), function(x, table) {
  contains(table@metadata, x@metadata, values = FALSE)
}, sealed = SEALED)

setMethod("%K%", c("formula", WMD), function(x, table) {
  tryCatch({
    eval(x[[length(x)]], table@metadata, baseenv())
    TRUE
  }, error = function(e) FALSE)
}, sealed = SEALED)

setMethod("%K%", c("expression", WMD), function(x, table) {
  tryCatch({
    eval(x, table@metadata, baseenv())
    TRUE
  }, error = function(e) FALSE)
}, sealed = SEALED)

setGeneric("%q%", function(x, table) standardGeneric("%q%"))

setMethod("%q%", c("character", WMD), function(x, table) {
  if (length(keys <- names(x)) == 0L && length(x) > 0L)
    return(FALSE)
  got <- unlist(table@metadata[keys])
  length(x) == length(got) && all(x == got)
}, sealed = SEALED)

setMethod("%q%", c("list", WMD), function(x, table) {
  contains(table@metadata, x, values = TRUE, exact = FALSE)
}, sealed = SEALED)

setMethod("%q%", c(WMD, WMD), function(x, table) {
  contains(table@metadata, x@metadata, values = TRUE, exact = FALSE)
}, sealed = SEALED)

setMethod("%q%", c("formula", WMD), function(x, table) {
  eval(x[[length(x)]], table@metadata, parent.frame())
}, sealed = SEALED)

setMethod("%q%", c("expression", WMD), function(x, table) {
  eval(x, table@metadata, parent.frame())
}, sealed = SEALED)

setGeneric("%Q%", function(x, table) standardGeneric("%Q%"))

setMethod("%Q%", c("character", WMD), function(x, table) {
  if (length(keys <- names(x)) == 0L && length(x) > 0L)
    return(FALSE)
  all(vapply(keys, function(key) identical(x[[key]], table@metadata[[key]]),
    NA))
}, sealed = SEALED)

setMethod("%Q%", c("list", WMD), function(x, table) {
  contains(table@metadata, x, values = TRUE, exact = TRUE)
}, sealed = SEALED)

setMethod("%Q%", c(WMD, WMD), function(x, table) {
  contains(table@metadata, x@metadata, values = TRUE, exact = TRUE)
}, sealed = SEALED)

setMethod("%Q%", c("formula", WMD), function(x, table) {
  eval(x[[length(x)]], table@metadata, baseenv())
}, sealed = SEALED)

setMethod("%Q%", c("expression", WMD), function(x, table) {
  eval(x, table@metadata, baseenv())
}, sealed = SEALED)

lapply(c(
    #+
    `%k%`,
    `%K%`,
    `%q%`,
    `%Q%`
    #-
  ), FUN = function(func_) {
  setMethod(func_, c("factor", WMD), function(x, table) {
    func_(structure(as.character(x), names = names(x)), table)
  }, sealed = SEALED)
})

lapply(c(
    #+
    `%k%`,
    `%K%`,
    `%q%`,
    `%Q%`
    #-
  ), FUN = function(func_) {
  setMethod(func_, c("list", OPMS), function(x, table) {
    vapply(table@plates, func_, NA, x = x, USE.NAMES = FALSE)
  }, sealed = SEALED)
})

lapply(c(
    #+
    `%k%`,
    `%K%`,
    `%q%`,
    `%Q%`
    #-
  ), FUN = function(func_) {
  setMethod(func_, c(WMD, OPMS), function(x, table) {
    vapply(table@plates, func_, NA, x = x, USE.NAMES = FALSE)
  }, sealed = SEALED)
})

lapply(c(
    #+
    `%k%`,
    `%K%`,
    `%q%`,
    `%Q%`
    #-
  ), FUN = function(func_) {
  setMethod(func_, c("character", OPMS), function(x, table) {
    vapply(table@plates, func_, NA, x = x, USE.NAMES = FALSE)
  }, sealed = SEALED)
})

lapply(c(
    #+
    `%k%`,
    `%K%`,
    `%q%`,
    `%Q%`
    #-
  ), FUN = function(func_) {
  setMethod(func_, c("factor", OPMS), function(x, table) {
    vapply(table@plates, func_, NA, x = x, USE.NAMES = FALSE)
  }, sealed = SEALED)
})

lapply(c(
    #+
    `%k%`,
    `%K%`,
    `%q%`,
    `%Q%`
    #-
  ), FUN = function(func_) {
  setMethod(func_, c("formula", OPMS), function(x, table) {
    vapply(table@plates, func_, NA, x = x, USE.NAMES = FALSE)
  }, sealed = SEALED)
})

lapply(c(
    #+
    `%k%`,
    `%K%`,
    `%q%`,
    `%Q%`
    #-
  ), FUN = function(func_) {
  setMethod(func_, c("expression", OPMS), function(x, table) {
    vapply(table@plates, func_, NA, x = x, USE.NAMES = FALSE)
  }, sealed = SEALED)
})

lapply(c(
    #+
    `%k%`,
    `%K%`,
    `%q%`,
    `%Q%`
    #-
  ), FUN = function(func_) {
  setMethod(func_, c(WMD, "ANY"), function(x, table) {
    func_(table, x)
  }, sealed = SEALED)
})

lapply(c(
    #+
    `%k%`,
    `%K%`,
    `%q%`,
    `%Q%`
    #-
  ), FUN = function(func_) {
  setMethod(func_, c(OPMS, "ANY"), function(x, table) {
    func_(table, x)
  }, sealed = SEALED)
})

