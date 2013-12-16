setClass(WMD,
  slots = c(metadata = "list"),
  contains = "VIRTUAL",
  sealed = SEALED
)

NULL

setClassUnion(MOA, c("matrix", "array"))

NULL

setClassUnion(FOE, c("formula", "expression"))

setClass(OPM,
  slots = c(measurements = "matrix", csv_data = "character"),
  contains = WMD,
  validity = function(object) {
    errs <- c(opm_problems(object@measurements), opm_problems(object@csv_data))
    if (length(errs))
      errs
    else
      TRUE
  },
  sealed = SEALED
)

setClass(OPMA,
  slots = c(aggregated = "matrix", aggr_settings = "list"),
  contains = OPM,
  validity = function(object) {
    settings <- object@aggr_settings
    if (length(errs <- opma_problems(settings)))
      settings <- NULL # => no settings-based checks of the matrix
    errs <- c(errs, opma_problems(object@aggregated, object@measurements,
      settings))
    if (length(errs))
      errs
    else
      TRUE
  },
  sealed = SEALED
)

setClass(OPMD,
  slots = c(discretized = "logical", disc_settings = "list"),
  contains = OPMA,
  validity = function(object) {
    errs <- opmd_problems(object@disc_settings)
    errs <- c(errs, opmd_problems(object@aggregated, object@discretized,
      object@disc_settings[[c(OPTIONS, "parameter")]]))
    if (length(errs))
      errs
    else
      TRUE
  },
  sealed = SEALED
)

setClass(OPMS,
  slots = c(plates = "list"),
  validity = function(object) {
    if (length(errs <- opms_problems(object@plates)))
      errs
    else
      TRUE
  },
  sealed = SEALED
)

setClass(MOPMX,
  contains = "list",
  prototype = structure(list(), names = character()),
  validity = function(object) {
    if (all(vapply(object@.Data, is, NA, OPMX)))
      TRUE
    else
      "not ell elements inherit from 'OPMX'"
  }, sealed = SEALED
)

NULL

setClassUnion(OPMX, c(OPM, OPMS))

NULL

setClassUnion(YAML_VIA_LIST, c(OPM, OPMS, "list"))

setClass(CMAT,
  contains = "matrix",
  validity = function(object) {
    errs <- character()
    if (is.null(rownames(object)) || any(is.na(rownames(object))))
      errs <- c(errs, "missing row names")
    mode <- typeof(object)
    if (mode == "list") {
      mode <- unique.default(vapply(object, typeof, ""))
      if (length(mode) > 1L)
        errs <- c(errs, "non-uniform list elements contained")
      if (any(vapply(object, length, 0L) < 1L))
        errs <- c(errs, "empty list elements contained")
    }
    if (!all(mode %in% c("character", "integer", "double", "logical")))
      errs <- c(errs, sprintf("unsupported storage mode: '%s'", mode))
    if (length(errs))
      errs
    else
      TRUE
  },
  sealed = SEALED
)

setMethod("initialize", OPM, function(.Object, ...) {
  .Object <- callNextMethod()
  plate.type <- CSV_NAMES[["PLATE_TYPE"]]
  .Object@csv_data[plate.type] <- plate_type(.Object@csv_data[plate.type])
  .Object
}, sealed = SEALED)

setMethod("initialize", OPMS, function(.Object, ...) {
  .Object <- callNextMethod()
  names(.Object@plates) <- NULL
  .Object
}, sealed = SEALED)

setMethod("initialize", CMAT, function(.Object, ...) {
  map2int <- function(x) match(toupper(x), CHARACTER_STATES)
  .Object <- callNextMethod()
  switch(typeof(.Object),
    character = {
      .Object[] <- map2int(.Object)
      storage.mode(.Object) <- "integer"
    },
    list = {
      if (length(.Object) && typeof(.Object[[1L]]) == "character")
        .Object[] <- lapply(.Object, map2int)
      .Object[] <- lapply(.Object, sort.int, na.last = TRUE)
    },
    logical = .Object[] <- .Object + 1L
  )
  .Object
}, sealed = SEALED)

setGeneric("opm_problems",
  function(object, ...) standardGeneric("opm_problems"))

setMethod("opm_problems", "matrix", function(object) {
  errs <- character()
  # Check content
  if (any(is.na(object)))
    errs <- c(errs, "matrix contains NAs")
  if (!is.numeric(object))
    errs <- c(errs, "matrix is not numeric")
  # Check row names
  if (!is.null(rownames(object)))
    errs <- c(errs, "non-empty row names")
  # Check column names
  col.names <- colnames(object)
  pattern <- sprintf("^([A-H][01]\\d|%s)$", HOUR)
  if (length(bad <- grep(pattern, col.names, invert = TRUE, value = TRUE)))
    errs <- c(errs, paste("invalid entry in header:", bad[1L]))
  if (bad <- anyDuplicated(col.names))
    errs <- c(errs, paste("duplicated entry in header:", col.names[bad]))
  if (col.names[1L] != HOUR)
    errs <- c(errs, paste("first entry in header must be", HOUR))
  if (is.unsorted(col.names[-1L]))
    errs <- c(errs, "names of wells must be sorted")
  errs
}, sealed = SEALED)

setMethod("opm_problems", "character", function(object) {
  errs <- character()
  wanted <- CSV_NAMES[c("FILE", "PLATE_TYPE", "POS", "SETUP")]
  missing <- !wanted %in% names(object)
  if (any(missing))
    errs <- c(errs, sprintf("need '%s' in CSV data", wanted[missing]))
  errs
}, sealed = SEALED)

setGeneric("opma_problems",
  function(object, ...) standardGeneric("opma_problems"))

setMethod("opma_problems", "matrix", function(object, orig.data, settings) {
  errs <- character()
  # Check content. In contrast to the raw measurements we have to allow NAs.
  if (!is.numeric(object))
    errs <- c(errs, "aggregated values are not numeric")
  # Compare column names with non-aggregated data
  cols <- colnames(object)
  if (length(bad <- cols[colnames(orig.data)[-1] != cols]))
    errs <- c(errs, paste("unknown column name in aggregated data:", bad))
  if (!nrow(object)) {
    errs <- c(errs, "no rows in aggregated data")
    return(errs) # further checks are impossible in that case
  }
  if (!is.null(settings) && settings[[SOFTWARE]] == opm_string()) {
    method <- settings[[METHOD]]
    if (method %in% KNOWN_METHODS$aggregation) {
      # Check row names
      got <- rownames(object)
      bad <- got[got != map_param_names()]
      if (length(bad))
        errs <- c(errs, paste("missing row name in aggregated data:", bad))
    } else
      errs <- c(errs, sprintf("unknown aggregation method '%s'", method))
  }
  errs
}, sealed = SEALED)

setMethod("opma_problems", "list", function(object) {
  check_string <- function(what) {
    if (length(x <- object[[what]]) == 1L && is.character(x) && !is.na(x))
      character()
    else
      sprintf("'%s' entry not a non-NA character scalar", what)
  }
  errs <- character()
  for (name in c(SOFTWARE, METHOD, VERSION))
    errs <- c(errs, check_string(name))
  if (!is.list(options <- object[[OPTIONS]]) || !length(options))
    errs <- c(errs, sprintf("non-empty list as '%s' entry needed", OPTIONS))
  else if (is.null(names(options)) || any(!nzchar(names(options))))
    errs <- c(errs, sprintf("all '%s' elements must be named", OPTIONS))
  bad <- setdiff(names(object), c(METHOD, OPTIONS, SOFTWARE, VERSION))
  if (length(bad))
    errs <- c(errs, paste("unknown settings key:", bad[1L]))
  errs
}, sealed = SEALED)

setGeneric("opmd_problems",
  function(object, ...) standardGeneric("opmd_problems"))

setMethod("opmd_problems", "list", function(object) {
  opma_problems(object)
}, sealed = SEALED)

setMethod("opmd_problems", "matrix", function(object, disc, param) {
  errs <- character()
  # uncomment this once numeric vectors are allowed, too:
  #if (!is.vector(disc) || !(is.numeric(disc) || is.logical(disc)))
  #  errs <- c(errs, "discretized data have wrong storage mode")
  if (!identical(names(disc), colnames(object)))
    errs <- c(errs, "discretized data have wrong names")
  if (length(param) != 1L || !param %in% rownames(object))
    errs <- c(errs, "missing name of discretized parameter")
  if (length(errs))
    return(errs) # further tests are impossible in these cases
  ok <- !is.na(disc)
  ok <- identical(order(disc[ok], object[param, ok]), order(object[param, ok]))
  if (!ok) {
    text <- sprintf("discretized data inconsistent with '%s' parameter", param)
    if (get("strict.OPMD", OPM_OPTIONS))
      errs <- c(errs, text)
    else
      warning(text)
  }
  errs
}, sealed = SEALED)

setGeneric("opms_problems",
  function(object, ...) standardGeneric("opms_problems"))

setMethod("opms_problems", "list", function(object) {
  errs <- character()
  if (length(object) < 2L) {
    errs <- c(errs, "less than two plates submitted")
    return(errs) # further checks are useless in that case
  }
  if (length(no.opm <- which(!vapply(object, is, NA, OPM))) > 0L) {
    bad.classes <- unlist(lapply(object[no.opm], class))
    errs <- c(errs, paste("wrong class:", bad.classes))
    return(errs) # further checks are impossible in that case
  }
  if (!isTRUE(isuni <- is_uniform(vapply(object, plate_type, ""))))
    errs <- c(errs, paste("plate types are not uniform:",
      paste0(isuni, collapse = " <=> ")))
  if (!isTRUE(is_uniform(lapply(object, wells))))
    errs <- c(errs, "wells are not uniform")
  if (!length(errs) &&
      !isTRUE(is_uniform(lapply(object, FUN = hours, what = "all"))))
    warning("running times are not uniform")
  errs
}, sealed = SEALED)

setGeneric("attach_attr", function(object, ...) standardGeneric("attach_attr"))

setMethod("attach_attr", OPM, function(object, other) {
  for (name in setdiff(slotNames(object), "measurements"))
    attr(other, name) <- slot(object, name)
  other
}, sealed = SEALED)

setGeneric("update_settings_list",
  function(x, ...) standardGeneric("update_settings_list"))

setMethod("update_settings_list", "list", function(x) {
  if (is.null(names(x)))
    stop("expected named list 'x'")
  x <- map_names(x, rescue_dots)
  if (!length(software <- x[[SOFTWARE]])) {
    x[[SOFTWARE]] <- software <- opm_string()
    warning(sprintf("inserting '%s' as '%s' entry", software, SOFTWARE))
  }
  if (!length(version <- x[[VERSION]])) {
    x[[VERSION]] <- version <- if (software == opm_string())
      opm_string(version = TRUE)[2L]
    else
      UNKNOWN_VERSION
    warning(sprintf("inserting '%s' as '%s' entry", version, VERSION))
  }
  if (m <- match(PROGRAM, names(x), nomatch = 0L)) {
    names(x)[m] <- METHOD
    warning(sprintf("renaming '%s' to '%s'", PROGRAM, METHOD))
  }
  x
}, sealed = SEALED)

setGeneric("rename_wells",
  function(object, keys) standardGeneric("rename_wells"))

setMethod("rename_wells", c(OPM, "ANY"), function(object, keys) {
  colnames(object@measurements)[-1L] <- keys
  object
}, sealed = SEALED)

setMethod("rename_wells", c(OPMA, "ANY"), function(object, keys) {
  object <- callNextMethod()
  colnames(object@aggregated) <- keys
  object
}, sealed = SEALED)

setMethod("rename_wells", c(OPMS, "ANY"), function(object, keys) {
  object <- callNextMethod()
  names(object@discretized) <- keys
  object
}, sealed = SEALED)

setAs(from = OPM, to = "matrix", function(from) {
  attach_attr(from, from@measurements)
})

setAs(from = OPM, to = "data.frame", function(from) {
  attach_attr(from, as.data.frame(from@measurements))
})

setAs(from = OPM, to = "list", function(from) {
  list(metadata = metadata(from), csv_data = as.list(csv_data(from)),
    measurements = as.list(as.data.frame(measurements(from))))
})

setAs(from = "list", to = OPM, function(from) {
  convert_measurements <- function(mat) {
    mat <- must(do.call(cbind, lapply(mat, as.numeric)))
    if (length(hour.pos <- which(colnames(mat) == HOUR)) != 1L)
      stop("uninterpretable column names in list element 'measurements'")
    sorted.names <- c(colnames(mat)[hour.pos],
      sort.int(colnames(mat)[-hour.pos]))
    mat[, sorted.names, drop = FALSE]
  }
  md <- repair_na_strings.list(as.list(from$metadata), "character")
  new(OPM, csv_data = map_names(unlist(from$csv_data), rescue_dots),
    metadata = map_names(md, rescue_dots),
    measurements = convert_measurements(from$measurements))
})

setAs(from = OPMA, to = "matrix", function(from) {
  attach_attr(from, from@measurements)
})

setAs(from = OPMA, to = "data.frame", function(from) {
  attach_attr(from, as.data.frame(from@measurements))
})

setAs(from = OPMA, to = "list", function(from) {
  result <- as(as(from, OPM), "list")
  result$aggregated <- apply(aggregated(from), MARGIN = 2L, FUN = as.list)
  result$aggr_settings <- aggr_settings(from)
  result
})

setAs(from = "list", to = OPMA, function(from) {
  select_aggr <- function(x, wanted) {
    x <- repair_na_strings(lapply(x, `[`, unlist(map_param_names())))
    x <- do.call(cbind, x[wanted])
    must(mode(x) <- "numeric")
    x # should now be matrix, reduced to the known wells, parameters and CIs
  }
  x <- as(from, OPM)
  new(OPMA, measurements = measurements(x),
    csv_data = csv_data(x), metadata = metadata(x),
    aggregated = select_aggr(from$aggregated, colnames(x@measurements)[-1L]),
    aggr_settings = update_settings_list(as.list(from$aggr_settings)))
})

setAs(from = OPMD, to = "matrix", function(from) {
  attach_attr(from, from@measurements)
})

setAs(from = OPMD, to = "data.frame", function(from) {
  attach_attr(from, as.data.frame(from@measurements))
})

setAs(from = OPMD, to = "list", function(from) {
  result <- as(as(from, OPMA), "list")
  result$discretized <- as.list(from@discretized)
  result$disc_settings <- from@disc_settings
  result
})

setAs(from = "list", to = OPMD, function(from) {
  # up to official release opm 0.10.0, the discretized curve parameter had
  # not been included in the discretization settings
  repair_missing_parameter <- function(x) {
    if (x[[SOFTWARE]] != opm_string())
      return(x)
    if (is.null(x[[c(OPTIONS, "parameter")]])) {
      warning("assuming discretized parameter is opm_opt('curve.param')")
      x[[c(OPTIONS, "parameter")]] <- opm_opt("curve.param")
    }
    x
  }
  x <- as(from, OPMA)
  settings <- update_settings_list(as.list(from$disc_settings))
  settings <- repair_missing_parameter(settings)
  discretized <- from$discretized[colnames(x@aggregated)]
  discretized <- unlist(repair_na_strings(discretized, "logical"))
  new(OPMD, csv_data = csv_data(x), measurements = measurements(x),
    metadata = metadata(x), aggr_settings = aggr_settings(x),
    aggregated = aggregated(x), discretized = discretized,
    disc_settings = settings)
})

setAs(from = OPMS, to = "list", function(from) {
  lapply(from@plates, as, Class = "list")
})

setAs(from = "list", to = OPMS, function(from) {
  opmd.slots <- setdiff(slotNames(OPMD), opma.slots <- slotNames(OPMA))
  opma.slots <- setdiff(opma.slots, slotNames(OPM))
  new(OPMS, plates = lapply(from, FUN = function(x) {
    as(x, if (all(opma.slots %in% names(x)))
      if (all(opmd.slots %in% names(x)))
        OPMD
      else
        OPMA
      else
        OPM)
  }))
})

setAs(from = "list", to = MOPMX, function(from) {
  new(MOPMX, from)
})

setAs(from = OPMX, to = MOPMX, function(from) {
  new(MOPMX, list(from))
})

setAs(from = MOPMX, to = OPMX, function(from) {
  if (length(from) != 1L)
    stop("conversion impossible: number of elements is not 1")
  from[[1L]]
})

setAs(from = "matrix", to = CMAT, function(from) {
  new(CMAT, from) # overwritten to enforce consistency checks
})

