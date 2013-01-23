

################################################################################
################################################################################
#
# Class definitions and associated functions
#


#' Update settings entries
#'
#' This converts old-style to new-style aggregation or discretization settings.
#'
#' @param x List.
#' @return List.
#' @keywords internal
#'
setGeneric("update_settings_list",
  function(x, ...) standardGeneric("update_settings_list"))

setMethod("update_settings_list", "list", function(x) {
  if (is.null(names(x)))
    stop("expected named list 'x'")
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


################################################################################


#' WMD class
#'
#' This is a virtual class facilitating the management of metadata. No objects
#' can be created from it because metadata without data make not much sense. It
#' is used by its child classes such as \code{\link{OPM}}, but it is not
#' directly applied by an \pkg{opm} user.
#'
#' @details \itemize{
#'   \item \acronym{WMD} is an acronym for \sQuote{with metadata}.
#'   \item Conceptually, this class treats metadata as arbitrarily nested lists
#'     with arbitrary content. Containers of objects that inherit from this
#'     class are not forced to contain the same metadata entries. Problems might
#'     arise if such data are queried and attempted to be converted to, e.g.,
#'     data frames because some values might be missing. But metadata can be
#'     queried beforehand for the keys as well as the values they contain, and
#'     other methods support setting, modifying and deleting metadata.
#'   \item For \code{\link{OPM}} and the other \pkg{opm} classes that use it,
#'     \sQuote{metadata} refers to information that, in contrast to, e.g.,
#'     \code{\link{csv_data}} must be added by the user \strong{after} reading
#'     OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} \acronym{CSV} files.
#'     Metadata might already be present in \acronym{YAML} files created by the
#'     \pkg{opm} package, however.
#' }
#'
#' @docType class
#' @export
#' @aliases WMD-class
#' @seealso methods::Methods
#' @family classes
#' @keywords methods classes
#'
setClass(WMD,
  representation = representation(metadata = "list"),
  contains = "VIRTUAL",
  sealed = SEALED
)


################################################################################


#' OPM class
#'
#' Class for holding single-plate
#' OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} phenotype microarray data
#' without aggregated values, but with information read from the original input
#' \acronym{CSV} files as well as an additional arbitrary amount of arbitrarily
#' organised metadata. \acronym{OPM} is an acronym for
#' \sQuote{OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} Phenotype
#' Microarray}.
#'
#' @details Objects of this class are usually created by inputting files with
#'   \code{\link{read_single_opm}} or \code{\link{read_opm}}.
#'
#' @note Regarding the coercion of this class to other classes (see the
#'   \code{coerce} methods listed above and \code{as} from the \pkg{methods}
#'   package), consider the following:
#'   \itemize{
#'     \item The coercion of this class (and its child classes) to a list (and
#'       vice versa) relies on a mapping between slot names and keys in the
#'       list, i.e. the list must be appropriately named. For instance, this is
#'       the mechanism when reading from and writing to \acronym{YAML}, see
#'       \code{\link{to_yaml}}.
#'     \item Coercions to other data frames and matrices first coerce the
#'       \code{\link{measurements}} and then add the other slots as attributes.
#'     \item Methods such as \code{\link{flatten}} might be more appropriate for
#'       converting \code{\link{OPM}} objects.
#'   }
#'
#' @docType class
#' @export
#' @aliases OPM-class
#' @seealso methods::Methods methods::new
#' @family classes
#' @keywords methods classes
#'
setClass(OPM,
  representation = representation(measurements = "matrix",
    csv_data = "character"),
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


################################################################################


#' Check OPM
#'
#' Check whether a matrix fulfils the requirements for \code{\link{OPM}}
#' measurements, or check whether a character vector fulfils the requirements
#' for \code{\link{OPM}} \acronym{CSV} data. Called when constructing an object
#' of the class.
#'
#' @param object Matrix or character vector.
#' @return Character vector with description of problems, empty if there are
#'   none.
#' @keywords internal
#'
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


################################################################################


#' Attach slots
#'
#' Attach the contents of all slots, except the measurements, to another object.
#' Useful in conversions (coercions).
#'
#' @param object \code{\link{OPM}} object.
#' @param other Arbitrary other object.
#' @return \code{other} with additional attributes.
#' @keywords internal
#' @note This method is deliberately \strong{not} defined for \code{\link{OPMS}}
#'   objects.
#'
setGeneric("attach_attr", function(object, ...) standardGeneric("attach_attr"))

setMethod("attach_attr", OPM, function(object, other) {
  for (name in setdiff(slotNames(object), "measurements"))
    attr(other, name) <- slot(object, name)
  other
}, sealed = SEALED)


################################################################################


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
  new(OPM, csv_data = unlist(from$csv_data), metadata = as.list(from$metadata),
    measurements = convert_measurements(from$measurements))
})


################################################################################


#' OPMA class
#'
#' Class for holding single-plate
#' OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} phenotype microarray data
#' together with aggregated values. For further details see its parent class,
#' \code{\link{OPM}}. \acronym{OPMA} is an acronym for \sQuote{acronym{OPM},
#' aggregated}.
#'
#' @details Objects of this class are usually created by calling
#'   \code{\link{do_aggr}} on an \code{\link{OPM}} object, or by inputting files
#'   with \code{\link{read_single_opm}} or \code{\link{read_opm}} if these files
#'   already contain aggregated data.
#'
#' @docType class
#' @export
#' @aliases OPMA-class
#' @seealso methods::Methods methods::new
#' @family classes
#' @keywords methods classes
#'
setClass(OPMA,
  representation = representation(aggregated = "matrix",
    aggr_settings = "list"),
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


################################################################################


#' Check OPMA
#'
#' Check whether a matrix fulfils the requirements for  \code{\link{OPMA}}
#' aggregated data, or check whether a list fulfils the requirements for
#' \code{\link{OPMA}} aggregation settings. Called when constructing an object
#' of the class.
#'
#' @param object Matrix of aggregated data or list describing the aggregation
#'   settings.
#' @param orig.data Matrix of original, non-aggregated data.
#' @param settings List or \code{NULL}. If a list, settings used for aggregating
#'   the data (currently only the \pkg{opm}-native programs are checked).
#' @return Character vector with description of problems, empty if there are
#'   none.
#' @keywords internal
#'
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
      bad <- got[got != map_grofit_names()]
      if (length(bad))
        errs <- c(errs, paste("missing row name in aggregated data:", bad))
    } else
      errs <- c(errs, sprintf("unknown aggregation method '%s'", method))
  }
  errs
}, sealed = SEALED)

# NB: this function is currently also called when checking @disc_settings
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


################################################################################
#
# Conversion functions: OPMA <=> other objects. For principle, see description
# of OPM class. Conversion of OPMA to matrix/data frame is just repeated here
# from OPM because otherwise some elements would be missing.
#

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
  convert_aggregated <- function(mat) {
    mat <- repair_na_strings(mat)
    mat <- as.matrix(as.data.frame(lapply(mat, unlist)))
    mat[, sort.int(colnames(mat)), drop = FALSE]
  }
  opm <- as(from, OPM)
  settings <- update_settings_list(as.list(from$aggr_settings))
  mat <- convert_aggregated(from$aggregated)
  new(OPMA, csv_data = csv_data(opm), measurements = measurements(opm),
    metadata = metadata(opm), aggr_settings = settings, aggregated = mat)
})


################################################################################


#' OPMD class
#'
#' Class for holding single-plate
#' OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} phenotype microarray data
#' together with aggregated and discretized values. For further details see its
#' parent class, \code{\link{OPMA}}. \acronym{OPMD} is an acronym for
#' \sQuote{\acronym{OPM}, discretized}.
#'
#' @details Objects of this class are usually created by calling
#'   \code{\link{do_disc}} on an \code{\link{OPMA}} object, or by inputting
#'   files with \code{\link{read_single_opm}} or \code{\link{read_opm}} if these
#'   files already contain discretized data.
#'
#' @docType class
#' @export
#' @aliases OPMD-class
#' @seealso methods::Methods methods::new
#' @family classes
#' @keywords methods classes
#'
setClass(OPMD,
  representation = representation(discretized = "logical",
    disc_settings = "list"),
  contains = OPMA,
  validity = function(object) {
    errs <- opmd_problems(object@disc_settings)
    errs <- c(errs, opmd_problems(object@aggregated, object@discretized))
    if (length(errs))
      errs
    else
      TRUE
  },
  sealed = SEALED
)


################################################################################


#' Check OPMD
#'
#' Check whether a matrix fulfils the requirements for  \code{\link{OPMD}}
#' discretized data, or check whether a list fulfils the requirements for
#' \code{\link{OPMD}} discretization settings. Called when constructing an
#' object of the class.
#'
#' @param object Matrix of original, non-discretized data, or list describing
#'   the discretization settings.
#' @param disc Vector of discretized data.
#' @return Character vector with description of problems, empty if there are
#'   none.
#' @keywords internal
#'
setGeneric("opmd_problems",
  function(object, ...) standardGeneric("opmd_problems"))

setMethod("opmd_problems", "list", function(object) {
  opma_problems(object)
}, sealed = SEALED)

setMethod("opmd_problems", "matrix", function(object, disc) {
  errs <- character()
  # uncomment this once numeric vectors are allowed, too:
  #if (!is.vector(disc) || !(is.numeric(disc) || is.logical(disc)))
  #  errs <- c(errs, "discretized data have wrong storage mode")
  if (!identical(names(disc), colnames(object)))
    errs <- c(errs, "discretized data have wrong names")
  if (length(errs))
    return(errs) # further tests are impossible in that case
  ok <- !is.na(disc)
  if (!identical(order(disc[ok], object["A", ok]), order(object["A", ok])))
    errs <- c(errs, "discretized data inconsistent with 'A' parameter")
  errs
}, sealed = SEALED)


################################################################################
#
# Conversion functions: OPMD <=> other objects. For principle, see description
# of OPM class. Conversion of OPMD to matrix/data frame is just repeated here
# from OPM because otherwise some elements would be missing.
#

setAs(from = OPMD, to = "matrix", function(from) {
  attach_attr(from, from@measurements)
})

setAs(from = OPMD, to = "data.frame", function(from) {
  attach_attr(from, as.data.frame(from@measurements))
})

setAs(from = OPMD, to = "list", function(from) {
  result <- as(as(from, OPMA), "list")
  result$discretized <- as.list(discretized(from))
  result$disc_settings <- disc_settings(from)
  result
})

setAs(from = "list", to = OPMD, function(from) {
  convert_discretized <- function(x) unlist(repair_na_strings(x, "logical"))
  opma <- as(from, OPMA)
  settings <- update_settings_list(as.list(from$disc_settings))
  discretized <- convert_discretized(from$discretized)
  new(OPMD, csv_data = csv_data(opma), measurements = measurements(opma),
    metadata = metadata(opma), aggr_settings = aggr_settings(opma),
    aggregated = aggregated(opma), discretized = discretized,
    disc_settings = settings)
})


################################################################################


#' OPMS class
#'
#' Class for holding multi-plate
#' OmniLog\eqn{\textsuperscript{\textregistered}}{(R)} phenotype microarray data
#' with or without aggregated values. The data may have been obtained from
#' distinct organisms and/or replicates, but \strong{must} correspond to the
#' same plate type and \strong{must} contain the same wells. Regarding the name:
#' \acronym{OPMS} is just the plural of \acronym{OPM}.
#'
#' @details Objects of this class are usually created by calling
#'   \code{\link{opms}} or other combination functions on \code{\link{OPM}} or
#'   \code{\link{OPM}}-derived objects, or by inputting files with
#'   \code{\link{read_opm}} if these files altogether contain more than a single
#'   plate.
#'
#' @note As a rule, OPMS has the same methods as the \code{\link{OPM}} class,
#'   but adapted to a collection of more than one \code{\link{OPM}} object.
#'   Also, OPMS can hold \code{\link{OPMD}} and \code{\link{OPMA}} as well as
#'   \code{\link{OPM}} objects, even though this is not indicated for all its
#'   methods in this manual.
#'
#' @docType class
#' @export
#' @aliases OPMS-class
#' @seealso methods::Methods methods::new
#' @family classes
#' @keywords methods classes
#'
setClass(OPMS,
  representation = representation(plates = "list"),
  validity = function(object) {
    if (length(errs <- opms_problems(object@plates)))
      errs
    else
      TRUE
  },
  sealed = SEALED
)


################################################################################


#' Check OPMS list
#'
#' Check whether a list fulfils the requirements for \code{\link{OPMS}}
#' \code{\link{plates}}. Called when constructing an object of that class.
#'
#' @param object List to be checked.
#' @return Character vector with description of problems, empty if there are
#'   none.
#' @keywords internal
#'
setGeneric("opms_problems",
  function(object, ...) standardGeneric("opms_problems"))

setMethod("opms_problems", "list", function(object) {
  errs <- character()
  if (length(object) < 2L) {
    errs <- c(errs, "less than two plates submitted")
    return(errs) # further checks are useless in that case
  }
  if (length(no.opm <- which(!vapply(object, is, logical(1L), OPM))) > 0L) {
    bad.classes <- unlist(lapply(object[no.opm], class))
    errs <- c(errs, paste("wrong class:", bad.classes))
    return(errs) # further checks are impossible in that case
  }
  if (!isTRUE(isuni <- is_uniform(vapply(object, plate_type, character(1L)))))
    errs <- c(errs, paste("plate types are not uniform:",
      paste(isuni, collapse = " <=> ")))
  if (!isTRUE(is_uniform(lapply(object, wells))))
    errs <- c(errs, "wells are not uniform")
  if (!length(errs) &&
      !isTRUE(is_uniform(lapply(object, FUN = hours, what = "all"))))
    warning("running times are not uniform")
  errs
}, sealed = SEALED)


################################################################################


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


################################################################################


# CMAT class: undocumented, as for internal use only.
#
setClass(CMAT,
  contains = "matrix",
  validity = function(object) {
    errs <- character()
    if (is.null(rownames(object)) || any(is.na(rownames(object))))
      errs <- c(errs, "missing row names")
    mode <- typeof(object)
    if (mode == "list") {
      mode <- unique.default(vapply(object, typeof, character(1L)))
      if (length(mode) > 1L)
        errs <- c(errs, "non-uniform list elements contained")
      if (any(vapply(object, length, integer(1L)) < 1L))
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


setAs(from = "matrix", to = CMAT, function(from) {
  new(CMAT, from) # overwritten to enforce consistency checks
})


################################################################################


#' MOA class
#'
#' This is a virtual class facilitating the implementation of functionality for
#' both matrices and arrays. Methods defined for objects from the class can be
#' applied to either kind of object, but this class is not directly dealt with
#' by an \pkg{opm} user.
#'
#' @details \sQuote{MOA} is an acronym for \sQuote{matrix or array}.
#'
#' @name MOA
#'
#' @docType class
#' @export
#' @aliases MOA-class
#' @seealso methods::Methods base::matrix base::array
#' @family classes
#' @keywords methods classes
#'
NULL

setClassUnion(MOA, c("matrix", "array"))


################################################################################


#' OPMX class
#'
#' This is a virtual class containing helper methods for plotting
#' \code{\link{OPM}} and \code{\link{OPMS}} objects. It is not directly applied
#' by an \pkg{opm} user. Regarding the name: \acronym{OPMX} stands for
#' \sQuote{\acronym{OPM} or \acronym{OPMS}}.
#'
#' @name OPMX
#'
#' @docType class
#' @export
#' @aliases OPMX-class
#' @seealso methods::Methods
#' @family classes
#' @keywords methods classes
#'
NULL

# Currently the child classes must provide plate_type() and minmax() for the
# methods to work
#
setClassUnion(OPMX, c(OPM, OPMS))


################################################################################


#' YAML_VIA_LIST class
#'
#' This is a virtual class facilitating the conversion to \acronym{YAML} format.
#' It can currently be used by any class that can be coerced to a list, but it
#' is not directly applied by an \pkg{opm} user.
#'
#' @name YAML_VIA_LIST
#'
#' @docType class
#' @export
#' @aliases YAML_VIA_LIST-class
#' @seealso methods::Methods
#' @family classes
#' @keywords methods classes
#'
NULL

setClassUnion(YAML_VIA_LIST, c(OPM, OPMS, "list"))


################################################################################
#
# The definitions of initialize() must be located after the class definitions
# to avoid a warning during the Roxygen2 runs.
#


#' Initialize
#'
#' Initialize methods for some classes.
#'
#' @param .Object \code{\link{OPM}} or \code{\link{OPMS}} object.
#' @param ... Additional arguments.
#' @return \code{\link{OPM}} or \code{\link{OPMS}} object.
#' @keywords internal
#'
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


################################################################################

