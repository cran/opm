

################################################################################
################################################################################
#
# Class definitions
#


#' WMD class
#'
#' This is a virtual class facilitating the management of metadata. No objects
#' can be created from it because metadata without data make not much sense. It
#' is used by its child classes such as \code{\link{OPM}}, but it is not
#' directly applied by an \pkg{opm} user.
#'
#' @details \itemize{
#'   \item \sQuote{WMD} is an acronym for \sQuote{with metadata}.
#'   \item Conceptually, this class treats metadata as arbitrarily nested
#'     lists with arbitrary content. Containers of objects that inherit from
#'     this class are not forced to contain the same metadata entries. Problems
#'     might arise if such data are queried and attempted to be converted to,
#'     e.g., data frames because some values might be missing. But metadata can
#'     be queried beforehand for the keys as well as the values they contain,
#'     and other methods support setting, modifying and deleting metadata.
#'   \item For \code{\link{OPM}} and the other \pkg{opm} classes that use it,
#'     \sQuote{metadata} refers to information that, in contrast to, e.g.,
#'     \code{\link{csv_data}} must be added by the user \strong{after} reading
#'     OmniLog(R) CSV files. Metadata might already be present in YAML files
#'     created by the \pkg{opm} package, however.
#' }
#'
#' @name WMD
#'
#' @docType class
#' @seealso Methods
#' @export
#' @family classes
#' @keywords methods
#'
setClass(WMD,
  representation = representation(metadata = "list"),
  contains = "VIRTUAL",
  sealed = SEALED
)


################################################################################


#' OPM class
#'
#' Class for holding single-plate OmniLog(R) phenotype microarray data without
#' aggregated values, but with information read from the original input CSV
#' files as well as an additional arbitrary amount of arbitrarily organized
#' metadata. \sQuote{OPM} is an acronym for \sQuote{OmniLog(R) Phenotype
#' Microarray}.
#'
#' @docType class
#'
#' @note Regarding the coercion of this class to other classes (see the
#'   \code{coerce} methods listed above and \code{as} from the \pkg{methods}
#'   package), consider the following:
#'   \itemize{
#'     \item The coercion of this class (and its child classes) to a list (and
#'       vice versa) relies on a mapping between slot names and keys in the
#'       list, i.e. the list must be appropriately named. For instance, this is
#'       the mechanism when reading from and writing to YAML, see
#'       \code{\link{to_yaml}}.
#'     \item Coercions to other data frames and matrices first coerce the
#'       \code{\link{measurements}} and then add the other slots as attributes.
#'     \item Methods such as \code{\link{flatten}} might be more
#'       appropriate for converting \code{\link{OPM}} objects.
#'   }
#' @export
#' @seealso Methods
#' @family classes
#' @keywords methods
#'
setClass(OPM,
    representation = representation(
    measurements = "matrix",
    csv_data = "character"
  ),
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


setAs(from = OPM, to = "matrix", function(from) {
  attach_attr(from, from@measurements)
})


setAs(from = OPM, to = "data.frame", function(from) {
  attach_attr(from, as.data.frame(from@measurements))
})


setAs(from = OPM, to = "list", function(from) {
  list(
    metadata = repair_names(metadata(from), fill = TRUE),
    csv_data = as.list(csv_data(from)),
    measurements = as.list(as.data.frame(measurements(from)))
  )
})


################################################################################


#' OPMA class
#'
#' Class for holding single-plate OmniLog(R) phenotype microarray data
#' together with aggregated values. For further details see its parent class,
#' \code{\link{OPM}}. \sQuote{OPMA} is an acronym for \sQuote{OPM, aggregated}.
#'
#' @docType class
#'
#' @export
#' @seealso Methods
#' @family classes
#' @keywords methods
#'
setClass(OPMA,
  representation = representation(
    aggregated = "matrix",
    aggr_settings = "list"
  ),
  contains = OPM,
  validity = function(object) {
    errs <- opma_problems(object@aggr_settings)
    errs <- c(errs, opma_problems(object@aggregated, object@measurements,
      object@aggr_settings[[PROGRAM]]))
    if (length(errs))
      errs
    else
      TRUE
  },
  sealed = SEALED
)


# Conversion functions: OPMA => other objects. For principle, see description
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


################################################################################


#' OPMS class
#'
#' Class for holding multi-plate OmniLog(R) phenotype microarray data with or
#' without aggregated values. The data may have been obtained from distinct
#' organisms and/or replicates, but \strong{must} correspond to the same plate
#' type and \strong{must} contain the same wells. Regarding the name:
#' \sQuote{OPMS} is just the plural of \sQuote{OPM}.
#'
#' @docType class
#'
#' @export
#' @note As a rule, OPMS has the same methods as the \code{\link{OPM}} class,
#'   but adapted to a collection of more than one \code{\link{OPM}} object.
#'   Only the additional ones and those with special arguments and/or behaviors
#'   are documented in detail. Also, OPMS can hold \code{\link{OPMA}} as well
#'   as \code{\link{OPM}} objects, even though this is not indicated for all its
#'   methods in this manual.
#' @family classes
#' @seealso Methods
#' @keywords methods
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


setAs(from = OPMS, to = "list", function(from) {
  lapply(from@plates, as, Class = "list")
})


################################################################################


#' MOA class
#'
#' This is a virtual class facilitating the implementation of functionality for
#' both matrices and arrays. Methods defined for objects from the class can be
#' applied to either kind of object, but this class is not directly dealt with
#' by an \pkg{opm} user.
#'
#' @details
#'   \sQuote{MOA} is an acronym for \sQuote{matrix or array}.
#'
#' @name MOA
#'
#' @docType class
#' @export
#' @family classes
#' @keywords methods
#'
NULL

setClassUnion(MOA, c("matrix", "array"))


################################################################################


#' OPMX class
#'
#' This is a virtual class containing helper methods for plotting
#' \code{\link{OPM}} and \code{\link{OPMS}} objects. It is not directly applied
#' by an \pkg{opm} user. Regarding the name: \sQuote{OPMX} stands for
#' \sQuote{OPM or OPMS}.
#'
#' @name OPMX
#'
#' @docType class
#' @export
#' @seealso Methods
#' @family classes
#' @keywords methods
#'
NULL

# Currently the child classes must provide plate_type() and minmax() for the
# methods to work
#
setClassUnion(OPMX, c(OPM, OPMS))


################################################################################


#' YAML_VIA_LIST class
#'
#' This is a virtual class facilitating the conversion to YAML format. It can
#' currently be used by any class that can be coerced to a list, but it is not
#' directly applied by an \pkg{opm} user.
#'
#' @name YAML_VIA_LIST
#'
#' @docType class
#' @export
#' @family classes
#' @keywords methods
#'
NULL

setClassUnion(YAML_VIA_LIST, c(OPM, OPMS, "list"))


################################################################################
