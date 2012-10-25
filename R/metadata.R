
################################################################################
################################################################################
#
# Metadata functions
#


setGeneric("include_metadata",
  function(object, ...) standardGeneric("include_metadata"))
#' Add metadata (from file or data frame)
#'
#' Include metadata by mapping CSV data and column names in a data frame.
#'
#' @param object \code{\link{OPM}} object.
#' @param md Dataframe containing keys as column names, or name of file
#'   from which to read the data frame. Handled by \code{\link{to_metadata}}.
#' @param keys Character vector.
#' @param replace Logical scalar indicating whether the previous metadata, if
#'   any, shall be replaced by the novel ones, or whether these shall be
#'   appended.
#' @param skip.failure Logical scalar. Do not stop with an error message if
#'   (unambiguous) selection is impossible but raise a warning only?
#' @param remove.keys Logical scalar. When including \code{md} in the metadata,
#'   discard the \code{keys} columns?
#' @param ... Optional argument passed to \code{\link{to_metadata}}.
#' @export
#' @return Novel \code{\link{OPM}} object.
#' @family metadata-functions
#' @keywords manip
#' @examples
#' data(vaas_1)
#' (x <- collect_template(vaas_1, add.cols = "Location")) # generate data frame
#' x[1, "Location"] <- "Braunschweig" # insert additional information
#' copy <- include_metadata(vaas_1, x) # include the data in new OPM object
#' stopifnot(is.null(metadata(vaas_1, "Location")))
#' stopifnot(identical(metadata(copy, "Location"), "Braunschweig"))
#'
setMethod("include_metadata", OPM, function(object, md,
    keys = opm_opt("csv.keys"), replace = FALSE, skip.failure = FALSE,
    remove.keys = TRUE, ...) {

  selection <- as.list(csv_data(object, keys))

  # Get and check metadata.
  md <- to_metadata(md, ...)
  absent.keys <- setdiff(keys, colnames(md))
  if (length(absent.keys) > 0L)
    stop("key missing in 'metadata': ", absent.keys[1L])

  # Try to select the necessary information from the metadata.
  found <- pick_from(md, selection)
  msg <- if ((nr <- nrow(found)) == 1L)
    NULL
  else if (nr == 0L)
    pkgutils::listing(selection,
      header = "could not find this key/value combination in 'metadata':")
  else
    pkgutils::listing(selection,
      header = "the selection resulted in more than one row for:")

  # Failures.
  if (!is.null(msg)) {
    if (skip.failure) {
      warning(msg)
      return(object)
    } else
      stop(msg)
  }

  # Success.
  wanted <- colnames(found)
  if (remove.keys)
    wanted <- setdiff(wanted, keys)
  found <- as.list(found[, wanted, drop = FALSE])
  result <- object
  result@metadata <- if (replace)
    found
  else
    c(metadata(result), found)

  result

}, sealed = SEALED)

setMethod("include_metadata", OPMS, function(object, ...) {
  new(OPMS, plates = lapply(object@plates, FUN = include_metadata, ...))
}, sealed = SEALED)


################################################################################
################################################################################
#
# Setter methods
#


setGeneric("metadata<-",
  function(object, key, ..., value) standardGeneric("metadata<-"))
#' Replace metadata
#'
#' Set the meta-information stored together with the data. The
#' \code{\link{OPMS}} methods set the meta-information stored together with the
#' measurements for all plates at once (but can address the plates
#' individually if \code{value} is a formula or a data frame, see below).
#'
#' @name metadata.set
#' @aliases metadata<-
#'
#' @param object \code{\link{WMD}} or \code{\link{OPMS}} object..
#' @param key Missing, numeric scalar, character vector, factor, or list.
#'   If missing, replace all metadata by \code{value} (unless \code{value} is
#'   a formula that specifies the key to replace).
#'   If a numeric scalar, then if positive, prepend \code{value} to
#'   old metadata. If negative, append \code{value} to old metadata. If zero,
#'   replace old metadata entirely by \code{value}.
#'   If a list, treat it as list of keys; expect \code{value} to be a list
#'   of corresponding metadata values to be set. Names are replaced by the
#'   values of either list if they are missing.
#'   If a character vector, use it as key and set/replace this metadata
#'   entry to/by \code{value}. It is an error if \code{key} has zero length.
#'   If it contains more than one entry, a nested query is done. See \code{[[}
#'   from the \pkg{base} package for details. The factor method calls the
#'   character method after converting \code{key} to mode \sQuote{character}.
#' @param value If \code{key} is a character vector, this can be arbitrary
#'   value(s) to be included in the metadata (if \code{NULL}, this
#'   metadata entry is deleted). If \code{key} is otherwise, \code{value} must
#'   be list of values to be prepended, appended or set as metadata,
#'   either entirely or specifically, depending on \code{key}. Formulas can
#'   also be used as \code{value}. In that case, the formula can specify the
#'   key to be replaced. See the examples below and \code{\link{map_values}}
#'   for details. If \code{object} is of class \sQuote{OPMS}, \code{value} can
#'   be a data frame whose number of rows must be equal to the number of
#'   plates. Metadata to be set will then be selected from each individual row
#'   in turn and in input order.
#'
#' @return \code{value}.
#' @export
#' @exportMethod "metadata<-"
#' @family metadata-functions
#' @keywords manip
#' @examples
#'
#' ############################################################
#' #
#' # WMD methods
#' data(vaas_1)
#'
#' # WMD/missing/list method
#' copy <- vaas_1
#' new.md <- list(Species = "Thermomicrobium roseum")
#' metadata(copy) <- new.md
#' stopifnot(identical(metadata(copy), new.md))
#'
#' # WMD/missing/formula method (operates on previous entries!)
#' copy <- vaas_1
#' metadata(copy) <- Organism ~ paste(Species, Strain)
#' (x <- metadata(copy, "Organism"))
#' stopifnot(is.null(metadata(vaas_1, "Organism")), !is.null(x))
#'
#' # WMD/numeric/list method
#' copy <- vaas_1
#' metadata(copy, 1) <- list(Authors = "Vaas et al.")
#' stopifnot(length(metadata(copy)) > length(metadata(vaas_1)))
#'
#' # WMD/list/list method
#' copy <- vaas_1
#' stopifnot(identical(metadata(copy, "Species"), "Escherichia coli"))
#'
#' # You can use this to translate the keys on-the-fly...
#' metadata(copy, list(Organism = "Species")) <- list(
#'   Organism = "Bacillus subtilis")
#' stopifnot(length(metadata(copy)) == length(metadata(vaas_1)))
#' stopifnot(identical(metadata(copy, "Species"), "Bacillus subtilis"))
#' stopifnot(is.null(metadata(copy, "Organism"))) # this was not set!
#'
#' # ...but you need not
#' metadata(copy, list("Species")) <- list(Species = "Yersinia pestis")
#' stopifnot(length(metadata(copy)) == length(metadata(vaas_1)))
#' stopifnot(identical(metadata(copy, "Species"), "Yersinia pestis"))
#'
#' # Names need not be duplicated
#' metadata(copy, list("Species")) <- list("Gen. sp.")
#' stopifnot(length(metadata(copy)) == length(metadata(vaas_1)))
#' stopifnot(identical(metadata(copy, "Species"), "Gen. sp."))
#'
#' # ...but this would delete the entry because nothing would be found in
#' # 'value'
#' metadata(copy, list("Species")) <- list(Organism = "E. coli")
#' stopifnot(length(metadata(copy)) < length(metadata(vaas_1)))
#' stopifnot(is.null(metadata(copy, "Species")))
#'
#' # ...this yields a general mechanism for metadata deletion by providing an
#' # empty list as 'value'.
#'
#' # WMD/character/any method
#' copy <- vaas_1
#' metadata(copy, "Strain") <- "08/15"
#' stopifnot(length(metadata(copy)) == length(metadata(vaas_1)))
#' stopifnot(metadata(copy, "Strain") != metadata(vaas_1, "Strain"))
#'
#' # WMD/factor/any method
#' metadata(copy, as.factor("Strain")) <- metadata(vaas_1, "Strain")
#' stopifnot(metadata(copy, "Strain") == metadata(vaas_1, "Strain"))
#'
#' ############################################################
#' #
#' # OPMS methods
#' data(vaas_4)
#'
#' # OPMS/missing/list method
#' copy <- vaas_4
#' (metadata(copy) <- list(x = -99))
#' stopifnot(identical(unique(metadata(copy)), list(list(x = -99))))
#'
#' # OPMS/missing/formula method
#' copy <- vaas_4
#' metadata(copy) <- Organism ~ paste(Species, Strain)
#' (x <- metadata(copy, "Organism"))
#' stopifnot(length(x) == length(metadata(vaas_4, "Organism")) + 4)
#'
#' # OPMS/ANY/ANY method
#' copy <- vaas_4
#' (metadata(copy, "Species") <- "Bacillus subtilis")
#' stopifnot(identical(unique(metadata(copy, "Species")), "Bacillus subtilis"))
#'
#' # OPMS/character/data frame method
#' copy <- vaas_4
#' (x <- data.frame(Type = grepl("T$", metadata(vaas_4, "Strain"))))
#' metadata(copy, "Type") <- x
#' # one-column data frames are simplified
#' stopifnot(identical(metadata(copy, "Type"), x$Type))
#' # if keys match, a subselection of the data frame is used
#' (x <- cbind(x, Notype = !x$Type))
#' metadata(copy, "Type") <- x
#' stopifnot(identical(metadata(copy, "Type"), x$Type))
#' # if keys do not match, the entire data-frame rows are included
#' metadata(copy, "Type2") <- x
#' stopifnot(!identical(metadata(copy, "Type2"), x$Type))
#'
setMethod("metadata<-", c(WMD, "missing", "list"), function(object, value) {
  object@metadata <- value
  object
}, sealed = SEALED)

#' @name metadata.set
#'
setMethod("metadata<-", c(WMD, "missing", "formula"), function(object, value) {
  object@metadata <- map_values(object@metadata, value)
  object
}, sealed = SEALED)

#-------------------------------------------------------------------------------

#' @name metadata.set
#'
setMethod("metadata<-", c(WMD, "numeric", "list"), function(object, key,
    value) {
  object@metadata <- if (L(key) > 0)
    c(value, object@metadata)
  else if (key < 0)
    c(object@metadata, value)
  else
    value
  object
}, sealed = SEALED)

#-------------------------------------------------------------------------------

#' @name metadata.set
#'
setMethod("metadata<-", c(WMD, "list", "list"), function(object, key, value) {
  if (is.null(names(key)))
    names(key) <- unlist(key)
  if (is.null(names(value)))
    names(value) <- names(key)
  for (k in names(key))
    object@metadata[[key[[k]]]] <- value[[k]]
  object
}, sealed = SEALED)

#-------------------------------------------------------------------------------

#' @name metadata.set
#'
setMethod("metadata<-", c(WMD, "character", "ANY"), function(object, key,
    value) {
  object@metadata[[key]] <- value
  object
}, sealed = SEALED)

#' @name metadata.set
#'
setMethod("metadata<-", c(WMD, "factor", "ANY"), function(object, key,
    value) {
  metadata(object, as.character(key)) <- value
  object
}, sealed = SEALED)

#-------------------------------------------------------------------------------

#' @name metadata.set
#'
setMethod("metadata<-", c(OPMS, "missing", "list"), function(object, value) {
  for (i in seq_along(object@plates))
    metadata(object@plates[[i]]) <- value
  object
}, sealed = SEALED)

#' @name metadata.set
#'
setMethod("metadata<-", c(OPMS, "missing", "formula"), function(object, value) {
  for (i in seq_along(object@plates))
    metadata(object@plates[[i]]) <- value
  object
}, sealed = SEALED)

#' @name metadata.set
#'
setMethod("metadata<-", c(OPMS, "missing", "data.frame"), function(object,
    value) {
  LL(object, .wanted = nrow(value))
  if (ncol(value) > 1L)
    for (i in seq_along(object@plates))
      metadata(object@plates[[i]]) <- value[i, , drop = TRUE]
  else
    for (i in seq_along(object@plates))
      metadata(object@plates[[i]]) <- as.list(value[i, , drop = FALSE])
  object
}, sealed = SEALED)

#-------------------------------------------------------------------------------

#' @name metadata.set
#'
setMethod("metadata<-", c(OPMS, "ANY", "ANY"), function(object, key, value) {
  for (i in seq_along(object@plates))
    metadata(object@plates[[i]], key) <- value
  object
}, sealed = SEALED)

#' @name metadata.set
#'
setMethod("metadata<-", c(OPMS, "ANY", "data.frame"), function(object, key,
    value) {
  LL(object, .wanted = nrow(value))
  for (i in seq_along(object@plates))
    metadata(object@plates[[i]], key) <- value[i, , drop = TRUE]
  object
}, sealed = SEALED)

#-------------------------------------------------------------------------------

#' @name metadata.set
#'
setMethod("metadata<-", c(OPMS, "character", "data.frame"), function(
    object, key, value) {
  LL(object, .wanted = nrow(value))
  j <- last(key)
  if (!j %in% colnames(value))
    j <- TRUE
  for (i in seq_along(object@plates))
    metadata(object@plates[[i]], key) <- value[i, j, drop = TRUE]
  object
}, sealed = SEALED)

#' @name metadata.set
#'
setMethod("metadata<-", c(OPMS, "factor", "data.frame"), function(
    object, key, value) {
  `metadata<-`(object, as.character(key), value)
}, sealed = SEALED)


################################################################################
################################################################################
#
# Metadata mapping
#


setGeneric("map_metadata",
  function(object, mapping, ...) standardGeneric("map_metadata"))
#' Map metadata
#'
#' Modify meta-information stored together with the measurements by using a
#' function (this is just a wrapper for \code{rapply}, with \code{how} set to
#' \sQuote{replace}, if \code{values} is \code{TRUE}) or a \sQuote{character}
#' vector-based mapping.
#' The \code{\link{OPMS}} method applies this to all plates in turn
#' and returns an \code{\link{OPMS}} object with accordingly modified
#' metadata.
#'
#' @param object \code{\link{WMD}} object or \code{\link{OPMS}} object.
#' @param mapping A function. It is applied to all non-list elements of
#'   \code{\link{metadata}}, which is traversed recursively. Alternatively, a
#'   character vector. See \code{\link{map_values}} for usage
#'   details. \code{\link{metadata_chars}} can be used to create a template
#'   for such a vector. \code{mapping} can also be a formula; in that case,
#'   \code{\link{metadata}} is replaced by the result of the list+formula
#'   method of \code{\link{map_values}}. If the left side of the formula is
#'   missing, the entire metadata are replaced by the result, which is an
#'   error if the result is not a list.
#' @param values Logical scalar. If \code{FALSE}, metadata names, not values,
#'   are mapped, and \code{classes} is ignored (names are always of class
#'   \sQuote{character}).
#' @param classes Character vector or (for the character vector-based mapping)
#'   \code{TRUE}. For the mapping with a function or vector, this specifies the
#'   classes in addition to \sQuote{character} that are mapped (after
#'   converting to \sQuote{character} mode). If \code{classes} is \code{TRUE},
#'   \code{mapping} is treated as a mapping between class names, and the
#'   according conversions are applied. See the \code{coerce} argument of
#'   \code{\link{map_values}} for details.
#' @param ... Optional argument passed to \code{mapping} if it is a function,
#'   and from the \code{\link{OPMS}} method to the \code{\link{WMD}} method.
#' @return \code{\link{WMD}} or \code{\link{OPMS}} object with modified
#'   metadata.
#' @export
#' @family metadata-functions
#' @keywords manip
#' @examples
#'
#' # WMD methods
#' data(vaas_1)
#'
#' # WMD+function method
#' copy <- map_metadata(vaas_1, identity)
#' stopifnot(identical(copy, vaas_1))
#' copy <- map_metadata(vaas_1, identity, values = FALSE)
#' stopifnot(identical(copy, vaas_1))
#' copy <- map_metadata(vaas_1, function(x) paste(x, "!"), values = FALSE)
#' (x <- metadata_chars(vaas_1, values = FALSE))
#' (y <- metadata_chars(copy, values = FALSE))
#' stopifnot(identical(as.character(y), paste(x, "!")))
#'
#' # WMD+character method: mapping a value
#' map <- metadata_chars(vaas_1)
#' map["First replicate"] <- "Rep. 1"
#' copy <- map_metadata(vaas_1, map)
#' stopifnot(identical(names(metadata(copy)), names(metadata(vaas_1))))
#' stopifnot(!identical(metadata(copy, "Experiment"),
#'   metadata(vaas_1, "Experiment")))
#'
#' # WMD+character method: mapping a name
#' map <- metadata_chars(vaas_1, values = FALSE)
#' map["Plate number"] <- "Plate no."
#' copy <- map_metadata(vaas_1, map, values = FALSE)
#' stopifnot(!identical(names(metadata(copy)), names(metadata(vaas_1))))
#'
#' # WMD+formula method
#' copy <- map_metadata(vaas_1, Organism ~ paste(Species, Strain))
#' (x <- setdiff(metadata_chars(copy), metadata_chars(vaas_1)))
#' stopifnot(length(x) == 1, x == "Escherichia coli DSM30083T")
#'
#' # OPMS method
#' data(vaas_4)
#'
#' # using a function
#' copy <- map_metadata(vaas_4, identity)
#' stopifnot(identical(copy, vaas_4))
#' copy <- map_metadata(vaas_4, identity, values = FALSE)
#' stopifnot(identical(copy, vaas_4))
#' copy <- map_metadata(vaas_4, function(x) paste(x, "!"), values = FALSE)
#' (x <- metadata_chars(vaas_4, values = FALSE))
#' (y <- metadata_chars(copy, values = FALSE))
#' stopifnot(identical(as.character(y), paste(x, "!")))
#'
#' # using a character vector
#' map <- metadata_chars(vaas_4)
#' map["First replicate"] <- "Rep. 1"
#' copy <- map_metadata(vaas_4, map)
#' x <- metadata(vaas_4, "Experiment")
#' stopifnot(x == "First replicate")
#' y <- metadata(copy, "Experiment")
#' stopifnot(y == "Rep. 1")
#'
#' # using a formula
#' copy <- map_metadata(vaas_4, Organism ~ paste(Species, Strain))
#' (x <- setdiff(metadata_chars(copy), metadata_chars(vaas_4)))
#' stopifnot(length(x) == 4) # one entry per plate
#'
setMethod("map_metadata", c(WMD, "function"), function(object, mapping,
    values = TRUE, classes = "ANY", ...) {
  object@metadata <- if (L(values))
    map_values(object = object@metadata, mapping = mapping, coerce = classes,
      ...)
  else
    map_names(object = object@metadata, mapping = mapping, ...)
  object
}, sealed = SEALED)

setMethod("map_metadata", c(WMD, "character"), function(object, mapping,
    values = TRUE, classes = "factor") {
  object@metadata <- if (L(values))
    map_values(object@metadata, mapping, coerce = classes)
  else
    map_names(object@metadata, mapping)
  object
}, sealed = SEALED)

setMethod("map_metadata", c(WMD, "formula"), function(object, mapping) {
  object@metadata <- map_values(object@metadata, mapping)
  object
}, sealed = SEALED)

#-------------------------------------------------------------------------------

setMethod("map_metadata", c(OPMS, "ANY"), function(object, mapping, ...) {
  object@plates <- lapply(object@plates, FUN = map_metadata, mapping = mapping,
    ...)
  object
}, sealed = SEALED)


################################################################################
################################################################################
#
# Metadata characters
#

setGeneric("metadata_chars",
  function(object, ...) standardGeneric("metadata_chars"))
#' Get metadata characters
#'
#' Collect all \sQuote{character} entries from the meta-information stored
#' together with the measurements. Optionally coerce data of other types. The
#' result can be used to create a mapping for \code{\link{map_metadata}}. The
#' \code{\link{OPMS}} method just applies the \code{\link{WMD}} method to all
#' contained plates in turn.
#'
#' @param object \code{\link{WMD}} or \code{\link{OPMS}} object.
#' @param values Logical scalar. If \code{FALSE}, metadata names, not values,
#'   are collected, and \code{classes} is ignored (names are always of class
#'   \sQuote{character} and need not be coerced).
#' @param classes Character vector containing the names of classes that should
#'   also be collected (and coerced to \sQuote{character}), or \code{TRUE}. In
#'   that case, a mapping template for the classes themselves is returned. See
#'   the \code{coerce} argument of \code{map_values} for details.
#' @param ... Optional argument passed from the \code{\link{OPMS}} to the
#'   \code{\link{WMD}} method.
#' @return Character vector, sorted and made unique. Original \code{names}
#'   attributes, if any, are dropped and replaced by the character vector
#'   itself. (This might be convenient regarding its use with
#'   \code{\link{map_metadata}}.)
#' @export
#' @family metadata-functions
#' @keywords attribute
#' @examples
#'
#' # WMD method
#' data(vaas_1)
#' (x <- metadata_chars(vaas_1, values = FALSE))
#' stopifnot(names(x) == x)
#' (x <- metadata_chars(vaas_1, values = TRUE))
#' stopifnot(names(x) == x)
#' # See map_metadata() for a potential usage of the metadata_chars() result
#'
#' # OPMS method
#' data(vaas_4)
#' (x <- metadata_chars(vaas_4, values = TRUE)) # the values
#' (y <- metadata_chars(vaas_4, values = FALSE)) # the keys
#' stopifnot(length(x) > length(y))
#'
setMethod("metadata_chars", WMD, function(object, values = TRUE,
    classes = "factor") {
  if (L(values))
    map_values(object@metadata, coerce = classes)
  else
    map_names(object@metadata)
}, sealed = SEALED)

setMethod("metadata_chars", OPMS, function(object, ...) {
  map_values(unlist(lapply(object@plates, FUN = metadata_chars, ...)))
}, sealed = SEALED)


################################################################################


