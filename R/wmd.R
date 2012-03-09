


################################################################################
################################################################################
#
# Pure metadata functions
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
#'     e.g., dataframes because some values might be missing. But metadata can
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
################################################################################
#
# Getter methods
#  

  
setGeneric("metadata", function(object, ...) standardGeneric("metadata"))
#' Get metadata
#'
#' Get meta-information stored together with the data.
#'
#' @param object \code{\link{WMD}} object.
#' @param key If \code{NULL} or otherwise empty, return all metadata. If a 
#'   non-empty list, treat it as list of keys and return list of corresponding
#'   metadata values. Here, character vectors of length > 1 can be used to 
#'   query nested metadata lists. If neither empty nor a list (i.e. usually a
#'   character or numeric scalar), treat \code{key} as a single list key.
#' @param exact Logical scalar. Use exact or partial matching of keys? Has no
#'   effect if \code{key} is empty.
#' @param strict Logical scalar. Is it an error if a \code{NULL} value results
#'   from fetching a metadata key?
#' @return List (empty if metadata were not set or if subselection using
#'   \code{key} did not result).
#' @export
#' @family getter-functions
#' @family metadata-functions
#' @keywords attribute
#' @examples
#' data(vaas_1)
#' (x <- metadata(vaas_1, "Strain"))
#' stopifnot(identical(x, "DSM30083T"))
#'
setMethod("metadata", WMD, function(object, key = NULL, exact = TRUE, 
    strict = FALSE) {
  if (length(key) == 0L)
    return(object@metadata)
  fetch_fun <- if (strict)
    function(key) {
      if (is.null(result <- object@metadata[[key, exact = exact]]))
        stop("got NULL value when using key ", paste(key, collapse = " -> "))
      result
    }
  else
    function(key) {
      object@metadata[[key, exact = exact]]  
    }
  if (is.list(key)) {
    result <- lapply(key, fetch_fun)
    if (is.null(names(result)))
      names(result) <- unlist(key)
    result
  } else
    fetch_fun(key)
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
#' measurements for all plates at once.
#'
#' @name metadata.set
#' @aliases metadata<-
#'
#' @param object \code{\link{WMD}} or \code{\link{OPMS}} object..
#' @param key Missing, numeric scalar, character vector, or list.
#'   If missing, replace all metadata by \code{value}.
#'   If a numeric scalar, then if positive, prepend \code{value} to
#'   old metadata. If negative, append \code{value} to old metadata. If zero, 
#'   replace old metadata entirely by \code{value}.
#'   If a list, treat it as list of keys; expect \code{value} to be a list 
#'   of corresponding metadata values to be set. Names are replaced by the
#'   values of either list if they are missing.
#'   If a character vector, use it as key and set/replace this metadata 
#'   entry to/by \code{value}. It is an error if \code{key} has zero length.
#'   If it contains more than one entry, a nested query is done. See \code{[[}
#'   from the \pkg{base} package for details.
#' @param value If \code{key} is a character vector, this can be arbitrary
#'   value(s) to be included in the metadata (if \code{NULL}, this
#'   metadata entry is deleted). If \code{key} is otherwise, \code{value} must
#'   be list of values to be prepended, appended or set as metadata,
#'   either entirely or specifically, depending on \code{key}.
#' @return \code{value}.
#' @exportMethod "metadata<-"
#' @family metadata-functions
#' @family setter-functions
#' @keywords manip
#' @examples
#'
#' ############################################################
#' #
#' # WMD methods
#' data(vaas_1)
#'
#' # WMD+missing method
#' copy <- vaas_1
#' new.md <- list(Species = "Thermomicrobium roseum")
#' metadata(copy) <- new.md
#' stopifnot(identical(metadata(copy), new.md))
#'
#' # WMD+numeric method
#' copy <- vaas_1
#' metadata(copy, 1) <- list(Authors = "Vaas et al.")
#' stopifnot(length(metadata(copy)) > length(metadata(vaas_1)))
#'
#' # WMD+list method
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
#' # WMD+character method
#' copy <- vaas_1
#' metadata(copy, "Strain") <- "08/15"
#' stopifnot(length(metadata(copy)) == length(metadata(vaas_1)))
#' stopifnot(metadata(copy, "Strain") != metadata(vaas_1, "Strain"))
#'
#' ############################################################
#' #
#' # OPMS methods
#' data(vaas_4)
#'
#' # OPMS+missing method
#' copy <- vaas_4
#' (metadata(copy) <- list(x = -99))
#' stopifnot(identical(unique(metadata(copy)), list(list(x = -99))))
#'
#' # OPMS+ANY method
#' copy <- vaas_4
#' (metadata(copy, "Species") <- "Bacillus subtilis")
#' stopifnot(identical(unique(metadata(copy, "Species")), "Bacillus subtilis"))
#'
setMethod("metadata<-", c(WMD, "missing", "list"), function(object, value) {
  object@metadata <- value
  object
}, sealed = SEALED)

#' @name metadata.set
#' @export
#'
setMethod("metadata<-", c(WMD, "numeric", "list"), function(object, key,
    value) {
  assert_length(key)
  object@metadata <- if (key > 0)
    c(value, object@metadata)
  else if (key < 0)
    c(object@metadata, value)
  else
    value
  object
}, sealed = SEALED)

#' @name metadata.set
#' @export
#'
setMethod("metadata<-", c(WMD, "list", "list"), function(object, key, value) {
  if (is.null(names(key)))
    names(key) <- unlist(key)
  if (is.null(names(value)))
    names(value) <- names(key)
  sapply(names(key), function(k) object@metadata[[key[[k]]]] <<- value[[k]])
  object
}, sealed = SEALED)

#' @name metadata.set
#' @export
#'
setMethod("metadata<-", c(WMD, "character", "ANY"), function(object, key,
    value) {
  object@metadata[[key]] <- value
  object
}, sealed = SEALED)


################################################################################
################################################################################
#
# Infix operators
#  


setGeneric("%k%", function(x, table) standardGeneric("%k%"))
#' Search in metadata keys
#'
#' Using a character vector as query, this method tests whether all given keys 
#' are present in the top-level names of the metadata (these may be nested, but 
#' all sublists are ignored here). An empty query vector results in 
#' \code{TRUE}. Note that the values of the character vector, not its names, if
#' any, are used for querying the metadata.
#' Using a list as query, this method tests whether all given keys are present
#' in the names of the metadata.
#' This works like the character method, but because a query list is given, the 
#' comparison of keys can be applied recursively (by using, of course, a nested 
#' query list). This is based on \code{\link{contains}} with the \code{values} 
#' argument set to \code{FALSE}.
#'
#' @name %k%
#' @aliases infix.k
#' @rdname infix.k
#'
#' @param x Character vector or list.
#' @param table \code{\link{WMD}} object.
#' @return Logical scalar.
#' @exportMethod "%k%"
#' @export
#~ @family getter-functions
#' @keywords attribute
#'
#' @examples 
#'
#' # The dataset contains the metadata keys 'Species' and 'Experiment' but
#' # neither 'Trial' nor 'Organism' nor 'Run':
#' data(vaas_1)
#'
#' # Character method
#' stopifnot("Experiment" %k% vaas_1)
#' stopifnot("Species" %k% vaas_1)
#' stopifnot(!"Run" %k% vaas_1)
#' stopifnot(c("Species", "Experiment") %k% vaas_1)
#' stopifnot(!c("Species", "Trial") %k% vaas_1)
#' stopifnot(!c("Organism", "Experiment") %k% vaas_1)
#' stopifnot(character() %k% vaas_1)
#'
#' # List method
#' stopifnot(list(Experiment = "whatever") %k% vaas_1)
#' stopifnot(list(Species = "ignored") %k% vaas_1)
#'
#' # This fails because we query with a named sublist but 'Species' is not
#' # even a list
#' stopifnot(!list(Species = list(Genus = "X", Epithet = "Y")) %k% vaas_1)
#'
#' # This is OK because we query with an unnamed sublist: it has no names that
#' # one would fail to find
#' stopifnot(list(Species = list("X", "Y")) %k% vaas_1)
#'
#' # More non-nested query examples
#' stopifnot(!list(Run = 99) %k% vaas_1)
#' stopifnot(list(Species = "?", Experiment = NA) %k% vaas_1)
#' stopifnot(!list(Species = "?", Trial = NA) %k% vaas_1)
#' stopifnot(!list(Organism = "?", Experiment = NA) %k% vaas_1)
#' stopifnot(list() %k% vaas_1)
#'
setMethod("%k%", c("character", WMD), function(x, table) {
  all(x %in% names(table@metadata))
}, sealed = SEALED)

#' @export
#'
setMethod("%k%", c("list", WMD), function(x, table) {
  contains(table@metadata, x, values = FALSE)
}, sealed = SEALED)


################################################################################


setGeneric("%K%", function(x, table) standardGeneric("%K%"))
#' Search in metadata keys
#'
#' Using a character vector as query, this method tests whether a given key is
#' present in the metadata and fetches an object that is not \code{NULL}. If
#' the key has a length > 1, sublists are queried. An empty vector results in
#' \code{TRUE}. Note that the values of the character vector, not its names, if 
#' any, are used for querying the metadata.
#' Using a list as query, this function behaves like \code{\link{infix.k}}.
#'
#' @name %K%
#' @aliases infix.largek
#' @rdname infix.largek
#'
#' @param x Character vector or list.
#' @param table \code{\link{WMD}} object.
#' @return Logical scalar.
#' @export
#' @exportMethod "%K%"
#~ @family getter-functions
#' @keywords attribute
#'
#' @examples 
#'
#' # The dataset contains the metadata keys 'Species' and 'Experiment' but
#' # neither 'Trial' nor 'Organism' nor 'Run':
#' data(vaas_1)
#'
#' # Character method
#'
#' # Single-element queries
#' stopifnot("Experiment" %K% vaas_1)
#' stopifnot("Species" %K% vaas_1)
#' stopifnot(!"Run" %K% vaas_1)
#' stopifnot(!"Trial" %K% vaas_1)
#' stopifnot(!"Organism" %k% vaas_1)
#'
#' # Zero-element queries
#' stopifnot(character() %K% vaas_1)
#'
#' # Querying with vectors of length > 1 mean nested queries; compare this to
#' # the behavior of %k%!
#' stopifnot(!c("Species", "Experiment") %K% vaas_1)
#'
#' # List method
#' # See %k% -- the behavior is identical for lists.
#'
setMethod("%K%", c("character", WMD), function(x, table) {
  if (length(x) == 0L)
    return(TRUE) # for consistency with %k%
  tryCatch(!is.null(table@metadata[[x]]), error = function(e) FALSE)
}, sealed = SEALED)

#' @export
#'
setMethod("%K%", c("list", WMD), function(x, table) {
  contains(table@metadata, x, values = FALSE)
}, sealed = SEALED)


################################################################################


setGeneric("%q%", function(x, table) standardGeneric("%q%"))
#' Query metadata (non-exact version)
#'
#' Using a character vector, test whether all given query keys are present in
#' the top-level names of the metadata and refer to the same query elements.
#' Using a list, conduct a non-exact query with a query list.
#'
#' @name %q%
#' @aliases infix.q
#' @rdname infix.q
#'
#' @param x Character vector or list used as query. 
#'   If a character vector, its \code{names} are used to select elements from
#'   the top level of the metadata. These elements are then converted to 
#'   \sQuote{character} mode before comparison with the values of \code{x}. A 
#'   non-empty vector without a \code{names} attribute is accepted but will 
#'   always yield \code{FALSE}. In contrast, an entirely empty vector yields 
#'   \code{TRUE}.
#'   If a list, the comparison is applied recursively using
#'   \code{\link{contains}} with the \code{values} argument set to \code{TRUE}
#'   but \code{exact} set to \code{FALSE}. The main advantage of using a list
#'   over the character-based search is that it allows one a nested query.
#' @param table \code{\link{WMD}} object.
#' @return Logical scalar.
#' @exportMethod "%q%"
#' @export
#~ @family getter-functions
#' @keywords attribute
#'
#' @examples 
#'
#' # The dataset contains the metadata keys 'Species' and 'Experiment' with the
#' # values 'Escherichia coli' and 'First replicate':
#' data(vaas_1)
#'
#' # Character method
#' stopifnot(!"Experiment" %q% vaas_1) # wrong query here; compare to %k%
#' stopifnot(!"First replicate" %q% vaas_1) # again wrong query
#' stopifnot(c(Experiment = "First replicate") %q% vaas_1) # right query
#'
#' stopifnot(!"Species" %q% vaas_1)
#' stopifnot(!"Escherichia coli" %q% vaas_1)
#' stopifnot(c(Species = "Escherichia coli") %q% vaas_1)
#'
#' stopifnot(c(Species = "Escherichia coli", 
#'   Experiment = "First replicate") %q% vaas_1) # Combined query
#'
#' stopifnot(character() %q% vaas_1) # Empty query
#'
#' # List method
#' stopifnot(list(Experiment = "First replicate") %q% vaas_1)
#'
#' # Choice among alternatives
#' stopifnot(list(Experiment = c("First replicate", 
#'   "Second replicate")) %q% vaas_1)
#' stopifnot(!list(Experiment = c("Second replicate", 
#'   "Third replicate")) %q% vaas_1)
#'
#' # Combined query together with choice among alternatives
#' stopifnot(list(Experiment = c("First replicate", "Second replicate"),
#'   Species = c("Escherichia coli", "Bacillus subtilis")) %q% vaas_1)
#'
#' stopifnot(list() %q% vaas_1) # Empty query
#'
setMethod("%q%", c("character", WMD), function(x, table) {
  keys <- names(x)
  if (length(keys) == 0L && length(x) > 0L)
    return(FALSE)
  all(x == sapply(table@metadata[keys], as.character))
}, sealed = SEALED)

#' @export
#'
setMethod("%q%", c("list", WMD), function(x, table) {
  contains(table@metadata, x, values = TRUE, exact = FALSE)
}, sealed = SEALED)


################################################################################


setGeneric("%Q%", function(x, table) standardGeneric("%Q%"))
#' Query metadata (strict version)
#'
#' Using a character vector as query, test whether all given query keys are 
#' present in the top-level names of the metadata and refer to the same query 
#' elements (without coercion to character).
#' Using a list, conduct an exact query with this query list.
#'
#' @name %Q%
#' @aliases infix.largeq
#' @rdname infix.largeq
#'
#' @param x Character vector or list used as query. 
#'   If a character vector, the result is identical to the 
#'   one of \code{\link{infix.q}} except for the fact that metadata elements 
#'   are not coerced to \sQuote{character} mode, making the query more strict.
#'   If a list, the comparison is applied recursively 
#'   using \code{\link{contains}} with the arguments \code{values} and 
#'   \code{exact} set to \code{TRUE}. This might be too strict for most
#'   applications. The main advantage of using a list over the character-based
#'   search is that it allows one a nested query.
#' @param table \code{\link{WMD}} object.
#' @return Logical scalar.
#' @exportMethod "%Q%"
#' @export
#~ @family getter-functions
#' @keywords attribute
#'
#' @examples 
#'
#' # The dataset contains the metadata keys 'Species' and 'Experiment' with the
#' # values 'Escherichia coli' and 'First replicate':
#' data(vaas_1)
#'
#' # Character method
#' stopifnot(c(Experiment = "First replicate") %Q% vaas_1)
#'
#' # This does not work because the value has the wrong type
#' stopifnot(!c(`Plate number` = "6") %Q% vaas_1)
#' # Compare to %q%
#' stopifnot(c(`Plate number` = "6") %q% vaas_1)
#'
#' # Combined query
#' stopifnot(c(Species = "Escherichia coli", 
#'   Experiment = "First replicate") %Q% vaas_1)
#'
#' stopifnot(character() %Q% vaas_1) # Empty query
#'
#' # List method
#' stopifnot(list(Experiment = "First replicate") %Q% vaas_1)
#'
#' # Choice among alternatives is not done here: this query fails unless this
#' # two-element vector is contained. Compare to %q%.
#' stopifnot(!list(Experiment = c("First replicate", 
#'   "Second replicate")) %Q% vaas_1)
#'
#' stopifnot(list() %Q% vaas_1) # Empty query
#'
setMethod("%Q%", c("character", WMD), function(x, table) {
  keys <- names(x)
  if (length(keys) == 0L && length(x) > 0L)
    return(FALSE)
  all(sapply(keys, function(key) identical(x[[key]], table@metadata[[key]])))
}, sealed = SEALED)

#' @export
#'
setMethod("%Q%", c("list", WMD), function(x, table) {
  contains(table@metadata, x, values = TRUE, exact = TRUE)
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
#'   for such a vector.
#' @param values Logical scalar. If \code{FALSE}, metadata names, not values,
#'   are mapped, and \code{classes} is ignored (names are always of class
#'   \sQuote{character}).
#' @param classes Character vector or (for the character vector-based mapping) 
#'  \code{TRUE}. For the mapping with a function, this specifies the classes
#'  that are mapped. For the mapping with a character vector, this specifies the
#'  classes in addition to \sQuote{character} that are mapped (after converting
#'  to \sQuote{character} mode). If \code{classes} is \code{TRUE}, 
#'  \code{mapping} is treated as a mapping between class names, and the 
#'  according conversions are applied. See the \code{coerce} argument of
#'  \code{\link{map_values}} for details.
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
setMethod("map_metadata", c(WMD, "function"), function(object, mapping, 
    values = TRUE, classes = "ANY", ...) {
  object@metadata <- if (values)
    rapply(object = object@metadata, f = mapping, classes = classes,
       how = "replace", ...)
  else
    map_names(object = object@metadata, mapping = mapping, ...)
  object
}, sealed = SEALED)

#' @export
#'
setMethod("map_metadata", c(WMD, "character"), function(object, mapping,
    values = TRUE, classes = "factor") {
  object@metadata <- if (values)
    map_values(object@metadata, mapping, coerce = classes)
  else
    map_names(object@metadata, mapping)
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
  if (values)
    map_values(object@metadata, coerce = classes)
  else
    map_names(object@metadata)
}, sealed = SEALED)
  
  
  
################################################################################
  
  
  

