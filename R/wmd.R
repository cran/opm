


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
#'   \item Conceptually, this class treats metadata are arbitrarily nested 
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
#
# Setter methods
#  
    
    
setGeneric("metadata<-",
  function(object, key, ..., value) standardGeneric("metadata<-"))
#' Replace metadata
#'
#' Set the meta-information stored together with the data. This version
#' replaces all metadata by its argument.
#'
#' @name metadata-set
#' @aliases metadata<-
#'
#' @param object \code{\link{WMD}} object.
#' @param value List. Values to be set as metadata. Previous metadata, if any,
#'   are replaced entirely by \code{value}.
#' @return \code{value}.
#' @exportMethod "metadata<-"
#' @family metadata-functions
#' @family setter-functions
#' @note See \sQuote{See Also} for the other \sQuote{metadata<-} methods.
#' @keywords manip
#' @examples
#' data(vaas_1)
#' copy <- vaas_1
#' new.md <- list(Species = "Thermomicrobium roseum")
#' metadata(copy) <- new.md
#' stopifnot(identical(metadata(copy), new.md))
#'
setMethod("metadata<-", c(WMD, "missing", "list"), function(object, value) {
  object@metadata <- value
  object
}, sealed = SEALED)


#' Set metadata (WMD+numeric+list version)
#'
#' Set meta-information stored together with the data.
#'
#' @name metadata-set,WMD+numeric+list
#' @aliases metadata<-,WMD+numeric+list
#'
#' @param object \code{\link{WMD}} object.
#' @param key Numeric scalar. If positive, prepend \code{value} to old 
#'   metadata. If negative, append \code{value} to old metadata. If zero, 
#'   replace old metadata entirely by \code{value}.
#' @param value List. Values to be prepended, appended or set as metadata,
#'   depending on \code{key}.
#' @return \code{value}.
#' @exportMethod "metadata<-"
#' @family metadata-functions
#' @family setter-functions
#' @note See \sQuote{See Also} for the other \sQuote{metadata<-} methods.
#' @keywords manip
#' @examples
#' data(vaas_1)
#' copy <- vaas_1
#' metadata(copy, 1) <- list(Authors = "Vaas et al.")
#' stopifnot(length(metadata(copy)) > length(metadata(vaas_1)))
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

  
#' Set metadata (WMD+list+list version)
#'
#' Set meta-information stored together with the data.
#'
#' @name metadata-set,WMD+list+list
#' @aliases metadata<-,WMD+list+list
#'
#' @param object \code{\link{WMD}} object.
#' @param key List. Treat it as list of keys; expect \code{value} to be a list 
#'   of corresponding metadata values to be set. Names are replaced by the
#'   values of either list if they are missing.
#' @param value List, Values to be included in the metadata. See \code{key} for 
#'   details.
#' @return \code{value}.
#' @exportMethod "metadata<-"
#' @family metadata-functions
#' @family setter-functions
#' @note See \sQuote{See Also} for the other \sQuote{metadata<-} methods.
#' @keywords manip
#' @examples
#'
#' data(vaas_1)
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
setMethod("metadata<-", c(WMD, "list", "list"), function(object, key, value) {
  if (is.null(names(key)))
    names(key) <- unlist(key)
  if (is.null(names(value)))
    names(value) <- names(key)
  sapply(names(key), function(k) object@metadata[[key[[k]]]] <<- value[[k]])
  object
}, sealed = SEALED)

  
#' Set metadata (WMD+character version)
#'
#' Set meta-information stored together with the data.
#'
#' @name metadata-set,WMD+character+ANY
#' @aliases metadata<-,WMD+character+ANY
#'
#' @param object \code{\link{WMD}} object.
#' @param key Character vector. Use it as key and set/replace this metadata 
#'   entry to/by \code{value}. It is an error if \code{key} has zero length.
#'   If it contains more than one entry, a nested query is done. See \code{[[}
#'   from the \pkg{base} package for details.
#' @param value Value(s) to be included in the metadata. If \code{NULL}, this
#'   metadata entry is deleted.
#' @return \code{value}.
#' @exportMethod "metadata<-"
#' @family metadata-functions
#' @family setter-functions
#' @note See \sQuote{See Also} for the other \sQuote{metadata<-} methods.
#' @keywords manip
#' @examples
#' data(vaas_1)
#' copy <- vaas_1
#' metadata(copy, "Strain") <- "08/15"
#' stopifnot(length(metadata(copy)) == length(metadata(vaas_1)))
#' stopifnot(metadata(copy, "Strain") != metadata(vaas_1, "Strain"))
#'
setMethod("metadata<-", c(WMD, "character", "ANY"), function(object, key,
    value) {
  object@metadata[[key]] <- value
  object
}, sealed = SEALED)

  
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
#'
#' @name %k%
#' @aliases infix-k
#' @rdname infix-k
#'
#' @param x Character vector.
#' @param table \code{\link{WMD}} object.
#' @return Logical scalar.
#' @exportMethod "%k%"
#' @export
#~ @family getter-functions
#' @keywords attribute
#' @note There is also a list-based version, \code{\link{infix-k,list}}.
#'
#' @examples 
#' data(vaas_1)
#' # The dataset contains the metadata keys 'Species' and 'Experiment' but
#' # neither 'Trial' nor 'Organism' nor 'Run':
#' stopifnot("Experiment" %k% vaas_1)
#' stopifnot("Species" %k% vaas_1)
#' stopifnot(!"Run" %k% vaas_1)
#' stopifnot(c("Species", "Experiment") %k% vaas_1)
#' stopifnot(!c("Species", "Trial") %k% vaas_1)
#' stopifnot(!c("Organism", "Experiment") %k% vaas_1)
#' stopifnot(character() %k% vaas_1)
#'
setMethod("%k%", c("character", WMD), function(x, table) {
  all(x %in% names(table@metadata))
}, sealed = SEALED)


#' Search in metadata keys (list version)
#'
#' Using a list as query, this method tests whether all given keys are present
#' in the names of the metadata.
#' This works like \code{\link{infix-k}}, but because a query list is given, the 
#' comparison of keys can be applied recursively (by using, of course, a nested 
#' query list). This is based on \code{\link{contains}} with the \code{values} 
#' argument set to \code{FALSE}.
#'
#' @name %k%,list
#' @aliases infix-k,list
#' @rdname infix-k-list
#'
#' @param x List.
#' @param table \code{\link{WMD}} object.
#' @return Logical scalar.
#' @exportMethod "%k%"
#' @export
#~ @family getter-functions
#' @keywords attribute
#' @note There is also a character-based version, \code{\link{infix-k}}.
#' @examples
#'
#' data(vaas_1)
#'
#' # The dataset contains the metadata keys 'Species' and 'Experiment' but
#' # neither 'Trial' nor 'Organism' nor 'Run':
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
setMethod("%k%", c("list", WMD), function(x, table) {
  contains(table@metadata, x, values = FALSE)
}, sealed = SEALED)


setGeneric("%K%", function(x, table) standardGeneric("%K%"))
#' Search in metadata keys
#'
#' Using a character vector as query, this method tests whether a given key is
#' present in the metadata and fetches an object that is not \code{NULL}. If
#' the key has a length > 1, sublists are queried. An empty vector results in
#' \code{TRUE}. Note that the values of the character vector, not its names, if 
#' any, are used for querying the metadata.
#'
#' @name %K%
#' @aliases infix-largek
#' @rdname infix-largek
#'
#' @param x Character vector.
#' @param table \code{\link{WMD}} object.
#' @return Logical scalar.
#' @export
#' @exportMethod "%K%"
#~ @family getter-functions
#' @keywords attribute
#' @note There is also a list-based version, \code{\link{infix-largek,list}}.
#'
#' @examples 
#'
#' # The dataset contains the metadata keys 'Species' and 'Experiment' but
#' # neither 'Trial' nor 'Organism' nor 'Run':
#' data(vaas_1)
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
#' # Querying with vectors of lenght > 1 mean nested queries; compare this to
#' # the behaviour of %k%!
#' stopifnot(!c("Species", "Experiment") %K% vaas_1)
#'
setMethod("%K%", c("character", WMD), function(x, table) {
  if (length(x) == 0L)
    return(TRUE) # for consistency with %k%
  tryCatch(!is.null(table@metadata[[x]]), error = function(e) FALSE)
}, sealed = SEALED)


#' Search in metadata keys (list version)
#'
#' Using a list as query, this function behaves like \code{\link{infix-k}}.
#'
#' @name %K%,list
#' @aliases infix-largek,list
#' @rdname infix-largek-list
#'
#' @param x List.
#' @param table \code{\link{WMD}} object.
#' @return Logical scalar.
#' @export
#' @exportMethod "%K%"
#~ @family getter-functions
#' @keywords attribute
#' @note There is also a character-based version, \code{\link{infix-largek}}.
#'
#' @examples
#' # See %k% -- the behaviour is identical for lists.
#'
setMethod("%K%", c("list", WMD), function(x, table) {
  contains(table@metadata, x, values = FALSE)
}, sealed = SEALED)


setGeneric("%q%", function(x, table) standardGeneric("%q%"))
#' Query metadata (non-exact version)
#'
#' Using a character vector, test whether all given query keys are present in
#' the top-level names of the metadata and refer to the same query elements.
#'
#' @name %q%
#' @aliases infix-q
#' @rdname infix-q
#'
#' @param x Character vector. Its \code{names} are used to select elements from
#'   the top level of the metadata. These elements are then converted to 
#'   \sQuote{character} mode before comparison with the values of \code{x}. A 
#'   non-empty vector without a \code{names} attribute is accepted but will 
#'   always yield \code{FALSE}. In contrast, an entirely empty vector yields 
#'   \code{TRUE}.
#' @param table \code{\link{WMD}} object.
#' @return Logical scalar.
#' @exportMethod "%q%"
#' @export
#~ @family getter-functions
#' @keywords attribute
#' @note There is also a list-based version, \code{\link{infix-q,list}}.
#'
#' @examples 
#'
#' # The dataset contains the metadata keys 'Species' and 'Experiment' with the
#' # values 'Escherichia coli' and 'First replicate':
#' data(vaas_1)
#'
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
setMethod("%q%", c("character", WMD), function(x, table) {
  keys <- names(x)
  if (length(keys) == 0L && length(x) > 0L)
    return(FALSE)
  all(x == sapply(table@metadata[keys], as.character))
}, sealed = SEALED)


#' Query metadata (non-exact list version)
#'
#' Conduct a non-exact query with a query list.
#'
#' @name %q%,list
#' @aliases infix-q,list
#' @rdname infix-q-list
#'
#' @param x A list, used as query. The comparison is applied recursively using
#'   \code{\link{contains}} with the \code{values} argument set to \code{TRUE}
#'   but \code{exact} set to \code{FALSE}. The main advantage of using a list
#'   over the character-based search is that it allows one a nested query.
#' @param table \code{\link{WMD}} object.
#' @return Logical scalar.
#' @exportMethod "%q%"
#' @export
#~ @family getter-functions
#' @keywords attribute
#' @note There is also a character-based version, \code{\link{infix-k}}.
#'
#' @examples 
#'
#' # The dataset contains the metadata keys 'Species' and 'Experiment' with the
#' # values 'Escherichia coli' and 'First replicate':
#' data(vaas_1)
#'
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
setMethod("%q%", c("list", WMD), function(x, table) {
  contains(table@metadata, x, values = TRUE, exact = FALSE)
}, sealed = SEALED)


setGeneric("%Q%", function(x, table) standardGeneric("%Q%"))
#' Query metadata (strict version)
#'
#' Using a character vector as query, test whether all given query keys are 
#' present in the top-level names of the metadata and refer to the same query 
#' elements (without coercion to character).
#'
#' @name %Q%
#' @aliases infix-largeq
#' @rdname infix-largeq
#'
#' @param x A character vector used as query. The result is identical to the 
#'   one of \code{\link{infix-q}} except for the fact that metadata elements 
#'   are not coerced to \sQuote{character} mode, making the query more strict.
#' @param table \code{\link{WMD}} object.
#' @return Logical scalar.
#' @exportMethod "%Q%"
#' @export
#~ @family getter-functions
#' @keywords attribute
#' @note There is also a list-based version, \code{\link{infix-largeq,list}}.
#'
#' @examples 
#'
#' # The dataset contains the metadata keys 'Species' and 'Experiment' with the
#' # values 'Escherichia coli' and 'First replicate':
#' data(vaas_1)
#'
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
setMethod("%Q%", c("character", WMD), function(x, table) {
  keys <- names(x)
  if (length(keys) == 0L && length(x) > 0L)
    return(FALSE)
  all(sapply(keys, function(key) identical(x[[key]], table@metadata[[key]])))
}, sealed = SEALED)


#' Query metadata (strict list version)
#'
#' Conduct an exact query with a query list.
#'
#' @name %Q%,list
#' @aliases infix-largeq,list
#' @rdname infix-largeq-list
#'
#' @param x A list used as query. The comparison is applied recursively 
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
#' @note There is also a character-based version, \code{\link{infix-largeq}}.
#'
#' @examples 
#'
#' # The dataset contains the metadata keys 'Species' and 'Experiment' with the
#' # values 'Escherichia coli' and 'First replicate':
#' data(vaas_1)
#'
#' stopifnot(list(Experiment = "First replicate") %Q% vaas_1)
#'
#' # Choice among alternatives is not done here: this query fails unless this
#' # two-element vector is contained. Compare to %q%.
#' stopifnot(!list(Experiment = c("First replicate", 
#'   "Second replicate")) %Q% vaas_1)
#'
#' stopifnot(list() %Q% vaas_1) # Empty query
#'
setMethod("%Q%", c("list", WMD), function(x, table) {
  contains(table@metadata, x, values = TRUE, exact = TRUE)
}, sealed = SEALED)


################################################################################
#
# Metadata mapping
#  


setGeneric("map_metadata",
  function(object, mapping, ...) standardGeneric("map_metadata"))
#' Map metadata using a function
#'
#' Modify meta-information stored together with the measurements by using a 
#' function. (This is just a wrapper for \code{rapply}, with \code{how} set to 
#' \sQuote{replace}, if \code{values} is \code{TRUE}.)
#'
#' @name map_metadata,WMD+function
#'
#' @param object \code{\link{WMD}} object.
#' @param mapping A function. It is applied to all non-list elements of
#'   \code{\link{metadata}}, which is traversed recursively.
#' @param values Logical scalar. If \code{FALSE}, metadata names, not values,
#'   are mapped, and \code{classes} is ignored (names are always of class
#'   \sQuote{character}).
#' @param classes Character vector specifying the classes to be mapped; others
#'   are returned unchanged.
#' @param ... Optional argument passed to \code{mapping}.
#' @return \code{\link{WMD}} object with modified metadata.
#' @export
#' @family metadata-functions
#' @keywords manip
#' @examples
#' data(vaas_1)
#' copy <- map_metadata(vaas_1, identity)
#' stopifnot(identical(copy, vaas_1))
#' copy <- map_metadata(vaas_1, identity, values = FALSE)
#' stopifnot(identical(copy, vaas_1))
#' copy <- map_metadata(vaas_1, function(x) paste(x, "!"), values = FALSE)
#' (x <- metadata_chars(vaas_1, values = FALSE))
#' (y <- metadata_chars(copy, values = FALSE))
#' stopifnot(identical(as.character(y), paste(x, "!")))
#'
setMethod("map_metadata", c(WMD, "function"), function(object, mapping, 
    values = TRUE, classes = "ANY", ...) {
  object@metadata <- if (values)
    rapply(object@metadata, f = mapping, classes = classes, how = "replace", 
      ...)
  else
    map_names(object@metadata, mapping = mapping, ...)
  object
}, sealed = SEALED)


#' Map metadata using a character vector
#'
#' Map meta-information stored together with the measurements by using a 
#' \sQuote{character} vector-based mapping.
#'
#' @param object \code{\link{WMD}} object.
#' @param mapping Character vector. See \code{\link{map_values,character}} for
#'   details. \code{\link{metadata_chars}} can be used to create a template 
#'   for such a vector.
#' @param values Logical scalar. If \code{FALSE}, metadata names, not values,
#'   are mapped, and \code{coerce} is ignored (names are always of class
#'   \sQuote{character} and need not be coerced).
#' @param coerce Character vector. See \code{\link{map_values}} for details.
#' @return \code{\link{WMD}} object with modified metadata.
#' @export
#' @family metadata-functions
#' @keywords manip
#' @examples
#' data(vaas_1)
#'
#' # Mapping a value
#' map <- metadata_chars(vaas_1)
#' map["First replicate"] <- "Rep. 1"
#' copy <- map_metadata(vaas_1, map)
#' stopifnot(identical(names(metadata(copy)), names(metadata(vaas_1))))
#' stopifnot(!identical(metadata(copy, "Experiment"), 
#'   metadata(vaas_1, "Experiment")))
#'
#' # Mapping a name
#' map <- metadata_chars(vaas_1, values = FALSE)
#' map["Plate number"] <- "Plate no."
#' copy <- map_metadata(vaas_1, map, values = FALSE)
#' stopifnot(!identical(names(metadata(copy)), names(metadata(vaas_1))))
#'
setMethod("map_metadata", c(WMD, "character"), function(object, mapping,
    values = TRUE, coerce = "factor") {
  object@metadata <- if (values)
    map_values(object@metadata, mapping, coerce)
  else
    map_names(object@metadata, mapping)
  object
}, sealed = SEALED)

  
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
#' result can be used to create a mapping for \code{\link{map_metadata}}.
#'
#' @param object \code{\link{WMD}} object.
#' @param values Logical scalar. If \code{FALSE}, metadata names, not values,
#'   are collected, and \code{coerce} is ignored (names are always of class
#'   \sQuote{character} and need not be coerced).
#' @param coerce Character vector containing the names of classes that should
#'   also be collected (and coerced to \sQuote{character}).
#' @return Character vector, sorted and made unique. Original \code{names}
#'   attributes, if any, are dropped and replaced by the character vector 
#'   itself. (This might be convenient regarding its use with 
#'   \code{\link{map_metadata}}.)
#' @export
#' @family metadata-functions
#' @keywords attribute
#' @examples
#' data(vaas_1)
#' (x <- metadata_chars(vaas_1, values = FALSE))
#' stopifnot(names(x) == x)
#' (x <- metadata_chars(vaas_1, values = TRUE))
#' stopifnot(names(x) == x)
#' # See map_metadata() for a potential usage of the metadata_chars() result
#'
setMethod("metadata_chars", WMD, function(object, values = TRUE, 
    coerce = "factor") {
  if (values) {
    result <- sort(unique(rapply(object@metadata,
      classes = c("character", coerce), how = "unlist", f = as.character)))
    structure(.Data = result, names = result)
  } else
    map_names(object@metadata)
}, sealed = SEALED)
  
  

  
  
