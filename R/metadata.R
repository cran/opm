setGeneric("metadata<-",
  function(object, key, ..., value) standardGeneric("metadata<-"))

setMethod("metadata<-", c(WMD, "missing", FOE), function(object, key,
    value) {
  object@metadata <- map_values(object@metadata, value)
  object
}, sealed = SEALED)
setMethod("metadata<-", c(WMD, "missing", "list"), function(object, key,
    value) {
  object@metadata <- value
  object
}, sealed = SEALED)

setMethod("metadata<-", c(WMD, "missing", "data.frame"), function(object,
    key, value) {
  if (nrow(value) != 1L)
    stop("need data frame with one row")
  object@metadata <- as.list.data.frame(value)
  object
}, sealed = SEALED)

setMethod("metadata<-", c(WMD, "missing", WMD), function(object, key, value) {
  object@metadata <- value@metadata
  object
}, sealed = SEALED)

setMethod("metadata<-", c(WMD, "missing", WMDS), function(object, key, value) {
  stop("lengths of 'object' and 'value' do not fit")
}, sealed = SEALED)

setMethod("metadata<-", c(WMD, "character", "ANY"), function(object, key,
    value) {
  object@metadata[[key]] <- value
  object
}, sealed = SEALED)

setMethod("metadata<-", c(WMD, "character", "data.frame"), function(object, key,
    value) {
  if (nrow(value) != 1L)
    stop("need data frame with one row")
  if (any(found <- key %in% colnames(value))) {
    j <- key[found <- which(found)[1L]]
    key <- key[seq_len(found)] # keys before the 1st => nested indexing
  } else
    j <- TRUE
  object@metadata[[key]] <- value[1L, j, drop = TRUE]
  object
}, sealed = SEALED)

setMethod("metadata<-", c(WMD, "character", "WMD"), function(object, key,
    value) {
  object@metadata[[key]] <- value@metadata
  object
}, sealed = SEALED)

setMethod("metadata<-", c(WMD, "character", "WMDS"), function(object, key,
    value) {
  stop("lengths of 'object' and 'value' do not fit")
}, sealed = SEALED)

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

setMethod("metadata<-", c(WMD, "numeric", "data.frame"), function(object, key,
    value) {
  if (nrow(value) != 1L)
    stop("need data frame with one row")
  metadata(object, key) <- as.list.data.frame(value)
  object
}, sealed = SEALED)

setMethod("metadata<-", c(WMD, "numeric", "WMD"), function(object, key,
    value) {
  metadata(object, key) <- value@metadata
  object
}, sealed = SEALED)

setMethod("metadata<-", c(WMD, "numeric", WMDS), function(object, key, value) {
  stop("lengths of 'object' and 'value' do not fit")
}, sealed = SEALED)

setMethod("metadata<-", c(WMD, "list", "list"), function(object, key, value) {
  if (is.null(names(key)))
    names(key) <- unlist(key, TRUE, FALSE)
  if (is.null(names(value)))
    names(value) <- names(key)
  for (k in names(key))
    object@metadata[[key[[k]]]] <- value[[k]]
  object
}, sealed = SEALED)

setMethod("metadata<-", c(WMD, "list", "data.frame"), function(object, key,
    value) {
  if (nrow(value) != 1L)
    stop("need data frame with one row")
  metadata(object, key) <- as.list.data.frame(value)
  object
}, sealed = SEALED)

setMethod("metadata<-", c(WMD, "list", WMD), function(object, key,
    value) {
  metadata(object, key) <- value@metadata
  object
}, sealed = SEALED)

setMethod("metadata<-", c(WMD, "list", WMDS), function(object, key, value) {
  stop("lengths of 'object' and 'value' do not fit")
}, sealed = SEALED)

setMethod("metadata<-", c(WMD, "ANY", "ANY"), function(object, key,
    value) {
  metadata(object, as.character(key)) <- value
  object
}, sealed = SEALED)

setMethod("metadata<-", c(WMDS, "missing", FOE), function(object, key,
    value) {
  for (i in seq_along(object@plates))
    object@plates[[i]]@metadata <- map_values(object@plates[[i]]@metadata,
      value)
  object
}, sealed = SEALED)

setMethod("metadata<-", c(WMDS, "missing", "list"), function(object, key,
    value) {
  for (i in seq_along(object@plates))
    object@plates[[i]]@metadata <- value
  object
}, sealed = SEALED)

setMethod("metadata<-", c(WMDS, "missing", "data.frame"), function(object,
    key, value) {
  LL(object, .wanted = nrow(value))
  for (i in seq_along(object@plates))
    object@plates[[i]]@metadata <- as.list.data.frame(value[i, , drop = FALSE])
  object
}, sealed = SEALED)

setMethod("metadata<-", c(WMDS, "missing", WMD), function(object, key, value) {
  for (i in seq_along(object@plates))
    metadata(object@plates[[i]]) <- value@metadata
  object
}, sealed = SEALED)

setMethod("metadata<-", c(WMDS, "missing", WMDS), function(object, key,
    value) {
  LL(object, .wanted = length(value))
  for (i in seq_along(object@plates))
    object@plates[[i]]@metadata <- value@plates[[i]]@metadata
  object
}, sealed = SEALED)

setMethod("metadata<-", c(WMDS, "character", WMDS), function(object, key,
    value) {
  LL(object, .wanted = length(value))
  for (i in seq_along(object@plates))
    object@plates[[i]]@metadata[[key]] <- value@plates[[i]]@metadata
  object
}, sealed = SEALED)

setMethod("metadata<-", c(WMDS, "character", "data.frame"), function(object,
    key, value) {
  LL(object, .wanted = nrow(value))
  if (any(found <- key %in% colnames(value))) {
    j <- key[found <- which(found)[1L]]
    key <- key[seq_len(found)]
  } else
    j <- TRUE
  for (i in seq_along(object@plates))
    object@plates[[i]]@metadata[[key]] <- value[i, j, drop = TRUE]
  object
}, sealed = SEALED)

setMethod("metadata<-", c(WMDS, "ANY", "data.frame"), function(object, key,
    value) {
  LL(object, .wanted = nrow(value))
  for (i in seq_along(object@plates))
    metadata(object@plates[[i]], key) <- value[i, , drop = FALSE]
  object
}, sealed = SEALED)

setMethod("metadata<-", c(WMDS, "ANY", WMD), function(object, key, value) {
  for (i in seq_along(object@plates))
    metadata(object@plates[[i]], key) <- value@metadata
  object
}, sealed = SEALED)

setMethod("metadata<-", c(WMDS, "ANY", WMDS), function(object, key, value) {
  LL(object, .wanted = length(value))
  for (i in seq_along(object@plates))
    metadata(object@plates[[i]], key) <- value@plates[[i]]@metadata
  object
}, sealed = SEALED)

setMethod("metadata<-", c(WMDS, "ANY", "ANY"), function(object, key, value) {
  for (i in seq_along(object@plates))
    metadata(object@plates[[i]], key) <- value
  object
}, sealed = SEALED)

setMethod("metadata<-", c(MOPMX, "missing", "ANY"), function(object, key,
    value) {
  for (i in seq_along(object@.Data))
    metadata(object@.Data[[i]]) <- value
  object
}, sealed = SEALED)

setMethod("metadata<-", c(MOPMX, "ANY", "ANY"), function(object, key,
    value) {
  for (i in seq_along(object@.Data))
    metadata(object@.Data[[i]], key) <- value
  object
}, sealed = SEALED)

setMethod("metadata<-", c(MOPMX, "missing", "data.frame"), function(object, key,
    value) {
  indexes <- sub_indexes(object)
  if (nrow(value) != attr(indexes, "total"))
    stop("number of rows in 'value' unequal to number of plates in 'object'")
  for (i in seq_along(object@.Data))
    metadata(object@.Data[[i]]) <- value[indexes[[i]], , drop = FALSE]
  object
}, sealed = SEALED)

setMethod("metadata<-", c(MOPMX, "ANY", "data.frame"), function(object, key,
    value) {
  indexes <- sub_indexes(object)
  if (nrow(value) != attr(indexes, "total"))
    stop("number of rows in 'value' unequal to number of plates in 'object'")
  for (i in seq_along(object@.Data))
    metadata(object@.Data[[i]], key) <- value[indexes[[i]], , drop = FALSE]
  object
}, sealed = SEALED)

setGeneric("include_metadata",
  function(object, ...) standardGeneric("include_metadata"))

setMethod("include_metadata", WMD, function(object, md, keys, replace = FALSE,
    skip.failure = FALSE, remove.keys = TRUE, ...) {

  pick_from <- function(object, selection) {
    matches <- lapply(names(selection), FUN = function(name) {
      m <- lapply(selection[[name]], `==`, y = object[, name])
      apply(do.call(cbind, m), 1L, any)
    })
    matches <- apply(do.call(cbind, matches), 1L, all)
    matches[is.na(matches)] <- FALSE # we get NA from all-NA rows
    object[matches, , drop = FALSE]
  }

  LL(replace, skip.failure, remove.keys)

  selection <- as.list(csv_data(object, keys))

  # Get and check metadata.
  md <- to_metadata(md, ...)
  if (length(absent.keys <- setdiff(keys, colnames(md))))
    stop("key missing in 'metadata': ", absent.keys[1L])

  # Try to select the necessary information from the metadata.
  found <- pick_from(md, selection)
  msg <- case(nrow(found), listing(selection,
      header = "could not find this key/value combination in 'metadata':"),
    NULL, listing(selection,
      header = "the selection resulted in more than one row for:"))

  # Failures.
  if (length(msg))
    if (skip.failure) {
      warning(msg)
      return(object)
    } else {
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

setMethod("include_metadata", OPM, function(object, md,
    keys = opm_opt("csv.keys"), ...) {
  callNextMethod(object = object, md = md, keys = keys, ...)
}, sealed = SEALED)

setMethod("include_metadata", WMDS, function(object, ...) {
  object@plates <- lapply(X = object@plates, FUN = include_metadata, ...)
  object
}, sealed = SEALED)

setMethod("include_metadata", MOPMX, function(object, ...) {
  object@.Data <- lapply(X = object@.Data, FUN = include_metadata, ...)
  object
}, sealed = SEALED)

setGeneric("map_metadata",
  function(object, mapping, ...) standardGeneric("map_metadata"))

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

setMethod("map_metadata", c(WMD, FOE), function(object, mapping,
    values = parent.frame(), classes = NULL) {
  object@metadata <- map_values(object@metadata, mapping, values)
  object
}, sealed = SEALED)

setMethod("map_metadata", c(WMD, "missing"), function(object, mapping,
    values = TRUE, classes = "factor") {
  if (L(values))
    object@metadata <- rapply(object@metadata, function(x) if (all(is.na(x)))
        NULL
      else
        x, "ANY", NULL, "replace")
  object@metadata <- map_values(object@metadata, NULL, classes)
  object
}, sealed = SEALED)

setMethod("map_metadata", c(WMDS, "missing"), function(object, mapping,
    values = TRUE, classes = "factor") {
  object@plates <- lapply(X = object@plates, FUN = map_metadata,
    values = values, classes = classes)
  object
}, sealed = SEALED)

setMethod("map_metadata", c(WMDS, "ANY"), function(object, mapping, ...) {
  object@plates <- lapply(X = object@plates, FUN = map_metadata,
    mapping = mapping, ...)
  object
}, sealed = SEALED)

setMethod("map_metadata", c(MOPMX, "missing"), function(object, mapping,
    values = TRUE, classes = "factor") {
  object@.Data <- lapply(X = object@.Data, FUN = map_metadata,
    values = values, classes = classes)
  object
}, sealed = SEALED)

setMethod("map_metadata", c(MOPMX, "ANY"), function(object, mapping, ...) {
  object@.Data <- lapply(X = object@.Data, FUN = map_metadata,
    mapping = mapping, ...)
  object
}, sealed = SEALED)

setGeneric("map_values")

setMethod("map_values", c("list", "formula"), function(object, mapping,
    coerce = parent.frame()) {
  if (length(mapping) > 2L) {
    right <- eval(mapping[[3L]], object, coerce)
    left <- metadata_key.formula(mapping[-3L], FALSE, envir = coerce)
    if (is.list(left)) {
      right <- rep(right, length.out = length(left))
      for (i in seq_along(left))
        object[[left[[i]]]] <- right[[i]]
    } else
      object[[left]] <- right
    object
  } else
    eval(mapping[[2L]], object, coerce)
}, sealed = SEALED)

setGeneric("edit")

setMethod("edit", WMDX, function(name, ...) {
  metadata(name) <- edit(to_metadata(name), ...)
  map_metadata(name)
}, sealed = SEALED)

setMethod("edit", MOPMX, function(name, ...) {
  for (i in seq_along(name))
    name[[i]] <- edit(name[[i]], ...)
  name
}, sealed = SEALED)

setGeneric("metadata", function(object, ...) standardGeneric("metadata"))

setMethod("metadata", WMD, function(object, key = NULL, exact = TRUE,
    strict = FALSE) {
  LL(exact, strict)
  if (!length(key))
    return(object@metadata)
  key <- metadata_key(key, FALSE)
  fetch_fun <- if (strict)
    function(key) {
      if (is.null(result <- object@metadata[[key, exact = exact]]))
        stop(sprintf("got NULL value when using key '%s'",
          paste0(key, collapse = " -> ")))
      result
    }
  else
    function(key) object@metadata[[key, exact = exact]]
  if (is.list(key))
    sapply(key, fetch_fun, simplify = FALSE)
  else # should be a (character) vector
    fetch_fun(key)
}, sealed = SEALED)

setMethod("metadata", WMDS, function(object, ...) {
  simplify_conditionally(lapply(X = object@plates, FUN = metadata, ...))
}, sealed = SEALED)

setGeneric("metadata_chars",
  function(object, ...) standardGeneric("metadata_chars"))

setMethod("metadata_chars", WMD, function(object, values = TRUE,
    classes = "factor") {
  if (L(values))
    map_values(object@metadata, coerce = classes)
  else
    map_names(object@metadata)
}, sealed = SEALED)

setMethod("metadata_chars", WMDS, function(object, ...) {
  # 2nd call of map_values unifies the vector but keeps the names
  map_values(unlist(lapply(object@plates, FUN = metadata_chars, ...)))
}, sealed = SEALED)

setMethod("metadata_chars", MOPMX, function(object, ...) {
  # 3rd call of map_values unifies the vector but keeps the names
  map_values(unlist(lapply(object@.Data, FUN = metadata_chars, ...)))
}, sealed = SEALED)

