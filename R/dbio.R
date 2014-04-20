setOldClass("RODBC")

setGeneric("opm_dbput",
  function(object, conn, ...) standardGeneric("opm_dbput"))

setMethod("opm_dbput", c("OPM_DB", "DBIConnection"), function(object, conn,
    map.tables = NULL, start = opm_dbnext(object, conn, map.tables)) {
  object <- update(object, start, TRUE)
  by(object, TRUE, dbWriteTable, conn = conn, append = TRUE, row.names = FALSE,
    do_quote = function(x) make.db.names(conn, x), do_map = map.tables)
  object@plates[, "id"]
}, sealed = SEALED)

setMethod("opm_dbput", c("OPM_DB", "RODBC"), function(object, conn,
    map.tables = NULL, start = opm_dbnext(object, conn, map.tables)) {
  object <- update(object, start, TRUE)
  by(object, TRUE, function(n, x, ...) sqlSave(dat = x, tablename = n, ...),
    channel = conn, append = TRUE, test = FALSE, rownames = FALSE, fast = TRUE,
    verbose = FALSE, do_quote = if (attr(conn, "isMySQL"))
      "`"
    else
      "\"", do_map = map.tables, simplify = FALSE)
  object@plates[, "id"]
}, sealed = FALSE)

setMethod("opm_dbput", c("ANY", "ANY"), function(object, conn, ...) {
  opm_dbput(as(object, opm_dbclass(object)), conn, ...)
})

setGeneric("opm_dbclass", function(object) standardGeneric("opm_dbclass"))

setMethod("opm_dbclass", "numeric", function(object) {
  int2dbclass(object)
}, sealed = SEALED)

setMethod("opm_dbclass", OPM, function(object) {
  paste0(class(object), "_DB")
}, sealed = SEALED)

setMethod("opm_dbclass", OPMS, function(object) {
  int2dbclass(all(has_aggr(object)) + all(has_disc(object)))
}, sealed = SEALED)

setMethod("opm_dbclass", MOPMX, function(object) {
  int2dbclass(all(unlist(has_disc(object), FALSE, FALSE)) +
    all(unlist(has_aggr(object), FALSE, FALSE)))
}, sealed = SEALED)

setGeneric("opm_dbfind",
  function(object, conn, ...) standardGeneric("opm_dbfind"))

setMethod("opm_dbfind", c("character", "DBIConnection"), function(object, conn,
    map.tables = NULL) {
  pk <- pkeys(new("OPM_DB"))[1L]
  sql <- sprintf("SELECT %s FROM %s WHERE %s;", make.db.names(conn, pk),
    make.db.names(conn, map_values(names(pk), map.tables)), object)
  ids <- dbGetQuery(conn, sql)
  if (ncol(ids))
    as.integer(ids[, 1L])
  else
    integer()
}, sealed = SEALED)

setMethod("opm_dbfind", c("character", "RODBC"), function(object, conn,
    map.tables = NULL) {
  pk <- pkeys(new("OPM_DB"))[1L]
  char <- if (attr(conn, "isMySQL"))
      "`"
    else
      "\""
  sql <- sprintf("SELECT %s FROM %s WHERE %s;", quote_protected(pk, char),
    quote_protected(map_values(names(pk), map.tables), char), object)
  ids <- sqlQuery(conn, sql)
  if (ncol(ids))
    as.integer(ids[, 1L])
  else
    integer()
}, sealed = SEALED)

setGeneric("opm_dbget",
  function(object, conn, ...) standardGeneric("opm_dbget"))

setMethod("opm_dbget", c("integer", "DBIConnection"), function(object, conn,
    map.tables = NULL, include = 2L) {
  as(by(new(int2dbclass(include)), object, dbGetQuery, conn = conn,
    do_map = map.tables, do_inline = TRUE, simplify = TRUE,
    do_quote = function(x) make.db.names(conn, x)), MOPMX)
}, sealed = SEALED)

setMethod("opm_dbget", c("integer", "RODBC"), function(object, conn,
    map.tables = NULL, include = 2L) {
  as(by(new(int2dbclass(include)), object, sqlQuery, channel = conn,
    do_map = map.tables, do_inline = TRUE, do_quote = if (attr(conn, "isMySQL"))
      "`"
    else
      "\"", stringsAsFactors = FALSE, simplify = TRUE), MOPMX)
}, sealed = FALSE)

setMethod("opm_dbget", c("character", "ANY"), function(object, conn,
    map.tables = NULL, include = 2L) {
  opm_dbget(opm_dbfind(object, conn, map.tables), conn, map.tables, include)
}, sealed = SEALED)

setGeneric("opm_dbnext",
  function(object, conn, ...) standardGeneric("opm_dbnext"))

setMethod("opm_dbnext", c("ANY", "ANY"), function(object, conn,
    map.tables = NULL) {
  opm_dbnext(new(opm_dbclass(object)), conn, map.tables)
}, sealed = SEALED)

setMethod("opm_dbnext", c("OPM_DB", "DBIConnection"), function(object, conn,
    map.tables = NULL) {
  get_last <- function(tn, id, conn) {
    sql <- sprintf("SELECT max(%s) FROM %s;", make.db.names(conn, id),
      make.db.names(conn, tn))
    dbGetQuery(conn, sql)
  }
  db2ids(by(object, TRUE, get_last, conn = conn, do_map = map.tables,
    do_inline = FALSE, simplify = TRUE))
}, sealed = SEALED)

setMethod("opm_dbnext", c("OPM_DB", "RODBC"), function(object, conn,
    map.tables = NULL) {
  get_last <- function(tn, id, conn, char) {
    sql <- sprintf("SELECT max(%s) FROM %s;",
      quote_protected(id, char), quote_protected(tn, char))
    sqlQuery(conn, sql)
  }
  db2ids(by(object, TRUE, get_last, conn = conn,
    char = if (attr(conn, "isMySQL"))
      "`"
    else
      "\"", do_map = map.tables, do_inline = FALSE, simplify = TRUE))
}, sealed = FALSE)

setGeneric("opm_dbclear",
  function(object, conn, ...) standardGeneric("opm_dbclear"))

setMethod("opm_dbclear", c("character", "ANY"), function(object, conn,
    map.tables = NULL) {
  opm_dbclear(opm_dbfind(object, conn, map.tables), conn, map.tables)
}, sealed = SEALED)

setMethod("opm_dbclear", c("integer", "DBIConnection"), function(object, conn,
    map.tables = NULL) {
  pk <- pkeys(new("OPM_DB"))[1L]
  sql <- sprintf("DELETE FROM %s WHERE %s;", make.db.names(conn,
    map_values(names(pk), map.tables)),
    paste(make.db.names(conn, pk), object, sep = " = ", collapse = " OR "))
  invisible(dbGetQuery(conn, sql))
}, sealed = SEALED)

setMethod("opm_dbclear", c("integer", "RODBC"), function(object, conn,
    map.tables = NULL) {
  pk <- pkeys(new("OPM_DB"))[1L]
  char <- if (attr(conn, "isMySQL"))
      "`"
    else
      "\""
  sql <- sprintf("DELETE FROM %s WHERE %s;",
    quote_protected(map_values(names(pk), map.tables), char),
    paste(quote_protected(pk, char), object, sep = " = ", collapse = " OR "))
  invisible(sqlQuery(conn, sql))
}, sealed = SEALED)

setGeneric("opm_dbcheck", function(conn, ...) standardGeneric("opm_dbcheck"))

setMethod("opm_dbcheck", "ANY", function(conn, metadata = NULL,
    time.points = TRUE, wells = TRUE) {
  slots_equal <- function(a, b) {
    fmt <- "%Y-%m-%d %H:%M:%S"
    old <- opm_opt("time.fmt")
    if (!fmt %in% old) {
      on.exit(opm_opt(time.fmt = old))
      opm_opt(time.fmt = c(fmt, old))
    }
    c(sapply(setdiff(slotNames(a), "csv_data"),
      function(n) all.equal(slot(a, n), slot(b, n))),
      all.equal(csv_data(a, normalize = TRUE), csv_data(b, normalize = TRUE)))
  }
  x <- vaas_4[1L:2L, time.points, wells]
  metadata(x) <- structure(list(), names = character())
  if (length(metadata))
    if (is.data.frame(metadata))
      metadata(x) <- metadata
    else
      stop("'metadata' must be empty or a data frame")
  result <- c(last1 = NA, insert = NA, receive = NA, clear = NA, object = NA,
    compare = NA, last2 = NA, samelast = NA)
  storage.mode(result) <- "character"
  step <- 0L
  tryCatch({
    last1 <- opm_dbnext(2L, conn)
    result[[step <- step + 1L]] <- "ok"
    ids <- opm_dbput(x, conn)
    result[[step <- step + 1L]] <- "ok"
    y <- opm_dbget(ids, conn)
    result[[step <- step + 1L]] <- "ok"
    opm_dbclear(ids, conn)
    result[[step <- step + 1L]] <- "ok"
    if (!is(y, MOPMX) || length(y) != 1L)
      stop("expect MOPMX object of length 1")
    result[[step <- step + 1L]] <- "ok"
    last2 <- opm_dbnext(y, conn)
    y <- y[[1L]]
    cmp <- sapply(seq_along(x), function(i) slots_equal(y[i], x[i]))
    if (!is.logical(cmp))
      stop(paste0(cmp, collapse = " / "))
    result[[step <- step + 1L]] <- "ok"
    last3 <- opm_dbnext(2L, conn)
    result[[step <- step + 1L]] <- "ok"
    stopifnot(last1 == last2, last1 == last3)
    result[[step <- step + 1L]] <- "ok"
  }, error = function(e) result[[step + 1L]] <<- conditionMessage(e))
  result
}, sealed = SEALED)

settings_forward <- function(x, plate.id) {
  x$options <- toJSON(x$options)
  x <- x[c("software", "version", "method", "options")]
  data.frame(id = 1L, plate_id = plate.id, x, stringsAsFactors = FALSE,
    check.names = FALSE)
}

settings_backward <- function(x) {
  x <- x[, c("method", "options", "software", "version"), drop = TRUE]
  x$options <- fromJSON(x$options)
  x
}

forward_OPM_to_list <- function(from) {
  p <- data.frame(id = 1L, plate_type = plate_type(from),
    setup_time = csv_data(from, what = "setup_time", normalize = TRUE),
    position = csv_data(from, what = "position", normalize = TRUE),
    machine_id = opm_opt("machine.id"), stringsAsFactors = FALSE,
    csv_data = toJSON(csv_data(from, normalize = TRUE)), check.names = FALSE)
  if (length(md <- from@metadata)) {
    if (any(bad <- names(md) %in% colnames(p)))
      stop("use of forbidden metadata name: ", names(md)[bad][1L])
    p <- cbind(p, md, stringsAsFactors = FALSE)
  }
  w <- wells(from)
  w <- data.frame(id = seq_along(w), plate_id = 1L, coordinate = w,
    stringsAsFactors = FALSE, check.names = FALSE)
  m <- opm::flatten(object = from, numbers = TRUE)
  names(m) <- map_values(names(m), MEASUREMENT_COLUMN_MAP)
  m <- cbind(id = seq.int(nrow(m)), m[, MEASUREMENT_COLUMN_MAP])
  list(plates = p, wells = w, measurements = m)
}

backward_OPM_to_list <- function(from) {
  to_measurements <- function(m, w) {
    m[, "well_id"] <- w[match(m[, "well_id"], w[, "id"]), "coordinate"]
    m <- split.data.frame(m, m[, "well_id"]) # <= probably most time-consuming
    c(list(Hour = sort.int(m[[1]][, "time"])),
      lapply(m, function(x) x[order(x[, "time"]), "value"]))
  }
  if (nrow(p <- from@plates) != 1L)
    stop("object does not contain a single plate")
  list(measurements = to_measurements(from@measurements, from@wells),
    metadata = p[, setdiff(colnames(p), c("id", "plate_type", "setup_time",
      "position", "machine_id", "csv_data")), drop = TRUE],
    csv_data = unlist(fromJSON(p[, "csv_data"])))
}

forward_OPMA_to_list <- function(from) {
  aggr_forward <- function(x, coords) data.frame(id = seq_along(x),
    well_id = rep(match(colnames(x), coords), each = nrow(x)),
    aggr_setting_id = 1L, parameter = rownames(x), value = as.vector(x),
    check.names = FALSE, stringsAsFactors = FALSE)
  x <- forward_OPM_to_list(from)
  x$aggregated <- aggr_forward(from@aggregated, x$wells[, "coordinate"])
  x$aggr_settings <- settings_forward(from@aggr_settings, x$plates[, "id"])
  x
}

backward_OPMA_to_list <- function(from) {
  aggr_backward <- function(a, w) {
    a[, "well_id"] <- w[match(a[, "well_id"], w[, "id"]), "coordinate"]
    a <- split.data.frame(a, a[, "well_id"])
    lapply(a, function(x) structure(x[, "value"], names = x[, "parameter"]))
  }
  c(backward_OPM_to_list(from), list(
    aggr_settings = settings_backward(from@aggr_settings),
    aggregated = aggr_backward(from@aggregated, from@wells)))
}

backward_OPMD_to_list <- function(from) {
  disc_backward <- function(d, w) as.list(structure(as.logical(d[, "value"]),
    names = w[match(d[, "well_id"], w[, "id"]), "coordinate"]))
  c(backward_OPMA_to_list(from), list(
    disc_settings = settings_backward(from@disc_settings),
    discretized = disc_backward(from@discretized, from@wells)))
}

quote_protected <- function(x, s) {
  sprintf(sprintf("%s%%s%s", s, s),
    gsub(s, sprintf("%s%s", s, s), x, FALSE, FALSE, TRUE))
}

int2dbclass <- function(x) {
  paste0(case(x, "OPM", "OPMA", "OPMD"), "_DB")
}

db2ids <- function(x) {
  x <- unlist(x, FALSE, FALSE)
  storage.mode(x) <- "integer"
  x[is.na(x)] <- 0L
  x + 1L
}

