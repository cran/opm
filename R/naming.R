opm_files <- function(what = c("scripts", "testdata", "auxiliary", "demo",
    "doc", "css", "sql", "omnilog", "single", "multiple", "growth")) {
  switch(what <- match.arg(what),
    css = grep("\\.css$", pkg_files(opm_string(), "auxiliary"),
      TRUE, TRUE, TRUE),
    growth = grep("\\.asc(\\.[^.]+)?$",
      pkg_files(opm_string(), "testdata"), TRUE, TRUE, TRUE),
    multiple = grep("Multiple\\.csv(\\.[^.]+)?$",
      pkg_files(opm_string(), "testdata"), TRUE, TRUE, TRUE),
    omnilog = grep("Example(_Old_Style)?_\\d+\\.csv(\\.[^.]+)?$",
      pkg_files(opm_string(), "testdata"), TRUE, TRUE, TRUE),
    single = grep("Multiple\\.csv(\\.[^.]+)?$", pkg_files(opm_string(),
      "testdata"), TRUE, TRUE, TRUE, FALSE, FALSE, TRUE),
    sql = grep("\\.sql$", pkg_files(opm_string(), "auxiliary"),
      TRUE, TRUE, TRUE),
    pkg_files(opm_string(), what)
  )
}

param_names <- function(
    what = c("param.names", "disc.name", "reserved.md.names", "split.at")) {
  case(match.arg(what),
    param.names = CURVE_PARAMS,
    disc.name = DISC_PARAM,
    reserved.md.names = unname(RESERVED_NAMES),
    split.at = RESERVED_NAMES[["parameter"]]
  )
}

select_colors <- function(
    set = c("w3c", "w3c.i", "nora", "nora.i", "brewer", "brewer.i",
      "roseo", "roseo.i")) {
  # Basic colour keywords from http://www.w3.org/TR/css3-color/ (accessed on
  # 29-8-2011), sorted darkest-first.
  w3c_colors <- function() c(black = "#000000", navy = "#000080",
    green = "#008000", maroon = "#800000", blue = "#0000FF", lime = "#00FF00",
    red = "#FF0000", teal = "#008080", purple = "#800080", olive = "#808000",
    gray = "#808080", aqua = "#00FFFF", fuchsia = "#FF00FF",
    yellow = "#FFFF00", silver = "#C0C0C0", white = "#FFFFFF")
  # Names of W3c colors (except white) sorted so as to maximize contrast
  # between adjacent colors. See pkgutils::max_rgb_contrast().
  sorted_w3c_colors <- function() w3c_colors()[c("teal", "purple", "olive",
    "black", "silver", "blue", "lime", "red", "aqua", "fuchsia", "yellow",
    "navy", "green", "maroon", "gray")]
  # Colours manually selected and sorted by Nora Buddruhs for maximum contrast.
  noras_colors <- function() c("midnightblue", "darkred", "darkgreen", "orange",
    "lightslateblue", "seashell4", "saddlebrown", "firebrick2",
    "palevioletred3", "purple4")
  # Shades of pink...
  roseo_colors <- function() c("maroon1", "palevioletred3", "hotpink1",
    "mediumvioletred", "violetred3", "deeppink3", "lightcoral", "pink1",
    "indianred3", "magenta1")
  # Colours from two ColorBrewer palettes, sorted so as to maximize contrast
  # between adjacent colors.
  brewer_colors <- function() c(
    "#CAB2D6", "#A6CEE3", "#80B1D3", "#CCEBC5", "#FDB462", "#8DD3C7",
    "#33A02C", "#B3DE69", "#B15928", "#FF7F00", "#1F78B4", "#B2DF8A",
    "#6A3D9A", "#E31A1C", "#FFED6F", "#FFFF99", "#FB8072", "#FFFFB3",
    "#FDBF6F", "#D9D9D9", "#FB9A99", "#FCCDE5", "#BC80BD", "#BEBADA"
  )
  case(match.arg(set),
    w3c = sorted_w3c_colors(), w3c.i = rev.default(sorted_w3c_colors()),
    nora = noras_colors(), nora.i = rev.default(noras_colors()),
    brewer = brewer_colors(), brewer.i = rev.default(brewer_colors()),
    roseo = roseo_colors(), roseo.i = rev.default(roseo_colors())
  )
}

custom_plate_is <- function(x) grepl("^Custom:", x, TRUE, TRUE)

custom_plate_proper <- function(x) substring(x, 8L, nchar(x))

custom_plate_prepend <- function(x) sprintf("CUSTOM:%s", x)

custom_plate_prepend_full <- function(x) sprintf("CUSTOM_FULL_NAME:%s", x)

custom_plate_normalize_proper <- function(x) {
  x <- sub("\\W+$", "", sub("^\\W+", "", x, FALSE, TRUE), FALSE, TRUE)
  toupper(gsub("\\W+", "-", x, FALSE, TRUE))
}

custom_plate_normalize <- function(x) {
  custom_plate_prepend(custom_plate_normalize_proper(custom_plate_proper(x)))
}

custom_plate_normalize_all <- function(x) {
  x <- ifelse(custom_plate_is(x), custom_plate_proper(x), x)
  custom_plate_prepend(custom_plate_normalize_proper(x))
}

custom_plate_exists <- function(x) {
  exists(x, MEMOIZED)
}

custom_plate_get <- function(x) {
  get(x, MEMOIZED)
}

custom_plate_assert <- function(x, coords) {
  if (custom_plate_exists(x)) {
    if (any(bad <- !coords %in% names(custom_plate_get(x))))
      stop("well coordinate missing from plate type '", x, "': ",
        coords[bad][1L])
  } else
    stop("unknown user-defined plate type: ", x)
  TRUE
}

custom_plate_set <- function(x, value) {
  if (exists(x, MEMOIZED))
    warning("overwriting well map for plate type ", x)
  MEMOIZED[[x]] <- value
  value
}

custom_plate_set_full <- function(x, value) {
  key <- custom_plate_prepend_full(custom_plate_proper(x))
  names(value) <- NULL
  if (exists(key, MEMOIZED) && !identical(value, get(key, MEMOIZED)))
    warning("overwriting full name for plate type ", x)
  MEMOIZED[[key]] <- value
  value
}

normalize_predefined_plate <- function(object, subtype = FALSE) {
  normalize_pm <- function(x, subtype) {
    x <- sub("^PMM", "PM-M", x, FALSE, TRUE)
    x <- sub("^PM-MTOX", "PM-M TOX", x, FALSE, TRUE)
    x <- sub("([A-Z]+)$", if (subtype)
      "-\\1"
    else
      "", x, FALSE, TRUE)
    sub("([^\\d])(\\d)([^\\d]|$)", "\\10\\2\\3", x, FALSE, TRUE)
  }
  normalize_sf <- function(x, subtype) {
    x <- if (subtype)
      sub("-$", "", sub(SP_PATTERN, "\\1-\\2", x, FALSE, TRUE), FALSE, TRUE)
    else
      sub(SP_PATTERN, "\\1", x, FALSE, TRUE)
    x <- sub("^(G|SF)([NP])", "SF-\\2", x, FALSE, TRUE)
    sub("^GENIII", "Gen III", x, FALSE, TRUE)
  }
  result <- toupper(gsub("\\W", "", object, FALSE, TRUE))
  pm <- grepl("^PM(M(TOX)?)?\\d+[A-Z]*$", result, FALSE, TRUE)
  result[pm] <- normalize_pm(result[pm], subtype)
  sf[sf] <- grepl(SP_PATTERN, result[sf <- !pm], FALSE, TRUE)
  result[sf] <- normalize_sf(result[sf], subtype)
  result[bad] <- object[bad <- !(pm | sf)]
  result
}

setGeneric("plate_type", function(object, ...) standardGeneric("plate_type"))

setMethod("plate_type", OPM, function(object, ..., normalize = FALSE,
    subtype = FALSE) {
  plate_type(object = object@csv_data[[CSV_NAMES[["PLATE_TYPE"]]]], ...,
    normalize = normalize, subtype = subtype)
}, sealed = SEALED)

setMethod("plate_type", MOPMX, function(object, ..., normalize = FALSE,
    subtype = FALSE) {
  vapply(X = object@.Data, FUN = plate_type, FUN.VALUE = "", ...,
    normalize = normalize, subtype = subtype)
}, sealed = SEALED)

setMethod("plate_type", "OPM_DB", function(object, ..., normalize = FALSE,
    subtype = FALSE) {
  plate_type(object = object@plates[, "plate_type"], ...,
    normalize = normalize, subtype = subtype)
}, sealed = SEALED)

setMethod("plate_type", "character", function(object, full = FALSE,
    in.parens = TRUE, max = opm_opt("max.chars"), clean = TRUE,
    brackets = FALSE, word.wise = FALSE, paren.sep = " ", downcase = FALSE,
    normalize = TRUE, subtype = FALSE) {
  do_normalize <- function(object, subtype) {
    is.custom <- custom_plate_is(object)
    object[!is.custom] <- normalize_predefined_plate(object[!is.custom],
      subtype)
    object[is.custom] <- custom_plate_normalize(object[is.custom])
    object
  }
  orig_and_full <- function(orig, full.name) {
    if (downcase)
      full.name <- substrate_info(full.name, "downcase")
    if (in.parens)
      add_in_parens(str.1 = orig, str.2 = full.name, max = max,
        clean = clean, brackets = brackets, word.wise = word.wise,
        paren.sep = paren.sep)
    else
      trim_string(str = full.name, max = max, clean = clean,
        word.wise = word.wise)
  }
  expand_predefined <- function(x) {
    pos <- match(x, names(PLATE_MAP))
    ok <- !is.na(pos)
    for (name in x[!ok])
      warning("cannot find full name of plate ", name)
    x[ok] <- orig_and_full(x[ok], PLATE_MAP[pos[ok]])
    x
  }
  expand_custom <- function(x) {
    if (!length(x))
      return(x)
    n <- custom_plate_prepend_full(custom_plate_proper(x))
    ok <- vapply(n, exists, NA, MEMOIZED)
    for (name in x[!ok])
      warning("cannot find full name of plate ", name)
    x[ok] <- orig_and_full(x[ok], unlist(mget(n[ok], MEMOIZED), FALSE, FALSE))
    x
  }
  LL(full, downcase, in.parens, normalize, subtype)
  result <- if (normalize)
    do_normalize(object, subtype)
  else
    object
  if (!full)
    return(result)
  is.custom <- custom_plate_is(result)
  result[!is.custom] <- expand_predefined(result[!is.custom])
  result[is.custom] <- expand_custom(result[is.custom])
  result
}, sealed = SEALED)

setMethod("plate_type", "factor", function(object, ...) {
  map_values(object = object, mapping = plate_type, ...)
}, sealed = SEALED)

setMethod("plate_type", "missing", function(object, ...) {
  x <- ls(MEMOIZED)
  plate_type(c(names(PLATE_MAP), x[custom_plate_is(x)]), ...)
}, sealed = SEALED)

setMethod("plate_type", "logical", function(object, ...) {
  if (is.na(L(object))) {
    x <- ls(MEMOIZED)
    x <- c(names(PLATE_MAP), x[custom_plate_is(x)])
  } else if (object) {
    x <- ls(MEMOIZED)
    x <- x[custom_plate_is(x)]
  } else {
    x <- names(PLATE_MAP)
  }
  plate_type(x, ...)
}, sealed = SEALED)

setGeneric("gen_iii", function(object, ...) standardGeneric("gen_iii"))

setMethod("gen_iii", OPM, function(object, to = "gen.iii") {
  if (custom_plate_is(L(to))) {
    to <- custom_plate_normalize(to)
    custom_plate_assert(to, colnames(object@measurements)[-1L])
  } else
    to <- SPECIAL_PLATES[[match.arg(tolower(to), names(SPECIAL_PLATES))]]
  object@csv_data[[CSV_NAMES[["PLATE_TYPE"]]]] <- to
  object
}, sealed = SEALED)

setMethod("gen_iii", OPMS, function(object, ...) {
  object@plates <- lapply(X = object@plates, FUN = gen_iii, ...)
  object
}, sealed = SEALED)

setMethod("gen_iii", MOPMX, function(object, ...) {
  object@.Data <- mapply(FUN = gen_iii, object = object@.Data, ...,
    MoreArgs = NULL, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  object
}, sealed = SEALED)

setGeneric("register_plate",
  function(object, ...) standardGeneric("register_plate"))

setMethod("register_plate", "character", function(object, ...) {
  x <- do.call(c, lapply(object, function(file) tryCatch(yaml.load_file(file),
    error = function(e) readRDS(file))))
  x <- mapply(FUN = function(d, n) {
      names(d) <- rep.int(n, length(d))
      d
    }, d = x, n = names(x), SIMPLIFY = FALSE, USE.NAMES = FALSE)
  register_plate(do.call(c, x), ...)
}, sealed = SEALED)

setMethod("register_plate", "missing", function(object, ...) {
  register_plate(list(...))
}, sealed = SEALED)

setMethod("register_plate", "list", function(object, ...) {
  valid_names <- function(n) length(n) && !any(is.na(n)) && all(nzchar(n))
  prepare_names <- function(n) {
    if (!valid_names(n))
      stop("all arguments must be validly named")
    n <- ifelse(custom_plate_is(n), custom_plate_proper(n), n)
    custom_plate_normalize_proper(n)
  }
  convert_rectangular_coords <- function(x) {
    if (!length(j <- as.integer(colnames(x))))
      j <- seq_len(ncol(x))
    if (!length(i <- rownames(x)))
      i <- rep(LETTERS, length.out = nrow(x))
    n <- vapply(i, sprintf, character(length(j)), fmt = "%s%02i", j)
    structure(c(t(x)), names = c(n))
  }
  prepare_well_map <- function(x) {
    if (inherits(x, "well_coords_map")) {
      case(ncol(x), stop("'well_coords_map' object has zero columns"),
        x <- x[, 1L], {
          warning("discarding additional columns in 'well_coords_map' object")
          x <- x[, 1L]
        })
    } else if (is.data.frame(x)) {
      for (i in which(vapply(x, is.factor, NA)))
        x[, i] <- as.character(x[, i])
      x <- convert_rectangular_coords(as.matrix(x))
    } else if (is.matrix(x)) {
      x <- convert_rectangular_coords(x)
    } else {
      names(x) <- clean_coords(names(x))
    }
    storage.mode(x) <- "character"
    if (dup <- anyDuplicated(names(x)))
      stop("duplicate well coordinate provided: ", names(x)[dup])
    x
  }
  insert_plate_types <- function(x) {
    named <- vapply(lapply(x, names), valid_names, NA) |
      vapply(x, is.data.frame, NA) | vapply(x, is.matrix, NA)
    if (any(vapply(x, length, 0L) > 1L & !named))
      stop("element unnamed but not of length 1")
    x[named] <- lapply(x[named], prepare_well_map)
    names(x) <- ifelse(named, custom_plate_prepend(names(x)),
      custom_plate_prepend_full(names(x)))
    list2env(x, MEMOIZED)
  }
  remove_plate_types <- function(x) {
    x <- c(custom_plate_prepend(x), custom_plate_prepend_full(x))
    suppressWarnings(rm(list = x, envir = MEMOIZED))
  }
  if (!missing(...))
    warning("arguments other than 'object' are ignored")
  names(object) <- prepare_names(names(object))
  nonempty <- vapply(object, length, 0L) > 0L
  insert_plate_types(object[nonempty])
  remove_plate_types(names(object)[!nonempty])
  structure(nonempty, names = custom_plate_prepend(names(object)))
}, sealed = SEALED)

opm_string <- function(version = FALSE) {
  x <- "opm"
  if (!version)
    return(x)
  if (exists("opm.version", MEMOIZED))
    y <- MEMOIZED$opm.version
  else
    MEMOIZED$opm.version <- y <- tryCatch(
      as.character(packageVersion(x)), error = function(e) {
        warning(sprintf("cannot find %s version", x))
        UNKNOWN_VERSION
      })
  c(x, y)
}

is_cas <- function(x) {
  ms <- function(x, m, i) { # get the substring from the chosen capture
    start <- attr(m, "capture.start")[, i]
    substr(x, start, start + attr(m, "capture.length")[, i] - 1L)
  }
  cmp <- function(digits, check) { # compare check digits
    sum_up <- function(x) sum(seq.int(length(x), 1L) * as.numeric(x)) / 10
    s <- vapply(strsplit(digits, "", TRUE), sum_up, 0)
    abs(s - floor(s) - as.numeric(check) / 10) < .Machine$double.eps ^ 0.5
  }
  m <- regexpr("^(?:CAS\\s+)?(\\d{2,7})-(\\d{2})-(\\d)$", x, TRUE, TRUE)
  f <- attr(m, "match.length") > 0L
  ok <- f & !is.na(x)
  f[ok] <- cmp(paste0(ms(x, m, 1L)[ok], ms(x, m, 2L)[ok]), ms(x, m, 3L)[ok])
  structure(f, names = x)
}

map_param_names <- function(subset = NULL, ci = TRUE, plain = FALSE,
    opm.fast = FALSE, disc = FALSE) {
  part.1 <- as.list(CURVE_PARAMS)
  names(part.1) <- if (opm.fast)
    c("mu", "lambda", "A", "AUC")
  else
    c("mu", "lambda", "A", "integral")
  if (disc)
    part.1$disc <- DISC_PARAM
  if (plain)
    return(part.1)
  if (length(subset) > 0L) {
    subset <- match.arg(subset, part.1, several.ok = TRUE)
    part.1 <- part.1[part.1 %in% subset]
  }
  if (ci) {
    part.2 <- paste(part.1, "CI95 low")
    part.3 <- paste(part.1, "CI95 high")
    if (opm.fast) {
      names(part.2) <- sprintf("%s.ci.low", names(part.1))
      names(part.3) <- sprintf("%s.ci.high", names(part.1))
    } else {
      names(part.2) <- sprintf("ci95.%s.bt.lo", names(part.1))
      names(part.3) <- sprintf("ci95.%s.bt.up", names(part.1))
    }
  } else {
    part.2 <- NULL
    part.3 <- NULL
  }
  if (opm.fast)
    names(part.1) <- sprintf("%s.point.est", names(part.1))
  else
    names(part.1) <- sprintf("%s.spline", names(part.1))
  c(part.1, part.2, part.3)
}

well_index <- function(x, names) {
  if (missing(x))
    TRUE
  else if (is.character(x))
    clean_coords(x)
  else if (inherits(x, "formula"))
    eval(x[[length(x)]], structure(as.list(seq_along(names)), names = names))
  else
    x
}

clean_coords <- function(x) {
  do_clean <- function(x) {
    x <- sub("\\s+$", "", sub("^\\s+", "", x, FALSE, TRUE), FALSE, TRUE)
    sprintf("%s%02i", toupper(substr(x, 1L, 1L)),
      as.integer(sub("^[A-Za-z]+", "", x, FALSE, TRUE)))
  }
  if (any(bad <- !grepl("^[A-Z]\\d{2,2}$", x, FALSE, TRUE)))
    x[bad] <- do_clean(x[bad])
  x
}

clean_plate_positions <- function(x) {
  x <- lapply(strsplit(x, "\\W+", FALSE, TRUE), function(s) s[nzchar(s)])
  n <- as.integer(vapply(x, `[[`, "", 1L))
  x <- toupper(substr(vapply(x, `[`, "", 2L), 1L, 1L))
  x[is.na(x)] <- "?" # Microstation positions are only integers
  sprintf("%02i-%s", n, x)
}

map_well_names <- function(wells, plate, in.parens = FALSE, brackets = FALSE,
    paren.sep = " ", downcase = FALSE, rm.num = FALSE,
    max = opm_opt("max.chars"), ...) {
  if ((L(paren.sep) == "@"))
    return(sprintf("%s@%s", wells, plate))
  if (custom_plate_is(plate)) {
    if (custom_plate_exists(plate))
      res <- custom_plate_get(plate)[wells]
    else
      res <- NULL
  } else {
    if (is.na(pos <- match(plate, colnames(WELL_MAP))))
      res <- NULL
    else
      res <- WELL_MAP[wells, pos, "name"]
  }
  if (is.null(res)) {
    warning("cannot find plate type ", plate)
    return(trim_string(str = wells, max = max, ...))
  }
  if (rm.num)
    res <- remove_concentration(res)
  if (downcase)
    res <- substrate_info(res, "downcase")
  if (in.parens)
    add_in_parens(str.1 = wells, str.2 = res, brackets = brackets,
      paren.sep = paren.sep, max = max, ...)
  else
    trim_string(str = res, max = max, ...)
}

well_to_substrate <- function(x, plate) {
  get_name <- function(x, plate) wells(x, TRUE, FALSE, plate = plate)[, 1L]
  if (length(plate)) {
    if (all(grepl(SUBSTRATE_PATTERN[["any"]], x, FALSE, TRUE)))
      get_name(substr(x, 1L, 3L), plate)
    else
      x # assume plain substrate names without wells as prefix
  } else if (all(grepl("^[A-Z][0-9]{2}@", x, FALSE, TRUE))) {
    plate <- as.factor(substr(x, 5L, nchar(x)))
    pos <- split.default(seq_along(x), plate)
    x <- split.default(substr(x, 1L, 3L), plate)
    x <- mapply(get_name, x, names(x), SIMPLIFY = FALSE)
    result <- character(length(plate))
    for (i in seq_along(x))
      result[pos[[i]]] <- x[[i]]
    result
  } else {
    for (p in SUBSTRATE_PATTERN[c("paren", "bracket")]) {
      m <- regexpr(p, x, FALSE, TRUE)
      if (all(attr(m, "match.length") > 0L))
        return(get_partial_match(1L, m, x))
    }
    x
  }
}

to_sentence <- function(x, ...) UseMethod("to_sentence")

to_sentence.logical <- function(x, html, ...) {
  sentence <- function(x, what) {
    if (length(x)) {
      if (html)
        x <- substrate_info(x, "html")
      sprintf("%s for %s.", what, listing(x, style = "sentence"))
    } else
      ""
  }
  LL(html)
  isna <- is.na(x)
  n <- c("Positive", "Negative", "Ambiguous")
  result <- c(sentence(names(x)[x & !isna], n[1L]),
    sentence(names(x)[!x & !isna], n[2L]), sentence(names(x)[isna], n[3L]))
  if (html)
    result <- sprintf("<div>%s</div>", result)
  names(result) <- n
  result
}

setGeneric("wells", function(object, ...) standardGeneric("wells"))

setMethod("wells", OPM, function(object, full = FALSE, in.parens = TRUE,
    max = opm_opt("max.chars"), brackets = FALSE, clean = TRUE,
    word.wise = FALSE, paren.sep = " ", downcase = FALSE, rm.num = FALSE,
    plate = plate_type(object), simplify = TRUE) {
  LL(full, simplify, plate)
  x <- colnames(object@measurements)[-1L]
  if (!missing(plate))
    plate <- if (custom_plate_is(plate))
      custom_plate_normalize(plate)
    else
      normalize_predefined_plate(plate)
  if (full)
    x <- structure(map_well_names(x, plate, in.parens = in.parens,
      max = max, brackets = brackets, clean = clean, word.wise = word.wise,
      paren.sep = paren.sep, downcase = downcase, rm.num = rm.num), names = x)
  if (simplify)
    return(x)
  x <- matrix(x, length(x), 1L, FALSE, list(names(x), plate))
  class(x) <- "well_coords_map"
  x
}, sealed = SEALED)

setMethod("wells", "ANY", function(object, full = TRUE, in.parens = FALSE,
    max = opm_opt("max.chars"), brackets = FALSE, clean = TRUE,
    word.wise = FALSE, paren.sep = " ", downcase = FALSE, rm.num = FALSE,
    plate = "PM01", simplify = FALSE) {
  LL(full, simplify)
  x <- well_index(object, rownames(WELL_MAP))
  if (!is.character(x))
    x <- rownames(WELL_MAP)[x]
  ok <- is.custom <- custom_plate_is(plate)
  x <- matrix(x, length(x), length(plate), FALSE, list(x, ifelse(is.custom,
    custom_plate_normalize(plate), normalize_predefined_plate(plate))))
  ok[is.custom] <- vapply(colnames(x)[is.custom], custom_plate_exists, NA)
  ok[!is.custom] <- match(colnames(x)[!is.custom], colnames(WELL_MAP), 0L) > 0L
  x[, !ok] <- NA_character_
  if (full)
    for (i in which(ok))
      x[, i] <- map_well_names(x[, i], colnames(x)[i], in.parens = in.parens,
        max = max, brackets = brackets, clean = clean, word.wise = word.wise,
        paren.sep = paren.sep, downcase = downcase, rm.num = rm.num)
  if (simplify && ncol(x) == 1L)
    return(x[, 1L])
  class(x) <- "well_coords_map"
  x
}, sealed = SEALED)

setMethod("wells", "missing", function(object, ...) {
  wells(object = TRUE, ...)
}, sealed = SEALED)

setGeneric("listing")

setOldClass("well_coords_map")

setClass("well_coords_listing", contains = "print_easy")

setMethod("listing", "well_coords_map", function(x) {
  x <- x[!apply(is.na(x), 1L, all), , drop = FALSE]
  result <- structure(vector("list", ncol(x)), names = plate <- colnames(x))
  full <- ifelse(custom_plate_is(plate),
    mget(custom_plate_prepend_full(custom_plate_proper(plate)), MEMOIZED,
    "character", rep.int(list(NA_character_), length(plate))), PLATE_MAP[plate])
  for (i in seq_along(result))
    result[[i]] <- list(full[[i]], as.list(x[, i]))
  class(result) <- c("well_coords_listing", "print_easy")
  result
}, sealed = SEALED)

setMethod("listing", OPMD, function(x, as.groups,
    cutoff = opm_opt("min.mode"), downcase = TRUE, full = TRUE,
    in.parens = FALSE, html = FALSE, sep = " ", ..., exact = TRUE,
    strict = TRUE) {
  res <- to_sentence(discretized(object = x, full = full,
    in.parens = in.parens, downcase = downcase, ...), html)
  if (length(as.groups)) {
    res <- matrix(res, 1L, length(res), FALSE, list(NULL, names(res)))
    rownames(res) <- paste0(metadata(x, as.groups, exact, strict),
      collapse = L(sep))
    attr(res, "cutoff") <- L(cutoff)
    class(res) <- "OPMS_Listing"
  } else
    class(res) <- "OPMD_Listing"
  attr(res, "html") <- html
  res
}, sealed = SEALED)

setMethod("listing", XOPMX, function(x, as.groups, cutoff = opm_opt("min.mode"),
    downcase = TRUE, full = TRUE, in.parens = FALSE, html = FALSE, sep = " ",
    ..., exact = TRUE, strict = TRUE) {
  add_stuff <- function(x, html, cutoff) {
    class(x) <- "OPMS_Listing"
    attr(x, "html") <- html
    attr(x, "cutoff") <- cutoff
    x
  }
  LL(cutoff, sep)
  if (!length(as.groups)) {
    res <- do.call(rbind, lapply(X = plates(x), FUN = listing, html = html,
      downcase = downcase, full = full, in.parens = in.parens,
      as.groups = NULL, ...))
    rownames(res) <- seq_len(nrow(res))
    return(add_stuff(res, html, cutoff))
  }
  res <- extract(object = x, subset = DISC_PARAM, as.groups = as.groups,
    sep = sep, exact = exact, strict = strict, downcase = downcase,
    full = full, in.parens = in.parens, dataframe = FALSE, as.labels = NULL,
    ...)
  res <- vapply(split.default(seq_len(nrow(res)), attr(res, "row.groups")),
    function(idx) to_sentence(reduce_to_mode.matrix(res[idx, , drop = FALSE],
      cutoff, TRUE), html), character(3L))
  add_stuff(t(res), html, cutoff)
}, sealed = SEALED)

setGeneric("find_substrate",
  function(object, ...) standardGeneric("find_substrate"))

setMethod("find_substrate", "character", function(object,
    search = c("exact", "glob", "approx", "regex", "pmatch"), max.dev = 0.2) {
  su <- function(x) lapply(lapply(x, unique.default), sort.int)
  find_name <- function(patterns, ...) {
    su(lapply(X = patterns, FUN = grep, x = WELL_MAP[, , "name"], value = TRUE,
      useBytes = TRUE, ...))
  }
  find_approx <- function(pattern, ...) {
    su(lapply(X = pattern, FUN = agrep, x = WELL_MAP[, , "name"], value = TRUE,
      ignore.case = TRUE, useBytes = TRUE, ...))
  }
  find_partial <- function(pattern) {
    # next step necessary because multiple <partial> matches are never allowed
    table <- unique.default(WELL_MAP[, , "name"])
    found <- table[pmatch(pattern, table, NA_integer_, TRUE)]
    names(found) <- pattern
    lapply(lapply(as.list(found), na.exclude), sort.int)
  }
  result <- case(match.arg(search),
    exact = find_name(object, fixed = TRUE),
    glob = find_name(structure(glob_to_regex(object), names = object),
      ignore.case = TRUE, perl = TRUE),
    regex = find_name(object, ignore.case = TRUE, perl = TRUE),
    approx = find_approx(object, max.distance = max.dev),
    pmatch = find_partial(object)
  )
  names(result) <- object
  class(result) <- c("substrate_match", "print_easy")
  result
}, sealed = SEALED)

setOldClass("substrate_match")

setGeneric("find_positions",
  function(object, ...) standardGeneric("find_positions"))

setMethod("find_positions", "character", function(object, type = NULL, ...) {
  if (length(type) && !identical(type, FALSE)) {
    x <- WELL_MAP[, plate_type(type)[1L], "name"]
    return(structure(names(x)[match(object, x)], names = object))
  }
  plates <- colnames(WELL_MAP)
  sapply(object, FUN = function(name) {
    result <- which(WELL_MAP[, , "name"] == name, arr.ind = TRUE)
    matrix(c(plates[result[, 2L]], rownames(result)), ncol = 2L,
      dimnames = list(NULL, RESERVED_NAMES[c("plate", "well")]))
  }, simplify = FALSE)
}, sealed = SEALED)

setMethod("find_positions", "substrate_match", function(object, ...) {
  rapply(object, f = find_positions, "character", how = "list", ...)
}, sealed = SEALED)

setMethod("find_positions", "list", function(object, ...) {
  rapply(object, f = find_positions, classes = c("character", "factor"),
    how = "list", ...)
}, sealed = SEALED)

setMethod("find_positions", OPM, function(object, type = NULL, ...) {
  object <- wells(object, full = TRUE, in.parens = FALSE)
  if (isTRUE(type))
    structure(names(object), names = object)
  else
    find_positions(object, ...)
}, sealed = SEALED)

setGeneric("substrate_info",
  function(object, ...) standardGeneric("substrate_info"))

setMethod("substrate_info", "character", function(object,
    what = c("cas", "kegg", "drug", "metacyc", "chebi", "mesh", "seed",
      "downcase", "greek", "concentration", "html", "peptide", "peptide2",
      "all"),
    browse = 0L, download = FALSE, ...) {

  find_substrate_id <- function(x) {
    result <- WELL_MAP[, , "substrate_id"][match(x, WELL_MAP[, , "name"])]
    structure(as.integer(result), names = x)
  }

  create_url <- function(x, how) {
    url_base <- c(
      kegg = "http://www.genome.jp/dbget-bin/www_bget?cpd:",
      drug = "http://www.genome.jp/dbget-bin/www_bget?dr:",
      chebi = "http://www.ebi.ac.uk/chebi/searchId.do?chebiId=CHEBI:",
      metacyc = "http://biocyc.org/META/NEW-IMAGE?type=COMPOUND&object=",
      cas = "http://chem.sis.nlm.nih.gov/chemidplus/direct.jsp?regno=",
      mesh = "http://www.ncbi.nlm.nih.gov/mesh/",
      seed = paste0("http://seed-viewer.theseed.org/seedviewer.cgi?",
        "page=CompoundViewer&compound=")
    )
    base <- url_base[[match.arg(how, names(url_base))]]
    x <- sub("^(CAS\\s+|CHEBI:)", "", x, TRUE, TRUE)
    ifelse(is.na(x), NA_character_, paste0(base, vapply(x, URLencode, "")))
  }

  map_words <- function(x, fun, ...) {
    y <- strsplit(x, "\\w+", FALSE, TRUE)
    x <- strsplit(x, "\\W+", FALSE, TRUE)
    bad <- !vapply(x, function(value) nzchar(value[1L]), NA)
    x[bad] <- lapply(x[bad], `[`, i = -1L)
    bad <- vapply(x, length, 0L) < vapply(y, length, 0L)
    x[bad] <- lapply(x[bad], function(value) c(value, ""))
    x <- lapply(X = x, FUN = fun, ...) # fun() must keep the length!
    mapply(paste0, y, x, MoreArgs = list(collapse = ""))
  }

  expand_greek_letters <- function(x) {
    map_words(x, fun = map_values, mapping = GREEK_LETTERS)
  }

  compound_name_to_html <- function(x) {
    x <- gsub("'", "&prime;", safe_labels(x, "html"), FALSE, FALSE, TRUE)
    map_words(x, fun = map_values, mapping = COMPOUND_NAME_HTML_MAP)
  }

  safe_downcase <- function(x) {
    good_case <- function(x) {
      bad <- nchar(x) > 1L # avoid changing acronyms and chemical elements
      bad[bad] <- !grepl("^(pH|[a-z]?[A-Z][A-Z]+|([A-Z][a-z]?\\d*)+)$", x[bad],
        FALSE, TRUE)
      x[bad] <- tolower(x[bad])
      x
    }
    map_words(x, function(y) map_values(good_case(y), GREEK_LETTERS))
  }

  extract_concentration <- function(x) {
    in.parens <- grepl(SUBSTRATE_PATTERN[["either"]], x, FALSE, TRUE)
    x <- ifelse(in.parens, substr(x, 1L, nchar(x) - 1L), x)
    m <- regexpr("(?<=#)\\s*\\d+\\s*$", x, FALSE, TRUE)
    ## The following code is currently not in use because the only plate to
    ## which it is applicable (PM09) does not show regularity anyway. Conversion
    ## to integer would also be problematic because contractions such as 5.5 or
    ## 6.5 are present.
    #if (all(m < 0L)) {
    #  x <- ifelse(in.parens, substr(x, 6L, nchar(x)), x)
    #  m <- regexpr("^(?:\\d+(?:\\.\\d+)?)(?=%|mM)", x, FALSE, TRUE)
    #}
    as.integer(substr(x, m, m + attr(m, "match.length") - 1L))
  }

  parse_peptide <- function(x, remove.L) {
    recognize_full_names <- function(x) {
      m <- regexpr("^(?:[A-Za-z][,-])*[A-Za-z]-", x, FALSE, TRUE)
      result <- AMINO_ACIDS[substr(x, m + attr(m, "match.length"), nchar(x))]
      ok <- !is.na(result)
      prefix <- m > 0L & ok
      m <- substr(x, m, m + attr(m, "match.length") - 1L)
      result[prefix] <- paste0(m[prefix], result[prefix])
      result <- as.list(result)
      result[!ok] <- list(character())
      result
    }
    result <- structure(vector("list", length(x)), names = x)
    x <- remove_concentration(x)
    pat <- "(([A-Za-z][,-])*[A-Za-z]-)?[A-Z][a-z]{2}"
    pat <- sprintf("^%s(-%s)*$", pat, pat)
    ok <- grepl(pat, x, FALSE, TRUE)
    result[ok] <- strsplit(x[ok], "(?<!\\b\\w)-", FALSE, TRUE)
    result[!ok] <- recognize_full_names(x[!ok])
    if (remove.L)
      result <- lapply(result, sub, pattern = "^L-", replacement = "",
        ignore.case = FALSE, perl = TRUE)
    result
  }

  all_information <- function(x) {
    result <- SUBSTRATE_INFO[find_substrate_id(x), , drop = FALSE]
    colnames(result) <- map_values(colnames(result),
      c(METACYC = "MetaCyc", MESH = "MeSH", CHEBI = "ChEBI",
        KEGG = "KEGG compound", DRUG = "KEGG drug"))
    result <- split.data.frame(result, seq_len(nrow(result)))
    result <- lapply(result, function(y) y[, !is.na(y), drop = TRUE])
    class(result) <- c("substrate_data", "print_easy")
    result
  }

  result <- case(what <- match.arg(what),
    all = all_information(object),
    chebi =, drug =, kegg =, metacyc =, mesh =, seed =,
    cas = SUBSTRATE_INFO[find_substrate_id(object), toupper(what)],
    concentration = extract_concentration(object),
    downcase = safe_downcase(object),
    greek = expand_greek_letters(object),
    html = compound_name_to_html(object),
    peptide = parse_peptide(object, TRUE),
    peptide2 = parse_peptide(object, FALSE)
  )
  browse <- must(as.integer(L(browse)))
  if (browse != 0L) {
    result <- create_url(result, what)
    if (browse > 0L)
      lapply(head(result[!is.na(result)], browse), browseURL)
  }
  names(result) <- object
  if (L(download))
    result <- web_query(result, what)
  result
}, sealed = SEALED)

setMethod("substrate_info", "substrate_match", function(object, ...) {
  rapply(object = object, f = substrate_info, classes = "character",
    how = "replace", ...)
}, sealed = SEALED)

setMethod("substrate_info", "list", function(object, ...) {
  rapply(object = object, f = substrate_info, how = "replace",
    classes = c("character", "factor"), ...)
}, sealed = SEALED)

setMethod("substrate_info", OPM, function(object, ...) {
  substrate_info(wells(object, full = TRUE, in.parens = FALSE), ...)
}, sealed = SEALED)

web_query <- function(ids, what = c("kegg", "drug")) {
  get_kegg <- function(x, prepend) {
    compound_object <- function(x) {
      pos <- match(c("EXACT_MASS", "MOL_WEIGHT"), names(x), 0L)
      for (p in pos[pos > 0L])
        x[[p]] <- as.numeric(x[[p]])
      class(x) <- c("kegg_compound", "print_easy")
      x
    }
    chunks <- function(x, n) split.default(x,
      rep(seq_len(ceiling(length(x) / n)), each = n)[seq_along(x)])
    run_keggrest <- function(x, prepend) {
      result <- lapply(chunks(paste0(prepend, x), 10), KEGGREST::keggGet)
      result <- lapply(unlist(result, FALSE), compound_object)
      names(result) <- vapply(result, `[[`, "", "ENTRY")
      found <- match(names(result), x, 0L)
      if (!all(found > 0L))
        stop("KEGG request yielded entries that do not match the query")
      structure(result[found], names = x)
    }
    prepend <- paste0(match.arg(prepend, c("cpd", "drug")), ":")
    got <- get_and_remember(x = x, prefix = "KEGG.", getfun = run_keggrest,
      default = compound_object(list()), prepend = prepend)
    structure(got, names = names(x), class = c("kegg_compounds", "print_easy"))
  }
  case(match.arg(what),
    kegg = get_kegg(ids, "cpd"),
    drug = get_kegg(ids, "drug")
  )
}

NULL

collect.kegg_compounds <- function(x,
    what = c("pathway", "brite", "activity", "exact_mass"),
    missing.na = TRUE, ...) {
  partial_matrix <- function(name, x) {
    convert <- list(
      ACTIVITY = function(x) {
        # notes in brackets make entries more specific; we use both variants
        unique.default(c(x, sub("\\s+\\[.*", "", x, FALSE, TRUE)))
      },
      BRITE = function(x) {
        if (!length(x))
          return(character())
        # remove the starting points of the classifications (which are just
        # their names) and the end points (the substrates themselves)
        m <- attr(regexpr("^\\s+", x, FALSE, TRUE), "match.length")
        x <- x[!(m < 0L | c(m[-1L] < m[-length(m)], TRUE))]
        gsub("\\s+", " ", sub("^\\s+", "", x, FALSE, TRUE), FALSE, TRUE)
      },
      PATHWAY = names,
      EXACT_MASS = function(x) if (is.null(x))
        NA_real_
      else
        x
    )
    result <- lapply(lapply(x, `[[`, name), convert[[name]])
    if (name == "EXACT_MASS")
      matrix(unlist(result), ncol = 1L, dimnames = list(NULL, tolower(name)))
    else
      pkgutils::collect(result, "occurrences")
  }
  what <- toupper(match.arg(what, several.ok = TRUE))
  result <- do.call(cbind, lapply(what, partial_matrix, x))
  if (L(missing.na))
    result[!vapply(x, length, 0L), ] <- as(NA, typeof(result))
  result
}

lapply(c(
    #+
    find_substrate,
    find_positions,
    substrate_info
    #-
  ), FUN = function(func_) {
  setMethod(func_, "factor", function(object, ...) {
    func_(as.character(object), ...)
  }, sealed = SEALED)
})

lapply(c(
    #+
    find_positions,
    substrate_info,
    wells,
    plate_type
    #-
  ), FUN = function(func_) {
  setMethod(func_, OPMS, function(object, ...) {
    func_(object@plates[[1L]], ...)
  }, sealed = SEALED)
})

lapply(c(
    #+
    find_positions,
    substrate_info,
    wells
    #-
  ), FUN = function(func_) {
  setMethod(func_, MOPMX, function(object, ...) {
    lapply(object@.Data, FUN = func_, ...)
  }, sealed = SEALED)
})

