opm_files <- function(what = c("scripts", "testdata", "auxiliary", "demo",
    "examples", "doc", "css", "omnilog", "single", "multiple")) {
  switch(match.arg(what),
    css = grep("\\.css$", pkg_files(opm_string(), "auxiliary"),
      TRUE, TRUE, TRUE),
    examples = {
      warning("'examples' is deprecated, use 'demo'")
      pkg_files(opm_string(), "demo")
    },
    multiple = grep("Multiple\\.csv(\\.[^.]+)?$",
      pkg_files(opm_string(), "testdata"), TRUE, TRUE, TRUE),
    omnilog = grep("Example(_Old_Style)?_\\d+\\.csv(\\.[^.]+)?$",
      pkg_files(opm_string(), "testdata"), TRUE, TRUE, TRUE),
    single = grep("Multiple\\.csv(\\.[^.]+)?$", pkg_files(opm_string(),
      "testdata"), TRUE, TRUE, TRUE, FALSE, FALSE, TRUE),
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

setGeneric("plate_type", function(object, ...) standardGeneric("plate_type"))

setMethod("plate_type", OPM, function(object, ..., normalize = FALSE,
    subtype = FALSE) {
  plate_type(object = object@csv_data[[CSV_NAMES[["PLATE_TYPE"]]]], ...,
    normalize = normalize, subtype = subtype)
}, sealed = SEALED)

setMethod("plate_type", "character", function(object, full = FALSE,
    in.parens = TRUE, max = opm_opt("max.chars"), clean = TRUE,
    brackets = FALSE, word.wise = FALSE, paren.sep = " ", downcase = FALSE,
    normalize = TRUE, subtype = FALSE) {
  do_normalize <- function(object, subtype) {
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
  LL(full, downcase, in.parens, normalize, subtype)
  result <- if (normalize)
    do_normalize(object, subtype)
  else
    object
  if (!full)
    return(result)
  pos <- match(result, names(PLATE_MAP))
  if (any(bad <- is.na(pos))) {
    warning("cannot find full name of plate ", result[bad][1L])
    return(result)
  }
  full.name <- PLATE_MAP[pos]
  if (downcase)
    full.name <- substrate_info(full.name, "downcase")
  if (in.parens)
    result <- add_in_parens(str.1 = result, str.2 = full.name, max = max,
      clean = clean, brackets = brackets, word.wise = word.wise,
      paren.sep = paren.sep)
  else
    result <- trim_string(str = full.name, max = max, clean = clean,
      word.wise = word.wise)
  result
}, sealed = SEALED)

setMethod("plate_type", "factor", function(object, ...) {
  map_values(object = object, mapping = plate_type, ...)
}, sealed = SEALED)

setMethod("plate_type", "missing", function(object, ...) {
  plate_type(names(PLATE_MAP), ...)
}, sealed = SEALED)

setGeneric("gen_iii", function(object, ...) standardGeneric("gen_iii"))

setMethod("gen_iii", OPM, function(object, to = "gen.iii") {
  to <- match.arg(tolower(to), names(SPECIAL_PLATES))
  object@csv_data[[CSV_NAMES[["PLATE_TYPE"]]]] <- SPECIAL_PLATES[[to]]
  object
}, sealed = SEALED)

setMethod("gen_iii", OPMS, function(object, ...) {
  object@plates <- lapply(X = object@plates, FUN = gen_iii, ...)
  object
}, sealed = SEALED)

setMethod("gen_iii", MOPMX, function(object, ...) {
  object@.Data <- lapply(X = object@.Data, FUN = gen_iii, ...)
  object
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
    paren.sep = " ", downcase = FALSE, ...) {
  pos <- match(L(plate), colnames(WELL_MAP))
  if (is.na(pos)) {
    warning("cannot find plate type ", plate)
    return(trim_string(wells, ...))
  }
  res <- WELL_MAP[wells, pos, "name"]
  if (downcase)
    res <- substrate_info(res, "downcase")
  if (in.parens)
    add_in_parens(str.1 = wells, str.2 = res, brackets = brackets,
      paren.sep = paren.sep, ...)
  else
    trim_string(str = res, ...)
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
    word.wise = FALSE, paren.sep = " ", downcase = FALSE,
    plate = plate_type(object)) {
  result <- setdiff(colnames(measurements(object)), HOUR)
  if (L(full))
    map_well_names(result, L(plate), in.parens = in.parens,
      max = max, brackets = brackets, clean = clean, word.wise = word.wise,
      paren.sep = paren.sep, downcase = downcase)
  else
    result
}, sealed = SEALED)

setMethod("wells", "ANY", function(object, full = TRUE, in.parens = FALSE,
    max = opm_opt("max.chars"), brackets = FALSE, clean = TRUE,
    word.wise = FALSE, paren.sep = " ", downcase = FALSE, plate = "PM01") {
  result <- well_index(object, rownames(WELL_MAP))
  if (!is.character(result))
    result <- rownames(WELL_MAP)[result]
  result <- do.call(cbind, rep.int(list(result), length(plate)))
  pos <- pmatch(plate_type(plate), colnames(WELL_MAP))
  colnames(result) <- plate
  if (is.character(object))
    rownames(result) <- object
  if (!L(full))
    return(result)
  for (i in which(!is.na(pos)))
    result[, i] <- map_well_names(result[, i], colnames(WELL_MAP)[pos[i]],
      in.parens = in.parens, max = max, brackets = brackets, clean = clean,
      word.wise = word.wise, paren.sep = paren.sep, downcase = downcase)
  for (i in which(is.na(pos)))
    result[, i] <- NA_character_
  result
}, sealed = SEALED)

setMethod("wells", "missing", function(object, ...) {
  wells(object = TRUE, ...)
}, sealed = SEALED)

setGeneric("listing")

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

setMethod("listing", OPMS, function(x, as.groups, cutoff = opm_opt("min.mode"),
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
    res <- do.call(rbind, lapply(X = x@plates, FUN = listing, html = html,
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
    what = c("cas", "kegg", "drug", "metacyc", "chebi", "mesh", "downcase",
      "greek", "concentration", "html", "peptide", "all"), browse = 0L,
    download = FALSE, ...) {

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
      mesh = "http://www.ncbi.nlm.nih.gov/mesh/"
    )
    base <- url_base[match.arg(how, names(url_base))]
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
    # TODO: maybe extraction method for "mM" and "%" data also useful
    x <- ifelse(grepl(SUBSTRATE_PATTERN[["either"]], x, FALSE, TRUE),
      substr(x, 1L, nchar(x) - 1L), x)
    m <- regexpr("(?<=#)\\s*\\d+\\s*$", x, FALSE, TRUE)
    as.integer(substr(x, m, m + attr(m, "match.length") - 1L))
  }

  parse_peptide <- function(x) {
    parse <- function(x) strsplit(gsub("(?<=\\b[A-Za-z])-", "_", x, FALSE,
      TRUE), "-", TRUE)
    result <- structure(vector("list", length(x)), names = x)
    pat <- sprintf("^%s(-%s)*$", pat <- "([A-Za-z]-)?[A-Z][a-z][a-z]", pat)
    result[ok] <- parse(x[ok <- grepl(pat, x, FALSE, TRUE)])
    result[!ok] <- list(character())
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
    chebi =, drug =, kegg =, metacyc =, mesh =,
    cas = SUBSTRATE_INFO[find_substrate_id(object), toupper(what)],
    concentration = extract_concentration(object),
    downcase = safe_downcase(object),
    greek = expand_greek_letters(object),
    html = compound_name_to_html(object),
    peptide = parse_peptide(object)
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
    what = c("pathway", "brite", "activity", "exact_mass"), min.cov = 2L,
    missing.na = TRUE, ...) {
  partial_matrix <- function(name, x, min.cov) {
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
      pkgutils::collect(result, "occurrences", min.cov)
  }
  what <- toupper(match.arg(what, several.ok = TRUE))
  result <- do.call(cbind, lapply(what, partial_matrix, x, min.cov))
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

